// roc-web entry point: boot a platform app in the page.
//
//   import { start } from './lib/roc-web.js';
//   start('app.wasm', { rom: 'game.gb' });
//
// Everything the page shows — canvas size, display scale, title, renderer —
// comes from the app's Roc config via wasm exports; no dimensions live here.
import { attachKeys } from './key-input.js';
import { attachFileInput } from './file-input.js';
import { createAudio } from './audio.js';

const BACKENDS = { 1: 'webgpu', 2: 'webgl', 3: 'canvas2d' };

async function createRenderer(code, canvas, w, h, correction) {
  const order = code === 0 ? [1, 2, 3] : [code];
  const errors = [];
  for (const c of order) {
    try {
      const mod = await import(`./renderer/${BACKENDS[c]}.js`);
      return { renderer: await mod.create(canvas, w, h, correction), name: BACKENDS[c] };
    } catch (e) {
      errors.push(`${BACKENDS[c]}: ${e.message}`);
    }
  }
  throw new Error(`no renderer available — ${errors.join('; ')}`);
}

export async function start(wasmUrl, opts = {}) {
  const status = opts.status ?? document.body.appendChild(Object.assign(document.createElement('div'), { className: 'roc-web-status' }));
  const canvas = opts.canvas ?? document.body.appendChild(document.createElement('canvas'));
  try {
    await run(wasmUrl, opts, status, canvas);
  } catch (e) {
    status.textContent = String(e);
    throw e;
  }
}

async function run(wasmUrl, opts, status, canvas) {
  status.textContent = 'loading…';

  const audio = await createAudio();
  let hasAudio = false;

  const module = await WebAssembly.compileStreaming(fetch(wasmUrl));
  const memory = new WebAssembly.Memory({ initial: 256, maximum: 16384 });
  const decoder = new TextDecoder();
  const env = {
    memory,
    js_log: (p, l) => console.log('[roc]', decoder.decode(new Uint8Array(memory.buffer, p, l))),
    js_audio_push: (p, l) => {
      hasAudio = true;
      audio.push(new Float32Array(memory.buffer, p, l));
    },
  };
  const instance = await WebAssembly.instantiate(module, { env });
  const x = instance.exports;
  window.__rocweb = { memory, exports: x }; // debug handle

  // App config -> page
  x.configure();
  const w = x.config_width(), h = x.config_height(), scale = x.config_scale();
  document.title = decoder.decode(new Uint8Array(memory.buffer, x.title_ptr(), x.title_len()));
  canvas.width = w;
  canvas.height = h;
  canvas.style.width = `${w * scale}px`;
  canvas.style.height = `${h * scale}px`;
  canvas.style.imageRendering = 'pixelated';

  // ?correction=0|1 overrides the app's configured color correction — an
  // A/B switch for judging the CGB LCD curve against raw output live
  const override = new URLSearchParams(location.search).get('correction');
  const correction = override !== null ? Number(override) : (x.config_correction ? x.config_correction() : 0);
  const { renderer, name: backend } = await createRenderer(x.config_renderer(), canvas, w, h, correction);

  // Input
  const getKeys = attachKeys();

  // File loading: stage bytes into the host buffer and (re)init
  const loadBytes = (bytes) => {
    if (bytes.length > x.rom_max_len()) throw new Error('file exceeds rom_max_len');
    new Uint8Array(memory.buffer, x.rom_ptr(), bytes.length).set(bytes);
    if (x.init(bytes.length) !== 0) throw new Error('init failed');
  };
  if (opts.rom) {
    loadBytes(new Uint8Array(await (await fetch(opts.rom)).arrayBuffer()));
  } else {
    x.init(0);
  }
  attachFileInput(loadBytes);

  // Frame pacing: once the app queues audio and the context is running,
  // emulation locks to the audio clock — each tick runs frames until the
  // worklet holds ~TARGET_MS of samples (raylib-style backpressure,
  // wasmboy's executeFrameAndCheckAudio). Silent apps get 1 frame per rAF.
  const TARGET_MS = 60;
  const MAX_FRAMES_PER_TICK = 4;

  const fb = () => new Uint8Array(memory.buffer, x.framebuffer_ptr(), x.framebuffer_len());
  let frames = 0, last = performance.now();
  function tick() {
    let ran = 0;
    if (hasAudio && audio.running()) {
      while (audio.queuedMs() < TARGET_MS && ran < MAX_FRAMES_PER_TICK) {
        x.render_frame(getKeys());
        ran += 1;
      }
    } else {
      x.render_frame(getKeys());
      ran = 1;
    }
    if (ran > 0) {
      renderer.uploadTexture(fb());
      renderer.renderTexture();
    }
    frames += ran;
    const now = performance.now();
    if (now - last >= 1000) {
      status.textContent = `${backend} · ${frames} fps · ${audio.health()}`;
      frames = 0;
      last = now;
    }
    requestAnimationFrame(tick);
  }
  requestAnimationFrame(tick);
}
