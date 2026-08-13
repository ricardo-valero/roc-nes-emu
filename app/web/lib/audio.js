// Audio path, v2 (pull-paced, ring-free): the wasm host pushes sample
// chunks synchronously via the js_audio_push import; we copy once and post
// the transferable to the AudioWorklet, whose queue is the only buffer.
// The worklet reports queued milliseconds back — that number drives the
// frame pacer in roc-web.js (raylib-style backpressure).
export async function createAudio() {
  let ctx = null, node = null, queuedMs = 0;

  try {
    ctx = new AudioContext({ sampleRate: 48000 });
    await ctx.audioWorklet.addModule(new URL('./audio-worklet.js', import.meta.url));
    node = new AudioWorkletNode(ctx, 'roc-web-audio', { outputChannelCount: [2] });
    node.port.onmessage = (e) => { queuedMs = e.data; };
    node.connect(ctx.destination);
    const resume = () => { ctx.resume(); };
    window.addEventListener('keydown', resume, { once: true });
    window.addEventListener('pointerdown', resume, { once: true });
  } catch (e) {
    console.warn('[roc-web] audio unavailable:', e.message);
  }

  return {
    // view is a Float32Array over wasm memory, only valid during this call
    push(view) {
      if (!node) return;
      const copy = new Float32Array(view);
      node.port.postMessage(copy, [copy.buffer]);
    },
    running: () => ctx?.state === 'running',
    queuedMs: () => queuedMs,
    health() {
      if (!node) return 'no audio';
      return ctx.state === 'running' ? `audio ${queuedMs | 0}ms` : 'audio: press a key';
    },
  };
}
