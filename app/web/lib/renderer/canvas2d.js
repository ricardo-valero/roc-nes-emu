// Canvas2D backend: putImageData. The last-resort fallback.
// correction 1 applies the CGB LCD curve on the CPU (same matrix as the
// shader backends; a few tens of thousands of pixels per frame is fine).
export function create(canvas, w, h, correction = 0) {
  const ctx = canvas.getContext('2d');
  if (!ctx) throw new Error('no 2d context');
  const imageData = ctx.createImageData(w, h);
  return {
    uploadTexture(buffer) {
      if (correction !== 1) { imageData.data.set(buffer); return; }
      const d = imageData.data;
      for (let i = 0; i < buffer.length; i += 4) {
        const r = buffer[i], g = buffer[i + 1], b = buffer[i + 2];
        d[i] = (r * 26 + g * 4 + b * 2) >> 5;
        d[i + 1] = (g * 24 + b * 8) >> 5;
        d[i + 2] = (r * 2 + g * 4 + b * 26) >> 5;
        d[i + 3] = 255;
      }
    },
    renderTexture() { ctx.putImageData(imageData, 0, 0); },
  };
}
