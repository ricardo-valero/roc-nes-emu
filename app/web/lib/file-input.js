// Runtime file loading: drag-and-drop anywhere, or a picker button.
// Hands raw bytes to the callback — what they mean is the app's business.
export function attachFileInput(onBytes) {
  const pick = document.body.appendChild(Object.assign(document.createElement('input'), {
    type: 'file',
    className: 'roc-web-file',
  }));
  const load = async (file) => {
    const bytes = new Uint8Array(await file.arrayBuffer());
    console.log(`[roc-web] loading "${file.name}" (${bytes.length} bytes)`);
    onBytes(bytes);
  };

  pick.addEventListener('change', () => {
    if (pick.files[0]) load(pick.files[0]);
  });

  window.addEventListener('dragover', (e) => e.preventDefault());
  window.addEventListener('drop', (e) => {
    e.preventDefault();
    const file = e.dataTransfer?.files?.[0];
    if (file) load(file);
  });
}
