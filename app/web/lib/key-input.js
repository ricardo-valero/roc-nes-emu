// Keyboard -> the platform's Key-enum bitmask. Bit assignments mirror
// platform/Host.roc — keep the two lists in sync.
const KEYMAP = {
  ArrowRight: 0, ArrowLeft: 1, ArrowUp: 2, ArrowDown: 3,
  KeyX: 4, KeyZ: 5, Backspace: 6, Enter: 7,
  Space: 8, ShiftLeft: 9, ShiftRight: 9,
  KeyA: 10, KeyS: 11, KeyD: 12, KeyW: 13, KeyQ: 14, KeyE: 15,
};

export function attachKeys(target = window) {
  let keys = 0;
  target.addEventListener('keydown', (e) => {
    const bit = KEYMAP[e.code];
    if (bit !== undefined) {
      keys |= 1 << bit;
      e.preventDefault();
    }
  });
  target.addEventListener('keyup', (e) => {
    const bit = KEYMAP[e.code];
    if (bit !== undefined) {
      keys &= ~(1 << bit);
      e.preventDefault();
    }
  });
  return () => keys;
}
