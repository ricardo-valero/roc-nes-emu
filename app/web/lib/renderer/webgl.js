// WebGL2 backend: texSubImage2D upload + fullscreen quad, nearest filtering
// (binjgb's pattern; NPOT textures are fine in WebGL2).
// correction 1 applies the CGB LCD curve (near's matrix, rows sum to 32 so
// grays pass through unchanged) in the fragment shader.
export function create(canvas, w, h, correction = 0) {
  const gl = canvas.getContext('webgl2');
  if (!gl) throw new Error('no WebGL2 context');

  const vs = `#version 300 es
    layout(location = 0) in vec2 pos;
    out vec2 uv;
    void main() {
      uv = vec2(pos.x, 1.0 - pos.y);
      gl_Position = vec4(pos * 2.0 - 1.0, 0.0, 1.0);
    }`;
  const fs = `#version 300 es
    precision mediump float;
    uniform sampler2D tex;
    in vec2 uv;
    out vec4 color;
    void main() {${correction === 1 ? `
      vec3 c = texture(tex, uv).rgb;
      color = vec4(vec3(
        dot(c, vec3(26.0, 4.0, 2.0)),
        dot(c, vec3(0.0, 24.0, 8.0)),
        dot(c, vec3(2.0, 4.0, 26.0))
      ) / 32.0, 1.0);` : ` color = texture(tex, uv);`}
    }`;

  const compile = (type, src) => {
    const s = gl.createShader(type);
    gl.shaderSource(s, src);
    gl.compileShader(s);
    if (!gl.getShaderParameter(s, gl.COMPILE_STATUS)) throw new Error(gl.getShaderInfoLog(s));
    return s;
  };
  const prog = gl.createProgram();
  gl.attachShader(prog, compile(gl.VERTEX_SHADER, vs));
  gl.attachShader(prog, compile(gl.FRAGMENT_SHADER, fs));
  gl.linkProgram(prog);
  if (!gl.getProgramParameter(prog, gl.LINK_STATUS)) throw new Error(gl.getProgramInfoLog(prog));
  gl.useProgram(prog);

  const quad = gl.createBuffer();
  gl.bindBuffer(gl.ARRAY_BUFFER, quad);
  gl.bufferData(gl.ARRAY_BUFFER, new Float32Array([0, 0, 1, 0, 0, 1, 1, 1]), gl.STATIC_DRAW);
  gl.enableVertexAttribArray(0);
  gl.vertexAttribPointer(0, 2, gl.FLOAT, false, 0, 0);

  const texture = gl.createTexture();
  gl.bindTexture(gl.TEXTURE_2D, texture);
  gl.texImage2D(gl.TEXTURE_2D, 0, gl.RGBA, w, h, 0, gl.RGBA, gl.UNSIGNED_BYTE, null);
  gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.NEAREST);
  gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.NEAREST);
  gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_S, gl.CLAMP_TO_EDGE);
  gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_T, gl.CLAMP_TO_EDGE);

  return {
    uploadTexture(buffer) {
      gl.texSubImage2D(gl.TEXTURE_2D, 0, 0, 0, w, h, gl.RGBA, gl.UNSIGNED_BYTE, buffer);
    },
    renderTexture() {
      gl.viewport(0, 0, canvas.width, canvas.height);
      gl.drawArrays(gl.TRIANGLE_STRIP, 0, 4);
    },
  };
}
