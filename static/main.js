import { keys } from "./keycodes.js";

const container = document.querySelector("#canvas_container");
const canvas = document.querySelector("#canvas");
const gl = canvas.getContext("webgl2", { antialias: true });

const gl_objects = [null];
function storeGlObject(obj) {
  gl_objects.push(obj);
  return gl_objects.length - 1;
}

window.addEventListener('resize', resizeCanvas);

// TODO: could this be done with pure CSS?
function resizeCanvas() {
  const container_width = container.clientWidth;
  const container_height = container.clientHeight;

  const aspect_ratio = wasm_exports.getDesiredAspectRatio();

  // Calculate the maximum canvas size that fits within the container
  // while maintaining the aspect ratio
  let canvas_width, canvas_height;

  if (container_width / aspect_ratio <= container_height) {
    // Width is the limiting factor
    canvas_width = container_width;
    canvas_height = canvas_width / aspect_ratio;
  } else {
    // Height is the limiting factor
    canvas_height = container_height;
    canvas_width = canvas_height * aspect_ratio;
  }

  canvas.width = Math.round(canvas_width);
  canvas.height = Math.round(canvas_height);
  gl.viewport(0, 0, canvas.width, canvas.height);
}

const image_promises = [];

// from https://www.fabiofranchino.com/log/load-an-image-with-javascript-using-await/
export function imageFromUrl(url) {
  return new Promise((resolve, reject) => {
    const img = new Image();
    img.crossOrigin = 'Anonymous'; // to avoid CORS if used with Canvas
    img.src = url
    img.onload = () => {
      resolve(img);
    }
    img.onerror = e => {
      reject(e);
    }
  })
}

const sounds = [];
const loops = [];
var loops_started = false;

class Sound {
  constructor(url) {
    this.loaded = false;
    this.audio = new Audio();
    const that = this;
    this.audio.addEventListener('canplaythrough', () => {
      that.loaded = true;
    })
    this.audio.src = url;
    this.audio.load();
  }
  // TODO: allow playing multiple copies of the same sound at once
  play() {
    if (this.loaded) {
      this.audio.currentTime = 0;
      this.audio.play();
    }
  }
}

class Loop {
  constructor(url) {
    this.loaded = false;
    this.audio = new Audio();
    const that = this;
    this.audio.addEventListener('canplaythrough', () => {
      that.loaded = true;
    })
    this.audio.src = url;
    this.audio.loop = true;
    this.audio.volume = 0;
    this.audio.load();
  }
  setVolume(v) {
    this.audio.volume = v;
  }
}

const text_decoder = new TextDecoder();
const text_encoder = new TextEncoder();

const wasm_memory = new WebAssembly.Memory({
  initial: 10,
});

function wasmMem() {
  // TODO: keep a permanent variable and only change it if '.detached' is true
  return new Uint8Array(wasm_memory.buffer);
}

function getString(ptr, len) {
  return text_decoder.decode(
    wasmMem().subarray(ptr, ptr + len),
  );
}

function getNullTerminatedString(ptr) {
  var len = 0;
  while (wasmMem().at(ptr + len) != 0) {
    len += 1;
  }
  return getString(ptr, len);
}

function storeString(str, ptr, max_len) {
  const bytes = text_encoder.encode(str);
  const wasm_mem = wasmMem();
  if (bytes.length > max_len) {
    console.warn("String too long:", str);
  }
  const len = Math.min(bytes.length, max_len);
  wasm_mem.set(bytes.subarray(0, len), ptr);
  return len;
}

async function getWasm() {
  const wasm_module = await WebAssembly.instantiateStreaming(fetch("main.wasm"), {
    env: {
      memory: wasm_memory,

      // debug
      logInt: (x) => console.log(x),
      logFloat: (arg) => console.log(arg),
      logString: (ptr, len) => console.log(getString(ptr, len)),

      // canvas
      getWidth: () => canvas.width,
      getHeight: () => canvas.height,

      // images
      preloadImage: (url_ptr, url_len) => {
        image_promises.push(imageFromUrl(getString(url_ptr, url_len)));
        return image_promises.length - 1;
      },

      // sound
      loadSound: (url_ptr, url_len) => {
        sounds.push(new Sound(getString(url_ptr, url_len)));
        return sounds.length - 1;
      },
      // isSoundLoaded: (sound_id) => sounds[sound_id].loaded,
      playSound: (sound_id) => sounds[sound_id].play(),
      loadAndStartLoop: (url_ptr, url_len) => {
        loops.push(new Loop(getString(url_ptr, url_len)));
        return loops.length - 1;
      },
      setLoopVolume: (loop_id, v) => loops[loop_id].setVolume(v),

      // webgl2
      createShader: (type) => storeGlObject(gl.createShader(type)),
      shaderSource: (shader, source_ptr, source_len) => gl.shaderSource(gl_objects[shader], "#version 300 es\n\n" + getString(source_ptr, source_len)),
      compileShader: (shader) => gl.compileShader(gl_objects[shader]),
      getShaderParameter: (shader, pname) => gl.getShaderParameter(gl_objects[shader], pname),
      getShaderInfoLog: (shader, buf_ptr, buf_len) => storeString(gl.getShaderInfoLog(gl_objects[shader]), buf_ptr, buf_len),
      createProgram: () => storeGlObject(gl.createProgram()),
      attachShader: (program, shader) => gl.attachShader(gl_objects[program], gl_objects[shader]),
      linkProgram: (program) => gl.linkProgram(gl_objects[program]),
      getProgramParameter: (program, param) => gl.getProgramParameter(gl_objects[program], param),
      getProgramInfoLog: (program, buf_ptr, buf_len) => storeString(gl.getProgramInfoLog(gl_objects[program]), buf_ptr, buf_len),
      useProgram: (program) => gl.useProgram(gl_objects[program]),
      bufferData_size: (target, size, usage) => gl.bufferData(target, size, usage),
      bufferData: (target, data_ptr, data_len, usage) => gl.bufferData(target, wasmMem().subarray(data_ptr, data_ptr + data_len), usage),
      enable: (capability) => gl.enable(capability),
      disable: (capability) => gl.enable(capability),
      blendFunc: (sfactor, dfactor) => gl.blendFunc(sfactor, dfactor),
      // TODO: clear mask
      clear: () => gl.clear(gl.COLOR_BUFFER_BIT),
      clearColor: (r, g, b, a) => gl.clearColor(r, g, b, a),
      deleteShader: (shader) => gl.deleteShader(gl_objects[shader]),
      deleteProgram: (program) => gl.deleteProgram(gl_objects[program]),
      getAttribLocation: (program, name_ptr, name_len) => gl.getAttribLocation(gl_objects[program], getString(name_ptr, name_len)),
      createBuffer: () => storeGlObject(gl.createBuffer()),
      bindBuffer: (target, buffer) => gl.bindBuffer(target, gl_objects[buffer]),
      enableVertexAttribArray: (index) => gl.enableVertexAttribArray(index),
      vertexAttribPointer: (index, size, type, normalized, stride, offset) => gl.vertexAttribPointer(index, size, type, normalized, stride, offset),
      vertexAttribDivisor: (index, divisor) => gl.vertexAttribDivisor(index, divisor),
      createVertexArray: () => storeGlObject(gl.createVertexArray()),
      bindVertexArray: (vao) => gl.bindVertexArray(gl_objects[vao]),
      drawArrays: (mode, first, count) => gl.drawArrays(mode, first, count),
      drawElements: (mode, count, type, offset) => gl.drawElements(mode, count, type, offset),
      drawElementsInstanced: (mode, count, type, offset, instanceCount) => gl.drawElementsInstanced(mode, count, type, offset, instanceCount),
      deleteBuffer: (buffer) => gl.deleteBuffer(gl_objects[buffer]),
      getUniformLocation: (program, name_ptr, name_len) => storeGlObject(gl.getUniformLocation(gl_objects[program], getString(name_ptr, name_len))),
      uniform4f: (location, v0, v1, v2, v3) => gl.uniform4f(gl_objects[location], v0, v1, v2, v3),
      createTexture: () => storeGlObject(gl.createTexture()),
      deleteTexture: (texture) => gl.deleteTexture(gl_objects[texture]),
      bindTexture: (target, texture) => gl.bindTexture(target, gl_objects[texture]),
      texParameteri: (target, pname, param) => gl.texParameteri(target, pname, param),
      texImage2D_basic: (target, level, internalformat, format, type, pixels) => gl.texImage2D(target, level, internalformat, format, type, images[pixels]),
      generateMipmap: (target) => gl.generateMipmap(target),
    },
  });
  return wasm_module.instance.exports;
}

let wasm_exports = await getWasm();
wasm_exports.preload();
resizeCanvas();
document.title = getNullTerminatedString(wasm_exports.getTitle());
const images = await Promise.all(image_promises);
wasm_exports.init();

// TODO(eternal): some more reliable way to detect if it's a hot reloading build
if (location.hostname === "localhost") {
  // HACK: without this, the first hot reloading resets the state
  // wasm_exports = await getWasm();
  // TODO(future): the above hack introduced a new bug where the initial state was lost, investigate

  const ws = new WebSocket("ws://" + location.host);
  ws.onmessage = (event) => {
    if (event.data === "reload") {
      getWasm().then((res) => {
        wasm_exports = res;
        console.log("Reloaded WASM");
        // HACK until hot reloading works properly
        wasm_exports.init();
      });
    }
  };
}

let last_timestamp_millis = 0;
function every_frame(cur_timestamp_millis) {
  const delta_seconds = (cur_timestamp_millis - last_timestamp_millis) / 1000;
  last_timestamp_millis = cur_timestamp_millis;
  wasm_exports.update(delta_seconds);
  requestAnimationFrame(every_frame);
}

requestAnimationFrame(every_frame);

document.addEventListener("pointermove", (ev) => {
  const rect = canvas.getBoundingClientRect();
  wasm_exports.pointermove(ev.clientX - rect.left, ev.clientY - rect.top);
});

document.addEventListener("pointerup", (ev) => {
  wasm_exports.pointerup(ev.button);
});

document.addEventListener("wheel", (ev) => {
  wasm_exports.wheel(ev.deltaY);
});

document.addEventListener("pointerdown", (ev) => {
  wasm_exports.pointerdown(ev.button);

  if (!loops_started && loops.every(x => x.loaded)) {
    loops_started = true;
    loops.forEach(x => x.audio.play());
  }
});

document.addEventListener("contextmenu", (ev) => {
  ev.preventDefault();
  return false;
});

document.addEventListener("keydown", (ev) => {
  if (ev.repeat) return;
  const key_num = keys[ev.code];
  if (key_num !== undefined) {
    wasm_exports.keydown(key_num);
  }
});

document.addEventListener("keyup", (ev) => {
  if (ev.repeat) return;
  const key_num = keys[ev.code];
  if (key_num !== undefined) {
    wasm_exports.keyup(key_num);
  }
});

function rgbToHex(r, g, b, a) {
  return (
    "#" +
    [r, g, b, a]
      .map((num) => {
        const hex = num.toString(16);
        return hex.length === 1 ? "0" + hex : hex;
      })
      .join("")
  );
}
