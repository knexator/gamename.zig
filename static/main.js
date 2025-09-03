import { keys } from "./keycodes.js";

import GUI from './lil-gui.esm.min.js';

const gui = new GUI();
const TWEAKABLE = {};
let params = new URLSearchParams(document.location.search);
if (params.size == 0) gui.hide();

const container = document.querySelector("#canvas_container");
const canvas = document.querySelector("#canvas");
const gl = canvas.getContext("webgl2", { antialias: true, alpha: false });

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


var readers = [null];

class JsReader {
  constructor() {
    this.buffer = new Uint8Array(0);
    this.position = 0;
    this.loaded = false;
    this.null = false;
  }

  /// returns the number of bytes read
  readInto(buf_ptr, buf_len) {
    if (this.null) throw new Error("Null reader");
    if (!this.loaded) throw new Error("Buffer not loaded yet");
    const data_len = Math.min(buf_len, this.buffer.length - this.position);
    if (data_len <= 0) return 0;
    wasmMem().set(
      this.buffer.subarray(this.position, this.position + data_len),
      buf_ptr,
    );
    this.position += data_len;
    return data_len;
  }

  isNull() {
    if (!this.loaded) throw new Error("Not loaded yet");
    return this.null;
  }
}


const image_promises = [];

// from https://www.fabiofranchino.com/log/load-an-image-with-javascript-using-await/
function imageFromUrl(url) {
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

function imageFromBase64(data) {
  return new Promise((resolve, reject) => {
    const img = new Image();
    img.crossOrigin = 'Anonymous'; // to avoid CORS if used with Canvas
    img.src = "data:image/png;base64," + data;
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
  initial: 60,
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

      setCursor: (k) => {
        const cursors = ["default", "grab", "grabbing", "pointer"];
        document.body.style.cursor = cursors[k];
      },

      // tweak
      addTweakableFColor: (index, name_ptr, name_len, starting_r, starting_g, starting_b) => {
        const name = getString(name_ptr, name_len);
        TWEAKABLE[name] = [starting_r, starting_g, starting_b];
        gui.addColor(TWEAKABLE, name).onChange(value => {
          wasm_exports.setTweakableFColor(index, value[0], value[1], value[2]);
        });
      },

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
      imageWidth: (image_id) => images[image_id].width,
      imageHeight: (image_id) => images[image_id].height,
      preloadImageFromBase64Data: (base64_ptr, base64_len) => {
        image_promises.push(imageFromBase64(getString(base64_ptr, base64_len)));
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

      queuedSeconds: () => {
        const rd = Atomics.load(audio_read_ptr, 0);
        const wr = Atomics.load(audio_write_ptr, 0);
        const dst = new Float32Array(sharedAudioBuffer);
        const available_read = (wr - rd + dst.length) % dst.length;
        return available_read / sample_rate;
      },
      enqueueSamples: (src_ptr, src_len) => {
        const src_bytes = wasmMem().subarray(src_ptr, src_ptr + src_len);
        if (src_bytes.byteOffset % 4 !== 0) {
          throw new Error("Byte length is not a multiple of 4, can't interpret as Float32Array");
        }
        const src = new Float32Array(src_bytes.buffer, src_bytes.byteOffset, src_bytes.byteLength / 4);
        const dst = new Float32Array(sharedAudioBuffer);

        const rd = Atomics.load(audio_read_ptr, 0);
        var wr = Atomics.load(audio_write_ptr, 0);

        // one less, since there's a reserved gap
        const available_space = (rd - wr + dst.length) - 1;
        if (available_space < src.length) {
          return;
          // TODO
          // throw new Error("queued too much audio");
        }

        // TODO: use .set
        // if (write_index + src.length < dst.length) {
        //   dst.set(src, write_index);
        // } else {
        //   dst.set(src.subarray(0, dst.length - write_index), write_index);
        //   dst.set(src.subarray(dst.length - write_index));
        // }
        // write_index = (write_index + src.length) % dst.length;

        // easier version
        for (let i = 0; i < src.length; i++) {
          dst[wr] = src[i];
          wr = (wr + 1) % dst.length;
        }

        Atomics.store(audio_write_ptr, 0, wr);
      },

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
      blendFuncSeparate: (srcRGB, dstRGB, srcAlpha, dstAlpha) => gl.blendFuncSeparate(srcRGB, dstRGB, srcAlpha, dstAlpha),
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
      uniform1f: (location, v0) => gl.uniform1f(gl_objects[location], v0),
      createTexture: () => storeGlObject(gl.createTexture()),
      deleteTexture: (texture) => gl.deleteTexture(gl_objects[texture]),
      bindTexture: (target, texture) => gl.bindTexture(target, gl_objects[texture]),
      texParameteri: (target, pname, param) => gl.texParameteri(target, pname, param),
      texImage2D_basic: (target, level, internalformat, format, type, pixels) => gl.texImage2D(target, level, internalformat, format, type, images[pixels]),
      generateMipmap: (target) => gl.generateMipmap(target),

      // storage
      downloadAsFile: async (filename_ptr, filename_len, mime_ptr, mime_len, contents_ptr, contents_len) => {
        if (window.showSaveFilePicker) {
          const contents = getString(contents_ptr, contents_len);
          const handle = await window.showSaveFilePicker({
            // TODO: let gamename.zig choose the id
            id: "gamename_zig",
            startIn: "downloads",
            suggestedName: getString(filename_ptr, filename_len),
          });
          const writable = await handle.createWritable();
          await writable.write(contents);
          await writable.close();
          return;
        } else {
          const blob = new Blob(
            [getString(contents_ptr, contents_len)],
            { type: getString(mime_ptr, mime_len) },
          );
          const url = URL.createObjectURL(blob);

          const a = document.createElement('a');
          a.href = url;
          a.download = getString(filename_ptr, filename_len);
          document.body.appendChild(a);
          a.click();
          document.body.removeChild(a);
          URL.revokeObjectURL(url);
        }
      },
      askUserForFile: () => {
        const js_reader = new JsReader();
        readers.push(js_reader);

        const fileInput = document.createElement('input');
        fileInput.type = 'file';
        fileInput.addEventListener('change', (event) => {
          // WARNING: selecting a null file doesn't trigger it
          const file = event.target.files[0];
          if (file) {
            const reader = new FileReader();
            reader.onload = (e) => {
              const contents = e.target.result;
              js_reader.buffer = text_encoder.encode(contents);
              js_reader.position = 0;
              js_reader.loaded = true;
            };
            reader.readAsText(file);
          } else {
            js_reader.null = true;
            js_reader.loaded = true;
          }
        });
        fileInput.click();

        return readers.length - 1; // Return the index of the reader
      },

      isLoaded: (reader_index) => readers[reader_index].loaded,
      isNull: (reader_index) => readers[reader_index].isNull(),
      readInto: (reader_index, buf_ptr, buf_len) => readers[reader_index].readInto(buf_ptr, buf_len),
    },
  });
  return wasm_module.instance.exports;
}

let wasm_exports = await getWasm();
wasm_exports.preload();
resizeCanvas();
document.title = getNullTerminatedString(wasm_exports.getTitle());
const images = await Promise.all(image_promises);
wasm_exports.init(Math.floor(Math.random() * 2 ** 32));

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

document.addEventListener("pointermove", (ev) => {
  const rect = canvas.getBoundingClientRect();
  wasm_exports.pointermove(ev.clientX - rect.left, ev.clientY - rect.top);
});

document.addEventListener("pointerup", (ev) => {
  const rect = canvas.getBoundingClientRect();
  wasm_exports.pointermove(ev.clientX - rect.left, ev.clientY - rect.top);
  wasm_exports.pointerup(ev.button);
});

document.addEventListener("wheel", (ev) => {
  wasm_exports.wheel(ev.deltaY);
});

document.addEventListener("pointerdown", (ev) => {
  const rect = canvas.getBoundingClientRect();
  wasm_exports.pointermove(ev.clientX - rect.left, ev.clientY - rect.top);
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

requestAnimationFrame(every_frame);

if (false) {
  // at 48000Hz, each 128-sample block is 0.00026[6] seconds long
  // at 60fps, each frame needs 6.25 blocks
  // To be safe, we put room for 512 blocks, around 1 second.
  const sharedAudioBuffer = new SharedArrayBuffer(Float32Array.BYTES_PER_ELEMENT * 128 * 512);
  (new Float32Array(sharedAudioBuffer)).fill(0);

  // see https://github.com/ringtailsoftware/zig-wasm-audio-framebuffer/blob/1f7fc03b94f5692139aa1c404ba6841c92f5b065/src/pcm-processor.js
  const sharedAudioInfoBuffer = new SharedArrayBuffer(Uint32Array.BYTES_PER_ELEMENT * 2);
  const audio_write_ptr = new Uint32Array(sharedAudioInfoBuffer, 0, 1);
  const audio_read_ptr = new Uint32Array(sharedAudioInfoBuffer, 4, 1);
  var sample_rate = 48000;
}

if (false) document.addEventListener('click', async _ => {
  const audioCtx = new AudioContext();

  const processorCode = `
    class MyProcessor extends AudioWorkletProcessor {
      constructor(options) {
        super();
        this.read_index = 0;
        this.buffer = new Float32Array(options.processorOptions.audio_buffer);
        this.write_ptr = new Uint32Array(options.processorOptions.audio_data, 0, 1);
        this.read_ptr = new Uint32Array(options.processorOptions.audio_data, 4, 1);
      }

      process(inputs, outputs, parameters) {
        const output = outputs[0];
        const channel = output[0];

        var rd = Atomics.load(this.read_ptr, 0);
        const wr = Atomics.load(this.write_ptr, 0);
        const available_read = (wr - rd + this.buffer.length) % this.buffer.length;

        if (available_read < channel.length) {
          console.log("UNDERFLOW");  
          return true;
        }

        // TODO: use .set
        for (let i = 0; i < channel.length; i++) {
          channel[i] = this.buffer[rd];
          rd = (rd + 1) % this.buffer.length;

          // channel[i] = this.buffer[this.read_index];
          // this.read_index = (this.read_index + 1) % this.buffer.length;
        }

        Atomics.store(this.read_ptr, 0, rd);

        return true;
      }
    }
    registerProcessor('my-processor', MyProcessor);
  `;

  const blob = new Blob([processorCode], { type: 'application/javascript' });
  const url = URL.createObjectURL(blob);
  await audioCtx.audioWorklet.addModule(url);
  const node = new AudioWorkletNode(audioCtx, 'my-processor', {
    processorOptions: {
      audio_buffer: sharedAudioBuffer,
      audio_data: sharedAudioInfoBuffer
    },
  });
  node.connect(audioCtx.destination);
  sample_rate = audioCtx.sampleRate;
  wasm_exports.setSampleRate(audioCtx.sampleRate);
}, { once: true });
