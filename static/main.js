import { keys } from "./keycodes.js";

const container = document.querySelector("#canvas_container");
const canvas = document.querySelector("#ctx_canvas");
const ctx = canvas.getContext("2d");

window.addEventListener('resize', resizeCanvas);

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
}

const text_decoder = new TextDecoder();
const text_encoder = new TextEncoder();

const wasm_memory = new WebAssembly.Memory({
  initial: 2,
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

async function getWasm() {
  const wasm_module = await WebAssembly.instantiateStreaming(fetch("main.wasm"), {
    env: {
      memory: wasm_memory,

      // debug
      logInt: (x) => console.log(x),
      logFloat: (arg) => console.log(arg),
      logString: (ptr, len) => console.log(getString(ptr, len)),

      // canvas
      beginPath: () => ctx.beginPath(),
      moveTo: (x, y) => {
        ctx.moveTo(x, y);
        // console.log(x,y);
      },
      lineTo: (x, y) => {
        ctx.lineTo(x, y);
        // console.log(x,y);
      },
      closePath: () => ctx.closePath(),
      fill: () => ctx.fill(),
      stroke: () => ctx.stroke(),
      setLineWidth: (w) => (ctx.lineWidth = w),
      setFillColor: (r, g, b, a) => {
        ctx.fillStyle = rgbToHex(r, g, b, a);
        // console.log('fillstyle:', rgbToHex(r, g, b, a));
      },
      setStrokeColor: (r, g, b, a) => (ctx.strokeStyle = rgbToHex(r, g, b, a)),
      getWidth: () => canvas.width,
      getHeight: () => canvas.height,
    },
  });
  return wasm_module.instance.exports;
}

let wasm_exports = await getWasm();
resizeCanvas();
document.title = getNullTerminatedString(wasm_exports.getTitle());
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
