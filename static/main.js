const container = document.querySelector("#canvas_container");
const canvas = document.querySelector("#ctx_canvas");
const ctx = canvas.getContext("2d");

canvas.width = canvas.clientWidth;
canvas.height = canvas.clientHeight;

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
wasm_exports.init();

// TODO(eternal): some more reliable way to detect if it's a hot reloading build
if (location.hostname === "localhost") {
  // HACK: without this, the first hot reloading resets the state
  wasm_exports = await getWasm();

  const ws = new WebSocket("ws://" + location.host);
  ws.onmessage = (event) => {
    if (event.data === "reload") {
      getWasm().then((res) => {
        wasm_exports = res;
        console.log("Reloaded WASM");
      });
    }
  };
}

let last_timestamp_millis = 0;
function every_frame(cur_timestamp_millis) {
  const delta_seconds = (cur_timestamp_millis - last_timestamp_millis) / 1000;
  last_timestamp_millis = cur_timestamp_millis;

  if (
    canvas.width !== canvas.clientWidth ||
    canvas.height !== canvas.clientHeight
  ) {
    canvas.width = canvas.clientWidth;
    canvas.height = canvas.clientHeight;
  }

  wasm_exports.update();
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
