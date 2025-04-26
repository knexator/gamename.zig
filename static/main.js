const wasm_memory = new WebAssembly.Memory({
  initial: 2,
});

async function getWasm() {
  const wasm_module = await WebAssembly.instantiateStreaming(fetch("main.wasm"), {
    env: {
      memory: wasm_memory,
      logInt: (x) => console.log(x),
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

function every_frame() {
  wasm_exports.update();
  requestAnimationFrame(every_frame);
}

requestAnimationFrame(every_frame);
