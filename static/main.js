const wasm_module = await WebAssembly.instantiateStreaming(fetch("main.wasm"), {
  env: {
    logInt: (x) => console.log(x),
  },
});
const wasm_exports = wasm_module.instance.exports;
wasm_exports.init();

function every_frame() {
  wasm_exports.update();
  requestAnimationFrame(every_frame);
}

requestAnimationFrame(every_frame);
