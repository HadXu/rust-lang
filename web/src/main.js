const fs = require('fs');

WebAssembly.instantiate(fs.readFileSync('./monkey.wasm'))
.then(obj => console.log(obj.instance.exports.hello()));

