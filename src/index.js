import EM from "../build/out.js"

addEventListener('fetch', event => {
  event.respondWith(handleRequest(event.request))
})

function parseParams(str) {
  const hashes = str.split('&')
  const params = {}
  hashes.map(hash => {
    const [key, val] = hash.split('=')
    params[key] = decodeURIComponent(val)
  })
  return params
}

async function readBody(body) {
  const reader = body.getReader();
  let chunks = [];
  while(true) {
    const {done, value} = await reader.read();

    if (done) {
      break;
    }
    chunks.push(value);
  }
  return new TextDecoder("utf-8").decode(...chunks);
}

async function handleRequest(request) {
  globalThis.response = {
    status: 200,
    body: "",
  };

  let params = {};
  if (request.body) {
     params = parseParams(await readBody(request.body));
  }

  globalThis.request = {
    params
  };

  const load = new Promise((resolve, reject) => {
    EM({
      instantiateWasm(info, receive) {
        let instance = new WebAssembly.Instance(wasm, info)
        receive(instance)
        return instance.exports
      },
    }).then(module => {
      delete module.then;
      resolve(module);
    });
  })

  try {
    const instance = (await load);
    try {
      instance._entry();
    } catch (e) {
      // emscripten throws an exception when the program terminates, even with 0
      if (e.name !== "ExitStatus") {
        throw e;
      }
    }

    return new Response(globalThis.response.body, {
	status: globalThis.response.status,
	headers: {'Content-Type': 'text/html'}
    });
  } catch (e) {
    console.log(e);
    return new Response(e.stack, { status: 500 });
  }
}
