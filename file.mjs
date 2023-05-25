
import {get_token, init} from './pkg/terser_tokens.js'
import {readFileSync} from 'fs'

const file = readFileSync('../terser/dist/bundle.min.js') + ''
init(file, "file.js")

console.time('get_tokens')
let tokens = []
while (1) {
    const tok = get_token()
    console.log(tok)
    tokens.push(tok)
    if (tok.type === "eof") {
        break
    }
}
console.timeEnd('get_tokens')
