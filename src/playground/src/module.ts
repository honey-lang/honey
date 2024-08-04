import loadHoney from "./assets/honey.wasm?init";

const honey = await loadHoney({
    env: {
      js_log: (ptr: number, len: number) => {
        const string = decodeString(ptr, len);
        console.log(string);
      },
      js_error(ptr: number, len: number) {
        const string = decodeString(ptr, len);
        console.error(string);
      },
      js_prompt(ptr: number, len: number): number {
        const question = decodeString(ptr, len);
        const result = prompt(question) ?? "";
        return encodeString(result);
      },
      js_random_bytes(count: number): number {
        const ptr = honey.exports.allocU8(count);
        let slice = new Uint8Array(memory.buffer, ptr, count);
        slice = self.crypto.getRandomValues(slice);
        return ptr;
      },
    },
});

const memory = honey.exports.memory as WebAssembly.Memory;

function decodeString(ptr: number, len: number): string {
    const slice = new Uint8Array(memory.buffer, ptr, len);
    return new TextDecoder().decode(slice);
}
  
function encodeString(str: string): number {
    const buffer = new TextEncoder().encode(str);
    const ptr = honey.exports.allocU8(buffer.length + 1);
    const slice = new Uint8Array(memory.buffer, ptr, buffer.length + 1);
    slice.set(buffer);
    slice[buffer.length] = 0;
    return ptr;
}
  
  
export function runHoney(source: string, print_result: boolean = true): string {
    const source_ptr = encodeString(source);
    const len = honey.exports.run(source_ptr, source.length, print_result);
    return decodeString(honey.exports.last_popped as unknown as number, len);
}

globalThis.runHoney = runHoney;