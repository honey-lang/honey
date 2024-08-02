/* @refresh reload */
import "./index.css";
import { render } from "solid-js/web";
import { createSignal } from "solid-js";
import { MonacoEditor } from "./editor";
import { runHoney } from "./module";
import { createShortcut } from "@solid-primitives/keyboard";
const root = document.getElementById("app");

const DefaultCode = `
let a = 1;
let b = 2;

@println("Result: ", a + b);
`.trimStart();

function App() {
  let [input, setInput] = createSignal(DefaultCode);
  let [output, setOutput] = createSignal("");

  const old_log = console.log;
  console.log = (message: string) => {
    // old_log(message);
    setOutput((prev) => prev + message);
  };

  const old_error = console.error;
  console.error = (message: string) => {
    // old_error(message);
    setOutput((prev) => prev + message);
  };

  const run = () => {
    setOutput("");
    const output = runHoney(input());
    if (output === "void") {
      return;
    }
    console.log(output);
  };

  // instinctive save shortcut should run the code
  createShortcut(["Control", "S"], run, { preventDefault: true, requireReset: false });

  return (
    <div class="w-screen h-screen flex flex-col items-center gap-4">
      <div class="flex flex-col items-center p-4">
        <h1 class="text-6xl text-primary font-extrabold uppercase">Honey</h1>
        <p class="text-xl italic">Run Honey code in the browser using WASM</p>
      </div>
      <div id="editor" class="relative">
        <MonacoEditor value={input()} width="95vw" height="60vh" class="resize-y overflow-auto rounded-md" onChange={setInput} options={{
            lineNumbers: "on",
            renderLineHighlight: "none",
            automaticLayout: true,
        }}/>
        <button
          class="btn btn-primary absolute bottom-2 right-6"
          onClick={run}
        >
          Run
        </button>
      </div>
      <div id="output">
        <MonacoEditor value={output()} width="95vw" height="20vh" class="overflow-auto rounded-md" options={{
            lineNumbers: "off",
            renderLineHighlight: "none",
            scrollbar: { vertical: "hidden", verticalScrollbarSize: 0}, 

            readOnly: true,
            domReadOnly: true,
          }} />
      </div>
    </div>
  );
}

render(() => <App/>, root!);
