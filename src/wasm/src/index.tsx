/* @refresh reload */
import "./index.css";
import { render } from "solid-js/web";
import { createSignal } from "solid-js";
import { MonacoEditor } from "./editor";
import { runHoney } from "./module";
const root = document.getElementById("app");

const DefaultCode = `
let a = 1;
let b = 2;

@println("Result: ", a + b);
`;

function App() {
  let [input, setInput] = createSignal(DefaultCode);
  let [output, setOutput] = createSignal("");

  const old_log = console.log;
  console.log = (message: string) => {
    old_log(message);
    setOutput((prev) => prev + message);
  };

  return (
    <div class="w-screen h-screen flex flex-col items-center gap-4">
      <div class="flex flex-col items-center p-4">
        <h1 class="text-6xl font-extrabold uppercase">Honey</h1>
        <p class="text-xl italic">Run Honey code in the browser using WASM</p>
      </div>
      <MonacoEditor value={input()} height="50vh" width="95vw" onChange={setInput} options={{
        lineNumbers: "on",
        renderLineHighlight: "none",
      }}/>
      <button
        class="btn btn-primary"
        onClick={() => {
          setOutput("");
          const output = runHoney(input());
          if (output === "void") {
            return;
          }
          console.log(output);
        }}
      >
        Run
      </button>
      <div class="flex flex-col items-center p-4 w-[95%] h-1/4">
        <MonacoEditor value={output()} options={{
          lineNumbers: "off",
          renderLineHighlight: "none",
          readOnly: true,
          domReadOnly: true,
        }} />
      </div>
    </div>
  );
}

render(() => <App/>, root!);
