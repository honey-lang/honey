import { createSignal, createEffect, onCleanup, JSX, onMount, mergeProps } from "solid-js";
import { editor as Editor } from "monaco-editor";
import loader, { Monaco } from "@monaco-editor/loader";


export interface MonacoEditorProps {
    language?: string
    value?: string
    loadingState?: JSX.Element
    class?: string
    theme?: Editor.BuiltinTheme | string
    overrideServices?: Editor.IEditorOverrideServices
    width?: string
    height?: string
    options?: Editor.IStandaloneEditorConstructionOptions
    saveViewState?: boolean
    onChange?: (value: string, event: Editor.IModelContentChangedEvent) => void
};

const HoneyTheme: Editor.IStandaloneThemeData = {
  base: "vs-dark",
  inherit: true,
  rules: [],
  colors: { "editor.background": "#0A101B" },
};

export const MonacoEditor = (inputProps: MonacoEditorProps) => {
    const props = mergeProps(
      {
        theme: "vs-dark",
        width: "100%",
        height: "100%",
        saveViewState: true,
      },
      inputProps,
    );
    let containerRef!: HTMLDivElement;
  
    const [monaco, setMonaco] = createSignal<Monaco>();
    const [editor, setEditor] = createSignal<Editor.IStandaloneCodeEditor>();
  
    let abortInitialization: (() => void) | undefined;
    let monacoOnChangeSubscription: { dispose: () => void } | undefined;
    let isOnChangeSuppressed = false;
  
    onMount(async () => {
      const loadMonaco = loader.init();
      abortInitialization = () => loadMonaco.cancel();
  
      try {
        const monaco = await loadMonaco;
        const editor = monaco.editor.create(containerRef, {
            automaticLayout: true,
            ...props.options,
          },
          props.overrideServices,
        );
        setMonaco(monaco);
        setEditor(editor);
  
        monaco.editor.defineTheme("honey", HoneyTheme);
        monaco.editor.setTheme("honey");
        // todo: custom language highlighting
        monaco.editor.setModelLanguage(editor.getModel()!, props.language ?? "text");

        editor.setValue(props.value ?? "");
  
        monacoOnChangeSubscription = editor.onDidChangeModelContent(event => {
          if (!isOnChangeSuppressed) props.onChange?.(editor.getValue(), event);
        });
      } catch (error: any) {
        if (error?.type === "cancelation") return;
        console.error("Could not initialize Monaco: ", error);
      }
    });
  
    onCleanup(() => {
      const _editor = editor();
      if (!_editor) {
        abortInitialization?.();
        return
      }
  
      monacoOnChangeSubscription?.dispose();
      _editor.getModel()?.dispose();
      _editor.dispose();
    });
  
    createEffect(() => {
      const _editor = editor();
      if (!_editor || typeof props.value === "undefined") {
        return;
      }
      if (_editor.getOption(monaco()!.editor.EditorOption.readOnly)) {
        _editor.setValue(props.value);
        return;
      }

      if (props.value !== _editor.getValue()) {
        isOnChangeSuppressed = true;
        _editor.executeEdits("", [
          {
            range: _editor.getModel()!.getFullModelRange(),
            text: props.value,
            forceMoveMarkers: true,
          },
        ]);
        _editor.pushUndoStop();
        isOnChangeSuppressed = false;
      }
    });
  
    return (
      <div ref={containerRef} class={props.class} style={{ width: props.width, height: props.height }}>
        {props.loadingState}
      </div>
    );
  }