import { createSignal, createEffect, onCleanup, JSX, onMount, mergeProps, on } from "solid-js";
import { editor as monacoEditor } from "monaco-editor";
import loader, { Monaco } from "@monaco-editor/loader";


export interface MonacoEditorProps {
    language?: string
    value?: string
    loadingState?: JSX.Element
    class?: string
    theme?: monacoEditor.BuiltinTheme | string
    overrideServices?: monacoEditor.IEditorOverrideServices
    width?: string
    height?: string
    options?: monacoEditor.IStandaloneEditorConstructionOptions
    saveViewState?: boolean
    onChange?: (value: string, event: monacoEditor.IModelContentChangedEvent) => void
    onMount?: (monaco: Monaco, editor: monacoEditor.IStandaloneCodeEditor) => void
    onBeforeUnmount?: (monaco: Monaco, editor: monacoEditor.IStandaloneCodeEditor) => void
  }
  
  export const MonacoEditor = (inputProps: MonacoEditorProps) => {
    const props = mergeProps(
      {
        theme: 'vs-dark',
        width: '100%',
        height: '100%',
        saveViewState: true,
      },
      inputProps,
    )
  
    let containerRef!: HTMLDivElement
  
    const [monaco, setMonaco] = createSignal<Monaco>()
    const [editor, setEditor] = createSignal<monacoEditor.IStandaloneCodeEditor>()
  
    let abortInitialization: (() => void) | undefined
    let monacoOnChangeSubscription: any
    let isOnChangeSuppressed = false
  
    onMount(async () => {
      const loadMonaco = loader.init()
  
      abortInitialization = () => loadMonaco.cancel()
  
      try {
        const monaco = await loadMonaco
        const editor = createEditor(monaco)
        setMonaco(monaco)
        setEditor(editor)
        props.onMount?.(monaco, editor)
  
        monaco.editor.defineTheme('honey', {
          base: "vs-dark",
          inherit: true,
          rules: [],
          colors: {
            "editor.background": '#0A101B',
          },
        })
  
        monaco.editor.setTheme("honey");
        // todo: custom language highlighting
        monaco.editor.setModelLanguage(editor.getModel()!, "rust");

        editor.setValue(props.value ?? "");
  
        monacoOnChangeSubscription = editor.onDidChangeModelContent(event => {
          if (!isOnChangeSuppressed) {
            props.onChange?.(editor.getValue(), event)
          }
        })
      } catch (error: any) {
        if (error?.type === 'cancelation') {
          return
        }
  
        console.error('Could not initialize Monaco', error)
      }
    })
  
    onCleanup(() => {
      const _editor = editor()
      if (!_editor) {
        abortInitialization?.()
        return
      }
  
      props.onBeforeUnmount?.(monaco()!, _editor)
      monacoOnChangeSubscription?.dispose()
      _editor.getModel()?.dispose()
      _editor.dispose()
    })
  
    createEffect(
      on(
        () => props.value,
        value => {
          const _editor = editor()
          if (!_editor || typeof value === 'undefined') {
            return
          }
  
          if (_editor.getOption(monaco()!.editor.EditorOption.readOnly)) {
            _editor.setValue(value)
            return
          }
  
          if (value !== _editor.getValue()) {
            isOnChangeSuppressed = true
  
            _editor.executeEdits('', [
              {
                range: _editor.getModel()!.getFullModelRange(),
                text: value,
                forceMoveMarkers: true,
              },
            ])
  
            _editor.pushUndoStop()
            isOnChangeSuppressed = false
          }
        },
        { defer: true },
      ),
    )
  
    createEffect(
      on(
        () => props.options,
        options => {
          editor()?.updateOptions(options ?? {})
        },
        { defer: true },
      ),
    )
  
    createEffect(
      on(
        () => props.theme,
        theme => {
          monaco()?.editor.setTheme(theme)
        },
        { defer: true },
      ),
    )
  
    createEffect(
      on(
        () => props.language,
        language => {
          const model = editor()?.getModel()
          if (!language || !model) {
            return
          }
  
          monaco()?.editor.setModelLanguage(model, language)
        },
        { defer: true },
      ),
    )
  
  
    const createEditor = (monaco: Monaco) => {
  
      return monaco.editor.create(
        containerRef,
        {
          automaticLayout: true,
          ...props.options,
        },
        props.overrideServices,
      )
    }
  
    return (
      <div
        ref={containerRef}
        class={props.class}
        style={{ width: props.width, height: props.height }}
      >
        {props.loadingState}
      </div>
    )
  }