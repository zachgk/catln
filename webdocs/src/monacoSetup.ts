// Point @monaco-editor/react at the bundled monaco-editor instead of the CDN
// copy it loads by default. monaco-vim imports monaco-editor directly, so its
// keybindings only apply if the editor comes from that same instance.
import * as monaco from 'monaco-editor';
import EditorWorker from 'monaco-editor/esm/vs/editor/editor.worker?worker';
import {loader} from '@monaco-editor/react';

// Only plaintext models are used, so the base editor worker is the only one requested.
self.MonacoEnvironment = {getWorker: () => new EditorWorker()};

loader.config({monaco});
