// The module 'vscode' contains the VS Code extensibility API
// Import the module and reference it with the alias vscode in your code below
import * as path from 'path';
import os from 'os';
import * as vscode from 'vscode';
import {
	LanguageClient,
	LanguageClientOptions,
	ServerOptions
} from 'vscode-languageclient/node';
import which from 'which';

let client: LanguageClient;

// This method is called when your extension is activated
// Your extension is activated the very first time the command is executed
// Using guide https://code.visualstudio.com/api/language-extensions/language-server-extension-guide
// Based on https://github.com/microsoft/vscode-extension-samples/blob/main/lsp-sample/client/src/extension.ts
export async function activate(context: vscode.ExtensionContext) {
	let serverExecutable = await which('catln', {nothrow: true});
	if (!serverExecutable) {
		serverExecutable = await which('catln', {path: path.join(os.homedir(), ".local/bin")});
	}

	const serverOptions: ServerOptions = {
		command: serverExecutable,
		args: ["lsp"]
	};
	const clientOptions: LanguageClientOptions = {
		documentSelector: [{scheme: 'file', language: 'catln'}]
	};

	client = new LanguageClient("catln", serverOptions, clientOptions);
	client.start();

	console.log('Congratulations, your extension "catln" is now active!');

    // Register editor commands for HIE, but only register the commands once at activation.
	const restartCmd = vscode.commands.registerCommand('catln.commands.restartServer', async () => {
		// The code you place here will be executed every time your command is executed
		// Display a message box to the user
		vscode.window.showInformationMessage('Hello World from Catln!');
        client?.info('Stopping the server');
        await client?.stop();
        client?.info('Starting the server');
        client?.start();
	});

	context.subscriptions.push(restartCmd);
}

// This method is called when your extension is deactivated
export async function deactivate() {
	client.stop();
}
