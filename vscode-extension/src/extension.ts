import * as path from 'path';
import { workspace, ExtensionContext } from 'vscode';

import {
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
  TransportKind
} from 'vscode-languageclient/node';

let client: LanguageClient;

// This function is called when your extension is activated.
// Activation happens based on the "activationEvents" in your package.json.
export function activate(context: ExtensionContext) {

  // --- SERVER LAUNCHING LOGIC ---
  // This is the most important part. You need to tell the client how to start your server.

  // OPTION 1: Server is a Node.js script (e.g., written in TypeScript/JavaScript)
  // const serverModule = context.asAbsolutePath(
  //   path.join('server', 'out', 'server.js') // Path to your compiled server entry file
  // );
  // const debugOptions = { execArgv: ['--nolazy', '--inspect=6009'] };

  // OPTION 2: Server is a standalone executable (e.g., written in Rust, Go, C++)
  // This is more flexible. Let's assume your server is an executable named 'my-lambda-server'.
  // You can either bundle it with your extension or require the user to have it in their PATH.
  const serverCommand = '/home/eyjafjallajokull/Desktop/gh/arbitrary-rank-tutorial/dist-newstyle/build/x86_64-linux/ghc-9.10.1/arbitrary-rank-tutorial-0.0.1/build/arralac/arralac'; // <-- IMPORTANT: CHANGE THIS
  const serverArgs: string[] = []; // <-- Add any arguments your server needs

  // ServerOptions configure how to start the server.
  // We use stdio transport, meaning the client and server will talk over stdin/stdout.
  const serverOptions: ServerOptions = {
    command: serverCommand,
    args: serverArgs,
    transport: TransportKind.stdio
  };

  // --- CLIENT CONFIGURATION ---

  // ClientOptions configure the client's behavior.
  const clientOptions: LanguageClientOptions = {
    // Register the server for 'lambda-calculus' documents.
    // This MUST match the "id" you defined in your package.json
    documentSelector: [{ scheme: 'file', language: 'arralac' }],

    // Synchronize the 'files.associations' setting from VS Code to the server.
    // This is useful if the user maps more file extensions to your language.
    synchronize: {
      fileEvents: workspace.createFileSystemWatcher('**/.clientrc')
    }
  };

  // --- CREATE AND START THE CLIENT ---

  // Create the language client.
  client = new LanguageClient(
    'lambdaCalculusLanguageServer', // An internal ID for the client.
    'Lambda Calculus Language Server', // The name displayed to the user.
    serverOptions,
    clientOptions
  );

  console.log('Lambda Calculus extension is now active!');

  // Start the client. This will also launch the server.
  client.start();
}

// This function is called when your extension is deactivated.
export function deactivate(): Thenable<void> | undefined {
  if (!client) {
    return undefined;
  }
  // Stop the client, which will also stop the server process.
  return client.stop();
}