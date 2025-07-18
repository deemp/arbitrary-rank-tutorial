import { workspace, ExtensionContext, window } from "vscode";

import {
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
  TransportKind,
} from "vscode-languageclient/node";
import which from "which";

let client: LanguageClient;

// This function is called when the extension is activated.
// Activation happens based on the "activationEvents" in the package.json.
export function activate(context: ExtensionContext) {
  // --- SERVER LAUNCHING ---

  const serverExecutableName = "arralac";
  let serverCommand: string = "";
  const serverArgs: string[] = ["language-server"];

  // Find the executable in the system's PATH.
  try {
    // which.sync throws an error if the command is not found.
    serverCommand = which.sync(serverExecutableName);
  } catch (err) {
    // If the executable is not found, show an error message and exit.
    window.showErrorMessage(
      `The '${serverExecutableName}' executable was not found in your PATH. Please make sure it is installed.`,
    );
    // Stop the activation.
    return;
  }

  // ServerOptions configure how to start the server.
  // We use stdio transport, meaning the client and server will talk over stdin/stdout.
  const serverOptions: ServerOptions = {
    command: serverCommand,
    args: serverArgs,
    transport: TransportKind.stdio,
  };

  // --- CLIENT CONFIGURATION ---

  // ClientOptions configure the client's behavior.
  const clientOptions: LanguageClientOptions = {
    // Register the server for 'arralac' documents.
    // This MUST match the "id" defined in the package.json
    documentSelector: [{ scheme: "file", language: "arralac" }],

    // Synchronize the 'files.associations' setting from VS Code to the server.
    // This is useful if the user maps more file extensions to the language.
    synchronize: {
      fileEvents: workspace.createFileSystemWatcher("**/.clientrc"),
    },
  };

  // --- CREATE AND START THE CLIENT ---

  // Create the language client.
  client = new LanguageClient(
    "arralacLanguageServer", // An internal ID for the client.
    "Arralac Language Server", // The name displayed to the user.
    serverOptions,
    clientOptions,
  );

  console.log("Arralac extension is now active!");

  // Start the client. This will also launch the server.
  client.start();
}

// This function is called when the extension is deactivated.
export function deactivate(): Thenable<void> | undefined {
  if (!client) {
    return undefined;
  }
  // Stop the client, which will also stop the server process.
  return client.stop();
}
