import * as path from 'path';
import { ExtensionContext, commands, window } from 'vscode';
import {
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
  TransportKind,
} from 'vscode-languageclient/node';

let client: LanguageClient;

export async function activate(context: ExtensionContext) {
  // In the packaged extension the root is metta-lsp/, so server/out/ is at the root level
  const serverModule = context.asAbsolutePath(path.join('server', 'out', 'server.js'));
  const debugOptions = { execArgv: ['--nolazy', '--inspect=6009'] };

  const serverOptions: ServerOptions = {
    run: { module: serverModule, transport: TransportKind.stdio },
    debug: {
      module: serverModule,
      transport: TransportKind.stdio,
      options: debugOptions,
    },
  };

  const clientOptions: LanguageClientOptions = {
    documentSelector: [{ scheme: 'file', language: 'metta' }],
    synchronize: {
      fileEvents: [
        // Watch for MeTTa files
      ],
    },
  };

  client = new LanguageClient(
    'mettaLanguageServer',
    'Metta Language Server',
    serverOptions,
    clientOptions
  );

  // Start the client
  await client.start();

  // Register restart command
  const restartCommand = commands.registerCommand('metta.restartServer', async () => {
    if (client.isRunning()) {
      await client.stop();
    }
    await client.start();
    window.showInformationMessage('Metta Language Server restarted');
  });

  context.subscriptions.push(restartCommand);

  console.log('Metta Language Client activated');
}

export async function deactivate(): Promise<void> {
  if (client && client.isRunning()) {
    await client.stop();
  }
}
