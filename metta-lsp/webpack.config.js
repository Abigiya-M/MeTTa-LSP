const path = require('path');
const webpack = require('webpack');

const baseConfig = {
  mode: 'production',
  target: 'node',
  externals: {
    vscode: 'commonjs vscode',
    'vscode-languageclient/node': 'commonjs vscode-languageclient/node',
    'vscode-languageserver/node': 'commonjs vscode-languageserver/node',
    'vscode-languageserver-textdocument': 'commonjs vscode-languageserver-textdocument',
  },
  resolve: {
    extensions: ['.ts', '.js'],
  },
  module: {
    rules: [
      {
        test: /\.ts$/,
        exclude: /node_modules/,
        use: {
          loader: 'ts-loader',
          options: {
            configFile: path.resolve(__dirname, 'tsconfig.json'),
          },
        },
      },
    ],
  },
  devtool: 'source-map',
  performance: {
    hints: false,
  },
};

const clientConfig = {
  ...baseConfig,
  entry: './client/src/extension.ts',
  output: {
    filename: 'extension.js',
    path: path.resolve(__dirname, 'client', 'out'),
    libraryTarget: 'commonjs2',
  },
};

const serverConfig = {
  ...baseConfig,
  entry: './server/src/server.ts',
  output: {
    filename: 'server.js',
    path: path.resolve(__dirname, 'server', 'out'),
    libraryTarget: 'commonjs2',
  },
};

module.exports = [clientConfig, serverConfig];
