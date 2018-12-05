const path = require('path');
const webpack = require('webpack');

var config = {
  target: "node",
  mode: "development",
  entry: {
    index: ['./lib/js/src/example/example_index.bs.js'],
  },
  output: {
    path: path.join(__dirname, "build"),
    filename: '[name].js',
  },
  module: {
    rules: [
      {
        test: /\.js$/,
        exclude: /(node_modules|bower_components)/,
        use: {
          loader: 'babel-loader',
          options: {
            presets: ['env'],
          }
        }
      },
    ],
  },
};

module.exports = [config];
