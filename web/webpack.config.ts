import MiniCssExtractPlugin from 'mini-css-extract-plugin';
import path from 'path';

const mode = process.env.NODE_ENV || 'development';
const prod = mode === 'production';

module.exports = {
  entry: {
    'build/bundle': ['./src/main.ts']
  },
  resolve: {
    alias: {
      svelte: path.dirname(require.resolve('svelte/package.json'))
    },
    extensions: ['.mjs', '.js', '.tsx', '.ts', '.svelte', '.ttf'],
    mainFields: ['svelte', 'browser', 'module', 'main']
  },
  output: {
    path: path.join(__dirname, '/public'),
    filename: '[name].js',
    chunkFilename: '[name].[id].js',
    publicPath: "/",
  },
  module: {
    rules: [
      {
        test: /\.tsx?$/,
        use: 'ts-loader',
        exclude: /node_modules/,
      },
      {
        test: /\.svelte$/,
        use: {
          loader: 'svelte-loader',
          options: {
            compilerOptions: {
              dev: !prod
            },
            preprocess: require('svelte-preprocess')({
              defaults: {
                script: 'typescript'
              }
            }),
            emitCss: true,
            hotReload: !prod
          }
        }
      },
      {
        test: /\.css$/,
        use: [
          MiniCssExtractPlugin.loader,
          'css-loader'
        ]
      },
	  {
		// required to load monaco
        test: /\.ttf$/,
        use: ['file-loader'],
      },
      {
        // required to prevent errors from Svelte on Webpack 5+
        test: /node_modules\/svelte\/.*\.mjs$/,
        resolve: {
          fullySpecified: false
        }
      },
    ]
  },
  mode,
  plugins: [
    new MiniCssExtractPlugin({
      filename: '[name].css',
      chunkFilename: '[name].[id].css'
    }),
  ],
  devtool: prod ? false : 'source-map',
  devServer: {
    hot: true,
    port: 8080,
    historyApiFallback: true,
    proxy: {
        '/api': {
            target: 'http://localhost:5000',
            secure: false,
            changeOrigin: true,
        }
    }
  }
};