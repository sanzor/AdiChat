const path = require('path');

module.exports = {
    entry: './ui/ts/app.ts', // Adjust the entry file path to your TypeScript entry file
    mode: 'development', // Set the mode to production or development
    module: {
        rules: [
            {
                test: /\.ts$/, // Use ts-loader for TypeScript files
                use: 'ts-loader',
                exclude: /node_modules/,
            },
        ],
    },
    resolve: {
        extensions: ['.ts', '.js'],
    },
    output: {
        filename: 'bundle.js', // Output bundle file name
        path: path.resolve(__dirname, './ui/dist'), // Output directory
    },
};