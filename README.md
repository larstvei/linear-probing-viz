# Linear probing

A visualization of [Linear probing](https://en.wikipedia.org/wiki/Linear_probing).

[See it running!](https://larstvei.github.io/linear-probing/)

# Development

Run `lein figwheel` in your terminal. Wait for a while until you see
`Successfully compiled "resources/public/js/main.js"`. Open
[localhost:3449](http://localhost:3449) in your browser.

# Build

Run `lein do clean, cljsbuild once optimized`. This will compile your code and
run Google Closure Compiler with advanced optimizations. Take
`resources/public/index.html` and `resources/public/js/main.js` and upload them
to server of your choice.
