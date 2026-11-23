// Loaded by WebViewSvgRenderer into a page that already has plotly.js.
// callback is a Java object: done(svg) on success, failed(message) on any error.
function renderFigure(json, callback) {
    try {
        let figure = JSON.parse(json);
        let layout = figure.layout || {};
        // 700x500 are orca's defaults, so report images keep their size
        Plotly.toImage(figure, {
            format: 'svg',
            width: layout.width || 700,
            height: layout.height || 500,
            imageDataOnly: true
        })
            .then(svg => callback.done(svg))
            .catch(err => callback.failed(describe(err)));
    } catch (err) {
        callback.failed(describe(err));
    }
}

// WebKit's stack does not include the error message
function describe(err) {
    return String(err) + '\n' + (err && err.stack || '');
}
