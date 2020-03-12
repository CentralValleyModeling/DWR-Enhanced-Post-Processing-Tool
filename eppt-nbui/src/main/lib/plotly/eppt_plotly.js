/*
 * Enhanced Post Processing Tool (EPPT) Copyright (c) 2020.
 *
 * EPPT is copyrighted by the State of California, Department of Water Resources. It is licensed
 * under the GNU General Public License, version 2. This means it can be
 * copied, distributed, and modified freely, but you may not restrict others
 * in their ability to copy, distribute, and modify it. See the license below
 * for more details.
 *
 * GNU General Public License
 */
//DEBUG flag to render plot on page load
const DEBUG = true;
var FORMATTER = '';
const PLOTLY_FONT = {
    family: 'Lucida Grande", "Lucida Sans Unicode", "Verdana", "Arial", "Helvetica", "sans-serif',
    color: 'black',
};

const PLOTLY_LINE_DASH_STYLES = ['solid', 'dashdot', 'dot'];

function buildModeBarButtons(graphDiv) {
    let lineButton = {
        name: 'Properties',
        icon: {
            'width': 24,
            'height': 24,
            'path': 'M12,8A4,4 0 0,1 16,12A4,4 0 0,1 12,16A4,4 0 0,1 8,12A4,4 0 0,1 12,8M12,10A2,2 0 0,0 10,12A2,2 0 0,0 12,14A2,2 0 0,0 14,12A2,2 0 0,0 12,10M10,22C9.75,22 9.54,21.82 9.5,21.58L9.13,18.93C8.5,18.68 7.96,18.34 7.44,17.94L4.95,18.95C4.73,19.03 4.46,18.95 4.34,18.73L2.34,15.27C2.21,15.05 2.27,14.78 2.46,14.63L4.57,12.97L4.5,12L4.57,11L2.46,9.37C2.27,9.22 2.21,8.95 2.34,8.73L4.34,5.27C4.46,5.05 4.73,4.96 4.95,5.05L7.44,6.05C7.96,5.66 8.5,5.32 9.13,5.07L9.5,2.42C9.54,2.18 9.75,2 10,2H14C14.25,2 14.46,2.18 14.5,2.42L14.87,5.07C15.5,5.32 16.04,5.66 16.56,6.05L19.05,5.05C19.27,4.96 19.54,5.05 19.66,5.27L21.66,8.73C21.79,8.95 21.73,9.22 21.54,9.37L19.43,11L19.5,12L19.43,13L21.54,14.63C21.73,14.78 21.79,15.05 21.66,15.27L19.66,18.73C19.54,18.95 19.27,19.04 19.05,18.95L16.56,17.95C16.04,18.34 15.5,18.68 14.87,18.93L14.5,21.58C14.46,21.82 14.25,22 14,22H10M11.25,4L10.88,6.61C9.68,6.86 8.62,7.5 7.85,8.39L5.44,7.35L4.69,8.65L6.8,10.2C6.4,11.37 6.4,12.64 6.8,13.8L4.68,15.36L5.43,16.66L7.86,15.62C8.63,16.5 9.68,17.14 10.87,17.38L11.24,20H12.76L13.13,17.39C14.32,17.14 15.37,16.5 16.14,15.62L18.57,16.66L19.32,15.36L17.2,13.81C17.6,12.64 17.6,11.37 17.2,10.2L19.31,8.65L18.56,7.35L16.15,8.39C15.38,7.5 14.32,6.86 13.12,6.62L12.75,4H11.25Z',
        },
        click: () => openNav(graphDiv)
    };
    return [[lineButton], ['zoom2d', 'pan2d', 'zoomIn2d', 'zoomOut2d', 'resetScale2d'],
        ['toggleSpikelines', 'hoverClosestCartesian', 'hoverCompareCartesian']];
}

const subtractLight = function (color, amount) {
    let cc = parseInt(color, 16) - amount;
    let c = (cc < 0) ? 0 : (cc);
    c = (c.toString(16).length > 1) ? c.toString(16) : `0${c.toString(16)}`;
    return c;
};

const darken = (color, amount) => {
    color = (color.indexOf("#") >= 0) ? color.substring(1, color.length) : color;
    amount = parseInt((255 * amount) / 100);
    return color = `#${subtractLight(color.substring(0, 2), amount)}${subtractLight(color.substring(2, 4), amount)}${subtractLight(color.substring(4, 6), amount)}`;
};

function plotData(layout, dataList) {
    let main = document.getElementById("main");
    let plots = [];
    for (let i = 0; i < dataList.length; i++) {
        let plot = document.createElement("div");
        plots.push(plot);
        plot.id = 'plot' + i;
        main.appendChild(plot);
        Plotly.newPlot(plot.id, dataList[i], layout[i], {
            displaylogo: false,
            modeBarButtons: buildModeBarButtons(plot.id),
            scrollZoom: true,
            responsive: true
        });
        $("#" + plot.id).mousedown((ev) => {
            if (ev.which === 3) {
                openContextMenu('#' + plot.id, ev, plotlyCopyToClipboard, plotlyExportFunction(plot));
            }
        });
        plot.on('plotly_relayout',
            function (eventdata) {
                if (eventdata['xaxis.range[0]']) {
                    for (let i = 0; i < plots.length; i++) {
                        if (plots[i].id != plot.id) {
                            let curentRange = plots[i]['_fullLayout']['xaxis']['range'];
                            if (curentRange[0] !== eventdata['xaxis.range[0]']
                                && curentRange[1] !== eventdata['xaxis.range[1]']) {
                                Plotly.relayout(plots[i].id, {
                                    'xaxis.range': [eventdata['xaxis.range[0]'], eventdata['xaxis.range[1]']]
                                });
                            }
                        }
                    }
                }else if(eventdata['xaxis.autorange']){
                    for (let i = 0; i < plots.length; i++) {
                        if (plots[i].id != plot.id) {
                            let curentRange = plots[i]['_fullLayout']['xaxis']['range'];
                            if (curentRange !== plot['_fullLayout']['xaxis']['range']) {
                                Plotly.relayout(plots[i].id, {
                                    'xaxis.range': plot['_fullLayout']['xaxis']['range']
                                });
                            }
                        }
                    }
                }
            });
    }
}


function openContextMenu(id, e, copyToClipboard, plotlyExportToFormat) {
    // prevent the browsers default context menu form appearing.
    e.preventDefault();
    $.contextMenu({
        selector: id,
        build: function ($triggerElement, e) {
            return {
                items: {
                    "export": {
                        name: "Export",
                        items: {
                            "SVG": {
                                name: "To SVG",
                                callback: function (key, options) {
                                    plotlyExportToFormat('svg');
                                }
                            },
                            "PNG": {
                                name: "To PNG",
                                callback: function (key, options) {
                                    plotlyExportToFormat('png');
                                }
                            },
                            "JPEG": {
                                name: "To JPEG",
                                callback: function (key, options) {
                                    plotlyExportToFormat('jpeg');
                                }
                            },
                            "PDF": {
                                name: "To PDF",
                                callback: function (key, options) {
                                    plotlyExportToFormat('pdf');
                                }
                            }
                        }
                    },
                    "copy": {
                        name: "Copy Data",
                        callback: function (key, options) {
                            copyToClipboard();
                        }
                    },
                }
            }
        }
    });

}

var javaObj;

function plotlyExportFunction(plot) {
    return (format) => {
        let width = plot.offsetWidth;
        let height = plot.offsetHeight;
        //javaObj instantiated from JavaFX
        if (javaObj) {
            javaObj.interruptFunction(format, JSON.stringify(plot.data), JSON.stringify(plot.layout), width, height);
        }
        Plotly.downloadImage(plot, {format: format, height: height, width: width});
    }
}

function copyTextToClipboard(text) {
    let textArea = document.createElement("textarea");
    textArea.value = text.replace(/<br>/g, ' ').replace(/<b>/g, '').replace(/<\/b>/g, '');
    document.body.appendChild(textArea);
    textArea.focus();
    textArea.select();
    document.execCommand('copy');
    document.body.removeChild(textArea);
}

function openSecondNav(option, plot) {
    var secondSidenav = document.getElementById("secondSidenav");
    if (!secondSidenav) {
        secondSidenav = document.createElement("div");
        secondSidenav.id = "secondSidenav";
        secondSidenav.className = "second-sidenav";
        document.body.appendChild(secondSidenav);
    }
    while (secondSidenav.firstChild) {
        secondSidenav.removeChild(secondSidenav.firstChild);
    }
    secondSidenav.style.width = "350px";
    document.getElementById("main").style.marginLeft = "600px";
    let plotTemplate = Plotly.makeTemplate(plot);

    function addDomainAxesOptions() {
        let domain = document.createElement("div");
        domain.appendChild(document.createTextNode("Domain"));
        domain.className = "secondary-nav-title";

        let domainTextField = document.createElement("input");
        domainTextField.type = "text";
        let currentXAxisTitle = plotTemplate.layout.xaxis.title;
        if (currentXAxisTitle) {
            domainTextField.value = currentXAxisTitle.text;
        }
        domainTextField.addEventListener('input', (event) => {
            Plotly.relayout(plot, {"xaxis.title.text": event.target.value});
        });
        let domainLabel = document.createElement("label");
        domainLabel.appendChild(document.createTextNode("Label"));
        domainLabel.className = "secondary-nav-label";


        let domainLabelColor = document.createElement("input");
        domainLabelColor.type = "color";
        if (currentXAxisTitle) {
            domainTextField.value = currentXAxisTitle.font.color;
        }
        domainLabelColor.addEventListener('input', (event) => {
            Plotly.relayout(plot, {"xaxis.title.font.color": event.target.value});
        });
        let domainLabelColorLabel = document.createElement("label");
        domainLabelColorLabel.appendChild(document.createTextNode("Label Color"));
        domainLabelColorLabel.className = "secondary-nav-label";

        let domainShowTicks = document.createElement("label");
        domainShowTicks.className = 'switch';
        let showTicksToggleCheckbox = document.createElement("input");
        showTicksToggleCheckbox.type = "checkbox";
        let checked = true;
        if (plotTemplate.layout.xaxis.visible === false) {
            checked = false;
        }
        showTicksToggleCheckbox.checked = checked;
        domainShowTicks.appendChild(showTicksToggleCheckbox);
        let showTicksToggle = document.createElement("div");
        showTicksToggle.className = "slider round";
        domainShowTicks.appendChild(showTicksToggle);
        showTicksToggleCheckbox.addEventListener('change', (event) => {
            Plotly.relayout(plot, {
                "xaxis.visible": event.target.checked
            });
        });
        let domainShowTicksLabel = document.createElement("label");
        domainShowTicksLabel.for = 'margin-color';
        domainShowTicksLabel.appendChild(document.createTextNode("Show Ticks"));
        domainShowTicksLabel.className = "secondary-nav-label";

        let domainShowTickLabels = document.createElement("label");
        domainShowTickLabels.className = 'switch';
        let showTickLabelsToggleCheckbox = document.createElement("input");
        showTickLabelsToggleCheckbox.type = "checkbox";
        checked = true;
        if (plotTemplate.layout.xaxis.showticklabels === false) {
            checked = false;
        }
        showTickLabelsToggleCheckbox.checked = checked;
        domainShowTickLabels.appendChild(showTickLabelsToggleCheckbox);
        let showTickLabelsToggle = document.createElement("div");
        showTickLabelsToggle.className = "slider round";
        domainShowTickLabels.appendChild(showTickLabelsToggle);
        showTickLabelsToggleCheckbox.addEventListener('change', (event) => {
            Plotly.relayout(plot, {
                "xaxis.showticklabels": event.target.checked
            });
        });
        let domainShowTicksLabelsLabel = document.createElement("label");
        domainShowTicksLabelsLabel.for = 'margin-color';
        domainShowTicksLabelsLabel.appendChild(document.createTextNode("Show Tick Labels"));
        domainShowTicksLabelsLabel.className = "secondary-nav-label";

        let domainAxisType = document.createElement("select");
        domainAxisType.type = "dropdown";
        let linear = document.createElement("option");
        linear.value = "linear";
        linear.appendChild(document.createTextNode("Linear"));
        let log = document.createElement("option");
        log.value = "log";
        log.appendChild(document.createTextNode("Logarithmic"));
        let date = document.createElement("option");
        date.value = "date";
        date.appendChild(document.createTextNode("Date"));
        let category = document.createElement("option");
        category.value = "category";
        category.appendChild(document.createTextNode("Category"));
        let multicategory = document.createElement("option");
        multicategory.value = "multicategory";
        multicategory.appendChild(document.createTextNode("Multicategory"));
        domainAxisType.appendChild(linear);
        domainAxisType.appendChild(log);
        domainAxisType.appendChild(date);
        domainAxisType.appendChild(category);
        domainAxisType.appendChild(multicategory);
        domainAxisType.addEventListener('change', (event) => {
            Plotly.relayout(plot, {
                "xaxis.type": event.target.value
            });
        });
        let domainAxisTypeLabel = document.createElement("label");
        domainAxisTypeLabel.appendChild(document.createTextNode("Axis Type"));

        let domainMax = document.createElement("input");
        let domainMin = document.createElement("input");
        domainMin.addEventListener('input', () => {
            Plotly.relayout(plot, {
                "xaxis.autorange": false,
                "xaxis.range": [domainMin.value, domainMax.value]
            });
        });
        if (plotTemplate.layout.xaxis.type === 'linear'
            || plotTemplate.layout.xaxis.type === 'log') {
            domainMin.type = "number";
        } else {
            domainMin.type = "text";
        }
        domainMin.value = plotTemplate.layout.xaxis.range[0];
        let domainMinLabel = document.createElement("label");
        domainMinLabel.appendChild(document.createTextNode("Min"));
        domainMinLabel.className = "secondary-nav-label";

        if (plotTemplate.layout.xaxis.type === 'linear'
            || plotTemplate.layout.xaxis.type === 'log') {
            domainMax.type = "number";
        } else {
            domainMax.type = "text";
        }
        domainMax.value = plotTemplate.layout.xaxis.range[1];
        domainMax.addEventListener('input', () => {
            Plotly.relayout(plot, {
                "xaxis.autorange": false,
                "xaxis.range": [domainMin.value, domainMax.value]
            });
        });
        let domainMaxLabel = document.createElement("label");
        domainMaxLabel.appendChild(document.createTextNode("Max"));
        domainMaxLabel.className = "secondary-nav-label";

        secondSidenav.appendChild(domain);
        secondSidenav.appendChild(document.createElement("br"));
        secondSidenav.appendChild(domainLabel);
        secondSidenav.appendChild(domainTextField);
        secondSidenav.appendChild(document.createElement("br"));
        secondSidenav.appendChild(domainLabelColorLabel);
        secondSidenav.appendChild(domainLabelColor);
        secondSidenav.appendChild(document.createElement("br"));
        secondSidenav.appendChild(domainShowTicksLabel);
        secondSidenav.appendChild(domainShowTicks);
        secondSidenav.appendChild(document.createElement("br"));
        secondSidenav.appendChild(domainShowTicksLabelsLabel);
        secondSidenav.appendChild(domainShowTickLabels);
        secondSidenav.appendChild(document.createElement("br"));
        secondSidenav.appendChild(domainMinLabel);
        secondSidenav.appendChild(domainMin);
        secondSidenav.appendChild(document.createElement("br"));
        secondSidenav.appendChild(domainMaxLabel);
        secondSidenav.appendChild(domainMax);
        secondSidenav.appendChild(document.createElement("br"));
    }

    function addRangeAxesOptions() {
        let range = document.createElement("div");
        range.appendChild(document.createTextNode("Range"));
        range.className = "secondary-nav-title";

        let rangeTextField = document.createElement("input");
        rangeTextField.type = "text";
        let currentYAxisTitle = plotTemplate.layout.yaxis.title;
        if (currentYAxisTitle) {
            rangeTextField.value = currentYAxisTitle.text;
        }
        rangeTextField.addEventListener('input', (event) => {
            Plotly.relayout(plot, {"yaxis.title.text": event.target.value});
        });
        let rangeLabel = document.createElement("label");
        rangeLabel.appendChild(document.createTextNode("Label"));
        rangeLabel.className = "secondary-nav-label";


        let rangeLabelColor = document.createElement("input");
        rangeLabelColor.type = "color";
        if (currentYAxisTitle && currentYAxisTitle.font) {
            rangeTextField.value = currentYAxisTitle.font.color;
        }
        rangeLabelColor.addEventListener('input', (event) => {
            Plotly.relayout(plot, {"yaxis.title.font.color": event.target.value});
        });
        let rangeLabelColorLabel = document.createElement("label");
        rangeLabelColorLabel.appendChild(document.createTextNode("Label Color"));
        rangeLabelColorLabel.className = "secondary-nav-label";

        let rangeShowTicks = document.createElement("label");
        rangeShowTicks.className = 'switch';
        let showTicksToggleCheckbox = document.createElement("input");
        showTicksToggleCheckbox.type = "checkbox";
        let checked = true;
        if (plotTemplate.layout.yaxis.visible === false) {
            checked = false;
        }
        showTicksToggleCheckbox.checked = checked;
        rangeShowTicks.appendChild(showTicksToggleCheckbox);
        let showTicksToggle = document.createElement("div");
        showTicksToggle.className = "slider round";
        rangeShowTicks.appendChild(showTicksToggle);
        showTicksToggleCheckbox.addEventListener('change', (event) => {
            Plotly.relayout(plot, {
                "yaxis.visible": event.target.checked
            });
        });
        let rangeShowTicksLabel = document.createElement("label");
        rangeShowTicksLabel.for = 'margin-color';
        rangeShowTicksLabel.appendChild(document.createTextNode("Show Ticks"));
        rangeShowTicksLabel.className = "secondary-nav-label";

        let rangeShowTickLabels = document.createElement("label");
        rangeShowTickLabels.className = 'switch';
        let showTickLabelsToggleCheckbox = document.createElement("input");
        showTickLabelsToggleCheckbox.type = "checkbox";
        checked = true;
        if (plotTemplate.layout.yaxis.showticklabels === false) {
            checked = false;
        }
        showTickLabelsToggleCheckbox.checked = checked;
        rangeShowTickLabels.appendChild(showTickLabelsToggleCheckbox);
        let showTickLabelsToggle = document.createElement("div");
        showTickLabelsToggle.className = "slider round";
        rangeShowTickLabels.appendChild(showTickLabelsToggle);
        showTickLabelsToggleCheckbox.addEventListener('change', (event) => {
            Plotly.relayout(plot, {
                "yaxis.showticklabels": event.target.checked
            });
        });
        let domainShowTicksLabelsLabel = document.createElement("label");
        domainShowTicksLabelsLabel.for = 'margin-color';
        domainShowTicksLabelsLabel.appendChild(document.createTextNode("Show Tick Labels"));
        domainShowTicksLabelsLabel.className = "secondary-nav-label";

        let rangeAxisType = document.createElement("select");
        rangeAxisType.type = "dropdown";
        let linear = document.createElement("option");
        linear.value = "linear";
        linear.appendChild(document.createTextNode("Linear"));
        let log = document.createElement("option");
        log.value = "log";
        log.appendChild(document.createTextNode("Logarithmic"));
        let date = document.createElement("option");
        date.value = "date";
        date.appendChild(document.createTextNode("Date"));
        let category = document.createElement("option");
        category.value = "category";
        category.appendChild(document.createTextNode("Category"));
        let multicategory = document.createElement("option");
        multicategory.value = "multicategory";
        multicategory.appendChild(document.createTextNode("Multicategory"));
        rangeAxisType.appendChild(linear);
        rangeAxisType.appendChild(log);
        rangeAxisType.appendChild(date);
        rangeAxisType.appendChild(category);
        rangeAxisType.appendChild(multicategory);
        rangeAxisType.addEventListener('change', (event) => {
            Plotly.relayout(plot, {
                "yaxis.type": event.target.value
            });
        });
        let rangeAxisTypeLabel = document.createElement("label");
        rangeAxisTypeLabel.appendChild(document.createTextNode("Axis Type"));

        let rangeMax = document.createElement("input");
        let rangeMin = document.createElement("input");
        rangeMin.addEventListener('input', () => {
            Plotly.relayout(plot, {
                "yaxis.autorange": false,
                "yaxis.range": [rangeMin.value, rangeMax.value]
            });
        });
        if (plotTemplate.layout.yaxis.type === 'linear'
            || plotTemplate.layout.yaxis.type === 'log') {
            rangeMin.type = "number";
        } else {
            rangeMin.type = "text";
        }
        rangeMin.value = plotTemplate.layout.yaxis.range[0];
        let rangeMinLabel = document.createElement("label");
        rangeMinLabel.appendChild(document.createTextNode("Min"));
        rangeMinLabel.className = "secondary-nav-label";

        if (plotTemplate.layout.yaxis.type === 'linear'
            || plotTemplate.layout.yaxis.type === 'log') {
            rangeMax.type = "number";
        } else {
            rangeMax.type = "text";
        }
        rangeMax.value = plotTemplate.layout.yaxis.range[1];
        rangeMax.addEventListener('input', () => {
            Plotly.relayout(plot, {
                "yaxis.autorange": false,
                "yaxis.range": [rangeMin.value, rangeMax.value]
            });
        });
        let rangeMaxLabel = document.createElement("label");
        rangeMaxLabel.appendChild(document.createTextNode("Max"));
        rangeMaxLabel.className = "secondary-nav-label";

        secondSidenav.appendChild(range);
        secondSidenav.appendChild(document.createElement("br"));
        secondSidenav.appendChild(rangeLabel);
        secondSidenav.appendChild(rangeTextField);
        secondSidenav.appendChild(document.createElement("br"));
        secondSidenav.appendChild(rangeLabelColorLabel);
        secondSidenav.appendChild(rangeLabelColor);
        secondSidenav.appendChild(document.createElement("br"));
        secondSidenav.appendChild(rangeShowTicksLabel);
        secondSidenav.appendChild(rangeShowTicks);
        secondSidenav.appendChild(document.createElement("br"));
        secondSidenav.appendChild(domainShowTicksLabelsLabel);
        secondSidenav.appendChild(rangeShowTickLabels);
        secondSidenav.appendChild(document.createElement("br"));
        secondSidenav.appendChild(rangeMinLabel);
        secondSidenav.appendChild(rangeMin);
        secondSidenav.appendChild(document.createElement("br"));
        secondSidenav.appendChild(rangeMaxLabel);
        secondSidenav.appendChild(rangeMax);
        secondSidenav.appendChild(document.createElement("br"));
    }

    function addScatterOptions(scatter) {
        for (let i = 0; i < scatter.length; i++) {
            let line = scatter[i];
            let traceHeader = document.createElement("div");
            traceHeader.appendChild(document.createTextNode("Trace: " + line.name));
            traceHeader.className = "secondary-nav-title";

            let traceLabelTextField = document.createElement("input");
            traceLabelTextField.type = "text";
            traceLabelTextField.value = line.name;
            traceLabelTextField.addEventListener('input', (event) => {
                Plotly.restyle(plot, {name: event.target.value}, [i]);
            });
            let traceLabel = document.createElement("label");
            traceLabel.appendChild(document.createTextNode("Label"));
            traceLabel.className = "secondary-nav-label";


            let traceColor = document.createElement("div");
            traceColor.className = "color-picker";
            new Picker({
                parent: traceColor,
                color: line.line.color,
                editor: true,
                alpha: true,
                // layout: 'left',
                onChange: function (color) {
                    traceColor.style.background = color.rgbaString;
                    Plotly.restyle(plot, {"line.color": color.hex}, [i]);
                }
            });
            let traceColorLabel = document.createElement("label");
            traceColorLabel.appendChild(document.createTextNode("Color"));
            traceColorLabel.className = "secondary-nav-label";

            let rangeShowTicks = document.createElement("label");
            rangeShowTicks.className = 'switch';
            let showTicksToggleCheckbox = document.createElement("input");
            showTicksToggleCheckbox.type = "checkbox";
            let checked = true;
            if (plotTemplate.layout.yaxis.visible === false) {
                checked = false;
            }
            showTicksToggleCheckbox.checked = checked;
            rangeShowTicks.appendChild(showTicksToggleCheckbox);
            let showTicksToggle = document.createElement("div");
            showTicksToggle.className = "slider round";
            rangeShowTicks.appendChild(showTicksToggle);
            showTicksToggleCheckbox.addEventListener('change', (event) => {
                Plotly.relayout(plot, {
                    "yaxis.visible": event.target.checked
                });
            });
            let rangeShowTicksLabel = document.createElement("label");
            rangeShowTicksLabel.for = 'margin-color';
            rangeShowTicksLabel.appendChild(document.createTextNode("Show Ticks"));
            rangeShowTicksLabel.className = "secondary-nav-label";

            let rangeShowTickLabels = document.createElement("label");
            rangeShowTickLabels.className = 'switch';
            let showTickLabelsToggleCheckbox = document.createElement("input");
            showTickLabelsToggleCheckbox.type = "checkbox";
            checked = true;
            if (plotTemplate.layout.yaxis.showticklabels === false) {
                checked = false;
            }
            showTickLabelsToggleCheckbox.checked = checked;
            rangeShowTickLabels.appendChild(showTickLabelsToggleCheckbox);
            let showTickLabelsToggle = document.createElement("div");
            showTickLabelsToggle.className = "slider round";
            rangeShowTickLabels.appendChild(showTickLabelsToggle);
            showTickLabelsToggleCheckbox.addEventListener('change', (event) => {
                Plotly.relayout(plot, {
                    "yaxis.showticklabels": event.target.checked
                });
            });
            let domainShowTicksLabelsLabel = document.createElement("label");
            domainShowTicksLabelsLabel.for = 'margin-color';
            domainShowTicksLabelsLabel.appendChild(document.createTextNode("Show Tick Labels"));
            domainShowTicksLabelsLabel.className = "secondary-nav-label";

            let rangeAxisType = document.createElement("select");
            rangeAxisType.type = "dropdown";
            let linear = document.createElement("option");
            linear.value = "linear";
            linear.appendChild(document.createTextNode("Linear"));
            let log = document.createElement("option");
            log.value = "log";
            log.appendChild(document.createTextNode("Logarithmic"));
            let date = document.createElement("option");
            date.value = "date";
            date.appendChild(document.createTextNode("Date"));
            let category = document.createElement("option");
            category.value = "category";
            category.appendChild(document.createTextNode("Category"));
            let multicategory = document.createElement("option");
            multicategory.value = "multicategory";
            multicategory.appendChild(document.createTextNode("Multicategory"));
            rangeAxisType.appendChild(linear);
            rangeAxisType.appendChild(log);
            rangeAxisType.appendChild(date);
            rangeAxisType.appendChild(category);
            rangeAxisType.appendChild(multicategory);
            rangeAxisType.addEventListener('change', (event) => {
                Plotly.relayout(plot, {
                    "yaxis.type": event.target.value
                });
            });
            let rangeAxisTypeLabel = document.createElement("label");
            rangeAxisTypeLabel.appendChild(document.createTextNode("Axis Type"));

            let rangeMax = document.createElement("input");
            let rangeMin = document.createElement("input");
            rangeMin.addEventListener('input', () => {
                Plotly.relayout(plot, {
                    "yaxis.autorange": false,
                    "yaxis.range": [rangeMin.value, rangeMax.value]
                });
            });
            if (plotTemplate.layout.yaxis.type === 'linear'
                || plotTemplate.layout.yaxis.type === 'log') {
                rangeMin.type = "number";
            } else {
                rangeMin.type = "text";
            }
            rangeMin.value = plotTemplate.layout.yaxis.range[0];
            let rangeMinLabel = document.createElement("label");
            rangeMinLabel.appendChild(document.createTextNode("Min"));
            rangeMinLabel.className = "secondary-nav-label";

            if (plotTemplate.layout.yaxis.type === 'linear'
                || plotTemplate.layout.yaxis.type === 'log') {
                rangeMax.type = "number";
            } else {
                rangeMax.type = "text";
            }
            rangeMax.value = plotTemplate.layout.yaxis.range[1];
            rangeMax.addEventListener('input', () => {
                Plotly.relayout(plot, {
                    "yaxis.autorange": false,
                    "yaxis.range": [rangeMin.value, rangeMax.value]
                });
            });
            let rangeMaxLabel = document.createElement("label");
            rangeMaxLabel.appendChild(document.createTextNode("Max"));
            rangeMaxLabel.className = "secondary-nav-label";

            secondSidenav.appendChild(traceHeader);
            secondSidenav.appendChild(document.createElement("br"));
            secondSidenav.appendChild(traceLabel);
            secondSidenav.appendChild(traceLabelTextField);
            secondSidenav.appendChild(document.createElement("br"));
            secondSidenav.appendChild(traceColorLabel);
            secondSidenav.appendChild(traceColor);
            secondSidenav.appendChild(document.createElement("br"));
            secondSidenav.appendChild(rangeShowTicksLabel);
            secondSidenav.appendChild(rangeShowTicks);
            secondSidenav.appendChild(document.createElement("br"));
            secondSidenav.appendChild(domainShowTicksLabelsLabel);
            secondSidenav.appendChild(rangeShowTickLabels);
            secondSidenav.appendChild(document.createElement("br"));
            secondSidenav.appendChild(rangeMinLabel);
            secondSidenav.appendChild(rangeMin);
            secondSidenav.appendChild(document.createElement("br"));
            secondSidenav.appendChild(rangeMaxLabel);
            secondSidenav.appendChild(rangeMax);
            secondSidenav.appendChild(document.createElement("br"));
        }
    }

    function addBoxOptions(box) {
        let range = document.createElement("div");
        range.appendChild(document.createTextNode("Range"));
        range.className = "secondary-nav-title";

        let rangeTextField = document.createElement("input");
        rangeTextField.type = "text";
        let currentYAxisTitle = plotTemplate.layout.yaxis.title;
        if (currentYAxisTitle) {
            rangeTextField.value = currentYAxisTitle.text;
        }
        rangeTextField.addEventListener('input', (event) => {
            Plotly.relayout(plot, {"yaxis.title.text": event.target.value});
        });
        let rangeLabel = document.createElement("label");
        rangeLabel.appendChild(document.createTextNode("Label"));
        rangeLabel.className = "secondary-nav-label";


        let rangeLabelColor = document.createElement("input");
        rangeLabelColor.type = "color";
        if (currentYAxisTitle && currentYAxisTitle.font) {
            rangeTextField.value = currentYAxisTitle.font.color;
        }
        rangeLabelColor.addEventListener('input', (event) => {
            Plotly.relayout(plot, {"yaxis.title.font.color": event.target.value});
        });
        let rangeLabelColorLabel = document.createElement("label");
        rangeLabelColorLabel.appendChild(document.createTextNode("Label Color"));
        rangeLabelColorLabel.className = "secondary-nav-label";

        let rangeShowTicks = document.createElement("label");
        rangeShowTicks.className = 'switch';
        let showTicksToggleCheckbox = document.createElement("input");
        showTicksToggleCheckbox.type = "checkbox";
        let checked = true;
        if (plotTemplate.layout.yaxis.visible === false) {
            checked = false;
        }
        showTicksToggleCheckbox.checked = checked;
        rangeShowTicks.appendChild(showTicksToggleCheckbox);
        let showTicksToggle = document.createElement("div");
        showTicksToggle.className = "slider round";
        rangeShowTicks.appendChild(showTicksToggle);
        showTicksToggleCheckbox.addEventListener('change', (event) => {
            Plotly.relayout(plot, {
                "yaxis.visible": event.target.checked
            });
        });
        let rangeShowTicksLabel = document.createElement("label");
        rangeShowTicksLabel.for = 'margin-color';
        rangeShowTicksLabel.appendChild(document.createTextNode("Show Ticks"));
        rangeShowTicksLabel.className = "secondary-nav-label";

        let rangeShowTickLabels = document.createElement("label");
        rangeShowTickLabels.className = 'switch';
        let showTickLabelsToggleCheckbox = document.createElement("input");
        showTickLabelsToggleCheckbox.type = "checkbox";
        checked = true;
        if (plotTemplate.layout.yaxis.showticklabels === false) {
            checked = false;
        }
        showTickLabelsToggleCheckbox.checked = checked;
        rangeShowTickLabels.appendChild(showTickLabelsToggleCheckbox);
        let showTickLabelsToggle = document.createElement("div");
        showTickLabelsToggle.className = "slider round";
        rangeShowTickLabels.appendChild(showTickLabelsToggle);
        showTickLabelsToggleCheckbox.addEventListener('change', (event) => {
            Plotly.relayout(plot, {
                "yaxis.showticklabels": event.target.checked
            });
        });
        let domainShowTicksLabelsLabel = document.createElement("label");
        domainShowTicksLabelsLabel.for = 'margin-color';
        domainShowTicksLabelsLabel.appendChild(document.createTextNode("Show Tick Labels"));
        domainShowTicksLabelsLabel.className = "secondary-nav-label";

        let rangeAxisType = document.createElement("select");
        rangeAxisType.type = "dropdown";
        let linear = document.createElement("option");
        linear.value = "linear";
        linear.appendChild(document.createTextNode("Linear"));
        let log = document.createElement("option");
        log.value = "log";
        log.appendChild(document.createTextNode("Logarithmic"));
        let date = document.createElement("option");
        date.value = "date";
        date.appendChild(document.createTextNode("Date"));
        let category = document.createElement("option");
        category.value = "category";
        category.appendChild(document.createTextNode("Category"));
        let multicategory = document.createElement("option");
        multicategory.value = "multicategory";
        multicategory.appendChild(document.createTextNode("Multicategory"));
        rangeAxisType.appendChild(linear);
        rangeAxisType.appendChild(log);
        rangeAxisType.appendChild(date);
        rangeAxisType.appendChild(category);
        rangeAxisType.appendChild(multicategory);
        rangeAxisType.addEventListener('change', (event) => {
            Plotly.relayout(plot, {
                "yaxis.type": event.target.value
            });
        });
        let rangeAxisTypeLabel = document.createElement("label");
        rangeAxisTypeLabel.appendChild(document.createTextNode("Axis Type"));

        let rangeMax = document.createElement("input");
        let rangeMin = document.createElement("input");
        rangeMin.addEventListener('input', () => {
            Plotly.relayout(plot, {
                "yaxis.autorange": false,
                "yaxis.range": [rangeMin.value, rangeMax.value]
            });
        });
        if (plotTemplate.layout.yaxis.type === 'linear'
            || plotTemplate.layout.yaxis.type === 'log') {
            rangeMin.type = "number";
        } else {
            rangeMin.type = "text";
        }
        rangeMin.value = plotTemplate.layout.yaxis.range[0];
        let rangeMinLabel = document.createElement("label");
        rangeMinLabel.appendChild(document.createTextNode("Min"));
        rangeMinLabel.className = "secondary-nav-label";

        if (plotTemplate.layout.yaxis.type === 'linear'
            || plotTemplate.layout.yaxis.type === 'log') {
            rangeMax.type = "number";
        } else {
            rangeMax.type = "text";
        }
        rangeMax.value = plotTemplate.layout.yaxis.range[1];
        rangeMax.addEventListener('input', () => {
            Plotly.relayout(plot, {
                "yaxis.autorange": false,
                "yaxis.range": [rangeMin.value, rangeMax.value]
            });
        });
        let rangeMaxLabel = document.createElement("label");
        rangeMaxLabel.appendChild(document.createTextNode("Max"));
        rangeMaxLabel.className = "secondary-nav-label";

        secondSidenav.appendChild(range);
        secondSidenav.appendChild(document.createElement("br"));
        secondSidenav.appendChild(rangeLabel);
        secondSidenav.appendChild(rangeTextField);
        secondSidenav.appendChild(document.createElement("br"));
        secondSidenav.appendChild(rangeLabelColorLabel);
        secondSidenav.appendChild(rangeLabelColor);
        secondSidenav.appendChild(document.createElement("br"));
        secondSidenav.appendChild(rangeShowTicksLabel);
        secondSidenav.appendChild(rangeShowTicks);
        secondSidenav.appendChild(document.createElement("br"));
        secondSidenav.appendChild(domainShowTicksLabelsLabel);
        secondSidenav.appendChild(rangeShowTickLabels);
        secondSidenav.appendChild(document.createElement("br"));
        secondSidenav.appendChild(rangeMinLabel);
        secondSidenav.appendChild(rangeMin);
        secondSidenav.appendChild(document.createElement("br"));
        secondSidenav.appendChild(rangeMaxLabel);
        secondSidenav.appendChild(rangeMax);
        secondSidenav.appendChild(document.createElement("br"));
    }

    function addGeneralOptions() {
        let plotLabel = document.createElement("label");
        plotLabel.appendChild(document.createTextNode("Plot"));
        plotLabel.className = "secondary-nav-title";
        let backgroundColorNode = document.createElement("input");
        backgroundColorNode.type = 'color';
        backgroundColorNode.id = 'general-background-color';
        let paperBgcolor = plotTemplate.layout.paper_bgcolor;
        if (!paperBgcolor) {
            paperBgcolor = "#FFFFFF";
        }
        backgroundColorNode.value = paperBgcolor;
        backgroundColorNode.addEventListener('change', (event) => {
            Plotly.relayout(plot, {
                paper_bgcolor: event.target.value
            });
        });
        let backgroundColorLabel = document.createElement("label");
        backgroundColorLabel.for = 'general-background-color';
        backgroundColorLabel.appendChild(document.createTextNode("Background Color"));
        backgroundColorLabel.className = "secondary-nav-label";

        let marginColorNode = document.createElement("input");
        marginColorNode.type = 'color';
        marginColorNode.id = 'margin-color';
        let marginBgcolor = plotTemplate.layout.plot_bgcolor;
        if (!marginBgcolor) {
            marginBgcolor = "#FFFFFF";
        }
        marginColorNode.value = marginBgcolor;
        marginColorNode.addEventListener('change', (event) => {
            Plotly.relayout(plot, {
                plot_bgcolor: event.target.value
            });
        });
        let marginColorLabel = document.createElement("label");
        marginColorLabel.for = 'margin-color';
        marginColorLabel.appendChild(document.createTextNode("Plot Color"));
        marginColorLabel.className = "secondary-nav-label";

        let onOffSwitchLabel = document.createElement("label");
        onOffSwitchLabel.className = 'switch';
        let showLegendToggleCheckbox = document.createElement("input");
        showLegendToggleCheckbox.type = "checkbox";
        showLegendToggleCheckbox.checked = plotTemplate.layout.showlegend;
        onOffSwitchLabel.appendChild(showLegendToggleCheckbox);
        let showLegendToggle = document.createElement("div");
        showLegendToggle.className = "slider round";
        onOffSwitchLabel.appendChild(showLegendToggle);
        showLegendToggleCheckbox.addEventListener('change', (event) => {
            Plotly.relayout(plot, {
                showlegend: event.target.checked
            });
        });
        let legendVisibleLabel = document.createElement("label");
        legendVisibleLabel.for = 'margin-color';
        legendVisibleLabel.className = "secondary-nav-label";
        legendVisibleLabel.appendChild(document.createTextNode("Show Legend"));

        let titleHeaderElement = document.createElement("label");
        titleHeaderElement.appendChild(document.createTextNode("Title"));
        titleHeaderElement.className = "secondary-nav-title";
        let titleTextElement = document.createElement("input");
        titleTextElement.type = "text";
        titleTextElement.value = plotTemplate.layout.title.text;
        titleTextElement.addEventListener('input', (event) => {
            Plotly.relayout(plot, {"title.text": event.target.value});
        });

        let showTitleLabel = document.createElement("label");
        showTitleLabel.className = "secondary-nav-label";
        showTitleLabel.for = 'margin-color';
        showTitleLabel.appendChild(document.createTextNode("Label"));
        let titleColorNode = document.createElement("input");
        titleColorNode.type = 'color';
        titleColorNode.value = Plotly.makeTemplate(plot).layout.title.font.color;
        titleColorNode.addEventListener('change', (event) => {
            Plotly.relayout(plot, {"title.font.color": event.target.value});
        });

        let titleColorLabel = document.createElement("label");
        titleColorLabel.className = "secondary-nav-label";
        titleColorLabel.appendChild(document.createTextNode("Title Color"));

        secondSidenav.appendChild(plotLabel);
        secondSidenav.appendChild(document.createElement("br"));
        secondSidenav.appendChild(backgroundColorLabel);
        secondSidenav.appendChild(backgroundColorNode);
        secondSidenav.appendChild(document.createElement("br"));
        secondSidenav.appendChild(marginColorLabel);
        secondSidenav.appendChild(marginColorNode);
        secondSidenav.appendChild(document.createElement("br"));
        secondSidenav.appendChild(legendVisibleLabel);
        secondSidenav.appendChild(onOffSwitchLabel);
        secondSidenav.appendChild(document.createElement("br"));
        secondSidenav.appendChild(titleHeaderElement);
        secondSidenav.appendChild(document.createElement("br"));
        secondSidenav.appendChild(showTitleLabel);
        secondSidenav.appendChild(titleTextElement);
        secondSidenav.appendChild(document.createElement("br"));
        secondSidenav.appendChild(titleColorLabel);
        secondSidenav.appendChild(titleColorNode);
    }

    if (option === 'general') {
        addGeneralOptions();
    } else if (option === 'axes') {
        addDomainAxesOptions();
        addRangeAxesOptions();
    } else if (option === "traces") {
        let scatter = plotTemplate.data.scatter;
        let box = plotTemplate.data.box;
        if (scatter) {
            addScatterOptions(scatter);
        } else if (box) {
            addBoxOptions(box);
        }
    } else if (option === "template") {
        let templateArea = document.createElement("textarea");
        templateArea.rows = 50;
        templateArea.cols = 33;
        templateArea.wrap = "soft";
        let template = Plotly.makeTemplate(plot);
        templateArea.appendChild(document.createTextNode(JSON.stringify(template.layout, null, 2)));
        templateArea.oninput = (event) => {
            let json = JSON.parse(event.target.value);
            Plotly.relayout(plot, json);
        };
        secondSidenav.appendChild(templateArea);
    }
}

function closeSecondNav() {
    let secondSidenav = document.getElementById("secondSidenav");
    if (secondSidenav) {
        secondSidenav.style.width = "0px";
        while (secondSidenav.firstChild) {
            secondSidenav.removeChild(secondSidenav.firstChild);
        }
    }
}

function openNav(plot) {
    let sideNav = document.getElementById("sidenav");
    if (!sideNav) {
        sideNav = document.createElement("div");
        sideNav.id = "sidenav";
        sideNav.className = "sidenav";

        let closeButton = document.createElement("a");
        closeButton.href = "javascript:void(0)";
        closeButton.appendChild(document.createTextNode("\u2573"));
        closeButton.className = "closebtn";
        closeButton.onclick = () => closeNav();

        let generalPlot = document.createElement("a");
        generalPlot.href = "javascript:void(0)";
        generalPlot.onclick = () => openSecondNav('general', plot);
        generalPlot.appendChild(document.createTextNode("General Plot"));

        let axes = document.createElement("a");
        axes.href = "javascript:void(0)";
        axes.onclick = () => openSecondNav('axes', plot);
        axes.appendChild(document.createTextNode("Axes"));

        let template = document.createElement("a");
        template.href = "javascript:void(0)";
        template.onclick = () => openSecondNav('template', plot);
        template.appendChild(document.createTextNode("Template"));

        let traces = document.createElement("a");
        traces.href = "javascript:void(0)";
        traces.onclick = () => openSecondNav('traces', plot);
        traces.appendChild(document.createTextNode("Traces"));

        sideNav.appendChild(closeButton);
        sideNav.appendChild(generalPlot);
        sideNav.appendChild(axes);
        sideNav.appendChild(traces);
        sideNav.appendChild(template);
        document.body.append(sideNav);
    }
    sideNav.style.width = "250px";
    document.getElementById("main").style.marginLeft = "250px";
    document.body.style.backgroundColor = "rgba(0,0,0,0.4)";
}

// <!--    <label> Shape-->
// <!--        <select>-->
// <!--            <option value="linear">Linear</option>-->
//     <!--            <option value="spline">Spline</option>-->
//     <!--            <option value="hv">Beginning of Period</option>-->
// <!--            <option value="vh">End of Period</option>-->
// <!--            <option value="hvh">Middle of Period X</option>-->
// <!--            <option value="hvh">Middle of Period Y</option>-->
// <!--        </select>-->
// <!--    </label>-->


function closeNav() {
    document.getElementById("sidenav").style.width = "0";
    document.getElementById("main").style.marginLeft = "0px";
    closeSecondNav();
    document.body.style.backgroundColor = "white";
}

function getD3Formatter(fullSeries) {
    let values = [];
    for (let i = 0; i < fullSeries.length; i++) {
        values.push(fullSeries[i][1]);
    }
    if (Math.max.apply(null, values) > 1) {
        return ',.0f';
    } else {
        return ',.3r';
    }
}


function buildMarkerLines(series) {
    let shapes = [];
    for (let i = 0; i < series.length; i++) {
        let trace = series[i];
        let minX = Math.min.apply(null, trace['x']);
        let maxX = Math.max.apply(null, trace['x']);
        let max = Math.max.apply(null, trace['y']);
        let maxLine = {
            type: 'line',
            x0: minX,
            y0: max,
            x1: maxX,
            y1: max,
            line: {
                color: trace['line']['color'],
                width: 1,
                dash: 'dashdot'
            }
        };
        shapes.push(maxLine);
        let min = Math.min.apply(null, trace['y']);
        let minLine = {
            type: 'line',
            x0: minX,
            y0: min,
            x1: maxX,
            y1: min,
            line: {
                color: trace['line']['color'],
                width: 1,
                dash: 'dashdot'
            }
        };
        shapes.push(minLine);
        let mean = trace['y'].reduce((previous, current) => current += previous) / trace['y'].length;
        let meanLine = {
            type: 'line',
            x0: minX,
            y0: mean,
            x1: maxX,
            y1: mean,
            line: {
                color: trace['line']['color'],
                width: 1,
                dash: 'dashdot'
            }
        };
        shapes.push(meanLine);
        let yCopy = trace['y'].slice();
        yCopy.sort((a, b) => a - b);
        let median = (yCopy[(yCopy.length - 1) >> 1] + yCopy[yCopy.length >> 1]) / 2;
        let medianLine = {
            type: 'line',
            x0: minX,
            y0: median,
            x1: maxX,
            y1: median,
            line: {
                color: trace['line']['color'],
                width: 1,
                dash: 'dashdot'
            }
        };
        shapes.push(medianLine);
    }
    return shapes;
}


if (DEBUG) {
    window.onload = () => plot({
        "last_record": 1064995200000, "scenario_run_data": [{
            "ts_list": [{
                "ts_name": "ROC_DV (CalSim2)", "monthly_filters": [{
                    "annual_filters": [{
                        "period_months": ["Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep"],
                        "annual_period": "Long Term<br>",
                        "computed_statistics": [{
                            "statistic": "Averages",
                            "statistically_computed_time_series_monthly": [["Jan", 1401.2036294819395], ["Feb", 1529.5581461588542], ["Mar", 1657.621772295163], ["Apr", 1801.2867672767168], ["May", 1777.2711942696278], ["Jun", 1713.7290001386477], ["Jul", 1584.6755510495034], ["Aug", 1437.0602096745997], ["Sep", 1304.0311154966], ["Oct", 1257.1429420753761], ["Nov", 1264.0022270296827], ["Dec", 1324.4291939441068]],
                            "statistic_aggregate": 1508.544593376842
                        }],
                        "aggregate_ts": [[1922, 1738.0345865885417], [1923, 1506.5907694498699], [1924, 945.6288045247396], [1925, 997.4355824788412], [1926, 1115.228510538737], [1927, 1648.9219055175781], [1928, 1700.6819356282551], [1929, 1015.4895477294922], [1930, 908.4419504801432], [1931, 637.1179860432943], [1932, 384.3154983520508], [1933, 475.7671941121419], [1934, 505.38362375895184], [1935, 524.0229619344076], [1936, 752.2773030598959], [1937, 819.0853576660156], [1938, 1715.8349202473958], [1939, 1675.2052764892578], [1940, 1366.8335571289062], [1941, 1936.6683858235676], [1942, 2033.0848897298176], [1943, 1916.3184204101562], [1944, 1197.4567464192708], [1945, 1124.0874633789062], [1946, 1521.5887044270833], [1947, 1232.5620829264324], [1948, 1198.240707397461], [1949, 1367.4791056315105], [1950, 1301.3377685546875], [1951, 1816.6462910970051], [1952, 1987.976338704427], [1953, 2060.9073384602866], [1954, 1966.8019002278645], [1955, 1566.2867635091145], [1956, 1917.9578755696614], [1957, 1948.7231750488281], [1958, 2098.7666015625], [1959, 1888.1380920410156], [1960, 1459.7119242350261], [1961, 1503.5514526367188], [1962, 1510.691385904948], [1963, 1920.4247233072917], [1964, 1630.3616129557292], [1965, 1804.2619120279949], [1966, 1762.4413553873699], [1967, 1909.3123779296875], [1968, 1856.2503255208333], [1969, 1884.3295796712239], [1970, 1866.4475708007812], [1971, 1844.1444905598958], [1972, 1828.6244608561199], [1973, 1901.9930013020833], [1974, 2025.9494222005208], [1975, 2057.987721761068], [1976, 1633.5041097005208], [1977, 811.0735626220703], [1978, 1106.847651163737], [1979, 1335.4562479654949], [1980, 1710.635030110677], [1981, 1555.6461486816406], [1982, 1898.9539286295574], [1983, 2099.4363606770835], [1984, 1935.4843444824219], [1985, 1598.4960835774739], [1986, 1549.8147786458333], [1987, 1332.9578959147136], [1988, 1188.8931274414062], [1989, 1177.1121978759766], [1990, 1082.2212371826172], [1991, 829.6509602864584], [1992, 802.8221282958984], [1993, 1219.200703938802], [1994, 1291.8346048990886], [1995, 1766.2174377441406], [1996, 1979.7498474121094], [1997, 1771.4755350748699], [1998, 1866.5422770182292], [1999, 2059.539784749349], [2000, 1939.2741800944011], [2001, 1521.8439025878906], [2002, 1506.5859985351562], [2003, 1849.5773518880208]],
                        "month_period": "October - September",
                        "discrete_ts": [[-1522684800000, 2000], [-1520006400000, 1818.9796142578125], [-1517414400000, 1639.712646484375], [-1514736000000, 1642.0218505859375], [-1512057600000, 1638.9539794921875], [-1509638400000, 1663.5225830078125], [-1506960000000, 1718.5157470703125], [-1504368000000, 1828.36865234375], [-1501689600000, 1916.6484375], [-1499097600000, 1946.7923583984375], [-1496419200000, 1828.3919677734375], [-1493740800000, 1679.1734619140625], [-1491148800000, 1535.333740234375], [-1488470400000, 1483.38916015625], [-1485878400000, 1458.0030517578125], [-1483200000000, 1466.0263671875], [-1480521600000, 1501.773681640625], [-1478102400000, 1530.5782470703125], [-1475424000000, 1577.8636474609375], [-1472832000000, 1685.27734375], [-1470153600000, 1713.9193115234375], [-1467561600000, 1642.57470703125], [-1464883200000, 1497.6953125], [-1462204800000, 1323.0732421875], [-1459612800000, 1198.9151611328125], [-1456934400000, 1176.970458984375], [-1454342400000, 1168.269775390625], [-1451664000000, 1158.3641357421875], [-1448985600000, 1009.553466796875], [-1446480000000, 1058.9945068359375], [-1443801600000, 1049.9114990234375], [-1441209600000, 1000], [-1438531200000, 915.5426635742188], [-1435939200000, 857.4027709960938], [-1433260800000, 756.7877197265625], [-1430582400000, 631.5804443359375], [-1427990400000, 564.168212890625], [-1425312000000, 565.8051147460938], [-1422720000000, 651.9761962890625], [-1420041600000, 709.205810546875], [-1417363200000, 622.0648193359375], [-1414944000000, 970.2730712890625], [-1412265600000, 1116.81982421875], [-1409673600000, 1400.7830810546875], [-1406995200000, 1383.0765380859375], [-1404403200000, 1296.545654296875], [-1401724800000, 1173.955078125], [-1399046400000, 1075.826416015625], [-1396454400000, 1002.8953857421875], [-1393776000000, 978.1297607421875], [-1391184000000, 973.2603759765625], [-1388505600000, 985.7023315429688], [-1385827200000, 989.683837890625], [-1383408000000, 1144.8846435546875], [-1380729600000, 1267.8455810546875], [-1378137600000, 1458.89013671875], [-1375459200000, 1334.4515380859375], [-1372867200000, 1254.72265625], [-1370188800000, 1106.51123046875], [-1367510400000, 992.2830200195312], [-1364918400000, 896.3770141601562], [-1362240000000, 871.78662109375], [-1359648000000, 1069.8948974609375], [-1356969600000, 1288.040283203125], [-1354291200000, 1418.64892578125], [-1351872000000, 1718.511474609375], [-1349193600000, 1908.496826171875], [-1346601600000, 2132.911865234375], [-1343923200000, 2126.205322265625], [-1341331200000, 2017.4195556640625], [-1338652800000, 1892.9815673828125], [-1335974400000, 1742.869384765625], [-1333382400000, 1599.296142578125], [-1330704000000, 1507.2904052734375], [-1328112000000, 1544.170166015625], [-1325433600000, 1555.9991455078125], [-1322755200000, 1614.218994140625], [-1320249600000, 1723.9229736328125], [-1317571200000, 1923.943603515625], [-1314979200000, 2075.325927734375], [-1312300800000, 1987.18603515625], [-1309708800000, 1877.4039306640625], [-1307030400000, 1718.279541015625], [-1304352000000, 1539.329345703125], [-1301760000000, 1341.1131591796875], [-1299081600000, 1166.79296875], [-1296489600000, 1104.776611328125], [-1293811200000, 1108.0196533203125], [-1291132800000, 1000], [-1288713600000, 1000], [-1286035200000, 1047.9639892578125], [-1283443200000, 1052.17041015625], [-1280764800000, 1076.103271484375], [-1278172800000, 1063.1290283203125], [-1275494400000, 970.2550048828125], [-1272816000000, 846.5291748046875], [-1270224000000, 750.1344604492188], [-1267545600000, 720.1107177734375], [-1264953600000, 704.1276245117188], [-1262275200000, 848.9110107421875], [-1259596800000, 863.79736328125], [-1257177600000, 973.98291015625], [-1254499200000, 1102.93603515625], [-1251907200000, 1198.9139404296875], [-1249228800000, 1096.767578125], [-1246636800000, 1029.9390869140625], [-1243958400000, 904.43994140625], [-1241280000000, 775.7431030273438], [-1238688000000, 681.6340942382812], [-1236009600000, 664.40283203125], [-1233417600000, 655.92822265625], [-1230739200000, 651.861572265625], [-1228060800000, 675.2313232421875], [-1225641600000, 695.8599243164062], [-1222963200000, 747.8250122070312], [-1220371200000, 761.8070068359375], [-1217692800000, 710.5588989257812], [-1215100800000, 675.9491577148438], [-1212422400000, 635.1715698242188], [-1209744000000, 446.58544921875], [-1207152000000, 324.23486328125], [-1204473600000, 240], [-1201881600000, 240], [-1199203200000, 245.41122436523438], [-1196524800000, 261.8176574707031], [-1194019200000, 291.828125], [-1191340800000, 446.8841552734375], [-1188748800000, 529.68212890625], [-1186070400000, 537.2214965820312], [-1183478400000, 531.2853393554688], [-1180800000000, 495.4370422363281], [-1178121600000, 428.63916015625], [-1175529600000, 363.57965087890625], [-1172851200000, 345.06982421875], [-1170259200000, 334.12322998046875], [-1167580800000, 329.07696533203125], [-1164902400000, 325.1401672363281], [-1162483200000, 326.5812072753906], [-1159804800000, 474.5640563964844], [-1157212800000, 619.7142333984375], [-1154534400000, 590.2031860351562], [-1151942400000, 667.9938354492188], [-1149264000000, 640.584228515625], [-1146585600000, 559.6420288085938], [-1143993600000, 496.51336669921875], [-1141315200000, 420.90966796875], [-1138723200000, 380.9886474609375], [-1136044800000, 394.9450378417969], [-1133366400000, 470.657958984375], [-1130947200000, 555.55859375], [-1128268800000, 701.0433959960938], [-1125676800000, 761.1302490234375], [-1122998400000, 710.5675659179688], [-1120406400000, 663.4547119140625], [-1117728000000, 500], [-1115049600000, 265.34765625], [-1112457600000, 240], [-1109779200000, 240], [-1107187200000, 309.9076843261719], [-1104508800000, 346.80889892578125], [-1101830400000, 399.8347473144531], [-1099411200000, 487.5300598144531], [-1096732800000, 549.496337890625], [-1094140800000, 738.9435424804688], [-1091462400000, 748.3956909179688], [-1088870400000, 734.462890625], [-1086192000000, 643.6461181640625], [-1083513600000, 576.4384155273438], [-1080921600000, 512.8111572265625], [-1078243200000, 498.77313232421875], [-1075651200000, 493.3726501464844], [-1072972800000, 496.6994323730469], [-1070294400000, 622.733154296875], [-1067788800000, 794.2900390625], [-1065110400000, 911.2439575195312], [-1062518400000, 1064.1864013671875], [-1059840000000, 969.5798950195312], [-1057248000000, 906.3944702148438], [-1054569600000, 847.33740234375], [-1051891200000, 751.6823120117188], [-1049299200000, 671.0347900390625], [-1046620800000, 653.0413818359375], [-1044028800000, 641.9869995117188], [-1041350400000, 632.106689453125], [-1038672000000, 623.96142578125], [-1036252800000, 628.8916625976562], [-1033574400000, 767.984375], [-1030982400000, 996.12060546875], [-1028304000000, 1054.4185791015625], [-1025712000000, 1062.48291015625], [-1023033600000, 1013.36328125], [-1020355200000, 917.9347534179688], [-1017763200000, 836.7316284179688], [-1015084800000, 820.1512451171875], [-1012492800000, 995.536865234375], [-1009814400000, 1235.8648681640625], [-1007136000000, 1348.7261962890625], [-1004716800000, 1563.2227783203125], [-1002038400000, 1814.7791748046875], [-999446400000, 2119.79345703125], [-996768000000, 2310.6923828125], [-994176000000, 2230.99169921875], [-991497600000, 2182.341796875], [-988819200000, 2063.990966796875], [-986227200000, 1903.9276123046875], [-983548800000, 1817.2021484375], [-980956800000, 1809.1739501953125], [-978278400000, 1826.4481201171875], [-975600000000, 1840.47412109375], [-973180800000, 1855.94921875], [-970502400000, 1960.41943359375], [-967910400000, 2026.104736328125], [-965232000000, 1850.8759765625], [-962640000000, 1632.474853515625], [-959961600000, 1400.468017578125], [-957283200000, 1157.3765869140625], [-954691200000, 925.4961547851562], [-952012800000, 770.6715087890625], [-949420800000, 700], [-946742400000, 778.7760620117188], [-944064000000, 974.0433959960938], [-941558400000, 1397.3739013671875], [-938880000000, 1754.6346435546875], [-936288000000, 1999.6064453125], [-933609600000, 1902.0211181640625], [-931017600000, 1764.1077880859375], [-928339200000, 1611.1241455078125], [-925660800000, 1434.987548828125], [-923068800000, 1314.6561279296875], [-920390400000, 1261.941162109375], [-917798400000, 1224.3482666015625], [-915120000000, 1435.791015625], [-912441600000, 1698.005126953125], [-910022400000, 2000], [-907344000000, 2100], [-904752000000, 2300], [-902073600000, 2420], [-899481600000, 2441.83056640625], [-896803200000, 2270], [-894124800000, 2150], [-891532800000, 1938.1044921875], [-888854400000, 1850], [-886262400000, 1833.403076171875], [-883584000000, 1850], [-880905600000, 1900], [-878486400000, 2000], [-875808000000, 2085.322265625], [-873216000000, 2268.55078125], [-870537600000, 2313.054443359375], [-867945600000, 2212.041748046875], [-865267200000, 2169.251220703125], [-862588800000, 2053.612060546875], [-859996800000, 1861.7830810546875], [-857318400000, 1768.24853515625], [-854726400000, 1762.2139892578125], [-852048000000, 1812.391845703125], [-849369600000, 1900], [-846950400000, 2000], [-844272000000, 2100], [-841680000000, 2251.180419921875], [-839001600000, 2127.765380859375], [-836409600000, 2039.1458740234375], [-833731200000, 1899.2403564453125], [-831052800000, 1753.2882080078125], [-828460800000, 1582.346435546875], [-825782400000, 1428.1688232421875], [-823190400000, 1245.567626953125], [-820512000000, 1209.334716796875], [-817833600000, 1137.8297119140625], [-815328000000, 1184.3077392578125], [-812649600000, 1250.9937744140625], [-810057600000, 1278.6605224609375], [-807379200000, 1321.396484375], [-804787200000, 1281.1959228515625], [-802108800000, 1122.184326171875], [-799430400000, 1002.1633911132812], [-796838400000, 907.6779174804688], [-794160000000, 869.6024169921875], [-791568000000, 902.8389282226562], [-788889600000, 982.0956420898438], [-786211200000, 1043.212158203125], [-783792000000, 1221.3311767578125], [-781113600000, 1283.3319091796875], [-778521600000, 1423.9732666015625], [-775843200000, 1365.719970703125], [-773251200000, 1264.2164306640625], [-770572800000, 1136.50732421875], [-767894400000, 1039.474365234375], [-765302400000, 956.7459716796875], [-762624000000, 965.9564208984375], [-760032000000, 1060.5322265625], [-757353600000, 1338.46240234375], [-754675200000, 1491.2147216796875], [-752256000000, 1545.7166748046875], [-749577600000, 1677.970947265625], [-746985600000, 1888.547607421875], [-744307200000, 1867.1510009765625], [-741715200000, 1787.8524169921875], [-739036800000, 1703.6878662109375], [-736358400000, 1529.36669921875], [-733766400000, 1402.60546875], [-731088000000, 1288.2957763671875], [-728496000000, 1237.12353515625], [-725817600000, 1251.813720703125], [-723139200000, 1207.6854248046875], [-720720000000, 1269.3214111328125], [-718041600000, 1388.1697998046875], [-715449600000, 1467.833251953125], [-712771200000, 1364.5191650390625], [-710179200000, 1318.4952392578125], [-707500800000, 1113.16796875], [-704822400000, 992.97607421875], [-702230400000, 891.3436279296875], [-699552000000, 926.3023071289062], [-696960000000, 937.7455444335938], [-694281600000, 936.5424194335938], [-691603200000, 1147.08740234375], [-689097600000, 1172.493408203125], [-686419200000, 1204.094970703125], [-683827200000, 1400.6463623046875], [-681148800000, 1393.43212890625], [-678556800000, 1427.154296875], [-675878400000, 1372.5491943359375], [-673200000000, 1254.5921630859375], [-670608000000, 1206.248291015625], [-667929600000, 1190.18017578125], [-665337600000, 1194.957275390625], [-662659200000, 1195.077392578125], [-659980800000, 1187.2894287109375], [-657561600000, 1229.453857421875], [-654883200000, 1431.0465087890625], [-652291200000, 1689.1107177734375], [-649612800000, 1688.0137939453125], [-647020800000, 1618.505615234375], [-644342400000, 1466.503173828125], [-641664000000, 1323.27294921875], [-639072000000, 1196.33837890625], [-636393600000, 1172.057373046875], [-633801600000, 1165.0477294921875], [-631123200000, 1151.603271484375], [-628444800000, 1181.318603515625], [-626025600000, 1277.041259765625], [-623347200000, 1403.88427734375], [-620755200000, 1537.7958984375], [-618076800000, 1509.61572265625], [-615484800000, 1478.08837890625], [-612806400000, 1386.20263671875], [-610128000000, 1239.95703125], [-607536000000, 1113.4410400390625], [-604857600000, 1245.35693359375], [-602265600000, 1382.33203125], [-599587200000, 1657.7547607421875], [-596908800000, 1758.5833740234375], [-594489600000, 2000], [-591811200000, 2100], [-589219200000, 2258.9560546875], [-586540800000, 2155.64453125], [-583948800000, 2052.138427734375], [-581270400000, 1904.2403564453125], [-578592000000, 1727.384033203125], [-576000000000, 1557.364990234375], [-573321600000, 1471.9599609375], [-570729600000, 1485.115966796875], [-568051200000, 1669.99462890625], [-565372800000, 1742.201416015625], [-562867200000, 1968.24658203125], [-560188800000, 2100], [-557596800000, 2300], [-554918400000, 2408.224609375], [-552326400000, 2314.972900390625], [-549648000000, 2270], [-546969600000, 2150], [-544377600000, 1975], [-541699200000, 1850], [-539107200000, 1844.1614990234375], [-536428800000, 1850], [-533750400000, 1900], [-531331200000, 2000], [-528652800000, 2100], [-526060800000, 2300], [-523382400000, 2243.793701171875], [-520790400000, 2339.1416015625], [-518112000000, 2270], [-515433600000, 2130.55908203125], [-512841600000, 1903.232177734375], [-510163200000, 1847.3558349609375], [-507571200000, 1830.8487548828125], [-504892800000, 1850], [-502214400000, 1900], [-499795200000, 2000], [-497116800000, 2100], [-494524800000, 2300], [-491846400000, 2237.46533203125], [-489254400000, 2145.5556640625], [-486576000000, 1994.168701171875], [-483897600000, 1779.474609375], [-481305600000, 1616.75390625], [-478627200000, 1528.231201171875], [-476035200000, 1551.773681640625], [-473356800000, 1597.375732421875], [-470678400000, 1617.0589599609375], [-468259200000, 1647.30859375], [-465580800000, 1674.4100341796875], [-462988800000, 1714.68994140625], [-460310400000, 1699.391845703125], [-457718400000, 1660.4522705078125], [-455040000000, 1507.9952392578125], [-452361600000, 1361.8680419921875], [-449769600000, 1234.8856201171875], [-447091200000, 1175.2205810546875], [-444499200000, 1188.14501953125], [-441820800000, 1546.9541015625], [-439142400000, 1900], [-436636800000, 2000], [-433958400000, 2100], [-431366400000, 2291.12255859375], [-428688000000, 2369.041748046875], [-426096000000, 2253.35986328125], [-423417600000, 2205.768798828125], [-420739200000, 2087.822021484375], [-418147200000, 1898.059814453125], [-415468800000, 1842.0882568359375], [-412876800000, 1818.3963623046875], [-410198400000, 1810.4339599609375], [-407520000000, 1801.909912109375], [-405100800000, 1975.7225341796875], [-402422400000, 2100], [-399830400000, 2226.59375], [-397152000000, 2205.79833984375], [-394560000000, 2158.39404296875], [-391881600000, 2005.8955078125], [-389203200000, 1801.826171875], [-386611200000, 1637.6192626953125], [-383932800000, 1728.11669921875], [-381340800000, 1815.7822265625], [-378662400000, 1850], [-375984000000, 1900], [-373564800000, 2313.801025390625], [-370886400000, 2100], [-368294400000, 2300], [-365616000000, 2420], [-363024000000, 2376.110107421875], [-360345600000, 2270], [-357667200000, 2150], [-355075200000, 1961.38916015625], [-352396800000, 1850], [-349804800000, 1820.192138671875], [-347126400000, 1805.7642822265625], [-344448000000, 1900], [-342028800000, 2000], [-339350400000, 2100], [-336758400000, 2260.341064453125], [-334080000000, 2144.1787109375], [-331488000000, 2021.7830810546875], [-328809600000, 1788.527587890625], [-326131200000, 1580.6865234375], [-323539200000, 1386.1837158203125], [-320860800000, 1327.215087890625], [-318268800000, 1291.1260986328125], [-315590400000, 1270.8076171875], [-312912000000, 1271.39208984375], [-310406400000, 1444.7486572265625], [-307728000000, 1656.044921875], [-305136000000, 1786.135009765625], [-302457600000, 1705.519287109375], [-299865600000, 1679.251953125], [-297187200000, 1499.6033935546875], [-294508800000, 1356.017822265625], [-291916800000, 1228.68115234375], [-289238400000, 1170.785888671875], [-286646400000, 1186.450927734375], [-283968000000, 1267.193115234375], [-281289600000, 1311.682861328125], [-278870400000, 1513.3375244140625], [-276192000000, 1619.258544921875], [-273600000000, 1809.90966796875], [-270921600000, 1826.118408203125], [-268329600000, 1818.9791259765625], [-265651200000, 1663.9012451171875], [-262972800000, 1490.073486328125], [-260380800000, 1364.9266357421875], [-257702400000, 1307.8060302734375], [-255110400000, 1289.6124267578125], [-252432000000, 1304.6722412109375], [-249753600000, 1323.164306640625], [-247334400000, 1459.1768798828125], [-244656000000, 1530.611572265625], [-242064000000, 1860.5155029296875], [-239385600000, 1838.0670166015625], [-236793600000, 1786.1107177734375], [-234115200000, 1631.9205322265625], [-231436800000, 1460.9075927734375], [-228844800000, 1335.7318115234375], [-226166400000, 1484.726318359375], [-223574400000, 1536.96044921875], [-220896000000, 1709.3326416015625], [-218217600000, 1752.406982421875], [-215798400000, 2000], [-213120000000, 2076.72607421875], [-210528000000, 2300], [-207849600000, 2291.17236328125], [-205257600000, 2210.177490234375], [-202579200000, 2057.15185546875], [-199900800000, 1909.597900390625], [-197308800000, 1716.8446044921875], [-194630400000, 1622.8746337890625], [-192038400000, 1696.5548095703125], [-189360000000, 1723.2916259765625], [-186681600000, 1758.9588623046875], [-184176000000, 1768.667236328125], [-181497600000, 1797.506591796875], [-178905600000, 1827.239990234375], [-176227200000, 1711.75341796875], [-173635200000, 1640.2445068359375], [-170956800000, 1498.480712890625], [-168278400000, 1318.336181640625], [-165686400000, 1200.4307861328125], [-163008000000, 1148.366455078125], [-160416000000, 1178.5555419921875], [-157737600000, 1778.3465576171875], [-155059200000, 1900], [-152640000000, 1991.114013671875], [-149961600000, 2049.548095703125], [-147369600000, 2248.468994140625], [-144691200000, 2128.560791015625], [-142099200000, 2036.7503662109375], [-139420800000, 1894.566162109375], [-136742400000, 1730.3306884765625], [-134150400000, 1566.5352783203125], [-131472000000, 1475.2135009765625], [-128880000000, 1544.9498291015625], [-126201600000, 1573.3433837890625], [-123523200000, 1671.3372802734375], [-121104000000, 1750.2210693359375], [-118425600000, 1953.0167236328125], [-115833600000, 2211.215087890625], [-113155200000, 2165.791748046875], [-110563200000, 2029.1473388671875], [-107884800000, 1808.1759033203125], [-105206400000, 1579.1339111328125], [-102614400000, 1387.75048828125], [-99936000000, 1332.22509765625], [-97344000000, 1436.8958740234375], [-94665600000, 1598.473388671875], [-91987200000, 1737.8475341796875], [-89568000000, 1884.7340087890625], [-86889600000, 2022.4874267578125], [-84297600000, 2131.146240234375], [-81619200000, 2232.38134765625], [-79027200000, 2280.285400390625], [-76348800000, 2235.611328125], [-73670400000, 2100.49072265625], [-71078400000, 1919.170166015625], [-68400000000, 1821.6513671875], [-65808000000, 1796.6478271484375], [-63129600000, 1802.748291015625], [-60451200000, 1900], [-57945600000, 2000], [-55267200000, 2100], [-52675200000, 2169.785888671875], [-49996800000, 2036.1566162109375], [-47404800000, 1930.6162109375], [-44726400000, 1743.837646484375], [-42048000000, 1570.1190185546875], [-39456000000, 1403.4410400390625], [-36777600000, 1349.0986328125], [-34185600000, 1343.66748046875], [-31507200000, 1411.6556396484375], [-28828800000, 1583.9627685546875], [-26409600000, 1726.7724609375], [-23731200000, 1897.8853759765625], [-21139200000, 2223.9423828125], [-18460800000, 2420], [-15868800000, 2300.689453125], [-13190400000, 2249.552490234375], [-10512000000, 2129.728271484375], [-7920000000, 1975], [-5241600000, 1850], [-2649600000, 1849.840576171875], [28800000, 1850], [2707200000, 1952.061279296875], [5126400000, 2000], [7804800000, 2100], [10396800000, 2155.18603515625], [13075200000, 2029.79052734375], [15667200000, 1936.25732421875], [18345600000, 1752.3912353515625], [21024000000, 1575.7862548828125], [23616000000, 1346.0576171875], [26294400000, 1150.33740234375], [28886400000, 1277.213134765625], [31564800000, 1448.4390869140625], [34243200000, 1734.655029296875], [36662400000, 1855.7249755859375], [39340800000, 2029.4298095703125], [41932800000, 2185.185791015625], [44611200000, 2205.64501953125], [47203200000, 2200.950927734375], [49881600000, 2159.09326171875], [52560000000, 2011.64697265625], [55152000000, 1871.4124755859375], [57830400000, 1777.033447265625], [60422400000, 1590.838623046875], [63100800000, 1596.3609619140625], [65779200000, 1695.2178955078125], [68284800000, 1796.683837890625], [70963200000, 2095.47216796875], [73555200000, 2210.826904296875], [76233600000, 2106.83544921875], [78825600000, 2027.2720947265625], [81504000000, 1855.126220703125], [84182400000, 1680.1746826171875], [86774400000, 1511.6512451171875], [89452800000, 1460.8984375], [92044800000, 1499.6429443359375], [94723200000, 1605.1805419921875], [97401600000, 1779.194091796875], [99820800000, 1971.0438232421875], [102499200000, 2100], [105091200000, 2270.141357421875], [107769600000, 2304.42578125], [110361600000, 2222.260009765625], [113040000000, 2075.10205078125], [115718400000, 1851.95703125], [118310400000, 1684.0699462890625], [120988800000, 1648.9010009765625], [123580800000, 1850], [126259200000, 1850], [128937600000, 1900], [131356800000, 2000], [134035200000, 2100], [136627200000, 2300], [139305600000, 2350.135498046875], [141897600000, 2261.88134765625], [144576000000, 2134.6279296875], [147254400000, 2014.9984130859375], [149846400000, 1900.848876953125], [152524800000, 1836.80908203125], [155116800000, 1800.7105712890625], [157795200000, 1809.6241455078125], [160473600000, 1823.6827392578125], [162892800000, 1953.8624267578125], [165571200000, 2100], [168163200000, 2236.8935546875], [170841600000, 2354.015625], [173433600000, 2436.98046875], [176112000000, 2270], [178790400000, 2130.457275390625], [181382400000, 1942.8167724609375], [184060800000, 1850], [186652800000, 1850], [189331200000, 1850], [192009600000, 1661.9715576171875], [194515200000, 1691.09033203125], [197193600000, 1726.5272216796875], [199785600000, 1780.3582763671875], [202464000000, 1723.6058349609375], [205056000000, 1637.16943359375], [207734400000, 1426.636474609375], [210412800000, 1263.2967529296875], [213004800000, 1141.3934326171875], [215683200000, 1085.121337890625], [218275200000, 1000], [220953600000, 982.4788818359375], [223632000000, 970.6820678710938], [226051200000, 961.4900512695312], [228729600000, 946.6987915039062], [231321600000, 914.0321655273438], [234000000000, 854.9295043945312], [236592000000, 813.84521484375], [239270400000, 586.4174194335938], [241948800000, 335.2364196777344], [244540800000, 281.9508972167969], [247219200000, 240], [249811200000, 265.5590515136719], [252489600000, 490.7522277832031], [255168000000, 942.3346557617188], [257587200000, 1149.302978515625], [260265600000, 1443.6328125], [262857600000, 1629.334716796875], [265536000000, 1619.7518310546875], [268128000000, 1509.7247314453125], [270806400000, 1416.7740478515625], [273484800000, 1325.8011474609375], [276076800000, 1249.20361328125], [278755200000, 1195.4425048828125], [281347200000, 1188.463623046875], [284025600000, 1180.562744140625], [286704000000, 1208.2186279296875], [289123200000, 1278.296142578125], [291801600000, 1430.9912109375], [294393600000, 1540.743408203125], [297072000000, 1578.561279296875], [299664000000, 1537.2125244140625], [302342400000, 1434.2589111328125], [305020800000, 1289.369384765625], [307612800000, 1163.3546142578125], [310291200000, 1188.9180908203125], [312883200000, 1254.5245361328125], [315561600000, 1325.5684814453125], [318240000000, 1578.3504638671875], [320745600000, 1922.950927734375], [323424000000, 2065.5927734375], [326016000000, 2216.9345703125], [328694400000, 2092.306640625], [331286400000, 1940.8914794921875], [333964800000, 1800.6619873046875], [336643200000, 1654.693115234375], [339235200000, 1486.227294921875], [341913600000, 1430.5567626953125], [344505600000, 1393.821044921875], [347184000000, 1434.55859375], [349862400000, 1551.24072265625], [352281600000, 1685.3948974609375], [354960000000, 1799.8887939453125], [357552000000, 1891.441162109375], [360230400000, 1794.5855712890625], [362822400000, 1725.00537109375], [365500800000, 1495.6436767578125], [368179200000, 1294.30810546875], [370771200000, 1171.30908203125], [373449600000, 1143.713623046875], [376041600000, 1415.3531494140625], [378720000000, 1779.001953125], [381398400000, 1878.3125], [383817600000, 2000], [386496000000, 2100], [389088000000, 2300], [391766400000, 2281.73828125], [394358400000, 2107.36181640625], [397036800000, 1971.4764404296875], [399715200000, 1944.80224609375], [402307200000, 1865.6871337890625], [404985600000, 1850], [407577600000, 1850], [410256000000, 1850], [412934400000, 1900], [415353600000, 2000], [418032000000, 2181.236328125], [420624000000, 2300], [423302400000, 2420], [425894400000, 2447], [428572800000, 2270], [431251200000, 2150], [433843200000, 1975], [436521600000, 1850], [439113600000, 1850], [441792000000, 1850], [444470400000, 1900], [446976000000, 2000], [449654400000, 2100], [452246400000, 2197.5888671875], [454924800000, 2145.0400390625], [457516800000, 2062.463623046875], [460195200000, 1918.213134765625], [462873600000, 1761.021240234375], [465465600000, 1591.4852294921875], [468144000000, 1501.968017578125], [470736000000, 1614.3140869140625], [473414400000, 1659.271728515625], [476092800000, 1689.275146484375], [478512000000, 1721.5311279296875], [481190400000, 1765.185791015625], [483782400000, 1894.510009765625], [486460800000, 1794.8406982421875], [489052800000, 1682.531982421875], [491731200000, 1503.1041259765625], [494409600000, 1263.8321533203125], [497001600000, 1091.588134765625], [499680000000, 1043.1973876953125], [502272000000, 1034.292236328125], [504950400000, 1053.6888427734375], [507628800000, 1205.2420654296875], [510048000000, 1757.0208740234375], [512726400000, 2100], [515318400000, 2116.00634765625], [517996800000, 1974.6180419921875], [520588800000, 1834.326904296875], [523267200000, 1651.431396484375], [525945600000, 1476.19287109375], [528537600000, 1351.7603759765625], [531216000000, 1300.7064208984375], [533808000000, 1269.3983154296875], [536486400000, 1262.2655029296875], [539164800000, 1273.6934814453125], [541584000000, 1356.07373046875], [544262400000, 1575.6346435546875], [546854400000, 1716.9166259765625], [549532800000, 1633.0380859375], [552124800000, 1422.8006591796875], [554803200000, 1213.2998046875], [557481600000, 1031.9149169921875], [560073600000, 939.7525634765625], [562752000000, 911.1941528320312], [565344000000, 897.153564453125], [568022400000, 1105.0504150390625], [570700800000, 1211.439208984375], [573206400000, 1310.3634033203125], [575884800000, 1388.751953125], [578476800000, 1449.7802734375], [581155200000, 1375.7078857421875], [583747200000, 1331.9906005859375], [586425600000, 1199.83154296875], [589104000000, 1088.98974609375], [591696000000, 996.4647827148438], [594374400000, 967.8063354492188], [596966400000, 997.4773559570312], [599644800000, 1000], [602323200000, 1029.1156005859375], [604742400000, 1054.309326171875], [607420800000, 1410.3365478515625], [610012800000, 1601.6136474609375], [612691200000, 1466.8712158203125], [615283200000, 1341.970947265625], [617961600000, 1189.55419921875], [620640000000, 1079.35009765625], [623232000000, 986.9411010742188], [625910400000, 1012.0980834960938], [628502400000, 1014.8975219726562], [631180800000, 1009.7457275390625], [633859200000, 1090.0028076171875], [636278400000, 1117.0999755859375], [638956800000, 1201.5802001953125], [641548800000, 1232.0584716796875], [644227200000, 1175.701416015625], [646819200000, 1168.815673828125], [649497600000, 1092.4010009765625], [652176000000, 982.1854248046875], [654768000000, 890.0685424804688], [657446400000, 861.730712890625], [660038400000, 847.7219848632812], [662716800000, 831.8701171875], [665395200000, 815.3024291992188], [667814400000, 817.5309448242188], [670492800000, 880.3597412109375], [673084800000, 911.8548583984375], [675763200000, 924.1795043945312], [678355200000, 915.201416015625], [681033600000, 826.2208862304688], [683712000000, 707.527099609375], [686304000000, 616.3118286132812], [688982400000, 600.7108154296875], [691574400000, 591.8984985351562], [694252800000, 588.2628784179688], [696931200000, 596.4417114257812], [699436800000, 763.1439208984375], [702115200000, 922.8635864257812], [704707200000, 1076.1556396484375], [707385600000, 1042.181640625], [709977600000, 1010.5653076171875], [712656000000, 923.2814331054688], [715334400000, 804.83935546875], [717926400000, 713.520751953125], [720604800000, 685.3243408203125], [723196800000, 675.84228515625], [725875200000, 706.8045654296875], [728553600000, 788.6881713867188], [730972800000, 945.3323364257812], [733651200000, 1331.5548095703125], [736243200000, 1523.413818359375], [738921600000, 1653.6168212890625], [741513600000, 1698.18310546875], [744192000000, 1660.0435791015625], [746870400000, 1526.6715087890625], [749462400000, 1434.93310546875], [752140800000, 1382.34375], [754732800000, 1345.733642578125], [757411200000, 1360.2669677734375], [760089600000, 1321.87451171875], [762508800000, 1348.9779052734375], [765187200000, 1400.1636962890625], [767779200000, 1433.54052734375], [770457600000, 1409.7528076171875], [773049600000, 1317.2923583984375], [775728000000, 1171.935546875], [778406400000, 1051.16796875], [780998400000, 958.965576171875], [783676800000, 930.6065063476562], [786268800000, 916.912109375], [788947200000, 903.3563842773438], [791625600000, 1381.32177734375], [794044800000, 1639.9459228515625], [796723200000, 2099.123779296875], [799315200000, 2300], [801993600000, 2359.109619140625], [804585600000, 2277.514892578125], [807264000000, 2261.71826171875], [809942400000, 2150], [812534400000, 1975], [815212800000, 1850], [817804800000, 1839.3939208984375], [820483200000, 1850], [823161600000, 1900], [825667200000, 2000], [828345600000, 2100], [830937600000, 2273.342529296875], [833616000000, 2183.30029296875], [836208000000, 2094.8349609375], [838886400000, 2034.43408203125], [841564800000, 1885.581787109375], [844156800000, 1746.110595703125], [846835200000, 1682.4254150390625], [849427200000, 1687.01171875], [852105600000, 1850], [854784000000, 1900], [857203200000, 1990.588134765625], [859881600000, 2067.729736328125], [862473600000, 2145.8203125], [865152000000, 1938.7017822265625], [867744000000, 1758.1317138671875], [870422400000, 1572.7969970703125], [873100800000, 1395.635009765625], [875692800000, 1268.8656005859375], [878371200000, 1112.1312255859375], [880963200000, 1100], [883641600000, 1146.311279296875], [886320000000, 1497.7535400390625], [888739200000, 1905.865966796875], [891417600000, 2100], [894009600000, 2289.962158203125], [896688000000, 2404.483154296875], [899280000000, 2447], [901958400000, 2270], [904636800000, 2150], [907228800000, 1975], [909907200000, 1850], [912499200000, 1850], [915177600000, 1850], [917856000000, 1900], [920275200000, 2000], [922953600000, 2100], [925545600000, 2299.83740234375], [928224000000, 2351.3359375], [930816000000, 2340.712646484375], [933494400000, 2194.433837890625], [936172800000, 2057.798828125], [938764800000, 1920.3587646484375], [941443200000, 1825.66162109375], [944035200000, 1661.1929931640625], [946713600000, 1502.6138916015625], [949392000000, 1683.3177490234375], [951897600000, 2000], [954576000000, 2100], [957168000000, 2299.457275390625], [959846400000, 2277.84423828125], [962438400000, 2240.8212890625], [965116800000, 2098.01025390625], [967795200000, 1874.0792236328125], [970387200000, 1708.2916259765625], [973065600000, 1617.677978515625], [975657600000, 1498.4013671875], [978336000000, 1489.0357666015625], [981014400000, 1491.1868896484375], [983433600000, 1538.7103271484375], [986112000000, 1718.67822265625], [988704000000, 1809.9520263671875], [991382400000, 1768.31591796875], [993974400000, 1578.70458984375], [996652800000, 1400.1962890625], [999331200000, 1234.8326416015625], [1001923200000, 1116.434814453125], [1004601600000, 1076.7418212890625], [1007193600000, 1129.414794921875], [1009872000000, 1266.149169921875], [1012550400000, 1521.453125], [1014969600000, 1636.603515625], [1017648000000, 1740.4498291015625], [1020240000000, 1925.248291015625], [1022918400000, 1819.83740234375], [1025510400000, 1726.1591796875], [1028188800000, 1572.14111328125], [1030867200000, 1395.6378173828125], [1033459200000, 1269.1959228515625], [1036137600000, 1209.5445556640625], [1038729600000, 1175.1895751953125], [1041408000000, 1400.58251953125], [1044086400000, 1781.8746337890625], [1046505600000, 1905.5660400390625], [1049184000000, 2067.465087890625], [1051776000000, 2270.27734375], [1054454400000, 2323.992431640625], [1057046400000, 2209.25537109375], [1059724800000, 2069.65283203125], [1062403200000, 1961.1492919921875], [1064995200000, 1820.3785400390625]]
                    }, {
                        "period_months": ["Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep"],
                        "annual_period": "Sacramento River Index<br>Wet",
                        "computed_statistics": [{
                            "statistic": "Averages",
                            "statistically_computed_time_series_monthly": [["Jan", 1759.8407897949219], ["Feb", 1939.6207987467449], ["Mar", 2071.8445027669272], ["Apr", 2243.091267903646], ["May", 2263.802693684896], ["Jun", 2204.175043741862], ["Jul", 2093.3194478352866], ["Aug", 1960.6475830078125], ["Sep", 1792.1432291666667], ["Oct", 1466.1740137736003], ["Nov", 1504.8130696614583], ["Dec", 1624.5510991414387]],
                            "statistic_aggregate": 1906.019568027594
                        }],
                        "aggregate_ts": [[1927, 1648.9219055175781], [1938, 1715.8349202473958], [1941, 1936.6683858235676], [1942, 2033.0848897298176], [1943, 1916.3184204101562], [1952, 1987.976338704427], [1953, 2060.9073384602866], [1956, 1917.9578755696614], [1958, 2098.7666015625], [1963, 1920.4247233072917], [1965, 1804.2619120279949], [1967, 1909.3123779296875], [1969, 1884.3295796712239], [1970, 1866.4475708007812], [1971, 1844.1444905598958], [1974, 2025.9494222005208], [1975, 2057.987721761068], [1982, 1898.9539286295574], [1983, 2099.4363606770835], [1984, 1935.4843444824219], [1986, 1549.8147786458333], [1995, 1766.2174377441406], [1996, 1979.7498474121094], [1997, 1771.4755350748699], [1998, 1866.5422770182292], [1999, 2059.539784749349]],
                        "month_period": "October - September",
                        "discrete_ts": [[-1362240000000, 871.78662109375], [-1359648000000, 1069.8948974609375], [-1356969600000, 1288.040283203125], [-1354291200000, 1418.64892578125], [-1351872000000, 1718.511474609375], [-1349193600000, 1908.496826171875], [-1346601600000, 2132.911865234375], [-1343923200000, 2126.205322265625], [-1341331200000, 2017.4195556640625], [-1338652800000, 1892.9815673828125], [-1335974400000, 1742.869384765625], [-1333382400000, 1599.296142578125], [-1015084800000, 820.1512451171875], [-1012492800000, 995.536865234375], [-1009814400000, 1235.8648681640625], [-1007136000000, 1348.7261962890625], [-1004716800000, 1563.2227783203125], [-1002038400000, 1814.7791748046875], [-999446400000, 2119.79345703125], [-996768000000, 2310.6923828125], [-994176000000, 2230.99169921875], [-991497600000, 2182.341796875], [-988819200000, 2063.990966796875], [-986227200000, 1903.9276123046875], [-920390400000, 1261.941162109375], [-917798400000, 1224.3482666015625], [-915120000000, 1435.791015625], [-912441600000, 1698.005126953125], [-910022400000, 2000], [-907344000000, 2100], [-904752000000, 2300], [-902073600000, 2420], [-899481600000, 2441.83056640625], [-896803200000, 2270], [-894124800000, 2150], [-891532800000, 1938.1044921875], [-888854400000, 1850], [-886262400000, 1833.403076171875], [-883584000000, 1850], [-880905600000, 1900], [-878486400000, 2000], [-875808000000, 2085.322265625], [-873216000000, 2268.55078125], [-870537600000, 2313.054443359375], [-867945600000, 2212.041748046875], [-865267200000, 2169.251220703125], [-862588800000, 2053.612060546875], [-859996800000, 1861.7830810546875], [-857318400000, 1768.24853515625], [-854726400000, 1762.2139892578125], [-852048000000, 1812.391845703125], [-849369600000, 1900], [-846950400000, 2000], [-844272000000, 2100], [-841680000000, 2251.180419921875], [-839001600000, 2127.765380859375], [-836409600000, 2039.1458740234375], [-833731200000, 1899.2403564453125], [-831052800000, 1753.2882080078125], [-828460800000, 1582.346435546875], [-573321600000, 1471.9599609375], [-570729600000, 1485.115966796875], [-568051200000, 1669.99462890625], [-565372800000, 1742.201416015625], [-562867200000, 1968.24658203125], [-560188800000, 2100], [-557596800000, 2300], [-554918400000, 2408.224609375], [-552326400000, 2314.972900390625], [-549648000000, 2270], [-546969600000, 2150], [-544377600000, 1975], [-541699200000, 1850], [-539107200000, 1844.1614990234375], [-536428800000, 1850], [-533750400000, 1900], [-531331200000, 2000], [-528652800000, 2100], [-526060800000, 2300], [-523382400000, 2243.793701171875], [-520790400000, 2339.1416015625], [-518112000000, 2270], [-515433600000, 2130.55908203125], [-512841600000, 1903.232177734375], [-447091200000, 1175.2205810546875], [-444499200000, 1188.14501953125], [-441820800000, 1546.9541015625], [-439142400000, 1900], [-436636800000, 2000], [-433958400000, 2100], [-431366400000, 2291.12255859375], [-428688000000, 2369.041748046875], [-426096000000, 2253.35986328125], [-423417600000, 2205.768798828125], [-420739200000, 2087.822021484375], [-418147200000, 1898.059814453125], [-383932800000, 1728.11669921875], [-381340800000, 1815.7822265625], [-378662400000, 1850], [-375984000000, 1900], [-373564800000, 2313.801025390625], [-370886400000, 2100], [-368294400000, 2300], [-365616000000, 2420], [-363024000000, 2376.110107421875], [-360345600000, 2270], [-357667200000, 2150], [-355075200000, 1961.38916015625], [-226166400000, 1484.726318359375], [-223574400000, 1536.96044921875], [-220896000000, 1709.3326416015625], [-218217600000, 1752.406982421875], [-215798400000, 2000], [-213120000000, 2076.72607421875], [-210528000000, 2300], [-207849600000, 2291.17236328125], [-205257600000, 2210.177490234375], [-202579200000, 2057.15185546875], [-199900800000, 1909.597900390625], [-197308800000, 1716.8446044921875], [-163008000000, 1148.366455078125], [-160416000000, 1178.5555419921875], [-157737600000, 1778.3465576171875], [-155059200000, 1900], [-152640000000, 1991.114013671875], [-149961600000, 2049.548095703125], [-147369600000, 2248.468994140625], [-144691200000, 2128.560791015625], [-142099200000, 2036.7503662109375], [-139420800000, 1894.566162109375], [-136742400000, 1730.3306884765625], [-134150400000, 1566.5352783203125], [-99936000000, 1332.22509765625], [-97344000000, 1436.8958740234375], [-94665600000, 1598.473388671875], [-91987200000, 1737.8475341796875], [-89568000000, 1884.7340087890625], [-86889600000, 2022.4874267578125], [-84297600000, 2131.146240234375], [-81619200000, 2232.38134765625], [-79027200000, 2280.285400390625], [-76348800000, 2235.611328125], [-73670400000, 2100.49072265625], [-71078400000, 1919.170166015625], [-36777600000, 1349.0986328125], [-34185600000, 1343.66748046875], [-31507200000, 1411.6556396484375], [-28828800000, 1583.9627685546875], [-26409600000, 1726.7724609375], [-23731200000, 1897.8853759765625], [-21139200000, 2223.9423828125], [-18460800000, 2420], [-15868800000, 2300.689453125], [-13190400000, 2249.552490234375], [-10512000000, 2129.728271484375], [-7920000000, 1975], [-5241600000, 1850], [-2649600000, 1849.840576171875], [28800000, 1850], [2707200000, 1952.061279296875], [5126400000, 2000], [7804800000, 2100], [10396800000, 2155.18603515625], [13075200000, 2029.79052734375], [15667200000, 1936.25732421875], [18345600000, 1752.3912353515625], [21024000000, 1575.7862548828125], [23616000000, 1346.0576171875], [26294400000, 1150.33740234375], [28886400000, 1277.213134765625], [31564800000, 1448.4390869140625], [34243200000, 1734.655029296875], [36662400000, 1855.7249755859375], [39340800000, 2029.4298095703125], [41932800000, 2185.185791015625], [44611200000, 2205.64501953125], [47203200000, 2200.950927734375], [49881600000, 2159.09326171875], [52560000000, 2011.64697265625], [55152000000, 1871.4124755859375], [120988800000, 1648.9010009765625], [123580800000, 1850], [126259200000, 1850], [128937600000, 1900], [131356800000, 2000], [134035200000, 2100], [136627200000, 2300], [139305600000, 2350.135498046875], [141897600000, 2261.88134765625], [144576000000, 2134.6279296875], [147254400000, 2014.9984130859375], [149846400000, 1900.848876953125], [152524800000, 1836.80908203125], [155116800000, 1800.7105712890625], [157795200000, 1809.6241455078125], [160473600000, 1823.6827392578125], [162892800000, 1953.8624267578125], [165571200000, 2100], [168163200000, 2236.8935546875], [170841600000, 2354.015625], [173433600000, 2436.98046875], [176112000000, 2270], [178790400000, 2130.457275390625], [181382400000, 1942.8167724609375], [373449600000, 1143.713623046875], [376041600000, 1415.3531494140625], [378720000000, 1779.001953125], [381398400000, 1878.3125], [383817600000, 2000], [386496000000, 2100], [389088000000, 2300], [391766400000, 2281.73828125], [394358400000, 2107.36181640625], [397036800000, 1971.4764404296875], [399715200000, 1944.80224609375], [402307200000, 1865.6871337890625], [404985600000, 1850], [407577600000, 1850], [410256000000, 1850], [412934400000, 1900], [415353600000, 2000], [418032000000, 2181.236328125], [420624000000, 2300], [423302400000, 2420], [425894400000, 2447], [428572800000, 2270], [431251200000, 2150], [433843200000, 1975], [436521600000, 1850], [439113600000, 1850], [441792000000, 1850], [444470400000, 1900], [446976000000, 2000], [449654400000, 2100], [452246400000, 2197.5888671875], [454924800000, 2145.0400390625], [457516800000, 2062.463623046875], [460195200000, 1918.213134765625], [462873600000, 1761.021240234375], [465465600000, 1591.4852294921875], [499680000000, 1043.1973876953125], [502272000000, 1034.292236328125], [504950400000, 1053.6888427734375], [507628800000, 1205.2420654296875], [510048000000, 1757.0208740234375], [512726400000, 2100], [515318400000, 2116.00634765625], [517996800000, 1974.6180419921875], [520588800000, 1834.326904296875], [523267200000, 1651.431396484375], [525945600000, 1476.19287109375], [528537600000, 1351.7603759765625], [783676800000, 930.6065063476562], [786268800000, 916.912109375], [788947200000, 903.3563842773438], [791625600000, 1381.32177734375], [794044800000, 1639.9459228515625], [796723200000, 2099.123779296875], [799315200000, 2300], [801993600000, 2359.109619140625], [804585600000, 2277.514892578125], [807264000000, 2261.71826171875], [809942400000, 2150], [812534400000, 1975], [815212800000, 1850], [817804800000, 1839.3939208984375], [820483200000, 1850], [823161600000, 1900], [825667200000, 2000], [828345600000, 2100], [830937600000, 2273.342529296875], [833616000000, 2183.30029296875], [836208000000, 2094.8349609375], [838886400000, 2034.43408203125], [841564800000, 1885.581787109375], [844156800000, 1746.110595703125], [846835200000, 1682.4254150390625], [849427200000, 1687.01171875], [852105600000, 1850], [854784000000, 1900], [857203200000, 1990.588134765625], [859881600000, 2067.729736328125], [862473600000, 2145.8203125], [865152000000, 1938.7017822265625], [867744000000, 1758.1317138671875], [870422400000, 1572.7969970703125], [873100800000, 1395.635009765625], [875692800000, 1268.8656005859375], [878371200000, 1112.1312255859375], [880963200000, 1100], [883641600000, 1146.311279296875], [886320000000, 1497.7535400390625], [888739200000, 1905.865966796875], [891417600000, 2100], [894009600000, 2289.962158203125], [896688000000, 2404.483154296875], [899280000000, 2447], [901958400000, 2270], [904636800000, 2150], [907228800000, 1975], [909907200000, 1850], [912499200000, 1850], [915177600000, 1850], [917856000000, 1900], [920275200000, 2000], [922953600000, 2100], [925545600000, 2299.83740234375], [928224000000, 2351.3359375], [930816000000, 2340.712646484375], [933494400000, 2194.433837890625], [936172800000, 2057.798828125], [938764800000, 1920.3587646484375]]
                    }, {
                        "period_months": ["Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep"],
                        "annual_period": "Dry Periods<br>1929 - 1934",
                        "computed_statistics": [{
                            "statistic": "Averages",
                            "statistically_computed_time_series_monthly": [["Jan", 531.4966278076172], ["Feb", 572.0630416870117], ["Mar", 693.0523147583008], ["Apr", 777.5293273925781], ["May", 733.6877899169922], ["Jun", 726.2918548583984], ["Jul", 668.9081954956055], ["Aug", 552.6524353027344], ["Sep", 466.49049377441406], ["Oct", 492.3958435058594], ["Nov", 483.5447692871094], ["Dec", 518.8151931762695]],
                            "statistic_aggregate": 654.4193000793457
                        }],
                        "aggregate_ts": [[1929, 1015.4895477294922], [1930, 908.4419504801432], [1931, 637.1179860432943], [1932, 384.3154983520508], [1933, 475.7671941121419], [1934, 505.38362375895184]],
                        "month_period": "October - September",
                        "discrete_ts": [[-1299081600000, 1166.79296875], [-1296489600000, 1104.776611328125], [-1293811200000, 1108.0196533203125], [-1291132800000, 1000], [-1288713600000, 1000], [-1286035200000, 1047.9639892578125], [-1283443200000, 1052.17041015625], [-1280764800000, 1076.103271484375], [-1278172800000, 1063.1290283203125], [-1275494400000, 970.2550048828125], [-1272816000000, 846.5291748046875], [-1270224000000, 750.1344604492188], [-1267545600000, 720.1107177734375], [-1264953600000, 704.1276245117188], [-1262275200000, 848.9110107421875], [-1259596800000, 863.79736328125], [-1257177600000, 973.98291015625], [-1254499200000, 1102.93603515625], [-1251907200000, 1198.9139404296875], [-1249228800000, 1096.767578125], [-1246636800000, 1029.9390869140625], [-1243958400000, 904.43994140625], [-1241280000000, 775.7431030273438], [-1238688000000, 681.6340942382812], [-1236009600000, 664.40283203125], [-1233417600000, 655.92822265625], [-1230739200000, 651.861572265625], [-1228060800000, 675.2313232421875], [-1225641600000, 695.8599243164062], [-1222963200000, 747.8250122070312], [-1220371200000, 761.8070068359375], [-1217692800000, 710.5588989257812], [-1215100800000, 675.9491577148438], [-1212422400000, 635.1715698242188], [-1209744000000, 446.58544921875], [-1207152000000, 324.23486328125], [-1204473600000, 240], [-1201881600000, 240], [-1199203200000, 245.41122436523438], [-1196524800000, 261.8176574707031], [-1194019200000, 291.828125], [-1191340800000, 446.8841552734375], [-1188748800000, 529.68212890625], [-1186070400000, 537.2214965820312], [-1183478400000, 531.2853393554688], [-1180800000000, 495.4370422363281], [-1178121600000, 428.63916015625], [-1175529600000, 363.57965087890625], [-1172851200000, 345.06982421875], [-1170259200000, 334.12322998046875], [-1167580800000, 329.07696533203125], [-1164902400000, 325.1401672363281], [-1162483200000, 326.5812072753906], [-1159804800000, 474.5640563964844], [-1157212800000, 619.7142333984375], [-1154534400000, 590.2031860351562], [-1151942400000, 667.9938354492188], [-1149264000000, 640.584228515625], [-1146585600000, 559.6420288085938], [-1143993600000, 496.51336669921875], [-1141315200000, 420.90966796875], [-1138723200000, 380.9886474609375], [-1136044800000, 394.9450378417969], [-1133366400000, 470.657958984375], [-1130947200000, 555.55859375], [-1128268800000, 701.0433959960938], [-1125676800000, 761.1302490234375], [-1122998400000, 710.5675659179688], [-1120406400000, 663.4547119140625], [-1117728000000, 500], [-1115049600000, 265.34765625], [-1112457600000, 240]]
                    }]
                }]
            }], "scenario_name": "ROConLTO", "scenario_color": "#62D2FAFF"
        }, {
            "ts_list": [{
                "ts_name": "M_DV (CalSim3)", "monthly_filters": [{
                    "annual_filters": [{
                        "period_months": ["Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep"],
                        "annual_period": "Long Term<br>",
                        "computed_statistics": [{
                            "statistic": "Averages",
                            "statistically_computed_time_series_monthly": [["Jan", 1491.8248857897881], ["Feb", 1599.848072807667], ["Mar", 1722.292638620831], ["Apr", 1872.948568630813], ["May", 1875.0881430298361], ["Jun", 1850.467602615176], ["Jul", 1724.1661998478087], ["Aug", 1571.3394007814832], ["Sep", 1433.5510506109058], ["Oct", 1378.0509527092495], ["Nov", 1387.5300022062675], ["Dec", 1435.1456976760055]],
                            "statistic_aggregate": 1616.6669235669506
                        }],
                        "aggregate_ts": [[1922, 1841.5797360300705], [1923, 1740.1594346844813], [1924, 1187.32108988423], [1925, 1070.8850846109347], [1926, 1163.8816935662692], [1927, 1652.8604975750088], [1928, 1879.8203155707233], [1929, 1294.6902831214745], [1930, 1070.3234416782216], [1931, 785.0043665926137], [1932, 531.8799596467431], [1933, 431.4020054978608], [1934, 408.1666010316299], [1935, 532.8193052111742], [1936, 817.1436777897585], [1937, 1022.5069820923903], [1938, 1855.0923735983267], [1939, 1732.8426016898268], [1940, 1459.0087882482574], [1941, 1936.2472223518537], [1942, 2081.5224219711477], [1943, 1962.2294724979547], [1944, 1573.8766180492987], [1945, 1385.244049615116], [1946, 1623.1959899025567], [1947, 1504.288999280625], [1948, 1528.2018186762261], [1949, 1899.856247256499], [1950, 1736.7426716605116], [1951, 1925.4657288375563], [1952, 1999.9201358253747], [1953, 2079.166662283231], [1954, 1998.6250986225798], [1955, 1738.8945568204251], [1956, 1988.3754948530823], [1957, 2059.881623805229], [1958, 2095.787914252148], [1959, 1916.849150228126], [1960, 1579.0232433541726], [1961, 1599.201772203147], [1962, 1502.7943638749575], [1963, 1929.8319697786253], [1964, 1778.119563691549], [1965, 1883.9440113010348], [1966, 1957.3913195565635], [1967, 2028.9490486783977], [1968, 1880.9790039097354], [1969, 1889.0944720982363], [1970, 1878.3032862745015], [1971, 1967.2830686410173], [1972, 1949.7899580085848], [1973, 1977.1664116677957], [1974, 2078.0575749833565], [1975, 2075.226115425811], [1976, 1693.8791352580922], [1977, 854.0678054494391], [1978, 1192.6028406350827], [1979, 1517.9344124327363], [1980, 1768.2255847503793], [1981, 1712.5292380160215], [1982, 1980.6598784110167], [1983, 2092.6666666666665], [1984, 1969.3273800578545], [1985, 1758.8742916637818], [1986, 1652.0402249417523], [1987, 1438.502789044871], [1988, 1181.8913479013534], [1989, 1181.2326601425523], [1990, 1080.8528932082386], [1991, 875.5631414315104], [1992, 892.4844990944099], [1993, 1344.2937427251352], [1994, 1570.6194306122477], [1995, 1707.5349467986018], [1996, 2024.4342041017615], [1997, 1849.3322183683194], [1998, 1942.7155393388905], [1999, 2083.2415157304436], [2000, 2035.6020042539392], [2001, 1884.9192469618047], [2002, 1807.2662670954376], [2003, 2006.478529040598]],
                        "month_period": "October - September",
                        "discrete_ts": [[-1525276800000, 1826.515380859375], [-1522684800000, 2000], [-1520006400000, 1773.2214198236736], [-1517414400000, 1745.3145636773404], [-1514736000000, 1746.68711908607], [-1512057600000, 1751.9650549707685], [-1509638400000, 1774.260653652005], [-1506960000000, 1823.6026399326593], [-1504368000000, 1941.955766961643], [-1501689600000, 2022.4186875530988], [-1499097600000, 2070.635429401276], [-1496419200000, 1961.2139217534007], [-1493740800000, 1815.0543049977505], [-1491148800000, 1672.6272705511612], [-1488470400000, 1664.4297262050404], [-1485878400000, 1665.851466257749], [-1483200000000, 1683.6510254276584], [-1480521600000, 1710.1535237803214], [-1478102400000, 1732.5988762215916], [-1475424000000, 1784.9850083836795], [-1472832000000, 1909.0440015840798], [-1470153600000, 1961.1979733858989], [-1467561600000, 1924.484537828315], [-1464883200000, 1787.681934566766], [-1462204800000, 1612.5476958679083], [-1459612800000, 1445.2874467047668], [-1456934400000, 1390.3751338286702], [-1454342400000, 1370.319195955063], [-1451664000000, 1354.6149702772866], [-1448985600000, 1261.9707208031336], [-1446480000000, 1307.219634249399], [-1443801600000, 1298.56994367436], [-1441209600000, 1267.7129998109754], [-1438531200000, 1183.9779382348404], [-1435939200000, 1126.6390627810215], [-1433260800000, 1026.1863405531712], [-1430582400000, 900.8863667635201], [-1427990400000, 759.3807716793193], [-1425312000000, 739.4215154717185], [-1422720000000, 811.4204485755234], [-1420041600000, 853.8103810780145], [-1417363200000, 719.773831986326], [-1414944000000, 990.5919803223803], [-1412265600000, 1128.6253501681904], [-1409673600000, 1425.5771289729075], [-1406995200000, 1444.435157989715], [-1404403200000, 1389.0027804684942], [-1401724800000, 1241.8715259281898], [-1399046400000, 1087.2478434753434], [-1396454400000, 1018.8430708944132], [-1393776000000, 995.9999561617323], [-1391184000000, 993.344017953874], [-1388505600000, 1014.3948482114814], [-1385827200000, 1018.9817311138598], [-1383408000000, 1166.1434815804423], [-1380729600000, 1280.9602610747354], [-1378137600000, 1503.504826564321], [-1375459200000, 1398.989367267462], [-1372867200000, 1328.9394862219924], [-1370188800000, 1195.5760462491937], [-1367510400000, 1082.4851699246558], [-1364918400000, 987.2611304714795], [-1362240000000, 933.9440197795144], [-1359648000000, 1090.598759722309], [-1356969600000, 1288.9698189544943], [-1354291200000, 1407.1820455759932], [-1351872000000, 1645.5603354500513], [-1349193600000, 1829.053769556805], [-1346601600000, 2056.3311215881567], [-1343923200000, 2048.137329596213], [-1341331200000, 2075.953132328195], [-1338652800000, 1956.517705361514], [-1335974400000, 1818.7232403991297], [-1333382400000, 1683.354692587731], [-1330704000000, 1630.435207307426], [-1328112000000, 1688.1926099138584], [-1325433600000, 1712.3948830314778], [-1322755200000, 1770.027616012169], [-1320249600000, 1874.7375940738175], [-1317571200000, 2081.584131948095], [-1314979200000, 2243.6902325773267], [-1312300800000, 2182.9146430931896], [-1309708800000, 2088.051792345649], [-1307030400000, 1936.5647107291945], [-1304352000000, 1759.3040036742136], [-1301760000000, 1589.9463621422638], [-1299081600000, 1496.8666812299289], [-1296489600000, 1471.9793670198853], [-1293811200000, 1463.60896510918], [-1291132800000, 1273.0150413518604], [-1288713600000, 1288.8575865218081], [-1286035200000, 1337.0090411615136], [-1283443200000, 1351.8998618706787], [-1280764800000, 1379.3798849093698], [-1278172800000, 1321.86991389899], [-1275494400000, 1164.1655067848712], [-1272816000000, 1041.7555983189739], [-1270224000000, 945.8759492806354], [-1267545600000, 915.8681326998345], [-1264953600000, 899.7610209700593], [-1262275200000, 1000.0000000000003], [-1259596800000, 1017.8432883271261], [-1257177600000, 1119.6042141504427], [-1254499200000, 1248.2398110253857], [-1251907200000, 1355.7151521833348], [-1249228800000, 1258.826495706341], [-1246636800000, 1205.2716756276104], [-1243958400000, 1046.1310358609958], [-1241280000000, 934.9835481823242], [-1238688000000, 841.6369254052047], [-1236009600000, 812.0992337191979], [-1233417600000, 799.2050307109104], [-1230739200000, 787.6416693584688], [-1228060800000, 798.4925710133889], [-1225641600000, 819.383551833239], [-1222963200000, 879.1068820293689], [-1220371200000, 900.85799214441], [-1217692800000, 860.4632395351548], [-1215100800000, 838.8317561685361], [-1212422400000, 745.5452032687592], [-1209744000000, 622.0755122758789], [-1207152000000, 556.3497570540519], [-1204473600000, 500.00000000000034], [-1201881600000, 492.3592409607608], [-1199203200000, 495.46154031005494], [-1196524800000, 424.9173615488914], [-1194019200000, 445.4411433139191], [-1191340800000, 598.4200897808057], [-1188748800000, 688.5790206389147], [-1186070400000, 700.0000000000002], [-1183478400000, 689.0964686831901], [-1180800000000, 552.3926250820545], [-1178121600000, 433.3626153790216], [-1175529600000, 362.52941006330394], [-1172851200000, 314.3542551844485], [-1170259200000, 287.5915531614153], [-1167580800000, 282.7247231230313], [-1164902400000, 279.8244232856785], [-1162483200000, 281.3032398922997], [-1159804800000, 398.98980486630006], [-1157212800000, 544.4738840248455], [-1154534400000, 533.5201290803245], [-1151942400000, 653.7466893700351], [-1149264000000, 641.0292856337875], [-1146585600000, 514.6164769513126], [-1143993600000, 444.64960140085077], [-1141315200000, 332.02291616068817], [-1138723200000, 261.0152858973736], [-1136044800000, 270.92933167286884], [-1133366400000, 332.234831161948], [-1130947200000, 422.54253425392676], [-1128268800000, 580.058979617591], [-1125676800000, 668.8616084510368], [-1122998400000, 553.4888042075877], [-1120406400000, 517.0770243267959], [-1117728000000, 479.7678966297429], [-1115049600000, 239.99999999999986], [-1112457600000, 239.99999999999997], [-1109779200000, 240], [-1107187200000, 305.3771208919302], [-1104508800000, 342.4203878621149], [-1101830400000, 383.4564108278783], [-1099411200000, 466.9802922689675], [-1096732800000, 527.8996028205488], [-1094140800000, 742.1720399994276], [-1091462400000, 780.2425840832716], [-1088870400000, 788.8487564317131], [-1086192000000, 706.0720549990139], [-1083513600000, 586.6772070962097], [-1080921600000, 523.6852052530148], [-1078243200000, 509.7050223523395], [-1075651200000, 504.29861691771765], [-1072972800000, 507.348696622246], [-1070294400000, 620.5004745971971], [-1067788800000, 755.4141121200197], [-1065110400000, 872.099340346308], [-1062518400000, 1043.383359026629], [-1059840000000, 1049.443054078523], [-1057248000000, 1092.738243064294], [-1054569600000, 1044.1050624980305], [-1051891200000, 949.4410785199829], [-1049299200000, 857.2470733338156], [-1046620800000, 826.9966558044409], [-1044028800000, 812.9463292307059], [-1041350400000, 798.6570579016945], [-1038672000000, 785.7935379563555], [-1036252800000, 785.6113048542828], [-1033574400000, 886.2254785381349], [-1030982400000, 1103.9588931636379], [-1028304000000, 1338.4444214687123], [-1025712000000, 1407.7135933686675], [-1023033600000, 1304.9771805324276], [-1020355200000, 1149.633048167655], [-1017763200000, 1069.1262841219689], [-1015084800000, 1048.6516321412826], [-1012492800000, 1196.3415799238323], [-1009814400000, 1415.7506726965362], [-1007136000000, 1513.2502794708892], [-1004716800000, 1670.2765101566836], [-1002038400000, 1931.5576500709915], [-999446400000, 2238.016981965261], [-996768000000, 2405.263176754446], [-994176000000, 2447], [-991497600000, 2269.999999999999], [-988819200000, 2150], [-986227200000, 1975], [-983548800000, 1849.9999999999998], [-980956800000, 1850], [-978278400000, 1849.9999999999998], [-975600000000, 1861.0190415574905], [-973180800000, 1874.9798745916933], [-970502400000, 1976.4414150395269], [-967910400000, 1998.82886852893], [-965232000000, 1871.1341622808188], [-962640000000, 1753.423774152276], [-959961600000, 1524.1528166090902], [-957283200000, 1282.251513541751], [-954691200000, 1101.8797539763464], [-952012800000, 1002.0347540649515], [-949420800000, 904.5759639404255], [-946742400000, 968.8701265119416], [-944064000000, 1145.2928552397625], [-941558400000, 1459.7248127574983], [-938880000000, 1762.5538391945786], [-936288000000, 1990.2206377240054], [-933609600000, 1929.3130207401425], [-931017600000, 1815.9149199110454], [-928339200000, 1670.256657131256], [-925660800000, 1496.1629468387152], [-923068800000, 1363.1849249247655], [-920390400000, 1307.900506615886], [-917798400000, 1288.467750733268], [-915120000000, 1427.9091427998446], [-912441600000, 1648.8059312543687], [-910022400000, 1899.8833368188803], [-907344000000, 2100.0000000000005], [-904752000000, 2300], [-902073600000, 2419.9999999999995], [-899481600000, 2446.999999999999], [-896803200000, 2270], [-894124800000, 2150], [-891532800000, 1974.9999999999995], [-888854400000, 1850.0000000000005], [-886262400000, 1850.0000000000002], [-883584000000, 1849.9999999999968], [-880905600000, 1900.0000000000002], [-878486400000, 2000], [-875808000000, 2078.3087793044974], [-873216000000, 2286.1239051224406], [-870537600000, 2348.409236224716], [-867945600000, 2420.427143002118], [-865267200000, 2270.0000000000005], [-862588800000, 2149.9999999999995], [-859996800000, 1975.0000000000002], [-857318400000, 1849.9999999999998], [-854726400000, 1850], [-852048000000, 1850.0000000000007], [-849369600000, 1899.9999999999895], [-846950400000, 1999.9999999999998], [-844272000000, 2099.999999999999], [-841680000000, 2281.1141070580043], [-839001600000, 2173.9550785618526], [-836409600000, 2097.332577165956], [-833731200000, 1956.0335412111133], [-831052800000, 1813.916933313995], [-828460800000, 1674.4014326645467], [-825782400000, 1617.4897715719746], [-823190400000, 1597.6243216306711], [-820512000000, 1585.7300506960307], [-817833600000, 1581.027441392909], [-815328000000, 1612.9111930053928], [-812649600000, 1644.8358899715188], [-810057600000, 1681.9524016240482], [-807379200000, 1739.1713127563398], [-804787200000, 1693.0735116690405], [-802108800000, 1559.8578147736246], [-799430400000, 1355.91852662164], [-796838400000, 1216.9271808783953], [-794160000000, 1154.1826823585493], [-791568000000, 1184.2134265628115], [-788889600000, 1261.1554362662077], [-786211200000, 1306.5175818012895], [-783792000000, 1471.7832454998686], [-781113600000, 1525.1730196594212], [-778521600000, 1659.2753722734067], [-775843200000, 1625.5925316864002], [-773251200000, 1562.4135532300515], [-770572800000, 1421.549345549123], [-767894400000, 1279.959730605619], [-765302400000, 1171.1126698886449], [-762624000000, 1174.501811495106], [-760032000000, 1229.5486804687475], [-757353600000, 1420.820852047589], [-754675200000, 1551.2755722192576], [-752256000000, 1606.0552745517139], [-749577600000, 1728.2834135072678], [-746985600000, 1939.9902761923922], [-744307200000, 1968.5541581816392], [-741715200000, 1922.2464940894617], [-739036800000, 1793.9962615052088], [-736358400000, 1654.7230277087162], [-733766400000, 1488.356056863578], [-731088000000, 1428.0597915222631], [-728496000000, 1451.3028958541056], [-725817600000, 1473.6398954749247], [-723139200000, 1471.5470558909271], [-720720000000, 1531.4381330881665], [-718041600000, 1650.7372258387193], [-715449600000, 1735.7615083025348], [-712771200000, 1644.4755213921042], [-710179200000, 1632.2385617252821], [-707500800000, 1494.9057129660396], [-704822400000, 1337.7148902165964], [-702230400000, 1199.6467990958365], [-699552000000, 1216.0728544820329], [-696960000000, 1203.0712222881912], [-694281600000, 1194.420401306851], [-691603200000, 1390.0438358941192], [-689097600000, 1410.751718061412], [-686419200000, 1443.799913146098], [-683827200000, 1628.914714334684], [-681148800000, 1702.9810063406046], [-678556800000, 1864.5478561645682], [-675878400000, 1868.0527133840922], [-673200000000, 1732.346822724532], [-670608000000, 1683.4187659875295], [-667929600000, 1671.9553025794598], [-665337600000, 1678.7181980942678], [-662659200000, 1679.3680435323095], [-659980800000, 1671.4581323696102], [-657561600000, 1682.72986673167], [-654883200000, 1899.2199060694702], [-652291200000, 2156.839438699965], [-649612800000, 2230.193116987013], [-647020800000, 2260.7884103535175], [-644342400000, 2157.5466773219982], [-641664000000, 1938.760319622233], [-639072000000, 1770.6975547164725], [-636393600000, 1698.5782817411764], [-633801600000, 1619.6557419152957], [-631123200000, 1600.6315790308013], [-628444800000, 1618.7008664525727], [-626025600000, 1674.8806967577193], [-623347200000, 1795.4927414811987], [-620755200000, 1949.5533837211026], [-618076800000, 1956.563888679564], [-615484800000, 1939.887865475302], [-612806400000, 1834.5238728816196], [-610128000000, 1659.5578084823119], [-607536000000, 1492.885333307475], [-604857600000, 1601.2116867656862], [-602265600000, 1716.9382409043383], [-599587200000, 1849.999999999995], [-596908800000, 1899.9999999999995], [-594489600000, 2000.0000000000002], [-591811200000, 2100.0000000000005], [-589219200000, 2277.3291287748575], [-586540800000, 2211.7801616962233], [-583948800000, 2121.8680279845967], [-581270400000, 1977.2574318881414], [-578592000000, 1758.56350053808], [-576000000000, 1590.6405674987554], [-573321600000, 1507.0891829019517], [-570729600000, 1527.268374946658], [-568051200000, 1674.7387347899003], [-565372800000, 1734.1800415375667], [-562867200000, 1917.146604221727], [-560188800000, 2082.910888459516], [-557596800000, 2300.000000000001], [-554918400000, 2413.7078030471735], [-552326400000, 2447.0000000000005], [-549648000000, 2269.9999999999995], [-546969600000, 2150.0000000000005], [-544377600000, 1975], [-541699200000, 1849.9999999999998], [-539107200000, 1845.1814484536183], [-536428800000, 1850.0000000000034], [-533750400000, 1899.9999999999998], [-531331200000, 1999.9999999999998], [-528652800000, 2099.9999999999995], [-526060800000, 2299.9999999999995], [-523382400000, 2294.7384762833553], [-520790400000, 2415.080022661797], [-518112000000, 2270.0000000000005], [-515433600000, 2149.999999999999], [-512841600000, 1974.9999999999995], [-510163200000, 1850], [-507571200000, 1850.0000000000005], [-504892800000, 1850.0000000000007], [-502214400000, 1899.9999999999982], [-499795200000, 2000], [-497116800000, 2100.000000000001], [-494524800000, 2300], [-491846400000, 2273.540233483553], [-489254400000, 2201.9160914587032], [-486576000000, 2068.9282909603885], [-483897600000, 1859.6179412474798], [-481305600000, 1729.4986263208352], [-478627200000, 1672.7469657352658], [-476035200000, 1693.50045668162], [-473356800000, 1749.9356263889172], [-470678400000, 1768.1409025705618], [-468259200000, 1796.9313721597243], [-465580800000, 1823.162086521091], [-462988800000, 1866.2792068333297], [-460310400000, 1875.9669665187057], [-457718400000, 1863.5017427199446], [-455040000000, 1758.0080346600498], [-452361600000, 1582.916277488295], [-449769600000, 1415.6450435675968], [-447091200000, 1353.3466432205382], [-444499200000, 1337.1824975091376], [-441820800000, 1645.4685724017622], [-439142400000, 1900], [-436636800000, 2000.0000000000002], [-433958400000, 2100.0000000000005], [-431366400000, 2300.000000000001], [-428688000000, 2396.6134815127175], [-426096000000, 2432.894743592829], [-423417600000, 2270.0000000000005], [-420739200000, 2150.0000000000005], [-418147200000, 1974.9999999999998], [-415468800000, 1849.9999999999998], [-412876800000, 1850.0000000000005], [-410198400000, 1842.4731224903098], [-407520000000, 1832.5649845668765], [-405100800000, 1993.8457393838494], [-402422400000, 2100.0000000000005], [-399830400000, 2211.9771348513236], [-397152000000, 2308.4739181016366], [-394560000000, 2365.4838597436587], [-391881600000, 2265.0712587674807], [-389203200000, 2123.689467757614], [-386611200000, 1975], [-383932800000, 1850.0000000000002], [-381340800000, 1850], [-378662400000, 1849.9999999999998], [-375984000000, 1899.9999999999952], [-373564800000, 2053.6979551533686], [-370886400000, 2100], [-368294400000, 2299.999999999998], [-365616000000, 2419.999999999999], [-363024000000, 2430.7570158724147], [-360345600000, 2270], [-357667200000, 2149.9999999999995], [-355075200000, 1975.0000000000002], [-352396800000, 1850], [-349804800000, 1849.9999999999995], [-347126400000, 1841.299615248639], [-344448000000, 1900.0000000000002], [-342028800000, 1999.9999999999989], [-339350400000, 2100], [-336758400000, 2268.677102576113], [-334080000000, 2170.4288510289434], [-331488000000, 2051.711641237634], [-328809600000, 1827.0229265055787], [-326131200000, 1652.8898113988053], [-323539200000, 1490.1598547418014], [-320860800000, 1428.8264719811675], [-318268800000, 1389.6019000632784], [-315590400000, 1367.97065700373], [-312912000000, 1366.7137468534318], [-310406400000, 1494.7723346840796], [-307728000000, 1683.4639708915672], [-305136000000, 1796.8465777172155], [-302457600000, 1816.1233287891641], [-299865600000, 1870.843413887587], [-297187200000, 1770.585128801557], [-294508800000, 1565.3872383320254], [-291916800000, 1397.144151245269], [-289238400000, 1336.3878394528087], [-286646400000, 1322.7207047050065], [-283968000000, 1392.9306960284991], [-281289600000, 1422.4432899836388], [-278870400000, 1637.7178472298929], [-276192000000, 1763.0206793913476], [-273600000000, 1897.8884905424597], [-270921600000, 1859.8033361552139], [-268329600000, 1874.2197799832545], [-265651200000, 1733.4625091525031], [-262972800000, 1559.0155787893148], [-260380800000, 1390.810515023825], [-257702400000, 1328.4811882184888], [-255110400000, 1306.4036922238795], [-252432000000, 1325.9177147309456], [-249753600000, 1339.0964955851748], [-247334400000, 1474.80167575127], [-244656000000, 1545.486532594446], [-242064000000, 1758.484494908878], [-239385600000, 1739.7122692801906], [-236793600000, 1753.3729485500248], [-234115200000, 1647.1258126964608], [-231436800000, 1477.9874893539893], [-228844800000, 1336.662052605742], [-226166400000, 1435.2869287620183], [-223574400000, 1481.640417304858], [-220896000000, 1648.374553069304], [-218217600000, 1688.5504237229477], [-215798400000, 1972.328755020869], [-213120000000, 2042.296273605075], [-210528000000, 2300.0000000000005], [-207849600000, 2337.2310648170737], [-205257600000, 2276.4763376487563], [-202579200000, 2137.10570936748], [-199900800000, 1988.804204860475], [-197308800000, 1849.8889691646452], [-194630400000, 1820.356622766032], [-192038400000, 1849.9999999999998], [-189360000000, 1849.9999999999998], [-186681600000, 1900.0000000000005], [-184176000000, 1891.565300934971], [-181497600000, 1925.2739705120941], [-178905600000, 1951.5815998347543], [-176227200000, 1850.5250057636406], [-173635200000, 1830.9668479477957], [-170956800000, 1636.9125329938493], [-168278400000, 1478.1129184474594], [-165686400000, 1352.1399650979934], [-163008000000, 1302.3966700244548], [-160416000000, 1310.0420462447057], [-157737600000, 1835.0454379247417], [-155059200000, 1899.9999999999995], [-152640000000, 1988.2007901071859], [-149961600000, 2051.9637594787896], [-147369600000, 2279.9165807398385], [-144691200000, 2192.756808377186], [-142099200000, 2124.822781274157], [-139420800000, 1998.151756673314], [-136742400000, 1875.8337517457587], [-134150400000, 1748.1977530222869], [-131472000000, 1689.395769622678], [-128880000000, 1773.505972331568], [-126201600000, 1811.073548705133], [-123523200000, 1884.4111718129213], [-121104000000, 1945.121234724116], [-118425600000, 2100], [-115833600000, 2300], [-113155200000, 2302.676282710861], [-110563200000, 2167.932239957931], [-107884800000, 2028.814674834474], [-105206400000, 1822.2220633440159], [-102614400000, 1663.542876635062], [-99936000000, 1576.2503535085832], [-97344000000, 1652.9565473242824], [-94665600000, 1789.432295980115], [-91987200000, 1899.999999999999], [-89568000000, 1999.9999999999998], [-86889600000, 2099.9999999999986], [-84297600000, 2207.6762268966786], [-81619200000, 2315.2191050298607], [-79027200000, 2410.8540554012557], [-76348800000, 2270], [-73670400000, 2150.0000000000005], [-71078400000, 1974.9999999999998], [-68400000000, 1850.0000000000005], [-65808000000, 1849.9999999999998], [-63129600000, 1849.9999999999995], [-60451200000, 1900.0000000000002], [-57945600000, 1999.9999999999975], [-55267200000, 2100.0000000000005], [-52675200000, 2186.4637874711866], [-49996800000, 2067.4752651166286], [-47404800000, 1963.1239494764336], [-44726400000, 1773.9371457873056], [-42048000000, 1598.4459295262552], [-39456000000, 1432.301969539017], [-36777600000, 1374.0700348866135], [-34185600000, 1365.9229035977073], [-31507200000, 1420.9398199935065], [-28828800000, 1561.1160137236225], [-26409600000, 1683.2237976303454], [-23731200000, 1841.7166344677582], [-21139200000, 2160.517444176812], [-18460800000, 2419.62701670247], [-15868800000, 2447.000000000001], [-13190400000, 2270.0000000000005], [-10512000000, 2150], [-7920000000, 1974.9999999999998], [-5241600000, 1849.9999999999998], [-2649600000, 1850], [28800000, 1849.9999999999993], [2707200000, 1900], [5126400000, 1999.9999999999986], [7804800000, 2100.0000000000005], [10396800000, 2163.9509259050787], [13075200000, 2067.9184374806673], [15667200000, 1927.322007003021], [18345600000, 1785.6576555108802], [21024000000, 1608.42879971303], [23616000000, 1436.361609681343], [26294400000, 1377.264060721272], [28886400000, 1478.4387032715042], [31564800000, 1607.3182050699718], [34243200000, 1832.8352581110348], [36662400000, 1961.5157489312012], [39340800000, 2099.9999999999995], [41932800000, 2267.8427418018236], [44611200000, 2295.2641800207366], [47203200000, 2294.54438886377], [49881600000, 2267.3735369008937], [52560000000, 2149.9999999999995], [55152000000, 1975.0000000000002], [57830400000, 1849.9999999999998], [60422400000, 1849.9999999999998], [63100800000, 1850], [65779200000, 1900.0000000000005], [68284800000, 1999.9999999999995], [70963200000, 2099.9999999999995], [73555200000, 2230.3113521535192], [76233600000, 2160.589419574503], [78825600000, 2106.541815581862], [81504000000, 1957.7101775626227], [84182400000, 1783.7549921603666], [86774400000, 1608.5717390701454], [89452800000, 1557.1990813984335], [92044800000, 1594.3001874489532], [94723200000, 1698.8270240364268], [97401600000, 1855.6223896061235], [99820800000, 1999.9999999999998], [102499200000, 2099.9999999999995], [105091200000, 2284.239637809641], [107769600000, 2361.883826295631], [110361600000, 2327.9690176011295], [113040000000, 2188.488433025601], [115718400000, 1961.6239485826927], [118310400000, 1795.8433942089157], [120988800000, 1809.0840160756547], [123580800000, 1850], [126259200000, 1849.9999999999995], [128937600000, 1899.9999999999995], [131356800000, 1995.696434053573], [134035200000, 2100], [136627200000, 2299.9999999999995], [139305600000, 2383.9482629270365], [141897600000, 2352.9621867440123], [144576000000, 2270], [147254400000, 2150.0000000000005], [149846400000, 1975], [152524800000, 1850.0000000000002], [155116800000, 1847.4876910345197], [157795200000, 1849.9999999999993], [160473600000, 1871.3229714770207], [162892800000, 1967.3182175483191], [165571200000, 2100], [168163200000, 2220.934328875539], [170841600000, 2353.650176174331], [173433600000, 2447.0000000000005], [176112000000, 2270.000000000001], [178790400000, 2150.000000000001], [181382400000, 1974.9999999999998], [184060800000, 1849.9999999999998], [186652800000, 1850], [189331200000, 1850], [192009600000, 1660.2366487345812], [194515200000, 1691.493886230047], [197193600000, 1736.7846026048271], [199785600000, 1813.037761992721], [202464000000, 1877.231463929722], [205056000000, 1796.5749282691331], [207734400000, 1577.5590270624023], [210412800000, 1381.153682519497], [213004800000, 1242.4776217541785], [215683200000, 1173.5476595065834], [218275200000, 1001.9798129927976], [220953600000, 979.6644343238246], [223632000000, 964.9241595824293], [226051200000, 953.6966216363699], [228729600000, 936.8567345957949], [231321600000, 903.223025868525], [234000000000, 852.4196915104801], [236592000000, 817.0529725888405], [239270400000, 717.7065963964978], [241948800000, 500.0000000000002], [244540800000, 447.7419563911253], [247219200000, 380.4541926357756], [249811200000, 368.07195665509676], [252489600000, 552.5679367892214], [255168000000, 942.8739532567713], [257587200000, 1124.7020751032096], [260265600000, 1402.8176877309768], [262857600000, 1594.8171683941798], [265536000000, 1593.749817662405], [268128000000, 1659.132223556839], [270806400000, 1663.1047831848373], [273484800000, 1557.809148993445], [276076800000, 1471.1331436582343], [278755200000, 1408.5128962942006], [281347200000, 1401.8100875726825], [284025600000, 1378.723697461457], [286704000000, 1391.4157407909313], [289123200000, 1442.3771482998459], [291801600000, 1587.2204503095427], [294393600000, 1700.9114694610728], [297072000000, 1769.6218752852255], [299664000000, 1736.82493387732], [302342400000, 1629.7586870411365], [305020800000, 1452.4505498086587], [307612800000, 1315.5854129907627], [310291200000, 1287.4300288901288], [312883200000, 1350.3069734915234], [315561600000, 1416.963667932802], [318240000000, 1638.2307092038852], [320745600000, 1933.148556387314], [323424000000, 2063.1467766020323], [326016000000, 2221.370033394228], [328694400000, 2126.9130212483683], [331286400000, 1991.2328015110925], [333964800000, 1868.7347966247867], [336643200000, 1721.2296517183904], [339235200000, 1600.0000000000005], [341913600000, 1581.8611144356778], [344505600000, 1573.8822232730986], [347184000000, 1614.280995734022], [349862400000, 1721.4127827500433], [352281600000, 1860.9478664072296], [354960000000, 1986.6956442033302], [357552000000, 2098.0645731255545], [360230400000, 1959.7289227735937], [362822400000, 1823.8611581387731], [365500800000, 1641.3524859848885], [368179200000, 1416.2741450880767], [370771200000, 1271.988944277968], [373449600000, 1221.306886924375], [376041600000, 1464.3142332240066], [378720000000, 1796.4692147671003], [381398400000, 1885.9283672662684], [383817600000, 2000.0000000000007], [386496000000, 2100], [389088000000, 2300], [391766400000, 2305.4385253876694], [394358400000, 2299.461313362784], [397036800000, 2269.999999999999], [399715200000, 2149.9999999999995], [402307200000, 1974.9999999999993], [404985600000, 1849.9999999999993], [407577600000, 1850], [410256000000, 1849.9999999999952], [412934400000, 1900], [415353600000, 2000], [418032000000, 2100], [420624000000, 2299.9999999999986], [423302400000, 2420.0000000000005], [425894400000, 2447], [428572800000, 2270.0000000000005], [431251200000, 2150.0000000000005], [433843200000, 1975.0000000000002], [436521600000, 1850.0000000000002], [439113600000, 1849.9999999999993], [441792000000, 1850.0000000000002], [444470400000, 1899.9999999999995], [446976000000, 1997.7100365217057], [449654400000, 2100], [452246400000, 2215.4522529126416], [454924800000, 2182.199132109691], [457516800000, 2120.494162136286], [460195200000, 1991.0565467388633], [462873600000, 1855.1281194003964], [465465600000, 1719.88831087467], [468144000000, 1662.8397721691424], [470736000000, 1791.2951257751104], [473414400000, 1845.4783327222906], [476092800000, 1781.5535206752688], [478512000000, 1820.7950082784419], [481190400000, 1867.1580864518062], [483782400000, 2012.7073828225457], [486460800000, 1935.5470176208073], [489052800000, 1892.495959927402], [491731200000, 1699.517306599193], [494409600000, 1466.6409539800795], [497001600000, 1330.463032943294], [499680000000, 1271.8692900110927], [502272000000, 1236.6168450054556], [504950400000, 1236.5725274838683], [507628800000, 1351.8735028479873], [510048000000, 1824.7595041799552], [512726400000, 2100.0000000000005], [515318400000, 2120.7332528337333], [517996800000, 2014.2340695221444], [520588800000, 1884.3373559019194], [523267200000, 1759.97723647853], [525945600000, 1575.645485706936], [528537600000, 1447.863629329407], [531216000000, 1394.6213010972995], [533808000000, 1374.0229388653468], [536486400000, 1361.5698791688362], [539164800000, 1371.265269671377], [541584000000, 1447.7915474271326], [544262400000, 1650.1776977163038], [546854400000, 1794.0498003233993], [549532800000, 1751.4263264813635], [552124800000, 1600.516963135116], [554803200000, 1377.8719504631076], [557481600000, 1141.8652446020183], [560073600000, 996.8545495871494], [562752000000, 952.4690951172104], [565344000000, 931.1417541534881], [568022400000, 1101.839929552366], [570700800000, 1183.7502542191312], [573206400000, 1269.8375589547857], [575884800000, 1344.6998209919664], [578476800000, 1423.9596268094115], [581155200000, 1365.70083613494], [583747200000, 1341.435628252468], [586425600000, 1196.2702505008324], [589104000000, 1085.826336749514], [591696000000, 985.7650833801264], [594374400000, 951.0363541700676], [596966400000, 987.769077422202], [599644800000, 999.9999999999999], [602323200000, 1023.318898541862], [604742400000, 1049.1076457560816], [607420800000, 1399.2737120131158], [610012800000, 1601.080348032696], [612691200000, 1495.3547764279206], [615283200000, 1378.869701105206], [617961600000, 1227.7339285687722], [620640000000, 1068.435796496115], [623232000000, 992.8116831765894], [625910400000, 1008.9957417506137], [628502400000, 1008.7955632259168], [631180800000, 999.2283556710338], [633859200000, 1061.8303127179092], [636278400000, 1086.5651652583467], [638956800000, 1169.5500989576092], [641548800000, 1205.3582944835234], [644227200000, 1173.660305865251], [646819200000, 1190.4696052956776], [649497600000, 1129.4582166963994], [652176000000, 1015.8429133639488], [654768000000, 920.4801452126327], [657446400000, 885.3953534007476], [660038400000, 867.6620563853013], [662716800000, 848.5361057912542], [665395200000, 837.3304897658493], [667814400000, 843.0499343370448], [670492800000, 915.5297794786551], [673084800000, 965.7407260031691], [675763200000, 985.7218378802257], [678355200000, 983.85619035213], [681033600000, 900.763754403944], [683712000000, 783.1582005689099], [686304000000, 690.013268810894], [688982400000, 671.8257646048891], [691574400000, 661.628066136139], [694252800000, 647.8201972417774], [696931200000, 655.298631675398], [699436800000, 795.1586696066659], [702115200000, 943.8801679028231], [704707200000, 1176.6109795817301], [707385600000, 1171.4281118209178], [709977600000, 1153.6635831218211], [712656000000, 1073.7980448063902], [715334400000, 950.6804690817911], [717926400000, 808.0213035525773], [720604800000, 779.6682043065413], [723196800000, 773.3659697470549], [725875200000, 791.9346612080683], [728553600000, 849.7595910988737], [730972800000, 989.4964668814366], [733651200000, 1368.2112222505687], [736243200000, 1582.3240772343866], [738921600000, 1764.5100675448089], [741513600000, 1862.0165661659844], [744192000000, 1845.7879522272594], [746870400000, 1804.2768472736625], [749462400000, 1720.1732867629769], [752140800000, 1704.3366889454376], [754732800000, 1691.3280959284723], [757411200000, 1706.4349733245733], [760089600000, 1658.6713067883932], [762508800000, 1689.751654438223], [765187200000, 1755.0339497420084], [767779200000, 1806.57872312178], [770457600000, 1726.908271729863], [773049600000, 1563.8987030535302], [775728000000, 1378.9391355216083], [778406400000, 1165.551664753083], [780998400000, 1000], [783676800000, 962.739147912801], [786268800000, 953.1506296249148], [788947200000, 949.1584548370647], [791625600000, 1200.2648161570376], [794044800000, 1457.0962753576262], [796723200000, 1907.8841604404267], [799315200000, 2156.332682054658], [801993600000, 2261.9273849794745], [804585600000, 2249.0174235804693], [807264000000, 2267.8483866387483], [809942400000, 2150], [812534400000, 1975.0000000000002], [815212800000, 1849.9999999999998], [817804800000, 1828.184070317985], [820483200000, 1849.999999999999], [823161600000, 1899.999999999996], [825667200000, 2000.0000000000007], [828345600000, 2100], [830937600000, 2279.4874638889223], [833616000000, 2241.771098808048], [836208000000, 2173.0933977230593], [838886400000, 2117.9403737019825], [841564800000, 2049.5219723183814], [844156800000, 1903.2120724627644], [846835200000, 1849.9999999999998], [849427200000, 1849.9999999999998], [852105600000, 1850], [854784000000, 1900], [857203200000, 1999.9999999999998], [859881600000, 2088.963477198023], [862473600000, 2188.108891346605], [865152000000, 2010.906230122651], [867744000000, 1851.0655315006652], [870422400000, 1700.2967055464735], [873100800000, 1522.553754645114], [875692800000, 1380.0920300603036], [878371200000, 1316.4230782026636], [880963200000, 1322.1162102433743], [883641600000, 1349.5486526502511], [886320000000, 1664.6625812193904], [888739200000, 1999.9999999999998], [891417600000, 2100], [894009600000, 2297.8359497510073], [896688000000, 2420], [899280000000, 2446.9999999999995], [901958400000, 2270], [904636800000, 2150], [907228800000, 1975], [909907200000, 1850.0000000000005], [912499200000, 1849.9999999999995], [915177600000, 1849.999999999997], [917856000000, 1900.0000000000002], [920275200000, 2000], [922953600000, 2099.999999999997], [925545600000, 2291.047143610901], [928224000000, 2361.295573808204], [930816000000, 2401.5554713462257], [933494400000, 2270], [936172800000, 2149.9999999999995], [938764800000, 1975], [941443200000, 1850.0000000000002], [944035200000, 1850], [946713600000, 1849.9999999999995], [949392000000, 1900], [951897600000, 2000], [954576000000, 2099.999999999999], [957168000000, 2299.9999999999995], [959846400000, 2283.0012094410076], [962438400000, 2279.2010377458428], [965116800000, 2156.410610938195], [967795200000, 2002.334613060637], [970387200000, 1856.2765798615867], [973065600000, 1842.7946575986964], [975657600000, 1839.5592395033973], [978336000000, 1831.3158148752134], [981014400000, 1833.6158045158706], [983433600000, 1867.8679365579376], [986112000000, 2035.4122439540483], [988704000000, 2138.0947746858155], [991382400000, 2116.3155731605507], [993974400000, 2015.485566626382], [996652800000, 1891.0220547840006], [999331200000, 1689.3275812921622], [1001923200000, 1518.219715987581], [1004601600000, 1449.4391611322894], [1007193600000, 1466.7430558905683], [1009872000000, 1555.3364379703585], [1012550400000, 1787.529660086602], [1014969600000, 1896.7913354878542], [1017648000000, 1995.2786910139948], [1020240000000, 2196.681410384504], [1022918400000, 2113.988846498541], [1025510400000, 2046.9027091083292], [1028188800000, 1902.2668909107979], [1030867200000, 1725.3841889236678], [1033459200000, 1550.8528177377439], [1036137600000, 1480.031973446273], [1038729600000, 1449.1500873498055], [1041408000000, 1639.8992192589455], [1044086400000, 1899.9999999999934], [1046505600000, 2000], [1049184000000, 2100.0000000000005], [1051776000000, 2300.0000000000005], [1054454400000, 2376.4728842776185], [1057046400000, 2446.9999999999995], [1059724800000, 2270], [1062403200000, 2140.188184154539], [1064995200000, 1974.9999999999993]]
                    }, {
                        "period_months": ["Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep"],
                        "annual_period": "Sacramento River Index<br>Wet",
                        "computed_statistics": [{
                            "statistic": "Averages",
                            "statistically_computed_time_series_monthly": [["Jan", 1777.1995911161714], ["Feb", 1932.86891523756], ["Mar", 2067.7334009593783], ["Apr", 2252.668488972043], ["May", 2295.6157810351374], ["Jun", 2287.0809351431362], ["Jul", 2156.7267270320117], ["Aug", 2028.7430425710036], ["Sep", 1864.9960753024986], ["Oct", 1556.8199346628828], ["Nov", 1580.637997864993], ["Dec", 1672.780261852665]],
                            "statistic_aggregate": 1949.3786275694006
                        }],
                        "aggregate_ts": [[1927, 1652.8604975750088], [1938, 1855.0923735983267], [1941, 1936.2472223518537], [1942, 2081.5224219711477], [1943, 1962.2294724979547], [1952, 1999.9201358253747], [1953, 2079.166662283231], [1956, 1988.3754948530823], [1958, 2095.787914252148], [1963, 1929.8319697786253], [1965, 1883.9440113010348], [1967, 2028.9490486783977], [1969, 1889.0944720982363], [1970, 1878.3032862745015], [1971, 1967.2830686410173], [1974, 2078.0575749833565], [1975, 2075.226115425811], [1982, 1980.6598784110167], [1983, 2092.6666666666665], [1984, 1969.3273800578545], [1986, 1652.0402249417523], [1995, 1707.5349467986018], [1996, 2024.4342041017615], [1997, 1849.3322183683194], [1998, 1942.7155393388905], [1999, 2083.2415157304436]],
                        "month_period": "October - September",
                        "discrete_ts": [[-1362240000000, 933.9440197795144], [-1359648000000, 1090.598759722309], [-1356969600000, 1288.9698189544943], [-1354291200000, 1407.1820455759932], [-1351872000000, 1645.5603354500513], [-1349193600000, 1829.053769556805], [-1346601600000, 2056.3311215881567], [-1343923200000, 2048.137329596213], [-1341331200000, 2075.953132328195], [-1338652800000, 1956.517705361514], [-1335974400000, 1818.7232403991297], [-1333382400000, 1683.354692587731], [-1015084800000, 1048.6516321412826], [-1012492800000, 1196.3415799238323], [-1009814400000, 1415.7506726965362], [-1007136000000, 1513.2502794708892], [-1004716800000, 1670.2765101566836], [-1002038400000, 1931.5576500709915], [-999446400000, 2238.016981965261], [-996768000000, 2405.263176754446], [-994176000000, 2447], [-991497600000, 2269.999999999999], [-988819200000, 2150], [-986227200000, 1975], [-920390400000, 1307.900506615886], [-917798400000, 1288.467750733268], [-915120000000, 1427.9091427998446], [-912441600000, 1648.8059312543687], [-910022400000, 1899.8833368188803], [-907344000000, 2100.0000000000005], [-904752000000, 2300], [-902073600000, 2419.9999999999995], [-899481600000, 2446.999999999999], [-896803200000, 2270], [-894124800000, 2150], [-891532800000, 1974.9999999999995], [-888854400000, 1850.0000000000005], [-886262400000, 1850.0000000000002], [-883584000000, 1849.9999999999968], [-880905600000, 1900.0000000000002], [-878486400000, 2000], [-875808000000, 2078.3087793044974], [-873216000000, 2286.1239051224406], [-870537600000, 2348.409236224716], [-867945600000, 2420.427143002118], [-865267200000, 2270.0000000000005], [-862588800000, 2149.9999999999995], [-859996800000, 1975.0000000000002], [-857318400000, 1849.9999999999998], [-854726400000, 1850], [-852048000000, 1850.0000000000007], [-849369600000, 1899.9999999999895], [-846950400000, 1999.9999999999998], [-844272000000, 2099.999999999999], [-841680000000, 2281.1141070580043], [-839001600000, 2173.9550785618526], [-836409600000, 2097.332577165956], [-833731200000, 1956.0335412111133], [-831052800000, 1813.916933313995], [-828460800000, 1674.4014326645467], [-573321600000, 1507.0891829019517], [-570729600000, 1527.268374946658], [-568051200000, 1674.7387347899003], [-565372800000, 1734.1800415375667], [-562867200000, 1917.146604221727], [-560188800000, 2082.910888459516], [-557596800000, 2300.000000000001], [-554918400000, 2413.7078030471735], [-552326400000, 2447.0000000000005], [-549648000000, 2269.9999999999995], [-546969600000, 2150.0000000000005], [-544377600000, 1975], [-541699200000, 1849.9999999999998], [-539107200000, 1845.1814484536183], [-536428800000, 1850.0000000000034], [-533750400000, 1899.9999999999998], [-531331200000, 1999.9999999999998], [-528652800000, 2099.9999999999995], [-526060800000, 2299.9999999999995], [-523382400000, 2294.7384762833553], [-520790400000, 2415.080022661797], [-518112000000, 2270.0000000000005], [-515433600000, 2149.999999999999], [-512841600000, 1974.9999999999995], [-447091200000, 1353.3466432205382], [-444499200000, 1337.1824975091376], [-441820800000, 1645.4685724017622], [-439142400000, 1900], [-436636800000, 2000.0000000000002], [-433958400000, 2100.0000000000005], [-431366400000, 2300.000000000001], [-428688000000, 2396.6134815127175], [-426096000000, 2432.894743592829], [-423417600000, 2270.0000000000005], [-420739200000, 2150.0000000000005], [-418147200000, 1974.9999999999998], [-383932800000, 1850.0000000000002], [-381340800000, 1850], [-378662400000, 1849.9999999999998], [-375984000000, 1899.9999999999952], [-373564800000, 2053.6979551533686], [-370886400000, 2100], [-368294400000, 2299.999999999998], [-365616000000, 2419.999999999999], [-363024000000, 2430.7570158724147], [-360345600000, 2270], [-357667200000, 2149.9999999999995], [-355075200000, 1975.0000000000002], [-226166400000, 1435.2869287620183], [-223574400000, 1481.640417304858], [-220896000000, 1648.374553069304], [-218217600000, 1688.5504237229477], [-215798400000, 1972.328755020869], [-213120000000, 2042.296273605075], [-210528000000, 2300.0000000000005], [-207849600000, 2337.2310648170737], [-205257600000, 2276.4763376487563], [-202579200000, 2137.10570936748], [-199900800000, 1988.804204860475], [-197308800000, 1849.8889691646452], [-163008000000, 1302.3966700244548], [-160416000000, 1310.0420462447057], [-157737600000, 1835.0454379247417], [-155059200000, 1899.9999999999995], [-152640000000, 1988.2007901071859], [-149961600000, 2051.9637594787896], [-147369600000, 2279.9165807398385], [-144691200000, 2192.756808377186], [-142099200000, 2124.822781274157], [-139420800000, 1998.151756673314], [-136742400000, 1875.8337517457587], [-134150400000, 1748.1977530222869], [-99936000000, 1576.2503535085832], [-97344000000, 1652.9565473242824], [-94665600000, 1789.432295980115], [-91987200000, 1899.999999999999], [-89568000000, 1999.9999999999998], [-86889600000, 2099.9999999999986], [-84297600000, 2207.6762268966786], [-81619200000, 2315.2191050298607], [-79027200000, 2410.8540554012557], [-76348800000, 2270], [-73670400000, 2150.0000000000005], [-71078400000, 1974.9999999999998], [-36777600000, 1374.0700348866135], [-34185600000, 1365.9229035977073], [-31507200000, 1420.9398199935065], [-28828800000, 1561.1160137236225], [-26409600000, 1683.2237976303454], [-23731200000, 1841.7166344677582], [-21139200000, 2160.517444176812], [-18460800000, 2419.62701670247], [-15868800000, 2447.000000000001], [-13190400000, 2270.0000000000005], [-10512000000, 2150], [-7920000000, 1974.9999999999998], [-5241600000, 1849.9999999999998], [-2649600000, 1850], [28800000, 1849.9999999999993], [2707200000, 1900], [5126400000, 1999.9999999999986], [7804800000, 2100.0000000000005], [10396800000, 2163.9509259050787], [13075200000, 2067.9184374806673], [15667200000, 1927.322007003021], [18345600000, 1785.6576555108802], [21024000000, 1608.42879971303], [23616000000, 1436.361609681343], [26294400000, 1377.264060721272], [28886400000, 1478.4387032715042], [31564800000, 1607.3182050699718], [34243200000, 1832.8352581110348], [36662400000, 1961.5157489312012], [39340800000, 2099.9999999999995], [41932800000, 2267.8427418018236], [44611200000, 2295.2641800207366], [47203200000, 2294.54438886377], [49881600000, 2267.3735369008937], [52560000000, 2149.9999999999995], [55152000000, 1975.0000000000002], [120988800000, 1809.0840160756547], [123580800000, 1850], [126259200000, 1849.9999999999995], [128937600000, 1899.9999999999995], [131356800000, 1995.696434053573], [134035200000, 2100], [136627200000, 2299.9999999999995], [139305600000, 2383.9482629270365], [141897600000, 2352.9621867440123], [144576000000, 2270], [147254400000, 2150.0000000000005], [149846400000, 1975], [152524800000, 1850.0000000000002], [155116800000, 1847.4876910345197], [157795200000, 1849.9999999999993], [160473600000, 1871.3229714770207], [162892800000, 1967.3182175483191], [165571200000, 2100], [168163200000, 2220.934328875539], [170841600000, 2353.650176174331], [173433600000, 2447.0000000000005], [176112000000, 2270.000000000001], [178790400000, 2150.000000000001], [181382400000, 1974.9999999999998], [373449600000, 1221.306886924375], [376041600000, 1464.3142332240066], [378720000000, 1796.4692147671003], [381398400000, 1885.9283672662684], [383817600000, 2000.0000000000007], [386496000000, 2100], [389088000000, 2300], [391766400000, 2305.4385253876694], [394358400000, 2299.461313362784], [397036800000, 2269.999999999999], [399715200000, 2149.9999999999995], [402307200000, 1974.9999999999993], [404985600000, 1849.9999999999993], [407577600000, 1850], [410256000000, 1849.9999999999952], [412934400000, 1900], [415353600000, 2000], [418032000000, 2100], [420624000000, 2299.9999999999986], [423302400000, 2420.0000000000005], [425894400000, 2447], [428572800000, 2270.0000000000005], [431251200000, 2150.0000000000005], [433843200000, 1975.0000000000002], [436521600000, 1850.0000000000002], [439113600000, 1849.9999999999993], [441792000000, 1850.0000000000002], [444470400000, 1899.9999999999995], [446976000000, 1997.7100365217057], [449654400000, 2100], [452246400000, 2215.4522529126416], [454924800000, 2182.199132109691], [457516800000, 2120.494162136286], [460195200000, 1991.0565467388633], [462873600000, 1855.1281194003964], [465465600000, 1719.88831087467], [499680000000, 1271.8692900110927], [502272000000, 1236.6168450054556], [504950400000, 1236.5725274838683], [507628800000, 1351.8735028479873], [510048000000, 1824.7595041799552], [512726400000, 2100.0000000000005], [515318400000, 2120.7332528337333], [517996800000, 2014.2340695221444], [520588800000, 1884.3373559019194], [523267200000, 1759.97723647853], [525945600000, 1575.645485706936], [528537600000, 1447.863629329407], [783676800000, 962.739147912801], [786268800000, 953.1506296249148], [788947200000, 949.1584548370647], [791625600000, 1200.2648161570376], [794044800000, 1457.0962753576262], [796723200000, 1907.8841604404267], [799315200000, 2156.332682054658], [801993600000, 2261.9273849794745], [804585600000, 2249.0174235804693], [807264000000, 2267.8483866387483], [809942400000, 2150], [812534400000, 1975.0000000000002], [815212800000, 1849.9999999999998], [817804800000, 1828.184070317985], [820483200000, 1849.999999999999], [823161600000, 1899.999999999996], [825667200000, 2000.0000000000007], [828345600000, 2100], [830937600000, 2279.4874638889223], [833616000000, 2241.771098808048], [836208000000, 2173.0933977230593], [838886400000, 2117.9403737019825], [841564800000, 2049.5219723183814], [844156800000, 1903.2120724627644], [846835200000, 1849.9999999999998], [849427200000, 1849.9999999999998], [852105600000, 1850], [854784000000, 1900], [857203200000, 1999.9999999999998], [859881600000, 2088.963477198023], [862473600000, 2188.108891346605], [865152000000, 2010.906230122651], [867744000000, 1851.0655315006652], [870422400000, 1700.2967055464735], [873100800000, 1522.553754645114], [875692800000, 1380.0920300603036], [878371200000, 1316.4230782026636], [880963200000, 1322.1162102433743], [883641600000, 1349.5486526502511], [886320000000, 1664.6625812193904], [888739200000, 1999.9999999999998], [891417600000, 2100], [894009600000, 2297.8359497510073], [896688000000, 2420], [899280000000, 2446.9999999999995], [901958400000, 2270], [904636800000, 2150], [907228800000, 1975], [909907200000, 1850.0000000000005], [912499200000, 1849.9999999999995], [915177600000, 1849.999999999997], [917856000000, 1900.0000000000002], [920275200000, 2000], [922953600000, 2099.999999999997], [925545600000, 2291.047143610901], [928224000000, 2361.295573808204], [930816000000, 2401.5554713462257], [933494400000, 2270], [936172800000, 2149.9999999999995], [938764800000, 1975]]
                    }, {
                        "period_months": ["Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep"],
                        "annual_period": "Dry Periods<br>1929 - 1934",
                        "computed_statistics": [{
                            "statistic": "Averages",
                            "statistically_computed_time_series_monthly": [["Jan", 630.2694110437712], ["Feb", 666.4330372974752], ["Mar", 781.1891469254651], ["Apr", 872.4065122478762], ["May", 838.2024660804551], ["Jun", 846.736647462343], ["Jul", 746.2745374613992], ["Aug", 626.2595381971344], ["Sep", 551.2914234808528], ["Oct", 635.5804054008703], ["Nov", 619.7292114507865], ["Dec", 641.4569831978888]],
                            "statistic_aggregate": 753.5777762614239
                        }],
                        "aggregate_ts": [[1929, 1294.6902831214745], [1930, 1070.3234416782216], [1931, 785.0043665926137], [1932, 531.8799596467431], [1933, 431.4020054978608], [1934, 408.1666010316299]],
                        "month_period": "October - September",
                        "discrete_ts": [[-1299081600000, 1496.8666812299289], [-1296489600000, 1471.9793670198853], [-1293811200000, 1463.60896510918], [-1291132800000, 1273.0150413518604], [-1288713600000, 1288.8575865218081], [-1286035200000, 1337.0090411615136], [-1283443200000, 1351.8998618706787], [-1280764800000, 1379.3798849093698], [-1278172800000, 1321.86991389899], [-1275494400000, 1164.1655067848712], [-1272816000000, 1041.7555983189739], [-1270224000000, 945.8759492806354], [-1267545600000, 915.8681326998345], [-1264953600000, 899.7610209700593], [-1262275200000, 1000.0000000000003], [-1259596800000, 1017.8432883271261], [-1257177600000, 1119.6042141504427], [-1254499200000, 1248.2398110253857], [-1251907200000, 1355.7151521833348], [-1249228800000, 1258.826495706341], [-1246636800000, 1205.2716756276104], [-1243958400000, 1046.1310358609958], [-1241280000000, 934.9835481823242], [-1238688000000, 841.6369254052047], [-1236009600000, 812.0992337191979], [-1233417600000, 799.2050307109104], [-1230739200000, 787.6416693584688], [-1228060800000, 798.4925710133889], [-1225641600000, 819.383551833239], [-1222963200000, 879.1068820293689], [-1220371200000, 900.85799214441], [-1217692800000, 860.4632395351548], [-1215100800000, 838.8317561685361], [-1212422400000, 745.5452032687592], [-1209744000000, 622.0755122758789], [-1207152000000, 556.3497570540519], [-1204473600000, 500.00000000000034], [-1201881600000, 492.3592409607608], [-1199203200000, 495.46154031005494], [-1196524800000, 424.9173615488914], [-1194019200000, 445.4411433139191], [-1191340800000, 598.4200897808057], [-1188748800000, 688.5790206389147], [-1186070400000, 700.0000000000002], [-1183478400000, 689.0964686831901], [-1180800000000, 552.3926250820545], [-1178121600000, 433.3626153790216], [-1175529600000, 362.52941006330394], [-1172851200000, 314.3542551844485], [-1170259200000, 287.5915531614153], [-1167580800000, 282.7247231230313], [-1164902400000, 279.8244232856785], [-1162483200000, 281.3032398922997], [-1159804800000, 398.98980486630006], [-1157212800000, 544.4738840248455], [-1154534400000, 533.5201290803245], [-1151942400000, 653.7466893700351], [-1149264000000, 641.0292856337875], [-1146585600000, 514.6164769513126], [-1143993600000, 444.64960140085077], [-1141315200000, 332.02291616068817], [-1138723200000, 261.0152858973736], [-1136044800000, 270.92933167286884], [-1133366400000, 332.234831161948], [-1130947200000, 422.54253425392676], [-1128268800000, 580.058979617591], [-1125676800000, 668.8616084510368], [-1122998400000, 553.4888042075877], [-1120406400000, 517.0770243267959], [-1117728000000, 479.7678966297429], [-1115049600000, 239.99999999999986], [-1112457600000, 239.99999999999997]]
                    }]
                }]
            }], "scenario_name": "CS3M37", "scenario_color": "#A3F244FF"
        }], "gui_link_title": "Trinity Reservoir Storage", "first_record": -1525276800000, "units": "TAF"
    });
}