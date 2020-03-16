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
const DEBUG = false;
var FORMATTER = '';
const PLOTLY_FONT = {
    family: 'Lucida Grande", "Lucida Sans Unicode", "Verdana", "Arial", "Helvetica", "sans-serif',
    color: 'black',
};

const PLOTLY_LINE_DASH_STYLES = ['solid', 'dashdot', 'dot'];

let syncPlotsEnabled = false;

function buildModeBarButtons(graphDiv) {
    let syncPlots = {
        name: 'Toggle Sync X-Axis',
        icon: {
            'width': 477.859,
            'height': 477.859,
            // 'viewBox' : "0 0 477.859 477.859",
            'path': "M472.863,175.662L353.396,56.195c-6.666-6.664-17.472-6.662-24.136,0.004c-3.199,3.2-4.996,7.538-4.997,12.063v51.2 H204.796c-9.426,0-17.067,7.641-17.067,17.067c0,9.426,7.641,17.067,17.067,17.067H341.33c9.426,0,17.067-7.641,17.067-17.067 V109.46l78.268,78.268l-78.268,78.268v-27.068c0-9.426-7.641-17.067-17.067-17.067H153.596v-51.2 c-0.002-9.426-7.645-17.065-17.07-17.063c-4.524,0.001-8.863,1.798-12.063,4.997L4.997,278.062 c-6.663,6.665-6.663,17.468,0,24.132l119.467,119.467c3.2,3.201,7.54,5,12.066,5.001c2.243,0.007,4.466-0.434,6.536-1.297 c6.376-2.644,10.532-8.867,10.53-15.77v-51.2h119.467c9.426,0,17.067-7.641,17.067-17.067s-7.641-17.067-17.067-17.067H136.53 c-9.426,0-17.067,7.641-17.067,17.067v27.068l-78.268-78.268l78.268-78.268v27.068c0,9.426,7.641,17.067,17.067,17.067h187.733 v51.2c0.002,9.426,7.645,17.065,17.07,17.063c4.524-0.001,8.863-1.798,12.063-4.997l119.467-119.467 C479.525,193.129,479.525,182.326,472.863,175.662z",
        },
        click: () => {
            alert('clicked custom button!');
            syncPlotsEnabled = !syncPlotsEnabled;
            let buttons = document.getElementsByClassName('modebar-btn');
            for (let i = 0; i < buttons.length; i++) {
                if (buttons[i].getAttribute('data-title') === 'Toggle Sync X-Axis') {
                    if (syncPlotsEnabled) {
                        buttons[i].classList.add('clicked');
                    } else {
                        buttons[i].classList.remove('clicked');
                    }
                }
            }
        }
    };
    return [['zoom2d', 'pan2d', 'zoomIn2d', 'zoomOut2d', 'resetScale2d'],
        ['toggleSpikelines', 'hoverClosestCartesian', 'hoverCompareCartesian'], [syncPlots]];
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
                if (eventdata['xaxis.range[0]'] && syncPlotsEnabled) {
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
                } else if (eventdata['xaxis.autorange'] && syncPlotsEnabled) {
                    for (let i = 0; i < plots.length; i++) {
                        if (plots[i].id != plot.id && plots[i]['syncPlots']) {
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
                            copyToClipboard(options['selector']);
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
