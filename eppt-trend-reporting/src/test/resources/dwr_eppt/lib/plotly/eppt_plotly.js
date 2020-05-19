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


if (DEBUG) {
    window.onload = () => plot({
        "last_record": 1064995200000,
        "scenario_run_data": [{
            "ts_list": [{
                "ts_name": "NewScenarioRun 3798104 (CalSim2)", "monthly_filters": [{
                    "annual_filters": [{
                        "period_months": ["Oct", "Nov", "Dec", "Jan"],
                        "annual_period": "Long Term<br>",
                        "computed_statistics": [{
                            "statistic": "Averages",
                            "statistically_computed_time_series_monthly": [["Jan", 1474.3252152039393], ["Oct", 1354.0210866180798], ["Nov", 1365.816271479528], ["Dec", 1415.5527550236732]],
                            "statistic_aggregate": 1403.3459383619536
                        }, {
                            "statistic": "Minimums",
                            "statistically_computed_time_series_monthly": [["Jan", 485.16441333514405], ["Oct", 240.00000000000003], ["Nov", 267.29062040796197], ["Dec", 458.20667482838076]],
                            "statistic_aggregate": 452.1979031398916
                        }],
                        "aggregate_ts": [[1922, 1760.356405665324], [1923, 1620.4172296740967], [1924, 1215.0135469765694], [1925, 737.8817277839216], [1926, 989.5499646096679], [1927, 1214.28181498836], [1928, 1658.589140622996], [1929, 1238.1563613229407], [1930, 911.9264522772154], [1931, 822.4991361420192], [1932, 543.6329766008913], [1933, 492.5039148148207], [1934, 592.5439492913894], [1935, 549.6810439783611], [1936, 694.9370491338913], [1937, 830.3349948407546], [1938, 1318.4848394724795], [1939, 1852.4967746515026], [1940, 1151.404140010262], [1941, 1507.062452253503], [1942, 1859.2193074194686], [1943, 1861.2853107776637], [1944, 1456.5652085306754], [1945, 1145.4244775701388], [1946, 1293.5921394099528], [1947, 1360.5876738548802], [1948, 1237.7900131631366], [1949, 1443.0521510277595], [1950, 1330.99165827805], [1951, 1585.755206034708], [1952, 1605.2500848540815], [1953, 1861.3286626537472], [1954, 1862.5], [1955, 1676.4955950352896], [1956, 1533.3955613753453], [1957, 1825.6491497622153], [1958, 1862.5], [1959, 1858.8626240714743], [1960, 1389.0958292428359], [1961, 1290.3276404553842], [1962, 1354.9483156923015], [1963, 1577.1756909788228], [1964, 1807.6924343622843], [1965, 1570.7714542695276], [1966, 1721.8659267542307], [1967, 1729.5530988186135], [1968, 1861.4621600515416], [1969, 1489.2058352540348], [1970, 1862.5], [1971, 1493.2125222149189], [1972, 1862.5], [1973, 1654.524710103519], [1974, 1829.9714645869983], [1975, 1850.0208256351777], [1976, 1804.460862745468], [1977, 1113.9448170227386], [1978, 452.1979031398916], [1979, 1233.1871378514325], [1980, 1361.414614636117], [1981, 1498.0101479230777], [1982, 1613.3619671223396], [1983, 1862.5], [1984, 1862.5], [1985, 1710.5437849576967], [1986, 1255.4419333962521], [1987, 1317.1995772356834], [1988, 1003.2719806801979], [1989, 978.5367925573563], [1990, 1032.1188229569502], [1991, 860.9404352466792], [1992, 667.0811067333905], [1993, 842.5602003535176], [1994, 1526.9450627164338], [1995, 1054.6898753961418], [1996, 1859.9715263693543], [1997, 1785.6283912172307], [1998, 1381.5897984563048], [1999, 1862.5], [2000, 1862.5], [2001, 1697.7901609292053], [2002, 1389.0218515925105], [2003, 1477.6315470944685]],
                        "month_period": "Oct - Jan",
                        "discrete_ts": [[-1522684800000, 2000], [-1520006400000, 1781.2779198935498], [-1517414400000, 1753.680411854264], [-1514736000000, 1755.0900871780752], [-1512057600000, 1751.377203735407], [-1509638400000, 1773.8588202179617], [-1506960000000, 1823.3779876894896], [-1504368000000, 1941.4609885062098], [-1501689600000, 2020.0440770669163], [-1499097600000, 2066.1119064423087], [-1496419200000, 1954.1479165086519], [-1493740800000, 1805.20424764003], [-1491148800000, 1661.277390821918], [-1488470400000, 1606.7084586644037], [-1485878400000, 1608.3827092073284], [-1483200000000, 1620.0122934537003], [-1480521600000, 1646.5654573709548], [-1478102400000, 1669.2307759507528], [-1475424000000, 1721.6334181430689], [-1472832000000, 1842.9746585412008], [-1470153600000, 1801.2034335562175], [-1467561600000, 1762.3330162481177], [-1464883200000, 1623.2067271978271], [-1462204800000, 1445.9817145628215], [-1459612800000, 1318.96194000093], [-1456934400000, 1267.0138374022983], [-1454342400000, 1235.1946301533685], [-1451664000000, 1222.6261477735661], [-1448985600000, 1135.2195725770448], [-1446480000000, 1194.6769194258063], [-1443801600000, 1187.060279040908], [-1441209600000, 1157.8162960264385], [-1438531200000, 1076.0792024410255], [-1435939200000, 1017.9796488539384], [-1433260800000, 916.5635495760231], [-1430582400000, 790.5815354927996], [-1427990400000, 692.9293326790333], [-1425312000000, 685.2761801757684], [-1422720000000, 761.9075870732373], [-1420041600000, 804.3431438866807], [-1417363200000, 700], [-1414944000000, 970.6326636228218], [-1412265600000, 1108.782885133069], [-1409673600000, 1408.496403859791], [-1406995200000, 1393.7063429446248], [-1404403200000, 1319.754896517185], [-1401724800000, 1209.0804626876377], [-1399046400000, 1068.0139759478946], [-1396454400000, 998.0079227298367], [-1393776000000, 979.7806425462186], [-1391184000000, 977.2052246832545], [-1388505600000, 998.2554848427302], [-1385827200000, 1002.9585063664682], [-1383408000000, 1149.9121685551552], [-1380729600000, 1264.1327508858071], [-1378137600000, 1485.5855061716436], [-1375459200000, 1382.6223685162938], [-1372867200000, 1319.0783658457287], [-1370188800000, 1199.2924878243061], [-1367510400000, 1084.8654665711636], [-1364918400000, 988.8276938986643], [-1362240000000, 964.9787809816952], [-1359648000000, 1121.6021900792975], [-1356969600000, 1323.1001109090655], [-1354291200000, 1447.446177983382], [-1351872000000, 1685.6309173703821], [-1349193600000, 1869.4534498532887], [-1346601600000, 2096.321160602065], [-1343923200000, 2085.710096182951], [-1341331200000, 2111.1036615612643], [-1338652800000, 1988.8826946346671], [-1335974400000, 1849.169613408825], [-1333382400000, 1711.870847766417], [-1330704000000, 1620.9488735970988], [-1328112000000, 1649.0205654890515], [-1325433600000, 1657.9679776295247], [-1322755200000, 1706.4191457763086], [-1320249600000, 1815.917621969111], [-1317571200000, 2022.385144377964], [-1314979200000, 2183.8152459078124], [-1312300800000, 2121.795429262445], [-1309708800000, 2029.2659918396362], [-1307030400000, 1885.2556516944194], [-1304352000000, 1691.104861538447], [-1301760000000, 1486.3560762676661], [-1299081600000, 1347.018187910708], [-1296489600000, 1270.1479156150513], [-1293811200000, 1263.456754879924], [-1291132800000, 1072.0025868860798], [-1288713600000, 1099.1712440349615], [-1286035200000, 1150.1912028646989], [-1283443200000, 1165.7450841953864], [-1280764800000, 1209.2071710658984], [-1278172800000, 1195.1356706196752], [-1275494400000, 1105.0005296858221], [-1272816000000, 980.9808317273778], [-1270224000000, 884.2599880058391], [-1267545600000, 854.4102245802743], [-1264953600000, 838.7406010501786], [-1262275200000, 968.3420885869782], [-1259596800000, 986.2128948914306], [-1257177600000, 1086.5106390816763], [-1254499200000, 1215.3761320622414], [-1251907200000, 1331.698371124693], [-1249228800000, 1235.4512826512607], [-1246636800000, 1213.3510818869818], [-1243958400000, 1070.7385258115853], [-1241280000000, 958.0948060077742], [-1238688000000, 863.8067764049755], [-1236009600000, 834.3150241157856], [-1233417600000, 821.5825651171108], [-1230739200000, 811.6296217669077], [-1228060800000, 822.4693335682726], [-1225641600000, 843.0713513518101], [-1222963200000, 902.5752826333479], [-1220371200000, 925.6007539571392], [-1217692800000, 886.083809240228], [-1215100800000, 862.7207138482978], [-1212422400000, 768.0027074469205], [-1209744000000, 643.3971618089848], [-1207152000000, 576.9922913028873], [-1204473600000, 561.1368858753079], [-1201881600000, 553.5291323720433], [-1199203200000, 556.446135062365], [-1196524800000, 503.4197530938489], [-1194019200000, 525.3817174807058], [-1191340800000, 677.9895482180863], [-1188748800000, 758.2565661698034], [-1186070400000, 781.278235744211], [-1183478400000, 771.7015736600978], [-1180800000000, 686.7130937520867], [-1178121600000, 591.8687089247851], [-1175529600000, 526.4022122773378], [-1172851200000, 504.0239732873246], [-1170259200000, 492.6049501840281], [-1167580800000, 488.22232245278593], [-1164902400000, 485.16441333514405], [-1162483200000, 486.5457894740959], [-1159804800000, 588.2283667790265], [-1157212800000, 741.5355686853476], [-1154534400000, 728.9815594619384], [-1151942400000, 847.4024207940521], [-1149264000000, 778.7556142642202], [-1146585600000, 659.6638059242582], [-1143993600000, 596.5028309216942], [-1141315200000, 578.7817135778438], [-1138723200000, 570.163877880316], [-1136044800000, 580.0103352500819], [-1133366400000, 641.2198704573157], [-1130947200000, 731.2535174733048], [-1128268800000, 887.9945742527568], [-1125676800000, 971.4650040230005], [-1122998400000, 943.1036539984946], [-1120406400000, 906.9261398879603], [-1117728000000, 812.6176571095303], [-1115049600000, 689.2333070997395], [-1112457600000, 500], [-1109779200000, 488.8700303826992], [-1107187200000, 554.0659769305328], [-1104508800000, 590.6755458680916], [-1101830400000, 565.1126227321208], [-1099411200000, 648.5747545827475], [-1096732800000, 709.6597220064965], [-1094140800000, 923.2158775449302], [-1091462400000, 955.4905342020431], [-1088870400000, 962.1820912873126], [-1086192000000, 887.687194321757], [-1083513600000, 777.0898398982085], [-1080921600000, 683.4226942866693], [-1078243200000, 669.1467925256757], [-1075651200000, 663.8146730478278], [-1072972800000, 666.8639954825488], [-1070294400000, 779.9227354795129], [-1067788800000, 920.1476282675824], [-1065110400000, 1036.555570178952], [-1062518400000, 1211.9646271733388], [-1059840000000, 1141.7687112772585], [-1057248000000, 1102.778830274539], [-1054569600000, 1053.1622881081223], [-1051891200000, 956.9538774108589], [-1049299200000, 875.6307678708306], [-1046620800000, 849.8611902625414], [-1044028800000, 836.035382382843], [-1041350400000, 823.3444557543882], [-1038672000000, 812.0989509632457], [-1036252800000, 813.2616654098324], [-1033574400000, 915.0790637144631], [-1030982400000, 1137.1654948369567], [-1028304000000, 1282.709583742305], [-1025712000000, 1357.2966692354116], [-1023033600000, 1290.8439754889594], [-1020355200000, 1171.910459615378], [-1017763200000, 1090.2079363714683], [-1015084800000, 1073.7497633338526], [-1012492800000, 1221.3169954364075], [-1009814400000, 1440.6723201790041], [-1007136000000, 1538.200278940654], [-1004716800000, 1695.103080885736], [-1002038400000, 1956.3388616496065], [-999446400000, 2263.772611756414], [-996768000000, 2420], [-994176000000, 2364.7458994444296], [-991497600000, 2270], [-988819200000, 2149.9999999999995], [-986227200000, 1945.2492707362485], [-983548800000, 1848.8299314899282], [-980956800000, 1850], [-978278400000, 1849.9999999999998], [-975600000000, 1861.157167116083], [-973180800000, 1875.2774732131365], [-970502400000, 1983.2145393876838], [-967910400000, 2058.507240661381], [-965232000000, 1976.7469507437113], [-962640000000, 1875.191053610787], [-959961600000, 1643.0521821846405], [-957283200000, 1398.9822938910966], [-954691200000, 1189.9053450608549], [-952012800000, 1121.7115635173332], [-949420800000, 1059.704288808632], [-946742400000, 1123.9303851257278], [-944064000000, 1300.2703225893542], [-941558400000, 1626.08798682379], [-938880000000, 1928.157980581903], [-936288000000, 2154.2774054419424], [-933609600000, 2091.0963589041967], [-931017600000, 1937.9463014532917], [-928339200000, 1788.8641455047239], [-925660800000, 1612.6248084718632], [-923068800000, 1447.4648548620523], [-920390400000, 1394.5661970834474], [-917798400000, 1378.1361537775454], [-915120000000, 1517.431024839591], [-912441600000, 1738.1164333134277], [-910022400000, 1988.7915057319153], [-907344000000, 2100], [-904752000000, 2300], [-902073600000, 2420], [-899481600000, 2447], [-896803200000, 2270], [-894124800000, 2150], [-891532800000, 1975], [-888854400000, 1850], [-886262400000, 1836.877229677874], [-883584000000, 1850], [-880905600000, 1900], [-878486400000, 2000], [-875808000000, 2078.540134242459], [-873216000000, 2285.258121595209], [-870537600000, 2344.795308522242], [-867945600000, 2416.522614065537], [-865267200000, 2270], [-862588800000, 2150], [-859996800000, 1928.1033953999029], [-857318400000, 1850], [-854726400000, 1845.1412431106548], [-852048000000, 1850], [-849369600000, 1900], [-846950400000, 2000.0000000000002], [-844272000000, 2100], [-841680000000, 2283.155661244806], [-839001600000, 2173.318610935777], [-836409600000, 2100.1013478625755], [-833731200000, 1955.632799582456], [-831052800000, 1810.9238053951467], [-828460800000, 1640.4268782084089], [-825782400000, 1552.3242620568308], [-823190400000, 1448.5865628213835], [-820512000000, 1438.2768847232037], [-817833600000, 1387.0731245212833], [-815328000000, 1424.2658218771678], [-812649600000, 1486.3263833722374], [-810057600000, 1525.7739348519233], [-807379200000, 1583.183427187927], [-804787200000, 1562.230589876419], [-802108800000, 1396.3866613282485], [-799430400000, 1224.875118011666], [-796838400000, 1099.9222359145306], [-794160000000, 1071.1073778316506], [-791568000000, 1099.666846002828], [-788889600000, 1176.6031889193787], [-786211200000, 1234.320497526698], [-783792000000, 1399.5740748184526], [-781113600000, 1453.4713230037976], [-778521600000, 1591.1807408796776], [-775843200000, 1555.9025031048423], [-773251200000, 1496.7498649794875], [-770572800000, 1376.6789340147495], [-767894400000, 1256.4127719564876], [-765302400000, 1128.6152206163235], [-762624000000, 1124.0893879494224], [-760032000000, 1179.0981527335039], [-757353600000, 1370.3339784167995], [-754675200000, 1500.8470385400851], [-752256000000, 1555.8710894235633], [-749577600000, 1678.1702190001129], [-746985600000, 1896.3436053109099], [-744307200000, 1891.799908971309], [-741715200000, 1825.9335678825084], [-739036800000, 1694.6572879816613], [-736358400000, 1522.9625851191254], [-733766400000, 1396.8938742436665], [-731088000000, 1343.9657909181578], [-728496000000, 1352.4678638762744], [-725817600000, 1373.4023886288246], [-723139200000, 1372.514651996264], [-720720000000, 1432.5238328631347], [-718041600000, 1551.5932078755172], [-715449600000, 1638.9200688653398], [-712771200000, 1548.851160822023], [-710179200000, 1541.2965192318286], [-707500800000, 1431.3302547831106], [-704822400000, 1287.344866921451], [-702230400000, 1162.8407650063748], [-699552000000, 1178.7859439538047], [-696960000000, 1192.683086581423], [-694281600000, 1192.0296948000075], [-691603200000, 1387.661327317311], [-689097600000, 1410.1399347408717], [-686419200000, 1444.5926420625644], [-683827200000, 1631.6982015393976], [-681148800000, 1625.891907669828], [-678556800000, 1705.339993581719], [-675878400000, 1669.4634155222304], [-673200000000, 1531.7520633520166], [-670608000000, 1484.6947220920547], [-667929600000, 1438.5100015646508], [-665337600000, 1445.3973078163935], [-662659200000, 1447.4427361025694], [-659980800000, 1440.8585586274244], [-657561600000, 1472.7400679214754], [-654883200000, 1688.9250292776117], [-652291200000, 1948.6313991055752], [-649612800000, 1949.537740205285], [-647020800000, 1898.8139697344589], [-644342400000, 1753.671335315007], [-641664000000, 1579.6513284515659], [-639072000000, 1410.6631910418785], [-636393600000, 1356.108282901795], [-633801600000, 1326.1007653581487], [-631123200000, 1310.2763372670374], [-628444800000, 1331.4812475852184], [-626025600000, 1387.6905012299703], [-623347200000, 1508.7524489060706], [-620755200000, 1666.7614556732237], [-618076800000, 1676.1364954634478], [-615484800000, 1657.6194573412329], [-612806400000, 1550.3161362239355], [-610128000000, 1404.5420545719082], [-607536000000, 1278.237948965723], [-604857600000, 1347.690986910112], [-602265600000, 1463.371054790445], [-599587200000, 1720.9913467665885], [-596908800000, 1810.967435671686], [-594489600000, 2000], [-591811200000, 2100], [-589219200000, 2285.6210802982428], [-586540800000, 2217.5091071736506], [-583948800000, 2133.2400239159238], [-581270400000, 1985.6232395973063], [-578592000000, 1764.3155817750212], [-576000000000, 1594.8513736444622], [-573321600000, 1510.5784833488794], [-570729600000, 1518.7352008707487], [-568051200000, 1666.1410492782995], [-565372800000, 1725.545605918398], [-562867200000, 1915.563785259593], [-560188800000, 2081.523545392229], [-557596800000, 2300], [-554918400000, 2410.9636868144175], [-552326400000, 2447], [-549648000000, 2269.9999999999995], [-546969600000, 2150], [-544377600000, 1974.9999999999998], [-541699200000, 1850], [-539107200000, 1845.3146506149888], [-536428800000, 1849.9999999999998], [-533750400000, 1900], [-531331200000, 2000], [-528652800000, 2100], [-526060800000, 2300], [-523382400000, 2259.9112123923715], [-520790400000, 2353.448239660465], [-518112000000, 2269.9999999999995], [-515433600000, 2149.9999999999995], [-512841600000, 1959.0443722661785], [-510163200000, 1850], [-507571200000, 1850], [-504892800000, 1850], [-502214400000, 1900], [-499795200000, 2000], [-497116800000, 2100], [-494524800000, 2300], [-491846400000, 2271.5030161556515], [-489254400000, 2203.9284707403995], [-486576000000, 2067.0659267830138], [-483897600000, 1854.7149140411373], [-481305600000, 1692.8638168713126], [-478627200000, 1635.7175848071697], [-476035200000, 1656.6056937569667], [-473356800000, 1697.683829744439], [-470678400000, 1715.9752718325833], [-468259200000, 1744.826099881744], [-465580800000, 1774.9390652282545], [-462988800000, 1821.0444285042188], [-460310400000, 1834.9302833710838], [-457718400000, 1820.2044588612607], [-455040000000, 1680.757344071697], [-452361600000, 1503.9436109588632], [-449769600000, 1376.7919752917066], [-447091200000, 1317.3012126888084], [-444499200000, 1304.0857010842735], [-441820800000, 1612.1953317282992], [-439142400000, 1900], [-436636800000, 2000], [-433958400000, 2100], [-431366400000, 2300], [-428688000000, 2402.4665039813262], [-426096000000, 2321.222056407487], [-423417600000, 2270], [-420739200000, 2149.9999999999995], [-418147200000, 1953.0690195881941], [-415468800000, 1850], [-412876800000, 1825.7510045818149], [-410198400000, 1818.3635572968415], [-407520000000, 1808.4820371702049], [-405100800000, 1955.607000808909], [-402422400000, 2100], [-399830400000, 2218.3702490417722], [-397152000000, 2234.431188934835], [-394560000000, 2211.4354718735713], [-391881600000, 2067.2667816062085], [-389203200000, 1923.221168855265], [-386611200000, 1759.1747733089287], [-383932800000, 1850], [-381340800000, 1850], [-378662400000, 1850], [-375984000000, 1900], [-373564800000, 2208.1742813390774], [-370886400000, 2100], [-368294400000, 2300], [-365616000000, 2420], [-363024000000, 2430.2546446549427], [-360345600000, 2270.0000000000005], [-357667200000, 2150], [-355075200000, 1965.9581241632252], [-352396800000, 1850.0000000000002], [-349804800000, 1850], [-347126400000, 1835.4504962858969], [-344448000000, 1900], [-342028800000, 2000.0000000000002], [-339350400000, 2100], [-336758400000, 2271.8013674182434], [-334080000000, 2170.7253036678144], [-331488000000, 2053.05568815621], [-328809600000, 1825.442029625731], [-326131200000, 1648.7457096809633], [-323539200000, 1483.866573583779], [-320860800000, 1424.9175868923633], [-318268800000, 1388.9565311053684], [-315590400000, 1370.3883965322848], [-312912000000, 1372.120802441327], [-310406400000, 1508.6328009333947], [-307728000000, 1702.777428521017], [-305136000000, 1823.9990909947398], [-302457600000, 1768.9249936774424], [-299865600000, 1754.7704226996789], [-297187200000, 1615.6465028144446], [-294508800000, 1441.7018216411525], [-291916800000, 1314.3238212976692], [-289238400000, 1256.6110506705359], [-286646400000, 1243.3145046314148], [-283968000000, 1308.6206262988114], [-281289600000, 1352.764380220775], [-278870400000, 1544.061546757775], [-276192000000, 1641.6830616616924], [-273600000000, 1831.7397449361822], [-270921600000, 1859.784907233123], [-268329600000, 1894.6163744972243], [-265651200000, 1748.0525945241816], [-262972800000, 1575.1250467433786], [-260380800000, 1408.5745689811085], [-257702400000, 1355.862979417707], [-255110400000, 1333.753113422714], [-252432000000, 1356.5375243739634], [-249753600000, 1373.6396455548213], [-247334400000, 1471.4837737804926], [-244656000000, 1529.9831111395413], [-242064000000, 1855.7639582984036], [-239385600000, 1821.8080437510769], [-236793600000, 1788.808781404829], [-234115200000, 1640.5092476323107], [-231436800000, 1470.9699938918789], [-228844800000, 1345.8626290630316], [-226166400000, 1447.5487102352663], [-223574400000, 1496.1451832146618], [-220896000000, 1662.018447502056], [-218217600000, 1702.9904229633066], [-215798400000, 1983.1822096677809], [-213120000000, 2057.456543050294], [-210528000000, 2300], [-207849600000, 2323.768972513454], [-205257600000, 2266.0778772760577], [-202579200000, 2213.4910103292814], [-199900800000, 2066.4111905369864], [-197308800000, 1835.9767982901947], [-194630400000, 1742.1834356097497], [-192038400000, 1811.9187658383967], [-189360000000, 1837.5458295932533], [-186681600000, 1839.1217064077375], [-184176000000, 1840.1454306938003], [-181497600000, 1869.4939975004604], [-178905600000, 1904.2169280101946], [-176227200000, 1796.5306004449951], [-173635200000, 1766.586760753536], [-170956800000, 1619.4764145176568], [-168278400000, 1450.5154031207796], [-165686400000, 1333.0177840450158], [-163008000000, 1281.7357506188282], [-160416000000, 1291.265053594294], [-157737600000, 1810.0850128649877], [-155059200000, 1900], [-152640000000, 1987.636434425], [-149961600000, 2054.0129112455243], [-147369600000, 2277.9958455007563], [-144691200000, 2184.594332718428], [-142099200000, 2113.8459705101577], [-139420800000, 1977.3865602227456], [-136742400000, 1846.3686972546493], [-134150400000, 1713.069478467428], [-131472000000, 1652.5048995124087], [-128880000000, 1706.6958409779681], [-126201600000, 1728.5569082233242], [-123523200000, 1799.7060583032214], [-121104000000, 1865.1729436682433], [-118425600000, 2063.4076812733947], [-115833600000, 2300], [-113155200000, 2300.6042666254175], [-110563200000, 2225.687726289895], [-107884800000, 2041.8936341069427], [-105206400000, 1828.2564510105747], [-102614400000, 1666.225586809633], [-99936000000, 1577.240032627045], [-97344000000, 1654.7798635496722], [-94665600000, 1789.026485474115], [-91987200000, 1897.1660136236221], [-89568000000, 2000], [-86889600000, 2100], [-84297600000, 2205.250473305693], [-81619200000, 2310.2516540943643], [-79027200000, 2400.779247457748], [-76348800000, 2270], [-73670400000, 2150], [-71078400000, 1943.9086748723203], [-68400000000, 1845.8486402061653], [-65808000000, 1850.0000000000002], [-63129600000, 1850.0000000000002], [-60451200000, 1900.0000000000002], [-57945600000, 2000], [-55267200000, 2100], [-52675200000, 2191.009019304407], [-49996800000, 2077.487701506115], [-47404800000, 1971.0970102032252], [-44726400000, 1820.5758791420346], [-42048000000, 1647.9158328324097], [-39456000000, 1481.7815168854245], [-36777600000, 1428.1851001258094], [-34185600000, 1424.723189230479], [-31507200000, 1483.6965911501086], [-28828800000, 1620.2184605097423], [-26409600000, 1738.011104915257], [-23731200000, 1892.7236716813857], [-21139200000, 2214.370561932174], [-18460800000, 2420], [-15868800000, 2340.1647498014195], [-13190400000, 2269.9999999999995], [-10512000000, 2150], [-7920000000, 1974.9999999999998], [-5241600000, 1850], [-2649600000, 1850], [28800000, 1850], [2707200000, 1900], [5126400000, 2000], [7804800000, 2100], [10396800000, 2161.386748718372], [13075200000, 2060.638672231599], [15667200000, 1988.8234838992385], [18345600000, 1844.4047121675212], [21024000000, 1668.3971073240116], [23616000000, 1445.0196593072849], [26294400000, 1303.0055204902571], [28886400000, 1398.4895854025885], [31564800000, 1524.2939942336861], [34243200000, 1747.0609887331439], [36662400000, 1877.0412493849003], [39340800000, 2064.958950913993], [41932800000, 2233.422039816418], [44611200000, 2268.0680597720575], [47203200000, 2276.7753785950713], [49881600000, 2253.262391886064], [52560000000, 2126.8194998997124], [55152000000, 1975.0000000000005], [57830400000, 1850], [60422400000, 1850], [63100800000, 1850], [65779200000, 1900], [68284800000, 2000.0000000000002], [70963200000, 2100], [73555200000, 2233.747818808155], [76233600000, 2163.0551072842095], [78825600000, 2112.663296194655], [81504000000, 1964.164991715206], [84182400000, 1790.046312488748], [86774400000, 1621.778143966301], [89452800000, 1537.4034518890714], [92044800000, 1573.3059833706957], [94723200000, 1677.274620373853], [97401600000, 1830.114784780456], [99820800000, 1976.6948455900938], [102499200000, 2096.68274764028], [105091200000, 2284.391770253808], [107769600000, 2333.60894084383], [110361600000, 2280.0587384121886], [113040000000, 2140.228440088989], [115718400000, 1917.0728687605979], [118310400000, 1748.980913698329], [120988800000, 1719.885858347993], [123580800000, 1850], [126259200000, 1850], [128937600000, 1900], [131356800000, 2000], [134035200000, 2100], [136627200000, 2300], [139305600000, 2383.8642406856357], [141897600000, 2367.4432339418972], [144576000000, 2270], [147254400000, 2150], [149846400000, 1974.9999999999995], [152524800000, 1849.9999999999998], [155116800000, 1843.459845552196], [157795200000, 1847.274837747011], [160473600000, 1859.3486192415046], [162892800000, 1947.6387823684584], [165571200000, 2100], [168163200000, 2224.726727611186], [170841600000, 2320.5631986624617], [173433600000, 2431.459625841006], [176112000000, 2270.0000000000005], [178790400000, 2149.9999999999995], [181382400000, 1966.418145931095], [184060800000, 1850], [186652800000, 1850.0000000000002], [189331200000, 1850], [192009600000, 1667.8434509818715], [194515200000, 1698.9667925973654], [197193600000, 1747.2694044171842], [199785600000, 1828.6048869287995], [202464000000, 1803.0665008675649], [205056000000, 1752.9027827882471], [207734400000, 1542.1549599550035], [210412800000, 1407.067570733263], [213004800000, 1282.129752065428], [215683200000, 1227.7180964531656], [218275200000, 1191.648639568498], [220953600000, 1036.4125320692906], [223632000000, 1000], [226051200000, 991.32123914753], [228729600000, 977.4567240778305], [231321600000, 946.8749997896836], [234000000000, 894.4103622938579], [236592000000, 856.9124157878312], [239270400000, 629.6900811859233], [241948800000, 385.1832249108495], [244540800000, 331.8611964199329], [247219200000, 240.00000000000003], [249811200000, 267.29062040796197], [252489600000, 458.20667482838076], [255168000000, 843.2943173232236], [257587200000, 1021.360526344515], [260265600000, 1294.6939352668446], [262857600000, 1486.2069198797117], [265536000000, 1490.6420420832794], [268128000000, 1435.685614615596], [270806400000, 1373.1089864911023], [273484800000, 1287.2509813134582], [276076800000, 1290.8518368198884], [278755200000, 1237.754570746323], [281347200000, 1231.3824264810678], [284025600000, 1220.9978054857245], [286704000000, 1242.6137486926143], [289123200000, 1296.7604068607495], [291801600000, 1439.6899110370807], [294393600000, 1554.7824593239939], [297072000000, 1631.1366288142578], [299664000000, 1633.6309871051878], [302342400000, 1524.9498710621351], [305020800000, 1380.2635636984028], [307612800000, 1254.2311920917111], [310291200000, 1226.5043831719684], [312883200000, 1288.9961206661428], [315561600000, 1355.6918101024762], [318240000000, 1574.4661446038804], [320745600000, 1862.9335359674171], [323424000000, 1994.5811623113902], [326016000000, 2153.983980807646], [328694400000, 2058.8492254964376], [331286400000, 1923.5257193362174], [333964800000, 1800.5952640148594], [336643200000, 1656.2805709859026], [339235200000, 1518.036525945945], [341913600000, 1462.8541000647358], [344505600000, 1453.3129395723236], [347184000000, 1485.0038318576721], [349862400000, 1590.8697201975795], [352281600000, 1728.8169779239474], [354960000000, 1852.96842412533], [357552000000, 1961.2447638817073], [360230400000, 1893.203740423532], [362822400000, 1837.2720074979213], [365500800000, 1609.4640804263177], [368179200000, 1430.2654521387296], [370771200000, 1307.2576162869836], [373449600000, 1248.1232105872862], [376041600000, 1491.0533875139604], [378720000000, 1814.2712703881114], [381398400000, 1900], [383817600000, 2000], [386496000000, 2100], [389088000000, 2300], [391766400000, 2317.827169596444], [394358400000, 2187.3578334552903], [397036800000, 2163.2798276013855], [399715200000, 2138.8872982703283], [402307200000, 1975], [404985600000, 1849.9999999999998], [407577600000, 1850], [410256000000, 1850], [412934400000, 1900], [415353600000, 2000], [418032000000, 2100], [420624000000, 2300], [423302400000, 2420], [425894400000, 2447], [428572800000, 2270], [431251200000, 2150], [433843200000, 1975], [436521600000, 1850], [439113600000, 1850], [441792000000, 1850], [444470400000, 1900], [446976000000, 1996.7366353712937], [449654400000, 2100], [452246400000, 2217.6119610357614], [454924800000, 2183.315329659132], [457516800000, 2124.2652814817266], [460195200000, 1988.7868550547462], [462873600000, 1846.1780276564523], [465465600000, 1677.0721804104767], [468144000000, 1588.124427465747], [470736000000, 1717.757260599398], [473414400000, 1774.847812265657], [476092800000, 1761.4456394999847], [478512000000, 1800.3576326347957], [481190400000, 1848.4153381602764], [483782400000, 1994.9938914051818], [486460800000, 1917.90037326457], [489052800000, 1872.1526139675534], [491731200000, 1645.8414869398073], [494409600000, 1427.288598138054], [497001600000, 1304.633778751013], [499680000000, 1245.2718507425068], [502272000000, 1214.5211410504749], [504950400000, 1221.005228134549], [507628800000, 1340.9695136574776], [510048000000, 1814.4671371464387], [512726400000, 2100], [515318400000, 2122.058293127561], [517996800000, 2018.8355583014431], [520588800000, 1907.6219344278315], [523267200000, 1731.8326611765053], [525945600000, 1556.9252356790867], [528537600000, 1390.8888005568404], [531216000000, 1340.2964995637865], [533808000000, 1309.6570108164294], [536486400000, 1303.322490661915], [539164800000, 1315.5223079006028], [541584000000, 1394.252448633746], [544262400000, 1599.3158296695287], [546854400000, 1745.7889317993288], [549532800000, 1683.839881213154], [552124800000, 1488.2722412077662], [554803200000, 1263.1221034641658], [557481600000, 1081.938879035368], [560073600000, 989.778984974594], [562752000000, 917.3611188047898], [565344000000, 903.4503222747335], [568022400000, 1055.5292777975449], [570700800000, 1136.7472038437234], [573206400000, 1221.2329842405572], [575884800000, 1301.7292187648784], [578476800000, 1388.2072025121329], [581155200000, 1333.4523506635128], [583747200000, 1315.76703337384], [586425600000, 1188.7386561345204], [589104000000, 1067.8682112683816], [591696000000, 975.6567928493042], [594374400000, 947.3464410532132], [596966400000, 969.4458620774718], [599644800000, 987.250704691126], [602323200000, 1010.1041624076142], [604742400000, 1037.4438319093156], [607420800000, 1384.6769853904461], [610012800000, 1588.6570865538747], [612691200000, 1479.685233472167], [615283200000, 1371.7111272156092], [617961600000, 1222.8228079133428], [620640000000, 1079.4320035248907], [623232000000, 999.2917874488438], [625910400000, 1015.7849361920014], [628502400000, 1019.050698611336], [631180800000, 1013.9372866643739], [633859200000, 1079.7023703600894], [636278400000, 1106.4554814808525], [638956800000, 1194.4450436551606], [641548800000, 1233.7062886979336], [644227200000, 1201.1891486320276], [646819200000, 1219.9259419889881], [649497600000, 1111.9707532454336], [652176000000, 1002.2717902025047], [654768000000, 910.2523204362136], [657446400000, 882.0119251345476], [660038400000, 869.5842474101307], [662716800000, 853.9712206495789], [665395200000, 838.1943477924593], [667814400000, 843.6839280726939], [670492800000, 918.131198943555], [673084800000, 967.2424004665213], [675763200000, 990.8712570050408], [678355200000, 986.9446485884981], [681033600000, 900.8140230260691], [683712000000, 782.1502720716592], [686304000000, 690.7672670557998], [688982400000, 675.0309049880871], [691574400000, 666.0416949004375], [694252800000, 661.0739819894875], [696931200000, 666.1778450555498], [699436800000, 805.0508316339534], [702115200000, 952.6354378685378], [704707200000, 1186.1335863446727], [707385600000, 1179.2403993991347], [709977600000, 1155.004614088337], [712656000000, 1067.8581974183173], [715334400000, 948.8156818284945], [717926400000, 856.9505894613], [720604800000, 828.7365578147753], [723196800000, 814.8019384956298], [725875200000, 834.3676271394514], [728553600000, 892.3346779642139], [730972800000, 1029.8210726495768], [733651200000, 1407.764584074657], [736243200000, 1603.336985956103], [738921600000, 1772.6877706999164], [741513600000, 1887.1982173537535], [744192000000, 1870.1052310789062], [746870400000, 1737.9863010591268], [749462400000, 1601.9682000311561], [752140800000, 1541.7074048265936], [754732800000, 1531.9687495705525], [757411200000, 1543.7668427512474], [760089600000, 1490.3372537173411], [762508800000, 1524.2779533808432], [765187200000, 1592.2522115775682], [767779200000, 1637.2605111809703], [770457600000, 1641.5914671076464], [773049600000, 1495.7353405097967], [775728000000, 1332.4173856801356], [778406400000, 1160.502404017938], [780998400000, 1000.0000000000002], [783676800000, 971.8444437642389], [786268800000, 958.0537940241618], [788947200000, 944.0670895317306], [791625600000, 1344.794174264436], [794044800000, 1598.4891153592107], [796723200000, 2031.8177661671352], [799315200000, 2269.8850203844954], [801993600000, 2345.197510560271], [804585600000, 2330.394740403572], [807264000000, 2270], [809942400000, 2150], [812534400000, 1975], [815212800000, 1850], [817804800000, 1839.8861054774172], [820483200000, 1849.9999999999998], [823161600000, 1900], [825667200000, 2000], [828345600000, 2100], [830937600000, 2277.87476862332], [833616000000, 2233.7378084315374], [836208000000, 2169.6892748445516], [838886400000, 2032.2495186719775], [841564800000, 1895.4777605132456], [844156800000, 1756.4788425073161], [846835200000, 1693.3476343296702], [849427200000, 1699.1659305392523], [852105600000, 1850], [854784000000, 1900], [857203200000, 1990.4408221828824], [859881600000, 2076.459270329076], [862473600000, 2176.3063405029316], [865152000000, 1989.6788739925485], [867744000000, 1820.5544399836356], [870422400000, 1670.2942493085625], [873100800000, 1493.5938735098152], [875692800000, 1367.074740071836], [878371200000, 1307.5873421319006], [880963200000, 1282.0079220310236], [883641600000, 1313.0341381922276], [886320000000, 1623.729791470067], [888739200000, 1946.2769892306965], [891417600000, 2100], [894009600000, 2295.5724488973265], [896688000000, 2420], [899280000000, 2447], [901958400000, 2270], [904636800000, 2150], [907228800000, 1975], [909907200000, 1850], [912499200000, 1850], [915177600000, 1850], [917856000000, 1900], [920275200000, 2000], [922953600000, 2100], [925545600000, 2299.8805487206073], [928224000000, 2339.7516610243606], [930816000000, 2362.622819254627], [933494400000, 2270.0000000000005], [936172800000, 2135.8543367653], [938764800000, 1975], [941443200000, 1849.9999999999998], [944035200000, 1850], [946713600000, 1850], [949392000000, 1900], [951897600000, 2000], [954576000000, 2100], [957168000000, 2300], [959846400000, 2291.5704881582233], [962438400000, 2306.4403357574006], [965116800000, 2176.0114758811656], [967795200000, 1952.718735207226], [970387200000, 1787.0663336744583], [973065600000, 1727.1691603279062], [975657600000, 1695.1461971740428], [978336000000, 1684.5246575410881], [981014400000, 1684.3206286737839], [983433600000, 1714.6361222097703], [986112000000, 1875.8520507547896], [988704000000, 1975.3077059711397], [991382400000, 1955.7134469388898], [993974400000, 1873.3953477517596], [996652800000, 1660.929603429922], [999331200000, 1460.9076585954022], [1001923200000, 1341.824798702595], [1004601600000, 1269.6145051229864], [1007193600000, 1293.369325868846], [1009872000000, 1381.5605489285103], [1012550400000, 1611.5430264496993], [1014969600000, 1721.427600296709], [1017648000000, 1823.9042246307004], [1020240000000, 2030.9250905960787], [1022918400000, 1939.82265574909], [1025510400000, 1841.3700200174953], [1028188800000, 1691.5919199328766], [1030867200000, 1515.2632249651588], [1033459200000, 1388.7935507207378], [1036137600000, 1333.827877588737], [1038729600000, 1299.119710255751], [1041408000000, 1482.607949820857], [1044086400000, 1794.9706507125288], [1046505600000, 1906.434134603811], [1049184000000, 2061.03190457474], [1051776000000, 2260.869065374906], [1054454400000, 2326.703198307505], [1057046400000, 2401.7522514707675], [1059724800000, 2270.0000000000005], [1062403200000, 2138.277988812066], [1064995200000, 1974.9999999999998]]
                    }, {
                        "period_months": ["Oct", "Nov", "Dec", "Jan"],
                        "annual_period": "Sacramento River Index<br>Wet",
                        "computed_statistics": [{
                            "statistic": "Averages",
                            "statistically_computed_time_series_monthly": [["Jan", 1784.9225126098243], ["Oct", 1548.748796268991], ["Nov", 1574.7149239896946], ["Dec", 1672.7172008851574]],
                            "statistic_aggregate": 1637.053939135014
                        }, {
                            "statistic": "Minimums",
                            "statistically_computed_time_series_monthly": [["Jan", 1340.9695136574776], ["Oct", 971.8444437642389], ["Nov", 958.0537940241618], ["Dec", 944.0670895317306]],
                            "statistic_aggregate": 1054.6898753961418
                        }],
                        "aggregate_ts": [[1927, 1214.28181498836], [1938, 1318.4848394724795], [1941, 1507.062452253503], [1942, 1859.2193074194686], [1943, 1861.2853107776637], [1952, 1605.2500848540815], [1953, 1861.3286626537472], [1956, 1533.3955613753453], [1958, 1862.5], [1963, 1577.1756909788228], [1965, 1570.7714542695276], [1967, 1729.5530988186135], [1969, 1489.2058352540348], [1970, 1862.5], [1971, 1493.2125222149189], [1974, 1829.9714645869983], [1975, 1850.0208256351777], [1982, 1613.3619671223396], [1983, 1862.5], [1984, 1862.5], [1986, 1255.4419333962521], [1995, 1054.6898753961418], [1996, 1859.9715263693543], [1997, 1785.6283912172307], [1998, 1381.5897984563048], [1999, 1862.5]],
                        "month_period": "Oct - Jan",
                        "discrete_ts": [[-1362240000000, 964.9787809816952], [-1359648000000, 1121.6021900792975], [-1356969600000, 1323.1001109090655], [-1354291200000, 1447.446177983382], [-1351872000000, 1685.6309173703821], [-1349193600000, 1869.4534498532887], [-1346601600000, 2096.321160602065], [-1343923200000, 2085.710096182951], [-1341331200000, 2111.1036615612643], [-1338652800000, 1988.8826946346671], [-1335974400000, 1849.169613408825], [-1333382400000, 1711.870847766417], [-1015084800000, 1073.7497633338526], [-1012492800000, 1221.3169954364075], [-1009814400000, 1440.6723201790041], [-1007136000000, 1538.200278940654], [-1004716800000, 1695.103080885736], [-1002038400000, 1956.3388616496065], [-999446400000, 2263.772611756414], [-996768000000, 2420], [-994176000000, 2364.7458994444296], [-991497600000, 2270], [-988819200000, 2149.9999999999995], [-986227200000, 1945.2492707362485], [-920390400000, 1394.5661970834474], [-917798400000, 1378.1361537775454], [-915120000000, 1517.431024839591], [-912441600000, 1738.1164333134277], [-910022400000, 1988.7915057319153], [-907344000000, 2100], [-904752000000, 2300], [-902073600000, 2420], [-899481600000, 2447], [-896803200000, 2270], [-894124800000, 2150], [-891532800000, 1975], [-888854400000, 1850], [-886262400000, 1836.877229677874], [-883584000000, 1850], [-880905600000, 1900], [-878486400000, 2000], [-875808000000, 2078.540134242459], [-873216000000, 2285.258121595209], [-870537600000, 2344.795308522242], [-867945600000, 2416.522614065537], [-865267200000, 2270], [-862588800000, 2150], [-859996800000, 1928.1033953999029], [-857318400000, 1850], [-854726400000, 1845.1412431106548], [-852048000000, 1850], [-849369600000, 1900], [-846950400000, 2000.0000000000002], [-844272000000, 2100], [-841680000000, 2283.155661244806], [-839001600000, 2173.318610935777], [-836409600000, 2100.1013478625755], [-833731200000, 1955.632799582456], [-831052800000, 1810.9238053951467], [-828460800000, 1640.4268782084089], [-573321600000, 1510.5784833488794], [-570729600000, 1518.7352008707487], [-568051200000, 1666.1410492782995], [-565372800000, 1725.545605918398], [-562867200000, 1915.563785259593], [-560188800000, 2081.523545392229], [-557596800000, 2300], [-554918400000, 2410.9636868144175], [-552326400000, 2447], [-549648000000, 2269.9999999999995], [-546969600000, 2150], [-544377600000, 1974.9999999999998], [-541699200000, 1850], [-539107200000, 1845.3146506149888], [-536428800000, 1849.9999999999998], [-533750400000, 1900], [-531331200000, 2000], [-528652800000, 2100], [-526060800000, 2300], [-523382400000, 2259.9112123923715], [-520790400000, 2353.448239660465], [-518112000000, 2269.9999999999995], [-515433600000, 2149.9999999999995], [-512841600000, 1959.0443722661785], [-447091200000, 1317.3012126888084], [-444499200000, 1304.0857010842735], [-441820800000, 1612.1953317282992], [-439142400000, 1900], [-436636800000, 2000], [-433958400000, 2100], [-431366400000, 2300], [-428688000000, 2402.4665039813262], [-426096000000, 2321.222056407487], [-423417600000, 2270], [-420739200000, 2149.9999999999995], [-418147200000, 1953.0690195881941], [-383932800000, 1850], [-381340800000, 1850], [-378662400000, 1850], [-375984000000, 1900], [-373564800000, 2208.1742813390774], [-370886400000, 2100], [-368294400000, 2300], [-365616000000, 2420], [-363024000000, 2430.2546446549427], [-360345600000, 2270.0000000000005], [-357667200000, 2150], [-355075200000, 1965.9581241632252], [-226166400000, 1447.5487102352663], [-223574400000, 1496.1451832146618], [-220896000000, 1662.018447502056], [-218217600000, 1702.9904229633066], [-215798400000, 1983.1822096677809], [-213120000000, 2057.456543050294], [-210528000000, 2300], [-207849600000, 2323.768972513454], [-205257600000, 2266.0778772760577], [-202579200000, 2213.4910103292814], [-199900800000, 2066.4111905369864], [-197308800000, 1835.9767982901947], [-163008000000, 1281.7357506188282], [-160416000000, 1291.265053594294], [-157737600000, 1810.0850128649877], [-155059200000, 1900], [-152640000000, 1987.636434425], [-149961600000, 2054.0129112455243], [-147369600000, 2277.9958455007563], [-144691200000, 2184.594332718428], [-142099200000, 2113.8459705101577], [-139420800000, 1977.3865602227456], [-136742400000, 1846.3686972546493], [-134150400000, 1713.069478467428], [-99936000000, 1577.240032627045], [-97344000000, 1654.7798635496722], [-94665600000, 1789.026485474115], [-91987200000, 1897.1660136236221], [-89568000000, 2000], [-86889600000, 2100], [-84297600000, 2205.250473305693], [-81619200000, 2310.2516540943643], [-79027200000, 2400.779247457748], [-76348800000, 2270], [-73670400000, 2150], [-71078400000, 1943.9086748723203], [-36777600000, 1428.1851001258094], [-34185600000, 1424.723189230479], [-31507200000, 1483.6965911501086], [-28828800000, 1620.2184605097423], [-26409600000, 1738.011104915257], [-23731200000, 1892.7236716813857], [-21139200000, 2214.370561932174], [-18460800000, 2420], [-15868800000, 2340.1647498014195], [-13190400000, 2269.9999999999995], [-10512000000, 2150], [-7920000000, 1974.9999999999998], [-5241600000, 1850], [-2649600000, 1850], [28800000, 1850], [2707200000, 1900], [5126400000, 2000], [7804800000, 2100], [10396800000, 2161.386748718372], [13075200000, 2060.638672231599], [15667200000, 1988.8234838992385], [18345600000, 1844.4047121675212], [21024000000, 1668.3971073240116], [23616000000, 1445.0196593072849], [26294400000, 1303.0055204902571], [28886400000, 1398.4895854025885], [31564800000, 1524.2939942336861], [34243200000, 1747.0609887331439], [36662400000, 1877.0412493849003], [39340800000, 2064.958950913993], [41932800000, 2233.422039816418], [44611200000, 2268.0680597720575], [47203200000, 2276.7753785950713], [49881600000, 2253.262391886064], [52560000000, 2126.8194998997124], [55152000000, 1975.0000000000005], [120988800000, 1719.885858347993], [123580800000, 1850], [126259200000, 1850], [128937600000, 1900], [131356800000, 2000], [134035200000, 2100], [136627200000, 2300], [139305600000, 2383.8642406856357], [141897600000, 2367.4432339418972], [144576000000, 2270], [147254400000, 2150], [149846400000, 1974.9999999999995], [152524800000, 1849.9999999999998], [155116800000, 1843.459845552196], [157795200000, 1847.274837747011], [160473600000, 1859.3486192415046], [162892800000, 1947.6387823684584], [165571200000, 2100], [168163200000, 2224.726727611186], [170841600000, 2320.5631986624617], [173433600000, 2431.459625841006], [176112000000, 2270.0000000000005], [178790400000, 2149.9999999999995], [181382400000, 1966.418145931095], [373449600000, 1248.1232105872862], [376041600000, 1491.0533875139604], [378720000000, 1814.2712703881114], [381398400000, 1900], [383817600000, 2000], [386496000000, 2100], [389088000000, 2300], [391766400000, 2317.827169596444], [394358400000, 2187.3578334552903], [397036800000, 2163.2798276013855], [399715200000, 2138.8872982703283], [402307200000, 1975], [404985600000, 1849.9999999999998], [407577600000, 1850], [410256000000, 1850], [412934400000, 1900], [415353600000, 2000], [418032000000, 2100], [420624000000, 2300], [423302400000, 2420], [425894400000, 2447], [428572800000, 2270], [431251200000, 2150], [433843200000, 1975], [436521600000, 1850], [439113600000, 1850], [441792000000, 1850], [444470400000, 1900], [446976000000, 1996.7366353712937], [449654400000, 2100], [452246400000, 2217.6119610357614], [454924800000, 2183.315329659132], [457516800000, 2124.2652814817266], [460195200000, 1988.7868550547462], [462873600000, 1846.1780276564523], [465465600000, 1677.0721804104767], [499680000000, 1245.2718507425068], [502272000000, 1214.5211410504749], [504950400000, 1221.005228134549], [507628800000, 1340.9695136574776], [510048000000, 1814.4671371464387], [512726400000, 2100], [515318400000, 2122.058293127561], [517996800000, 2018.8355583014431], [520588800000, 1907.6219344278315], [523267200000, 1731.8326611765053], [525945600000, 1556.9252356790867], [528537600000, 1390.8888005568404], [783676800000, 971.8444437642389], [786268800000, 958.0537940241618], [788947200000, 944.0670895317306], [791625600000, 1344.794174264436], [794044800000, 1598.4891153592107], [796723200000, 2031.8177661671352], [799315200000, 2269.8850203844954], [801993600000, 2345.197510560271], [804585600000, 2330.394740403572], [807264000000, 2270], [809942400000, 2150], [812534400000, 1975], [815212800000, 1850], [817804800000, 1839.8861054774172], [820483200000, 1849.9999999999998], [823161600000, 1900], [825667200000, 2000], [828345600000, 2100], [830937600000, 2277.87476862332], [833616000000, 2233.7378084315374], [836208000000, 2169.6892748445516], [838886400000, 2032.2495186719775], [841564800000, 1895.4777605132456], [844156800000, 1756.4788425073161], [846835200000, 1693.3476343296702], [849427200000, 1699.1659305392523], [852105600000, 1850], [854784000000, 1900], [857203200000, 1990.4408221828824], [859881600000, 2076.459270329076], [862473600000, 2176.3063405029316], [865152000000, 1989.6788739925485], [867744000000, 1820.5544399836356], [870422400000, 1670.2942493085625], [873100800000, 1493.5938735098152], [875692800000, 1367.074740071836], [878371200000, 1307.5873421319006], [880963200000, 1282.0079220310236], [883641600000, 1313.0341381922276], [886320000000, 1623.729791470067], [888739200000, 1946.2769892306965], [891417600000, 2100], [894009600000, 2295.5724488973265], [896688000000, 2420], [899280000000, 2447], [901958400000, 2270], [904636800000, 2150], [907228800000, 1975], [909907200000, 1850], [912499200000, 1850], [915177600000, 1850], [917856000000, 1900], [920275200000, 2000], [922953600000, 2100], [925545600000, 2299.8805487206073], [928224000000, 2339.7516610243606], [930816000000, 2362.622819254627], [933494400000, 2270.0000000000005], [936172800000, 2135.8543367653], [938764800000, 1975]]
                    }]
                }, {
                    "annual_filters": [{
                        "period_months": ["Dec", "Jan", "Feb"],
                        "annual_period": "Long Term<br>",
                        "computed_statistics": [{
                            "statistic": "Averages",
                            "statistically_computed_time_series_monthly": [["Jan", 1474.3252152039393], ["Feb", 1585.0731594882384], ["Dec", 1415.5527550236732]],
                            "statistic_aggregate": 1494.5327408216306
                        }, {
                            "statistic": "Minimums",
                            "statistically_computed_time_series_monthly": [["Jan", 485.16441333514405], ["Feb", 486.5457894740959], ["Dec", 458.20667482838076]],
                            "statistic_aggregate": 486.6441750873419
                        }],
                        "aggregate_ts": [[1922, 1760.1087037104814], [1923, 1645.2695089251358], [1924, 1184.1742132588058], [1925, 824.9919358365008], [1926, 1050.3753865881179], [1927, 1485.39240208761], [1928, 1726.7682484583147], [1929, 1144.8768619336552], [1930, 1013.6885408533617], [1931, 825.7234355623301], [1932, 528.4158685456399], [1933, 486.6441750873419], [1934, 650.8279077269008], [1935, 601.4543077276534], [1936, 788.9781197432147], [1937, 816.2350240424888], [1938, 1557.9918933351316], [1939, 1862.1448801097397], [1940, 1350.0962315129573], [1941, 1748.1129879616446], [1942, 1916.6666666666667], [1943, 1916.6666666666667], [1944, 1416.538610373885], [1945, 1270.1659204215098], [1946, 1475.6840354601493], [1947, 1392.8136244960745], [1948, 1329.9436522860633], [1949, 1453.6804542171565], [1950, 1343.1493620274086], [1951, 1843.986260812758], [1952, 1769.0834801520969], [1953, 1916.6666666666667], [1954, 1916.6666666666667], [1955, 1719.4950671529223], [1956, 1837.398443909433], [1957, 1860.817531758652], [1958, 1986.0580937796924], [1959, 1911.816832095299], [1960, 1417.0473333023356], [1961, 1401.8155177591204], [1962, 1400.5536479030925], [1963, 1782.730360044381], [1964, 1838.9376555649305], [1965, 1899.2404824299958], [1966, 1797.8119700649295], [1967, 1895.3974996992456], [1968, 1916.6666666666667], [1969, 1613.9753855250358], [1970, 1916.6666666666667], [1971, 1716.1320774505766], [1972, 1916.6666666666667], [1973, 1828.0280835814676], [1974, 1916.6666666666667], [1975, 1884.754079785658], [1976, 1738.9367478597458], [1977, 1009.2445904056068], [1978, 774.2871728320398], [1979, 1253.4573203463626], [1980, 1597.6971635579246], [1981, 1601.5635099930662], [1982, 1904.7570901293705], [1983, 1916.6666666666667], [1984, 1915.5788784570977], [1985, 1778.8836948001456], [1986, 1458.8139596461551], [1987, 1337.6990823987546], [1988, 1137.836488627275], [1989, 1011.5995663360186], [1990, 1066.698379501772], [1991, 845.2831655049107], [1992, 710.7675528929968], [1993, 918.8411259177474], [1994, 1519.460683283144], [1995, 1295.7834597184592], [1996, 1916.6666666666667], [1997, 1913.480274060961], [1998, 1627.680306297664], [1999, 1916.6666666666667], [2000, 1916.6666666666667], [2001, 1694.4938028082142], [2002, 1571.5103918916395], [2003, 1728.0042450457322]],
                        "month_period": "Dec - Feb",
                        "discrete_ts": [[-1522684800000, 2000], [-1520006400000, 1781.2779198935498], [-1517414400000, 1753.680411854264], [-1514736000000, 1755.0900871780752], [-1512057600000, 1751.377203735407], [-1509638400000, 1773.8588202179617], [-1506960000000, 1823.3779876894896], [-1504368000000, 1941.4609885062098], [-1501689600000, 2020.0440770669163], [-1499097600000, 2066.1119064423087], [-1496419200000, 1954.1479165086519], [-1493740800000, 1805.20424764003], [-1491148800000, 1661.277390821918], [-1488470400000, 1606.7084586644037], [-1485878400000, 1608.3827092073284], [-1483200000000, 1620.0122934537003], [-1480521600000, 1646.5654573709548], [-1478102400000, 1669.2307759507528], [-1475424000000, 1721.6334181430689], [-1472832000000, 1842.9746585412008], [-1470153600000, 1801.2034335562175], [-1467561600000, 1762.3330162481177], [-1464883200000, 1623.2067271978271], [-1462204800000, 1445.9817145628215], [-1459612800000, 1318.96194000093], [-1456934400000, 1267.0138374022983], [-1454342400000, 1235.1946301533685], [-1451664000000, 1222.6261477735661], [-1448985600000, 1135.2195725770448], [-1446480000000, 1194.6769194258063], [-1443801600000, 1187.060279040908], [-1441209600000, 1157.8162960264385], [-1438531200000, 1076.0792024410255], [-1435939200000, 1017.9796488539384], [-1433260800000, 916.5635495760231], [-1430582400000, 790.5815354927996], [-1427990400000, 692.9293326790333], [-1425312000000, 685.2761801757684], [-1422720000000, 761.9075870732373], [-1420041600000, 804.3431438866807], [-1417363200000, 700], [-1414944000000, 970.6326636228218], [-1412265600000, 1108.782885133069], [-1409673600000, 1408.496403859791], [-1406995200000, 1393.7063429446248], [-1404403200000, 1319.754896517185], [-1401724800000, 1209.0804626876377], [-1399046400000, 1068.0139759478946], [-1396454400000, 998.0079227298367], [-1393776000000, 979.7806425462186], [-1391184000000, 977.2052246832545], [-1388505600000, 998.2554848427302], [-1385827200000, 1002.9585063664682], [-1383408000000, 1149.9121685551552], [-1380729600000, 1264.1327508858071], [-1378137600000, 1485.5855061716436], [-1375459200000, 1382.6223685162938], [-1372867200000, 1319.0783658457287], [-1370188800000, 1199.2924878243061], [-1367510400000, 1084.8654665711636], [-1364918400000, 988.8276938986643], [-1362240000000, 964.9787809816952], [-1359648000000, 1121.6021900792975], [-1356969600000, 1323.1001109090655], [-1354291200000, 1447.446177983382], [-1351872000000, 1685.6309173703821], [-1349193600000, 1869.4534498532887], [-1346601600000, 2096.321160602065], [-1343923200000, 2085.710096182951], [-1341331200000, 2111.1036615612643], [-1338652800000, 1988.8826946346671], [-1335974400000, 1849.169613408825], [-1333382400000, 1711.870847766417], [-1330704000000, 1620.9488735970988], [-1328112000000, 1649.0205654890515], [-1325433600000, 1657.9679776295247], [-1322755200000, 1706.4191457763086], [-1320249600000, 1815.917621969111], [-1317571200000, 2022.385144377964], [-1314979200000, 2183.8152459078124], [-1312300800000, 2121.795429262445], [-1309708800000, 2029.2659918396362], [-1307030400000, 1885.2556516944194], [-1304352000000, 1691.104861538447], [-1301760000000, 1486.3560762676661], [-1299081600000, 1347.018187910708], [-1296489600000, 1270.1479156150513], [-1293811200000, 1263.456754879924], [-1291132800000, 1072.0025868860798], [-1288713600000, 1099.1712440349615], [-1286035200000, 1150.1912028646989], [-1283443200000, 1165.7450841953864], [-1280764800000, 1209.2071710658984], [-1278172800000, 1195.1356706196752], [-1275494400000, 1105.0005296858221], [-1272816000000, 980.9808317273778], [-1270224000000, 884.2599880058391], [-1267545600000, 854.4102245802743], [-1264953600000, 838.7406010501786], [-1262275200000, 968.3420885869782], [-1259596800000, 986.2128948914306], [-1257177600000, 1086.5106390816763], [-1254499200000, 1215.3761320622414], [-1251907200000, 1331.698371124693], [-1249228800000, 1235.4512826512607], [-1246636800000, 1213.3510818869818], [-1243958400000, 1070.7385258115853], [-1241280000000, 958.0948060077742], [-1238688000000, 863.8067764049755], [-1236009600000, 834.3150241157856], [-1233417600000, 821.5825651171108], [-1230739200000, 811.6296217669077], [-1228060800000, 822.4693335682726], [-1225641600000, 843.0713513518101], [-1222963200000, 902.5752826333479], [-1220371200000, 925.6007539571392], [-1217692800000, 886.083809240228], [-1215100800000, 862.7207138482978], [-1212422400000, 768.0027074469205], [-1209744000000, 643.3971618089848], [-1207152000000, 576.9922913028873], [-1204473600000, 561.1368858753079], [-1201881600000, 553.5291323720433], [-1199203200000, 556.446135062365], [-1196524800000, 503.4197530938489], [-1194019200000, 525.3817174807058], [-1191340800000, 677.9895482180863], [-1188748800000, 758.2565661698034], [-1186070400000, 781.278235744211], [-1183478400000, 771.7015736600978], [-1180800000000, 686.7130937520867], [-1178121600000, 591.8687089247851], [-1175529600000, 526.4022122773378], [-1172851200000, 504.0239732873246], [-1170259200000, 492.6049501840281], [-1167580800000, 488.22232245278593], [-1164902400000, 485.16441333514405], [-1162483200000, 486.5457894740959], [-1159804800000, 588.2283667790265], [-1157212800000, 741.5355686853476], [-1154534400000, 728.9815594619384], [-1151942400000, 847.4024207940521], [-1149264000000, 778.7556142642202], [-1146585600000, 659.6638059242582], [-1143993600000, 596.5028309216942], [-1141315200000, 578.7817135778438], [-1138723200000, 570.163877880316], [-1136044800000, 580.0103352500819], [-1133366400000, 641.2198704573157], [-1130947200000, 731.2535174733048], [-1128268800000, 887.9945742527568], [-1125676800000, 971.4650040230005], [-1122998400000, 943.1036539984946], [-1120406400000, 906.9261398879603], [-1117728000000, 812.6176571095303], [-1115049600000, 689.2333070997395], [-1112457600000, 500], [-1109779200000, 488.8700303826992], [-1107187200000, 554.0659769305328], [-1104508800000, 590.6755458680916], [-1101830400000, 565.1126227321208], [-1099411200000, 648.5747545827475], [-1096732800000, 709.6597220064965], [-1094140800000, 923.2158775449302], [-1091462400000, 955.4905342020431], [-1088870400000, 962.1820912873126], [-1086192000000, 887.687194321757], [-1083513600000, 777.0898398982085], [-1080921600000, 683.4226942866693], [-1078243200000, 669.1467925256757], [-1075651200000, 663.8146730478278], [-1072972800000, 666.8639954825488], [-1070294400000, 779.9227354795129], [-1067788800000, 920.1476282675824], [-1065110400000, 1036.555570178952], [-1062518400000, 1211.9646271733388], [-1059840000000, 1141.7687112772585], [-1057248000000, 1102.778830274539], [-1054569600000, 1053.1622881081223], [-1051891200000, 956.9538774108589], [-1049299200000, 875.6307678708306], [-1046620800000, 849.8611902625414], [-1044028800000, 836.035382382843], [-1041350400000, 823.3444557543882], [-1038672000000, 812.0989509632457], [-1036252800000, 813.2616654098324], [-1033574400000, 915.0790637144631], [-1030982400000, 1137.1654948369567], [-1028304000000, 1282.709583742305], [-1025712000000, 1357.2966692354116], [-1023033600000, 1290.8439754889594], [-1020355200000, 1171.910459615378], [-1017763200000, 1090.2079363714683], [-1015084800000, 1073.7497633338526], [-1012492800000, 1221.3169954364075], [-1009814400000, 1440.6723201790041], [-1007136000000, 1538.200278940654], [-1004716800000, 1695.103080885736], [-1002038400000, 1956.3388616496065], [-999446400000, 2263.772611756414], [-996768000000, 2420], [-994176000000, 2364.7458994444296], [-991497600000, 2270], [-988819200000, 2149.9999999999995], [-986227200000, 1945.2492707362485], [-983548800000, 1848.8299314899282], [-980956800000, 1850], [-978278400000, 1849.9999999999998], [-975600000000, 1861.157167116083], [-973180800000, 1875.2774732131365], [-970502400000, 1983.2145393876838], [-967910400000, 2058.507240661381], [-965232000000, 1976.7469507437113], [-962640000000, 1875.191053610787], [-959961600000, 1643.0521821846405], [-957283200000, 1398.9822938910966], [-954691200000, 1189.9053450608549], [-952012800000, 1121.7115635173332], [-949420800000, 1059.704288808632], [-946742400000, 1123.9303851257278], [-944064000000, 1300.2703225893542], [-941558400000, 1626.08798682379], [-938880000000, 1928.157980581903], [-936288000000, 2154.2774054419424], [-933609600000, 2091.0963589041967], [-931017600000, 1937.9463014532917], [-928339200000, 1788.8641455047239], [-925660800000, 1612.6248084718632], [-923068800000, 1447.4648548620523], [-920390400000, 1394.5661970834474], [-917798400000, 1378.1361537775454], [-915120000000, 1517.431024839591], [-912441600000, 1738.1164333134277], [-910022400000, 1988.7915057319153], [-907344000000, 2100], [-904752000000, 2300], [-902073600000, 2420], [-899481600000, 2447], [-896803200000, 2270], [-894124800000, 2150], [-891532800000, 1975], [-888854400000, 1850], [-886262400000, 1836.877229677874], [-883584000000, 1850], [-880905600000, 1900], [-878486400000, 2000], [-875808000000, 2078.540134242459], [-873216000000, 2285.258121595209], [-870537600000, 2344.795308522242], [-867945600000, 2416.522614065537], [-865267200000, 2270], [-862588800000, 2150], [-859996800000, 1928.1033953999029], [-857318400000, 1850], [-854726400000, 1845.1412431106548], [-852048000000, 1850], [-849369600000, 1900], [-846950400000, 2000.0000000000002], [-844272000000, 2100], [-841680000000, 2283.155661244806], [-839001600000, 2173.318610935777], [-836409600000, 2100.1013478625755], [-833731200000, 1955.632799582456], [-831052800000, 1810.9238053951467], [-828460800000, 1640.4268782084089], [-825782400000, 1552.3242620568308], [-823190400000, 1448.5865628213835], [-820512000000, 1438.2768847232037], [-817833600000, 1387.0731245212833], [-815328000000, 1424.2658218771678], [-812649600000, 1486.3263833722374], [-810057600000, 1525.7739348519233], [-807379200000, 1583.183427187927], [-804787200000, 1562.230589876419], [-802108800000, 1396.3866613282485], [-799430400000, 1224.875118011666], [-796838400000, 1099.9222359145306], [-794160000000, 1071.1073778316506], [-791568000000, 1099.666846002828], [-788889600000, 1176.6031889193787], [-786211200000, 1234.320497526698], [-783792000000, 1399.5740748184526], [-781113600000, 1453.4713230037976], [-778521600000, 1591.1807408796776], [-775843200000, 1555.9025031048423], [-773251200000, 1496.7498649794875], [-770572800000, 1376.6789340147495], [-767894400000, 1256.4127719564876], [-765302400000, 1128.6152206163235], [-762624000000, 1124.0893879494224], [-760032000000, 1179.0981527335039], [-757353600000, 1370.3339784167995], [-754675200000, 1500.8470385400851], [-752256000000, 1555.8710894235633], [-749577600000, 1678.1702190001129], [-746985600000, 1896.3436053109099], [-744307200000, 1891.799908971309], [-741715200000, 1825.9335678825084], [-739036800000, 1694.6572879816613], [-736358400000, 1522.9625851191254], [-733766400000, 1396.8938742436665], [-731088000000, 1343.9657909181578], [-728496000000, 1352.4678638762744], [-725817600000, 1373.4023886288246], [-723139200000, 1372.514651996264], [-720720000000, 1432.5238328631347], [-718041600000, 1551.5932078755172], [-715449600000, 1638.9200688653398], [-712771200000, 1548.851160822023], [-710179200000, 1541.2965192318286], [-707500800000, 1431.3302547831106], [-704822400000, 1287.344866921451], [-702230400000, 1162.8407650063748], [-699552000000, 1178.7859439538047], [-696960000000, 1192.683086581423], [-694281600000, 1192.0296948000075], [-691603200000, 1387.661327317311], [-689097600000, 1410.1399347408717], [-686419200000, 1444.5926420625644], [-683827200000, 1631.6982015393976], [-681148800000, 1625.891907669828], [-678556800000, 1705.339993581719], [-675878400000, 1669.4634155222304], [-673200000000, 1531.7520633520166], [-670608000000, 1484.6947220920547], [-667929600000, 1438.5100015646508], [-665337600000, 1445.3973078163935], [-662659200000, 1447.4427361025694], [-659980800000, 1440.8585586274244], [-657561600000, 1472.7400679214754], [-654883200000, 1688.9250292776117], [-652291200000, 1948.6313991055752], [-649612800000, 1949.537740205285], [-647020800000, 1898.8139697344589], [-644342400000, 1753.671335315007], [-641664000000, 1579.6513284515659], [-639072000000, 1410.6631910418785], [-636393600000, 1356.108282901795], [-633801600000, 1326.1007653581487], [-631123200000, 1310.2763372670374], [-628444800000, 1331.4812475852184], [-626025600000, 1387.6905012299703], [-623347200000, 1508.7524489060706], [-620755200000, 1666.7614556732237], [-618076800000, 1676.1364954634478], [-615484800000, 1657.6194573412329], [-612806400000, 1550.3161362239355], [-610128000000, 1404.5420545719082], [-607536000000, 1278.237948965723], [-604857600000, 1347.690986910112], [-602265600000, 1463.371054790445], [-599587200000, 1720.9913467665885], [-596908800000, 1810.967435671686], [-594489600000, 2000], [-591811200000, 2100], [-589219200000, 2285.6210802982428], [-586540800000, 2217.5091071736506], [-583948800000, 2133.2400239159238], [-581270400000, 1985.6232395973063], [-578592000000, 1764.3155817750212], [-576000000000, 1594.8513736444622], [-573321600000, 1510.5784833488794], [-570729600000, 1518.7352008707487], [-568051200000, 1666.1410492782995], [-565372800000, 1725.545605918398], [-562867200000, 1915.563785259593], [-560188800000, 2081.523545392229], [-557596800000, 2300], [-554918400000, 2410.9636868144175], [-552326400000, 2447], [-549648000000, 2269.9999999999995], [-546969600000, 2150], [-544377600000, 1974.9999999999998], [-541699200000, 1850], [-539107200000, 1845.3146506149888], [-536428800000, 1849.9999999999998], [-533750400000, 1900], [-531331200000, 2000], [-528652800000, 2100], [-526060800000, 2300], [-523382400000, 2259.9112123923715], [-520790400000, 2353.448239660465], [-518112000000, 2269.9999999999995], [-515433600000, 2149.9999999999995], [-512841600000, 1959.0443722661785], [-510163200000, 1850], [-507571200000, 1850], [-504892800000, 1850], [-502214400000, 1900], [-499795200000, 2000], [-497116800000, 2100], [-494524800000, 2300], [-491846400000, 2271.5030161556515], [-489254400000, 2203.9284707403995], [-486576000000, 2067.0659267830138], [-483897600000, 1854.7149140411373], [-481305600000, 1692.8638168713126], [-478627200000, 1635.7175848071697], [-476035200000, 1656.6056937569667], [-473356800000, 1697.683829744439], [-470678400000, 1715.9752718325833], [-468259200000, 1744.826099881744], [-465580800000, 1774.9390652282545], [-462988800000, 1821.0444285042188], [-460310400000, 1834.9302833710838], [-457718400000, 1820.2044588612607], [-455040000000, 1680.757344071697], [-452361600000, 1503.9436109588632], [-449769600000, 1376.7919752917066], [-447091200000, 1317.3012126888084], [-444499200000, 1304.0857010842735], [-441820800000, 1612.1953317282992], [-439142400000, 1900], [-436636800000, 2000], [-433958400000, 2100], [-431366400000, 2300], [-428688000000, 2402.4665039813262], [-426096000000, 2321.222056407487], [-423417600000, 2270], [-420739200000, 2149.9999999999995], [-418147200000, 1953.0690195881941], [-415468800000, 1850], [-412876800000, 1825.7510045818149], [-410198400000, 1818.3635572968415], [-407520000000, 1808.4820371702049], [-405100800000, 1955.607000808909], [-402422400000, 2100], [-399830400000, 2218.3702490417722], [-397152000000, 2234.431188934835], [-394560000000, 2211.4354718735713], [-391881600000, 2067.2667816062085], [-389203200000, 1923.221168855265], [-386611200000, 1759.1747733089287], [-383932800000, 1850], [-381340800000, 1850], [-378662400000, 1850], [-375984000000, 1900], [-373564800000, 2208.1742813390774], [-370886400000, 2100], [-368294400000, 2300], [-365616000000, 2420], [-363024000000, 2430.2546446549427], [-360345600000, 2270.0000000000005], [-357667200000, 2150], [-355075200000, 1965.9581241632252], [-352396800000, 1850.0000000000002], [-349804800000, 1850], [-347126400000, 1835.4504962858969], [-344448000000, 1900], [-342028800000, 2000.0000000000002], [-339350400000, 2100], [-336758400000, 2271.8013674182434], [-334080000000, 2170.7253036678144], [-331488000000, 2053.05568815621], [-328809600000, 1825.442029625731], [-326131200000, 1648.7457096809633], [-323539200000, 1483.866573583779], [-320860800000, 1424.9175868923633], [-318268800000, 1388.9565311053684], [-315590400000, 1370.3883965322848], [-312912000000, 1372.120802441327], [-310406400000, 1508.6328009333947], [-307728000000, 1702.777428521017], [-305136000000, 1823.9990909947398], [-302457600000, 1768.9249936774424], [-299865600000, 1754.7704226996789], [-297187200000, 1615.6465028144446], [-294508800000, 1441.7018216411525], [-291916800000, 1314.3238212976692], [-289238400000, 1256.6110506705359], [-286646400000, 1243.3145046314148], [-283968000000, 1308.6206262988114], [-281289600000, 1352.764380220775], [-278870400000, 1544.061546757775], [-276192000000, 1641.6830616616924], [-273600000000, 1831.7397449361822], [-270921600000, 1859.784907233123], [-268329600000, 1894.6163744972243], [-265651200000, 1748.0525945241816], [-262972800000, 1575.1250467433786], [-260380800000, 1408.5745689811085], [-257702400000, 1355.862979417707], [-255110400000, 1333.753113422714], [-252432000000, 1356.5375243739634], [-249753600000, 1373.6396455548213], [-247334400000, 1471.4837737804926], [-244656000000, 1529.9831111395413], [-242064000000, 1855.7639582984036], [-239385600000, 1821.8080437510769], [-236793600000, 1788.808781404829], [-234115200000, 1640.5092476323107], [-231436800000, 1470.9699938918789], [-228844800000, 1345.8626290630316], [-226166400000, 1447.5487102352663], [-223574400000, 1496.1451832146618], [-220896000000, 1662.018447502056], [-218217600000, 1702.9904229633066], [-215798400000, 1983.1822096677809], [-213120000000, 2057.456543050294], [-210528000000, 2300], [-207849600000, 2323.768972513454], [-205257600000, 2266.0778772760577], [-202579200000, 2213.4910103292814], [-199900800000, 2066.4111905369864], [-197308800000, 1835.9767982901947], [-194630400000, 1742.1834356097497], [-192038400000, 1811.9187658383967], [-189360000000, 1837.5458295932533], [-186681600000, 1839.1217064077375], [-184176000000, 1840.1454306938003], [-181497600000, 1869.4939975004604], [-178905600000, 1904.2169280101946], [-176227200000, 1796.5306004449951], [-173635200000, 1766.586760753536], [-170956800000, 1619.4764145176568], [-168278400000, 1450.5154031207796], [-165686400000, 1333.0177840450158], [-163008000000, 1281.7357506188282], [-160416000000, 1291.265053594294], [-157737600000, 1810.0850128649877], [-155059200000, 1900], [-152640000000, 1987.636434425], [-149961600000, 2054.0129112455243], [-147369600000, 2277.9958455007563], [-144691200000, 2184.594332718428], [-142099200000, 2113.8459705101577], [-139420800000, 1977.3865602227456], [-136742400000, 1846.3686972546493], [-134150400000, 1713.069478467428], [-131472000000, 1652.5048995124087], [-128880000000, 1706.6958409779681], [-126201600000, 1728.5569082233242], [-123523200000, 1799.7060583032214], [-121104000000, 1865.1729436682433], [-118425600000, 2063.4076812733947], [-115833600000, 2300], [-113155200000, 2300.6042666254175], [-110563200000, 2225.687726289895], [-107884800000, 2041.8936341069427], [-105206400000, 1828.2564510105747], [-102614400000, 1666.225586809633], [-99936000000, 1577.240032627045], [-97344000000, 1654.7798635496722], [-94665600000, 1789.026485474115], [-91987200000, 1897.1660136236221], [-89568000000, 2000], [-86889600000, 2100], [-84297600000, 2205.250473305693], [-81619200000, 2310.2516540943643], [-79027200000, 2400.779247457748], [-76348800000, 2270], [-73670400000, 2150], [-71078400000, 1943.9086748723203], [-68400000000, 1845.8486402061653], [-65808000000, 1850.0000000000002], [-63129600000, 1850.0000000000002], [-60451200000, 1900.0000000000002], [-57945600000, 2000], [-55267200000, 2100], [-52675200000, 2191.009019304407], [-49996800000, 2077.487701506115], [-47404800000, 1971.0970102032252], [-44726400000, 1820.5758791420346], [-42048000000, 1647.9158328324097], [-39456000000, 1481.7815168854245], [-36777600000, 1428.1851001258094], [-34185600000, 1424.723189230479], [-31507200000, 1483.6965911501086], [-28828800000, 1620.2184605097423], [-26409600000, 1738.011104915257], [-23731200000, 1892.7236716813857], [-21139200000, 2214.370561932174], [-18460800000, 2420], [-15868800000, 2340.1647498014195], [-13190400000, 2269.9999999999995], [-10512000000, 2150], [-7920000000, 1974.9999999999998], [-5241600000, 1850], [-2649600000, 1850], [28800000, 1850], [2707200000, 1900], [5126400000, 2000], [7804800000, 2100], [10396800000, 2161.386748718372], [13075200000, 2060.638672231599], [15667200000, 1988.8234838992385], [18345600000, 1844.4047121675212], [21024000000, 1668.3971073240116], [23616000000, 1445.0196593072849], [26294400000, 1303.0055204902571], [28886400000, 1398.4895854025885], [31564800000, 1524.2939942336861], [34243200000, 1747.0609887331439], [36662400000, 1877.0412493849003], [39340800000, 2064.958950913993], [41932800000, 2233.422039816418], [44611200000, 2268.0680597720575], [47203200000, 2276.7753785950713], [49881600000, 2253.262391886064], [52560000000, 2126.8194998997124], [55152000000, 1975.0000000000005], [57830400000, 1850], [60422400000, 1850], [63100800000, 1850], [65779200000, 1900], [68284800000, 2000.0000000000002], [70963200000, 2100], [73555200000, 2233.747818808155], [76233600000, 2163.0551072842095], [78825600000, 2112.663296194655], [81504000000, 1964.164991715206], [84182400000, 1790.046312488748], [86774400000, 1621.778143966301], [89452800000, 1537.4034518890714], [92044800000, 1573.3059833706957], [94723200000, 1677.274620373853], [97401600000, 1830.114784780456], [99820800000, 1976.6948455900938], [102499200000, 2096.68274764028], [105091200000, 2284.391770253808], [107769600000, 2333.60894084383], [110361600000, 2280.0587384121886], [113040000000, 2140.228440088989], [115718400000, 1917.0728687605979], [118310400000, 1748.980913698329], [120988800000, 1719.885858347993], [123580800000, 1850], [126259200000, 1850], [128937600000, 1900], [131356800000, 2000], [134035200000, 2100], [136627200000, 2300], [139305600000, 2383.8642406856357], [141897600000, 2367.4432339418972], [144576000000, 2270], [147254400000, 2150], [149846400000, 1974.9999999999995], [152524800000, 1849.9999999999998], [155116800000, 1843.459845552196], [157795200000, 1847.274837747011], [160473600000, 1859.3486192415046], [162892800000, 1947.6387823684584], [165571200000, 2100], [168163200000, 2224.726727611186], [170841600000, 2320.5631986624617], [173433600000, 2431.459625841006], [176112000000, 2270.0000000000005], [178790400000, 2149.9999999999995], [181382400000, 1966.418145931095], [184060800000, 1850], [186652800000, 1850.0000000000002], [189331200000, 1850], [192009600000, 1667.8434509818715], [194515200000, 1698.9667925973654], [197193600000, 1747.2694044171842], [199785600000, 1828.6048869287995], [202464000000, 1803.0665008675649], [205056000000, 1752.9027827882471], [207734400000, 1542.1549599550035], [210412800000, 1407.067570733263], [213004800000, 1282.129752065428], [215683200000, 1227.7180964531656], [218275200000, 1191.648639568498], [220953600000, 1036.4125320692906], [223632000000, 1000], [226051200000, 991.32123914753], [228729600000, 977.4567240778305], [231321600000, 946.8749997896836], [234000000000, 894.4103622938579], [236592000000, 856.9124157878312], [239270400000, 629.6900811859233], [241948800000, 385.1832249108495], [244540800000, 331.8611964199329], [247219200000, 240.00000000000003], [249811200000, 267.29062040796197], [252489600000, 458.20667482838076], [255168000000, 843.2943173232236], [257587200000, 1021.360526344515], [260265600000, 1294.6939352668446], [262857600000, 1486.2069198797117], [265536000000, 1490.6420420832794], [268128000000, 1435.685614615596], [270806400000, 1373.1089864911023], [273484800000, 1287.2509813134582], [276076800000, 1290.8518368198884], [278755200000, 1237.754570746323], [281347200000, 1231.3824264810678], [284025600000, 1220.9978054857245], [286704000000, 1242.6137486926143], [289123200000, 1296.7604068607495], [291801600000, 1439.6899110370807], [294393600000, 1554.7824593239939], [297072000000, 1631.1366288142578], [299664000000, 1633.6309871051878], [302342400000, 1524.9498710621351], [305020800000, 1380.2635636984028], [307612800000, 1254.2311920917111], [310291200000, 1226.5043831719684], [312883200000, 1288.9961206661428], [315561600000, 1355.6918101024762], [318240000000, 1574.4661446038804], [320745600000, 1862.9335359674171], [323424000000, 1994.5811623113902], [326016000000, 2153.983980807646], [328694400000, 2058.8492254964376], [331286400000, 1923.5257193362174], [333964800000, 1800.5952640148594], [336643200000, 1656.2805709859026], [339235200000, 1518.036525945945], [341913600000, 1462.8541000647358], [344505600000, 1453.3129395723236], [347184000000, 1485.0038318576721], [349862400000, 1590.8697201975795], [352281600000, 1728.8169779239474], [354960000000, 1852.96842412533], [357552000000, 1961.2447638817073], [360230400000, 1893.203740423532], [362822400000, 1837.2720074979213], [365500800000, 1609.4640804263177], [368179200000, 1430.2654521387296], [370771200000, 1307.2576162869836], [373449600000, 1248.1232105872862], [376041600000, 1491.0533875139604], [378720000000, 1814.2712703881114], [381398400000, 1900], [383817600000, 2000], [386496000000, 2100], [389088000000, 2300], [391766400000, 2317.827169596444], [394358400000, 2187.3578334552903], [397036800000, 2163.2798276013855], [399715200000, 2138.8872982703283], [402307200000, 1975], [404985600000, 1849.9999999999998], [407577600000, 1850], [410256000000, 1850], [412934400000, 1900], [415353600000, 2000], [418032000000, 2100], [420624000000, 2300], [423302400000, 2420], [425894400000, 2447], [428572800000, 2270], [431251200000, 2150], [433843200000, 1975], [436521600000, 1850], [439113600000, 1850], [441792000000, 1850], [444470400000, 1900], [446976000000, 1996.7366353712937], [449654400000, 2100], [452246400000, 2217.6119610357614], [454924800000, 2183.315329659132], [457516800000, 2124.2652814817266], [460195200000, 1988.7868550547462], [462873600000, 1846.1780276564523], [465465600000, 1677.0721804104767], [468144000000, 1588.124427465747], [470736000000, 1717.757260599398], [473414400000, 1774.847812265657], [476092800000, 1761.4456394999847], [478512000000, 1800.3576326347957], [481190400000, 1848.4153381602764], [483782400000, 1994.9938914051818], [486460800000, 1917.90037326457], [489052800000, 1872.1526139675534], [491731200000, 1645.8414869398073], [494409600000, 1427.288598138054], [497001600000, 1304.633778751013], [499680000000, 1245.2718507425068], [502272000000, 1214.5211410504749], [504950400000, 1221.005228134549], [507628800000, 1340.9695136574776], [510048000000, 1814.4671371464387], [512726400000, 2100], [515318400000, 2122.058293127561], [517996800000, 2018.8355583014431], [520588800000, 1907.6219344278315], [523267200000, 1731.8326611765053], [525945600000, 1556.9252356790867], [528537600000, 1390.8888005568404], [531216000000, 1340.2964995637865], [533808000000, 1309.6570108164294], [536486400000, 1303.322490661915], [539164800000, 1315.5223079006028], [541584000000, 1394.252448633746], [544262400000, 1599.3158296695287], [546854400000, 1745.7889317993288], [549532800000, 1683.839881213154], [552124800000, 1488.2722412077662], [554803200000, 1263.1221034641658], [557481600000, 1081.938879035368], [560073600000, 989.778984974594], [562752000000, 917.3611188047898], [565344000000, 903.4503222747335], [568022400000, 1055.5292777975449], [570700800000, 1136.7472038437234], [573206400000, 1221.2329842405572], [575884800000, 1301.7292187648784], [578476800000, 1388.2072025121329], [581155200000, 1333.4523506635128], [583747200000, 1315.76703337384], [586425600000, 1188.7386561345204], [589104000000, 1067.8682112683816], [591696000000, 975.6567928493042], [594374400000, 947.3464410532132], [596966400000, 969.4458620774718], [599644800000, 987.250704691126], [602323200000, 1010.1041624076142], [604742400000, 1037.4438319093156], [607420800000, 1384.6769853904461], [610012800000, 1588.6570865538747], [612691200000, 1479.685233472167], [615283200000, 1371.7111272156092], [617961600000, 1222.8228079133428], [620640000000, 1079.4320035248907], [623232000000, 999.2917874488438], [625910400000, 1015.7849361920014], [628502400000, 1019.050698611336], [631180800000, 1013.9372866643739], [633859200000, 1079.7023703600894], [636278400000, 1106.4554814808525], [638956800000, 1194.4450436551606], [641548800000, 1233.7062886979336], [644227200000, 1201.1891486320276], [646819200000, 1219.9259419889881], [649497600000, 1111.9707532454336], [652176000000, 1002.2717902025047], [654768000000, 910.2523204362136], [657446400000, 882.0119251345476], [660038400000, 869.5842474101307], [662716800000, 853.9712206495789], [665395200000, 838.1943477924593], [667814400000, 843.6839280726939], [670492800000, 918.131198943555], [673084800000, 967.2424004665213], [675763200000, 990.8712570050408], [678355200000, 986.9446485884981], [681033600000, 900.8140230260691], [683712000000, 782.1502720716592], [686304000000, 690.7672670557998], [688982400000, 675.0309049880871], [691574400000, 666.0416949004375], [694252800000, 661.0739819894875], [696931200000, 666.1778450555498], [699436800000, 805.0508316339534], [702115200000, 952.6354378685378], [704707200000, 1186.1335863446727], [707385600000, 1179.2403993991347], [709977600000, 1155.004614088337], [712656000000, 1067.8581974183173], [715334400000, 948.8156818284945], [717926400000, 856.9505894613], [720604800000, 828.7365578147753], [723196800000, 814.8019384956298], [725875200000, 834.3676271394514], [728553600000, 892.3346779642139], [730972800000, 1029.8210726495768], [733651200000, 1407.764584074657], [736243200000, 1603.336985956103], [738921600000, 1772.6877706999164], [741513600000, 1887.1982173537535], [744192000000, 1870.1052310789062], [746870400000, 1737.9863010591268], [749462400000, 1601.9682000311561], [752140800000, 1541.7074048265936], [754732800000, 1531.9687495705525], [757411200000, 1543.7668427512474], [760089600000, 1490.3372537173411], [762508800000, 1524.2779533808432], [765187200000, 1592.2522115775682], [767779200000, 1637.2605111809703], [770457600000, 1641.5914671076464], [773049600000, 1495.7353405097967], [775728000000, 1332.4173856801356], [778406400000, 1160.502404017938], [780998400000, 1000.0000000000002], [783676800000, 971.8444437642389], [786268800000, 958.0537940241618], [788947200000, 944.0670895317306], [791625600000, 1344.794174264436], [794044800000, 1598.4891153592107], [796723200000, 2031.8177661671352], [799315200000, 2269.8850203844954], [801993600000, 2345.197510560271], [804585600000, 2330.394740403572], [807264000000, 2270], [809942400000, 2150], [812534400000, 1975], [815212800000, 1850], [817804800000, 1839.8861054774172], [820483200000, 1849.9999999999998], [823161600000, 1900], [825667200000, 2000], [828345600000, 2100], [830937600000, 2277.87476862332], [833616000000, 2233.7378084315374], [836208000000, 2169.6892748445516], [838886400000, 2032.2495186719775], [841564800000, 1895.4777605132456], [844156800000, 1756.4788425073161], [846835200000, 1693.3476343296702], [849427200000, 1699.1659305392523], [852105600000, 1850], [854784000000, 1900], [857203200000, 1990.4408221828824], [859881600000, 2076.459270329076], [862473600000, 2176.3063405029316], [865152000000, 1989.6788739925485], [867744000000, 1820.5544399836356], [870422400000, 1670.2942493085625], [873100800000, 1493.5938735098152], [875692800000, 1367.074740071836], [878371200000, 1307.5873421319006], [880963200000, 1282.0079220310236], [883641600000, 1313.0341381922276], [886320000000, 1623.729791470067], [888739200000, 1946.2769892306965], [891417600000, 2100], [894009600000, 2295.5724488973265], [896688000000, 2420], [899280000000, 2447], [901958400000, 2270], [904636800000, 2150], [907228800000, 1975], [909907200000, 1850], [912499200000, 1850], [915177600000, 1850], [917856000000, 1900], [920275200000, 2000], [922953600000, 2100], [925545600000, 2299.8805487206073], [928224000000, 2339.7516610243606], [930816000000, 2362.622819254627], [933494400000, 2270.0000000000005], [936172800000, 2135.8543367653], [938764800000, 1975], [941443200000, 1849.9999999999998], [944035200000, 1850], [946713600000, 1850], [949392000000, 1900], [951897600000, 2000], [954576000000, 2100], [957168000000, 2300], [959846400000, 2291.5704881582233], [962438400000, 2306.4403357574006], [965116800000, 2176.0114758811656], [967795200000, 1952.718735207226], [970387200000, 1787.0663336744583], [973065600000, 1727.1691603279062], [975657600000, 1695.1461971740428], [978336000000, 1684.5246575410881], [981014400000, 1684.3206286737839], [983433600000, 1714.6361222097703], [986112000000, 1875.8520507547896], [988704000000, 1975.3077059711397], [991382400000, 1955.7134469388898], [993974400000, 1873.3953477517596], [996652800000, 1660.929603429922], [999331200000, 1460.9076585954022], [1001923200000, 1341.824798702595], [1004601600000, 1269.6145051229864], [1007193600000, 1293.369325868846], [1009872000000, 1381.5605489285103], [1012550400000, 1611.5430264496993], [1014969600000, 1721.427600296709], [1017648000000, 1823.9042246307004], [1020240000000, 2030.9250905960787], [1022918400000, 1939.82265574909], [1025510400000, 1841.3700200174953], [1028188800000, 1691.5919199328766], [1030867200000, 1515.2632249651588], [1033459200000, 1388.7935507207378], [1036137600000, 1333.827877588737], [1038729600000, 1299.119710255751], [1041408000000, 1482.607949820857], [1044086400000, 1794.9706507125288], [1046505600000, 1906.434134603811], [1049184000000, 2061.03190457474], [1051776000000, 2260.869065374906], [1054454400000, 2326.703198307505], [1057046400000, 2401.7522514707675], [1059724800000, 2270.0000000000005], [1062403200000, 2138.277988812066], [1064995200000, 1974.9999999999998]]
                    }, {
                        "period_months": ["Dec", "Jan", "Feb"],
                        "annual_period": "Sacramento River Index<br>Wet",
                        "computed_statistics": [{
                            "statistic": "Averages",
                            "statistically_computed_time_series_monthly": [["Jan", 1771.4234592247665], ["Feb", 1934.9273620255449], ["Dec", 1658.7325172861135]],
                            "statistic_aggregate": 1793.2959418385979
                        }, {
                            "statistic": "Minimums",
                            "statistically_computed_time_series_monthly": [["Jan", 1340.9695136574776], ["Feb", 1598.4891153592107], ["Dec", 944.0670895317306]],
                            "statistic_aggregate": 1295.7834597184592
                        }],
                        "aggregate_ts": [[1927, 1485.39240208761], [1938, 1557.9918933351316], [1941, 1748.1129879616446], [1942, 1916.6666666666667], [1943, 1916.6666666666667], [1952, 1769.0834801520969], [1953, 1916.6666666666667], [1956, 1837.398443909433], [1958, 1986.0580937796924], [1963, 1782.730360044381], [1965, 1899.2404824299958], [1967, 1895.3974996992456], [1969, 1613.9753855250358], [1970, 1916.6666666666667], [1971, 1716.1320774505766], [1974, 1916.6666666666667], [1975, 1884.754079785658], [1982, 1904.7570901293705], [1983, 1916.6666666666667], [1984, 1915.5788784570977], [1986, 1458.8139596461551], [1995, 1295.7834597184592], [1996, 1916.6666666666667], [1997, 1913.480274060961], [1998, 1627.680306297664], [1999, 1916.6666666666667]],
                        "month_period": "Dec - Feb",
                        "discrete_ts": [[-1362240000000, 964.9787809816952], [-1359648000000, 1121.6021900792975], [-1356969600000, 1323.1001109090655], [-1354291200000, 1447.446177983382], [-1351872000000, 1685.6309173703821], [-1349193600000, 1869.4534498532887], [-1346601600000, 2096.321160602065], [-1343923200000, 2085.710096182951], [-1341331200000, 2111.1036615612643], [-1338652800000, 1988.8826946346671], [-1335974400000, 1849.169613408825], [-1333382400000, 1711.870847766417], [-1015084800000, 1073.7497633338526], [-1012492800000, 1221.3169954364075], [-1009814400000, 1440.6723201790041], [-1007136000000, 1538.200278940654], [-1004716800000, 1695.103080885736], [-1002038400000, 1956.3388616496065], [-999446400000, 2263.772611756414], [-996768000000, 2420], [-994176000000, 2364.7458994444296], [-991497600000, 2270], [-988819200000, 2149.9999999999995], [-986227200000, 1945.2492707362485], [-920390400000, 1394.5661970834474], [-917798400000, 1378.1361537775454], [-915120000000, 1517.431024839591], [-912441600000, 1738.1164333134277], [-910022400000, 1988.7915057319153], [-907344000000, 2100], [-904752000000, 2300], [-902073600000, 2420], [-899481600000, 2447], [-896803200000, 2270], [-894124800000, 2150], [-891532800000, 1975], [-888854400000, 1850], [-886262400000, 1836.877229677874], [-883584000000, 1850], [-880905600000, 1900], [-878486400000, 2000], [-875808000000, 2078.540134242459], [-873216000000, 2285.258121595209], [-870537600000, 2344.795308522242], [-867945600000, 2416.522614065537], [-865267200000, 2270], [-862588800000, 2150], [-859996800000, 1928.1033953999029], [-857318400000, 1850], [-854726400000, 1845.1412431106548], [-852048000000, 1850], [-849369600000, 1900], [-846950400000, 2000.0000000000002], [-844272000000, 2100], [-841680000000, 2283.155661244806], [-839001600000, 2173.318610935777], [-836409600000, 2100.1013478625755], [-833731200000, 1955.632799582456], [-831052800000, 1810.9238053951467], [-828460800000, 1640.4268782084089], [-573321600000, 1510.5784833488794], [-570729600000, 1518.7352008707487], [-568051200000, 1666.1410492782995], [-565372800000, 1725.545605918398], [-562867200000, 1915.563785259593], [-560188800000, 2081.523545392229], [-557596800000, 2300], [-554918400000, 2410.9636868144175], [-552326400000, 2447], [-549648000000, 2269.9999999999995], [-546969600000, 2150], [-544377600000, 1974.9999999999998], [-541699200000, 1850], [-539107200000, 1845.3146506149888], [-536428800000, 1849.9999999999998], [-533750400000, 1900], [-531331200000, 2000], [-528652800000, 2100], [-526060800000, 2300], [-523382400000, 2259.9112123923715], [-520790400000, 2353.448239660465], [-518112000000, 2269.9999999999995], [-515433600000, 2149.9999999999995], [-512841600000, 1959.0443722661785], [-447091200000, 1317.3012126888084], [-444499200000, 1304.0857010842735], [-441820800000, 1612.1953317282992], [-439142400000, 1900], [-436636800000, 2000], [-433958400000, 2100], [-431366400000, 2300], [-428688000000, 2402.4665039813262], [-426096000000, 2321.222056407487], [-423417600000, 2270], [-420739200000, 2149.9999999999995], [-418147200000, 1953.0690195881941], [-383932800000, 1850], [-381340800000, 1850], [-378662400000, 1850], [-375984000000, 1900], [-373564800000, 2208.1742813390774], [-370886400000, 2100], [-368294400000, 2300], [-365616000000, 2420], [-363024000000, 2430.2546446549427], [-360345600000, 2270.0000000000005], [-357667200000, 2150], [-355075200000, 1965.9581241632252], [-226166400000, 1447.5487102352663], [-223574400000, 1496.1451832146618], [-220896000000, 1662.018447502056], [-218217600000, 1702.9904229633066], [-215798400000, 1983.1822096677809], [-213120000000, 2057.456543050294], [-210528000000, 2300], [-207849600000, 2323.768972513454], [-205257600000, 2266.0778772760577], [-202579200000, 2213.4910103292814], [-199900800000, 2066.4111905369864], [-197308800000, 1835.9767982901947], [-163008000000, 1281.7357506188282], [-160416000000, 1291.265053594294], [-157737600000, 1810.0850128649877], [-155059200000, 1900], [-152640000000, 1987.636434425], [-149961600000, 2054.0129112455243], [-147369600000, 2277.9958455007563], [-144691200000, 2184.594332718428], [-142099200000, 2113.8459705101577], [-139420800000, 1977.3865602227456], [-136742400000, 1846.3686972546493], [-134150400000, 1713.069478467428], [-99936000000, 1577.240032627045], [-97344000000, 1654.7798635496722], [-94665600000, 1789.026485474115], [-91987200000, 1897.1660136236221], [-89568000000, 2000], [-86889600000, 2100], [-84297600000, 2205.250473305693], [-81619200000, 2310.2516540943643], [-79027200000, 2400.779247457748], [-76348800000, 2270], [-73670400000, 2150], [-71078400000, 1943.9086748723203], [-36777600000, 1428.1851001258094], [-34185600000, 1424.723189230479], [-31507200000, 1483.6965911501086], [-28828800000, 1620.2184605097423], [-26409600000, 1738.011104915257], [-23731200000, 1892.7236716813857], [-21139200000, 2214.370561932174], [-18460800000, 2420], [-15868800000, 2340.1647498014195], [-13190400000, 2269.9999999999995], [-10512000000, 2150], [-7920000000, 1974.9999999999998], [-5241600000, 1850], [-2649600000, 1850], [28800000, 1850], [2707200000, 1900], [5126400000, 2000], [7804800000, 2100], [10396800000, 2161.386748718372], [13075200000, 2060.638672231599], [15667200000, 1988.8234838992385], [18345600000, 1844.4047121675212], [21024000000, 1668.3971073240116], [23616000000, 1445.0196593072849], [26294400000, 1303.0055204902571], [28886400000, 1398.4895854025885], [31564800000, 1524.2939942336861], [34243200000, 1747.0609887331439], [36662400000, 1877.0412493849003], [39340800000, 2064.958950913993], [41932800000, 2233.422039816418], [44611200000, 2268.0680597720575], [47203200000, 2276.7753785950713], [49881600000, 2253.262391886064], [52560000000, 2126.8194998997124], [55152000000, 1975.0000000000005], [120988800000, 1719.885858347993], [123580800000, 1850], [126259200000, 1850], [128937600000, 1900], [131356800000, 2000], [134035200000, 2100], [136627200000, 2300], [139305600000, 2383.8642406856357], [141897600000, 2367.4432339418972], [144576000000, 2270], [147254400000, 2150], [149846400000, 1974.9999999999995], [152524800000, 1849.9999999999998], [155116800000, 1843.459845552196], [157795200000, 1847.274837747011], [160473600000, 1859.3486192415046], [162892800000, 1947.6387823684584], [165571200000, 2100], [168163200000, 2224.726727611186], [170841600000, 2320.5631986624617], [173433600000, 2431.459625841006], [176112000000, 2270.0000000000005], [178790400000, 2149.9999999999995], [181382400000, 1966.418145931095], [373449600000, 1248.1232105872862], [376041600000, 1491.0533875139604], [378720000000, 1814.2712703881114], [381398400000, 1900], [383817600000, 2000], [386496000000, 2100], [389088000000, 2300], [391766400000, 2317.827169596444], [394358400000, 2187.3578334552903], [397036800000, 2163.2798276013855], [399715200000, 2138.8872982703283], [402307200000, 1975], [404985600000, 1849.9999999999998], [407577600000, 1850], [410256000000, 1850], [412934400000, 1900], [415353600000, 2000], [418032000000, 2100], [420624000000, 2300], [423302400000, 2420], [425894400000, 2447], [428572800000, 2270], [431251200000, 2150], [433843200000, 1975], [436521600000, 1850], [439113600000, 1850], [441792000000, 1850], [444470400000, 1900], [446976000000, 1996.7366353712937], [449654400000, 2100], [452246400000, 2217.6119610357614], [454924800000, 2183.315329659132], [457516800000, 2124.2652814817266], [460195200000, 1988.7868550547462], [462873600000, 1846.1780276564523], [465465600000, 1677.0721804104767], [499680000000, 1245.2718507425068], [502272000000, 1214.5211410504749], [504950400000, 1221.005228134549], [507628800000, 1340.9695136574776], [510048000000, 1814.4671371464387], [512726400000, 2100], [515318400000, 2122.058293127561], [517996800000, 2018.8355583014431], [520588800000, 1907.6219344278315], [523267200000, 1731.8326611765053], [525945600000, 1556.9252356790867], [528537600000, 1390.8888005568404], [783676800000, 971.8444437642389], [786268800000, 958.0537940241618], [788947200000, 944.0670895317306], [791625600000, 1344.794174264436], [794044800000, 1598.4891153592107], [796723200000, 2031.8177661671352], [799315200000, 2269.8850203844954], [801993600000, 2345.197510560271], [804585600000, 2330.394740403572], [807264000000, 2270], [809942400000, 2150], [812534400000, 1975], [815212800000, 1850], [817804800000, 1839.8861054774172], [820483200000, 1849.9999999999998], [823161600000, 1900], [825667200000, 2000], [828345600000, 2100], [830937600000, 2277.87476862332], [833616000000, 2233.7378084315374], [836208000000, 2169.6892748445516], [838886400000, 2032.2495186719775], [841564800000, 1895.4777605132456], [844156800000, 1756.4788425073161], [846835200000, 1693.3476343296702], [849427200000, 1699.1659305392523], [852105600000, 1850], [854784000000, 1900], [857203200000, 1990.4408221828824], [859881600000, 2076.459270329076], [862473600000, 2176.3063405029316], [865152000000, 1989.6788739925485], [867744000000, 1820.5544399836356], [870422400000, 1670.2942493085625], [873100800000, 1493.5938735098152], [875692800000, 1367.074740071836], [878371200000, 1307.5873421319006], [880963200000, 1282.0079220310236], [883641600000, 1313.0341381922276], [886320000000, 1623.729791470067], [888739200000, 1946.2769892306965], [891417600000, 2100], [894009600000, 2295.5724488973265], [896688000000, 2420], [899280000000, 2447], [901958400000, 2270], [904636800000, 2150], [907228800000, 1975], [909907200000, 1850], [912499200000, 1850], [915177600000, 1850], [917856000000, 1900], [920275200000, 2000], [922953600000, 2100], [925545600000, 2299.8805487206073], [928224000000, 2339.7516610243606], [930816000000, 2362.622819254627], [933494400000, 2270.0000000000005], [936172800000, 2135.8543367653], [938764800000, 1975]]
                    }]
                }]
            }], "scenario_name": "NewScenarioRun 3798104", "scenario_color": "#4C7EEEFF"
        }, {
            "ts_list": [{
                "ts_name": "NewScenarioRun 3840481 (CalSim2)", "monthly_filters": [{
                    "annual_filters": [{
                        "period_months": ["Oct", "Nov", "Dec", "Jan"],
                        "annual_period": "Long Term<br>",
                        "computed_statistics": [{
                            "statistic": "Averages",
                            "statistically_computed_time_series_monthly": [["Jan", 1463.1903795124244], ["Oct", 1338.2618514455125], ["Nov", 1349.563543821991], ["Dec", 1400.7085019067779]],
                            "statistic_aggregate": 1388.2692505647888
                        }, {
                            "statistic": "Minimums",
                            "statistically_computed_time_series_monthly": [["Jan", 382.80021625926156], ["Oct", 240], ["Nov", 267.29062040796197], ["Dec", 341.7657018054722]],
                            "statistic_aggregate": 317.40986403023715
                        }],
                        "aggregate_ts": [[1922, 1760.3564056653236], [1923, 1623.4911486358], [1924, 1229.3186663703596], [1925, 702.8939790412398], [1926, 1000.2149876066607], [1927, 1207.9015864682501], [1928, 1691.5188865763243], [1929, 1216.8505677349367], [1930, 890.9730910557446], [1931, 784.3292918913763], [1932, 421.42615604262545], [1933, 391.4807161594171], [1934, 448.337473006344], [1935, 317.40986403023715], [1936, 530.5293431966625], [1937, 670.619701946929], [1938, 1223.6226622220552], [1939, 1852.7892917790207], [1940, 1038.324891317301], [1941, 1467.4163767385758], [1942, 1862.5], [1943, 1862.5], [1944, 1561.9249330203074], [1945, 1239.3665477155914], [1946, 1351.5927960071333], [1947, 1378.1673919111122], [1948, 1197.0815804652511], [1949, 1407.9323153830676], [1950, 1328.5264746443268], [1951, 1583.3466960875862], [1952, 1605.2500848540813], [1953, 1861.3286626537474], [1954, 1862.5], [1955, 1645.7790674936232], [1956, 1533.643761700855], [1957, 1849.834215782299], [1958, 1851.670916066974], [1959, 1861.3336078690431], [1960, 1353.4904356337224], [1961, 1269.1830987620433], [1962, 1328.084066691406], [1963, 1544.8349651994881], [1964, 1858.5887467399225], [1965, 1554.341764202363], [1966, 1753.3956482380256], [1967, 1700.119351402682], [1968, 1862.5], [1969, 1492.4729146977686], [1970, 1862.5], [1971, 1560.8129395204119], [1972, 1862.5], [1973, 1685.2396314150394], [1974, 1823.1054717639774], [1975, 1853.688944287468], [1976, 1808.880642419379], [1977, 1046.4421236653286], [1978, 452.1979031398916], [1979, 1233.2491482201128], [1980, 1323.1243196071428], [1981, 1497.0230169585811], [1982, 1607.530623924809], [1983, 1862.5], [1984, 1862.5], [1985, 1735.9673991614302], [1986, 1248.340743044224], [1987, 1330.727615793427], [1988, 1006.2920386227529], [1989, 978.1380611256013], [1990, 1010.8083508691233], [1991, 884.6094409844462], [1992, 670.099839500632], [1993, 808.1735013417342], [1994, 1528.2924236686765], [1995, 1054.6898753961418], [1996, 1855.3351247188812], [1997, 1787.9839873319481], [1998, 1401.5149370475624], [1999, 1862.5], [2000, 1862.5], [2001, 1697.7901609292055], [2002, 1326.2632077402598], [2003, 1415.6619434068834]],
                        "month_period": "Oct - Jan",
                        "discrete_ts": [[-1522684800000, 2000], [-1520006400000, 1781.2779198935496], [-1517414400000, 1753.6804118542636], [-1514736000000, 1755.0900871780748], [-1512057600000, 1751.3772037354065], [-1509638400000, 1773.8588202179612], [-1506960000000, 1823.377987689489], [-1504368000000, 1941.4609885062093], [-1501689600000, 2020.0440770669159], [-1499097600000, 2066.1119064423083], [-1496419200000, 1954.1479165086514], [-1493740800000, 1805.2042476400295], [-1491148800000, 1661.2773908219176], [-1488470400000, 1606.7084586644032], [-1485878400000, 1608.382709207328], [-1483200000000, 1626.1605153984813], [-1480521600000, 1652.712911272988], [-1478102400000, 1675.3764846076203], [-1475424000000, 1727.773694560605], [-1472832000000, 1849.1059153816168], [-1470153600000, 1807.3192820072009], [-1467561600000, 1768.4308190291438], [-1464883200000, 1629.2816148891263], [-1462204800000, 1452.0332137551318], [-1459612800000, 1324.9955098605428], [-1456934400000, 1273.0393133452374], [-1454342400000, 1241.216962645222], [-1451664000000, 1228.6474814190387], [-1448985600000, 1174.3709080719398], [-1446480000000, 1233.8060443927482], [-1443801600000, 1226.1576898292942], [-1441209600000, 1196.8250792215842], [-1438531200000, 1114.946371920779], [-1435939200000, 1056.6768368615362], [-1433260800000, 955.040820999469], [-1430582400000, 753.6694060036803], [-1427990400000, 656.1455568884302], [-1425312000000, 648.5460301669519], [-1422720000000, 725.1923884395601], [-1420041600000, 767.6329276917554], [-1417363200000, 670.2045698666918], [-1414944000000, 940.8528985744387], [-1412265600000, 1079.0381733857957], [-1409673600000, 1377.3215105073523], [-1406995200000, 1362.6265824847649], [-1404403200000, 1288.797127048902], [-1401724800000, 1178.2775759846065], [-1399046400000, 1083.3623095951573], [-1396454400000, 1013.3093774665689], [-1393776000000, 990.4506876951313], [-1391184000000, 987.8705286434057], [-1388505600000, 1008.918715195414], [-1385827200000, 1013.6200188926917], [-1383408000000, 1160.5679964573915], [-1380729600000, 1274.7702893077444], [-1378137600000, 1496.197791645647], [-1375459200000, 1393.2008811548578], [-1372867200000, 1325.1603310981627], [-1370188800000, 1194.4008823490317], [-1367510400000, 1079.9959417808032], [-1364918400000, 983.9731555439773], [-1362240000000, 960.1326498735675], [-1359648000000, 1116.7589119637319], [-1356969600000, 1315.183579704499], [-1354291200000, 1439.531204331202], [-1351872000000, 1677.7192079715692], [-1349193600000, 1861.5459264712615], [-1346601600000, 2088.4247493615694], [-1343923200000, 2077.8308532631518], [-1341331200000, 2103.245767861831], [-1338652800000, 1981.0513926104738], [-1335974400000, 1841.3641872928786], [-1333382400000, 1704.082788894543], [-1330704000000, 1643.8958905010575], [-1328112000000, 1671.9601707210104], [-1325433600000, 1696.2755811122006], [-1322755200000, 1753.9439039710292], [-1320249600000, 1863.4281248744537], [-1317571200000, 2069.8537970945586], [-1314979200000, 2231.2259550071644], [-1312300800000, 2169.0932773296686], [-1309708800000, 2076.4311194212682], [-1307030400000, 1924.6657563707197], [-1304352000000, 1715.3406266376276], [-1301760000000, 1483.4519856483066], [-1299081600000, 1312.8712571238086], [-1296489600000, 1253.1201636390144], [-1293811200000, 1246.431380294254], [-1291132800000, 1054.9794698826697], [-1288713600000, 1082.1537007261425], [-1286035200000, 1133.189692136364], [-1283443200000, 1148.7704779670175], [-1280764800000, 1192.2902611082984], [-1278172800000, 1182.447988151475], [-1275494400000, 1092.3775450983082], [-1272816000000, 968.4198004622617], [-1270224000000, 871.7413135764752], [-1267545600000, 841.9139214238518], [-1264953600000, 826.2504542244756], [-1262275200000, 955.855283662534], [-1259596800000, 939.8727049121168], [-1257177600000, 1040.1967011047927], [-1254499200000, 1167.5788383251254], [-1251907200000, 1267.951509581913], [-1249228800000, 1179.1026690309377], [-1246636800000, 1141.7113883064756], [-1243958400000, 1050.0604163349622], [-1241280000000, 926.7889882995834], [-1238688000000, 832.6001104423804], [-1236009600000, 803.1584822209412], [-1233417600000, 785.3389361530847], [-1230739200000, 768.9844730084858], [-1228060800000, 779.8352761829938], [-1225641600000, 800.4671725510046], [-1222963200000, 860.031549172891], [-1220371200000, 883.1785257024842], [-1217692800000, 843.8397306717202], [-1215100800000, 820.670881588105], [-1212422400000, 658.9188386546883], [-1209744000000, 588.5524789966629], [-1207152000000, 500], [-1204473600000, 456.9261914946402], [-1201881600000, 410.24212392816486], [-1199203200000, 413.2055872261009], [-1196524800000, 405.33072152159593], [-1194019200000, 427.3344232500616], [-1191340800000, 580.110307632881], [-1188748800000, 669.8908310903988], [-1186070400000, 693.2401864531871], [-1183478400000, 700], [-1180800000000, 599.794976667157], [-1178121600000, 491.08564972783944], [-1175529600000, 426.0489077995353], [-1172851200000, 402.80953452501484], [-1170259200000, 391.600194902158], [-1167580800000, 387.2772609676166], [-1164902400000, 384.23587424287905], [-1162483200000, 385.65028705529244], [-1159804800000, 500.00000000000006], [-1157212800000, 651.9983122706526], [-1154534400000, 639.7495940556751], [-1151942400000, 758.5797426077723], [-1149264000000, 690.4580101475688], [-1146585600000, 625.4964507721056], [-1143993600000, 545.8252780346211], [-1141315200000, 499.99999999999994], [-1138723200000, 404.1041138607815], [-1136044800000, 413.9918286455671], [-1133366400000, 475.25394951902734], [-1130947200000, 565.3907114762035], [-1128268800000, 722.4179065542122], [-1125676800000, 806.3666639525917], [-1122998400000, 762.5559530297812], [-1120406400000, 727.2215353846308], [-1117728000000, 633.9284178860213], [-1115049600000, 412.3185365261456], [-1112457600000, 272.8109780826167], [-1109779200000, 240], [-1107187200000, 305.07353805621494], [-1104508800000, 341.7657018054722], [-1101830400000, 382.80021625926156], [-1099411200000, 466.35237795808195], [-1096732800000, 527.5588104112819], [-1094140800000, 741.5000907839534], [-1091462400000, 779.0394389095509], [-1088870400000, 786.565819023345], [-1086192000000, 702.3096033839132], [-1083513600000, 581.9454544770608], [-1080921600000, 518.6581211469536], [-1078243200000, 504.65401569456435], [-1075651200000, 499.39386637429476], [-1072972800000, 502.48119810091646], [-1070294400000, 615.5882926168746], [-1067788800000, 755.8865815125791], [-1065110400000, 872.5181454312384], [-1062518400000, 1049.1404727584884], [-1059840000000, 979.5303238091273], [-1057248000000, 942.1973141782462], [-1054569600000, 891.7434328153151], [-1051891200000, 796.3735251842928], [-1049299200000, 715.611659060015], [-1046620800000, 685.5292512816377], [-1044028800000, 674.759687268816], [-1041350400000, 665.1738540490086], [-1038672000000, 657.0160151882537], [-1036252800000, 661.0085954541157], [-1033574400000, 766.0877551038473], [-1030982400000, 991.4719916480095], [-1028304000000, 1137.5337589970086], [-1025712000000, 1242.417033210354], [-1023033600000, 1176.537900689841], [-1020355200000, 1081.1581175115175], [-1017763200000, 999.7469964242515], [-1015084800000, 978.8381422693712], [-1012492800000, 1126.4522949568352], [-1009814400000, 1345.8281291439516], [-1007136000000, 1443.3720825180628], [-1004716800000, 1600.305052060898], [-1002038400000, 1861.5814618503991], [-999446400000, 2169.145772342404], [-996768000000, 2342.985100062149], [-994176000000, 2287.9458277532203], [-991497600000, 2258.3797038243306], [-988819200000, 2149.9999999999995], [-986227200000, 1975], [-983548800000, 1850], [-980956800000, 1849.9999999999998], [-978278400000, 1850], [-975600000000, 1861.1571671160832], [-973180800000, 1875.2774732131365], [-970502400000, 1983.2145393876838], [-967910400000, 2058.507240661381], [-965232000000, 1953.0821845671883], [-962640000000, 1851.6018002881897], [-959961600000, 1619.5513992716624], [-957283200000, 1375.575410553875], [-954691200000, 1143.0160695673062], [-952012800000, 1030.2597226684816], [-949420800000, 939.3860371724832], [-946742400000, 1003.6413151749547], [-944064000000, 1180.0124902532855], [-941558400000, 1505.8917891861422], [-938880000000, 1808.0757492054472], [-936288000000, 2034.3993789250414], [-933609600000, 1971.502365511925], [-931017600000, 1855.7960305040394], [-928339200000, 1707.0209981250784], [-925660800000, 1531.0564987448201], [-923068800000, 1407.7414988330247], [-920390400000, 1354.9007080777938], [-917798400000, 1338.486465842741], [-915120000000, 1477.791260243608], [-912441600000, 1698.487072790161], [-910022400000, 1949.1775976454603], [-907344000000, 2100], [-904752000000, 2300], [-902073600000, 2420], [-899481600000, 2447], [-896803200000, 2270], [-894124800000, 2150], [-891532800000, 1975], [-888854400000, 1850], [-886262400000, 1850], [-883584000000, 1850], [-880905600000, 1900], [-878486400000, 2000], [-875808000000, 2078.5401342424584], [-873216000000, 2285.2581215952086], [-870537600000, 2344.7953085222416], [-867945600000, 2416.5226140655363], [-865267200000, 2270], [-862588800000, 2150], [-859996800000, 1974.9999999999995], [-857318400000, 1850.0000000000002], [-854726400000, 1850], [-852048000000, 1850], [-849369600000, 1900], [-846950400000, 2000], [-844272000000, 2100], [-841680000000, 2283.155661244806], [-839001600000, 2173.318610935777], [-836409600000, 2100.1013478625755], [-833731200000, 1955.632799582456], [-831052800000, 1810.9238053951467], [-828460800000, 1640.4268782084089], [-825782400000, 1583.0498075411322], [-823190400000, 1563.3481282303221], [-820512000000, 1551.4776864152889], [-817833600000, 1549.824109894487], [-815328000000, 1586.9643876747014], [-812649600000, 1648.8764145922105], [-810057600000, 1688.084247116321], [-807379200000, 1746.5857249883015], [-804787200000, 1697.7670848605617], [-802108800000, 1562.0190595213658], [-799430400000, 1356.0359235904748], [-796838400000, 1230.6450065466317], [-794160000000, 1170.8958180390327], [-791568000000, 1200.9021461401219], [-788889600000, 1268.5950581095], [-786211200000, 1317.0731685737105], [-783792000000, 1482.293723556446], [-781113600000, 1536.1463926975127], [-778521600000, 1673.6756571862545], [-775843200000, 1638.1914234749438], [-773251200000, 1578.7286112414731], [-770572800000, 1435.2377352099325], [-767894400000, 1314.727781697121], [-765302400000, 1186.7391616993718], [-762624000000, 1182.1171581798772], [-760032000000, 1237.1010993067373], [-757353600000, 1428.3252334878025], [-754675200000, 1558.8276930541163], [-752256000000, 1613.8397886182574], [-749577600000, 1736.0966449765817], [-746985600000, 1954.179482968568], [-744307200000, 1949.495681539694], [-741715200000, 1883.4532073000112], [-739036800000, 1751.9603430825946], [-736358400000, 1580.074281546491], [-733766400000, 1414.503589227336], [-731088000000, 1361.5524083091002], [-728496000000, 1370.047891375701], [-725817600000, 1390.9796145345913], [-723139200000, 1390.0896534250564], [-720720000000, 1450.0920801988643], [-718041600000, 1569.141537527109], [-715449600000, 1656.4301367957157], [-712771200000, 1566.3096981981807], [-710179200000, 1558.686017191306], [-707500800000, 1417.9574046398056], [-704822400000, 1246.4140874348693], [-702230400000, 1122.0522776600983], [-699552000000, 1138.0590286073518], [-696960000000, 1151.9725556729607], [-694281600000, 1151.3263037633462], [-691603200000, 1346.9684338173456], [-689097600000, 1369.4581022503878], [-686419200000, 1397.7868601833281], [-683827200000, 1584.9601658449133], [-681148800000, 1579.2708519226426], [-678556800000, 1658.8698255686163], [-675878400000, 1623.16890440377], [-673200000000, 1496.4658604552071], [-670608000000, 1449.5105015648498], [-667929600000, 1403.3765416117853], [-665337600000, 1410.2778435636649], [-662659200000, 1412.328045577411], [-659980800000, 1405.746830779409], [-657561600000, 1437.6370620594428], [-654883200000, 1653.8513002716847], [-652291200000, 1903.8577882414297], [-649612800000, 1904.8779589209141], [-647020800000, 1854.2977940760622], [-644342400000, 1709.3209513287693], [-641664000000, 1535.4432468446612], [-639072000000, 1408.1935310716099], [-636393600000, 1353.6420125275204], [-633801600000, 1323.635593000406], [-631123200000, 1307.8115251808376], [-628444800000, 1329.016767868543], [-626025600000, 1385.2269831925585], [-623347200000, 1506.290593182369], [-620755200000, 1664.3050868440525], [-618076800000, 1673.686358140288], [-615484800000, 1655.176886075836], [-612806400000, 1547.8836677389743], [-610128000000, 1402.1213457938682], [-607536000000, 1275.8244235577165], [-604857600000, 1345.2812464943854], [-602265600000, 1460.9624688441356], [-599587200000, 1718.5833139788476], [-596908800000, 1808.5597550329765], [-594489600000, 2000], [-591811200000, 2100], [-589219200000, 2285.6210802982428], [-586540800000, 2217.5091071736506], [-583948800000, 2133.2400239159238], [-581270400000, 1985.6232395973063], [-578592000000, 1764.3155817750212], [-576000000000, 1594.8513736444622], [-573321600000, 1510.5784833488797], [-570729600000, 1518.7352008707492], [-568051200000, 1666.1410492783], [-565372800000, 1725.5456059183966], [-562867200000, 1915.5637852595908], [-560188800000, 2081.5235453921127], [-557596800000, 2299.9999999999995], [-554918400000, 2410.963686814417], [-552326400000, 2447], [-549648000000, 2270], [-546969600000, 2150], [-544377600000, 1974.9999999999998], [-541699200000, 1850], [-539107200000, 1845.3146506149892], [-536428800000, 1850], [-533750400000, 1900], [-531331200000, 2000], [-528652800000, 2100], [-526060800000, 2300], [-523382400000, 2259.9112123923715], [-520790400000, 2353.448239660465], [-518112000000, 2270], [-515433600000, 2139.699638927823], [-512841600000, 1975.0000000000002], [-510163200000, 1850], [-507571200000, 1850.0000000000002], [-504892800000, 1850], [-502214400000, 1900.0000000000002], [-499795200000, 2000], [-497116800000, 2100], [-494524800000, 2300], [-491846400000, 2271.5030161556515], [-489254400000, 2203.9284707403995], [-486576000000, 2067.065926783014], [-483897600000, 1854.7149140411384], [-481305600000, 1692.863816871314], [-478627200000, 1604.9914122974592], [-476035200000, 1625.8888175989666], [-473356800000, 1666.9705269470671], [-470678400000, 1685.2655131310003], [-468259200000, 1714.1247468263025], [-465580800000, 1744.2625702022733], [-462988800000, 1790.4049529957765], [-460310400000, 1804.3706726705818], [-457718400000, 1789.739017439078], [-455040000000, 1681.0911841096988], [-452361600000, 1504.2761531315343], [-449769600000, 1377.12351822381], [-447091200000, 1317.6322586792123], [-444499200000, 1304.4166129956288], [-441820800000, 1612.5261751285796], [-439142400000, 1900], [-436636800000, 2000], [-433958400000, 2100], [-431366400000, 2300], [-428688000000, 2402.4665039813262], [-426096000000, 2321.222056407487], [-423417600000, 2270], [-420739200000, 2150], [-418147200000, 1974.9999999999995], [-415468800000, 1850], [-412876800000, 1849.9999999999998], [-410198400000, 1849.9999999999998], [-407520000000, 1849.336863129196], [-405100800000, 2000.0000000000002], [-402422400000, 2100], [-399830400000, 2218.3702490417722], [-397152000000, 2234.431188934835], [-394560000000, 2213.9086518272306], [-391881600000, 2069.73159988299], [-389203200000, 1925.6784213154615], [-386611200000, 1761.6258936629063], [-383932800000, 1806.6836642678961], [-381340800000, 1850], [-378662400000, 1850], [-375984000000, 1900], [-373564800000, 2208.1742813390774], [-370886400000, 2100], [-368294400000, 2300], [-365616000000, 2420], [-363024000000, 2430.2546446549427], [-360345600000, 2270.0000000000005], [-357667200000, 2150], [-355075200000, 1975], [-352396800000, 1850], [-349804800000, 1849.9999999999998], [-347126400000, 1845.3344314761728], [-344448000000, 1900], [-342028800000, 2000.0000000000002], [-339350400000, 2100], [-336758400000, 2271.801367418244], [-334080000000, 2170.725303667815], [-331488000000, 2053.05568815621], [-328809600000, 1825.4420296257313], [-326131200000, 1643.149071644791], [-323539200000, 1448.5693716909464], [-320860800000, 1389.6792380986888], [-318268800000, 1353.7340360012167], [-315590400000, 1335.1730002469928], [-312912000000, 1335.3754681879914], [-310406400000, 1471.9033960959105], [-307728000000, 1659.9483037303985], [-305136000000, 1781.2377151455137], [-302457600000, 1726.2709209823952], [-299865600000, 1722.7798681783931], [-297187200000, 1594.3194664669206], [-294508800000, 1420.457762059304], [-291916800000, 1293.1404197257407], [-289238400000, 1235.4572562609392], [-286646400000, 1222.1695919124277], [-283968000000, 1287.4794351518224], [-281289600000, 1331.6261117229838], [-278870400000, 1522.9322084951007], [-276192000000, 1620.5704098741398], [-273600000000, 1810.6559765816169], [-270921600000, 1838.7485240490385], [-268329600000, 1873.6470167253551], [-265651200000, 1727.1627430886988], [-262972800000, 1554.3067508451816], [-260380800000, 1387.8205706452818], [-257702400000, 1335.1380487376973], [-255110400000, 1313.0363796367806], [-252432000000, 1323.5275480995992], [-249753600000, 1340.6342902915464], [-247334400000, 1438.4890160577422], [-244656000000, 1497.0098445619867], [-242064000000, 1822.8615483102767], [-239385600000, 1788.9830278095676], [-236793600000, 1756.0843020055493], [-234115200000, 1607.9045655795728], [-231436800000, 1438.4829411570968], [-228844800000, 1313.4652992649212], [-226166400000, 1415.1934690590779], [-223574400000, 1463.804195426381], [-220896000000, 1629.6835230806355], [-218217600000, 1670.6586732318576], [-215798400000, 1950.8650133893266], [-213120000000, 2025.1571325707534], [-210528000000, 2282.3407241004584], [-207849600000, 2306.146565971952], [-205257600000, 2248.5028113332564], [-202579200000, 2195.9722115489176], [-199900800000, 2048.943122712088], [-197308800000, 1909.530320895925], [-194630400000, 1846.3826722413748], [-192038400000, 1849.9999999999998], [-189360000000, 1850], [-186681600000, 1887.9723147183158], [-184176000000, 1883.088504268295], [-181497600000, 1912.4108301411948], [-178905600000, 1947.071960840798], [-176227200000, 1839.2845925422828], [-173635200000, 1809.2139617778168], [-170956800000, 1597.3840765732348], [-168278400000, 1428.507015229122], [-165686400000, 1311.070693439766], [-163008000000, 1259.822385665771], [-160416000000, 1269.36018665332], [-157737600000, 1788.184484490361], [-155059200000, 1900], [-152640000000, 1987.636434425], [-149961600000, 2054.0129112455243], [-147369600000, 2277.9958455007545], [-144691200000, 2184.5943327184264], [-142099200000, 2113.845970510156], [-139420800000, 1977.3865602227438], [-136742400000, 1846.3686972546475], [-134150400000, 1713.0694784674263], [-131472000000, 1652.5048995124068], [-128880000000, 1736.4424836579053], [-126201600000, 1773.6720829469834], [-123523200000, 1850.9631268348069], [-121104000000, 1916.4184806562496], [-118425600000, 2100], [-115833600000, 2300], [-113155200000, 2300.6042666254175], [-110563200000, 2225.687726289895], [-107884800000, 2009.9996487874926], [-105206400000, 1796.4671466925795], [-102614400000, 1634.5098689340603], [-99936000000, 1545.5628098369955], [-97344000000, 1626.0900536739698], [-94665600000, 1760.3403556767087], [-91987200000, 1868.484186423054], [-89568000000, 2000], [-86889600000, 2100], [-84297600000, 2205.250473305693], [-81619200000, 2310.2516540943643], [-79027200000, 2400.779247457748], [-76348800000, 2269.9999999999995], [-73670400000, 2138.5949781316126], [-71078400000, 1975], [-68400000000, 1850], [-65808000000, 1850], [-63129600000, 1850], [-60451200000, 1900], [-57945600000, 2000], [-55267200000, 2100], [-52675200000, 2191.009019304407], [-49996800000, 2077.487701506115], [-47404800000, 1971.0970102032252], [-44726400000, 1820.5758791420346], [-42048000000, 1647.91583283241], [-39456000000, 1481.7815168854245], [-36777600000, 1428.1851001258094], [-34185600000, 1424.723189230479], [-31507200000, 1490.2312707283677], [-28828800000, 1626.752098706418], [-26409600000, 1744.5434492168465], [-23731200000, 1899.2516326181221], [-21139200000, 2220.8884938390192], [-18460800000, 2420], [-15868800000, 2340.1647498014195], [-13190400000, 2270], [-10512000000, 2150], [-7920000000, 1975], [-5241600000, 1850], [-2649600000, 1850], [28800000, 1850], [2707200000, 1900], [5126400000, 2000], [7804800000, 2100], [10396800000, 2161.386748718372], [13075200000, 2060.638672231599], [15667200000, 1916.8774545570625], [18345600000, 1772.7429555628744], [21024000000, 1596.96689142045], [23616000000, 1428.9257593522093], [26294400000, 1369.5210144772188], [28886400000, 1466.4634290854108], [31564800000, 1592.255731154013], [34243200000, 1815.0115833650048], [36662400000, 1944.9727355991427], [39340800000, 2100], [41932800000, 2268.42366063641], [44611200000, 2302.9967199961598], [47203200000, 2311.609626351704], [49881600000, 2270], [52560000000, 2150], [55152000000, 1975.0000000000002], [57830400000, 1850], [60422400000, 1849.9999999999998], [63100800000, 1850], [65779200000, 1900], [68284800000, 2000.0000000000002], [70963200000, 2100], [73555200000, 2233.747818808155], [76233600000, 2163.0551072842095], [78825600000, 2112.663296194655], [81504000000, 1964.1649917152063], [84182400000, 1790.0463124887483], [86774400000, 1621.7781439663013], [89452800000, 1568.128956171881], [92044800000, 1604.0208042958207], [94723200000, 1707.9864349820823], [97401600000, 1860.8223302103736], [99820800000, 2000], [102499200000, 2100], [105091200000, 2287.7040645885572], [107769600000, 2336.913207084849], [110361600000, 2283.3535459803284], [113040000000, 2143.5123270242116], [115718400000, 1920.3471698105482], [118310400000, 1752.2474051961096], [120988800000, 1692.42188705591], [123580800000, 1850], [126259200000, 1850], [128937600000, 1900], [131356800000, 2000], [134035200000, 2100], [136627200000, 2300], [139305600000, 2383.8642406856357], [141897600000, 2367.4432339418972], [144576000000, 2270], [147254400000, 2150], [149846400000, 1975], [152524800000, 1850], [155116800000, 1843.4598455521957], [157795200000, 1850], [160473600000, 1871.2959315976764], [162892800000, 1965.1356731414623], [165571200000, 2100], [168163200000, 2224.7267276111875], [170841600000, 2320.563198662463], [173433600000, 2431.4596258410074], [176112000000, 2270], [178790400000, 2149.9999999999995], [181382400000, 1975], [184060800000, 1850], [186652800000, 1850], [189331200000, 1850], [192009600000, 1685.5225696775158], [194515200000, 1716.6400679845533], [197193600000, 1764.9300920801936], [199785600000, 1846.2416164706385], [202464000000, 1820.655675440109], [205056000000, 1770.4368358461827], [207734400000, 1551.7441818722475], [210412800000, 1355.2434632932006], [213004800000, 1230.4553132281221], [215683200000, 1171.520794981765], [218275200000, 1026.0359645827386], [220953600000, 1000.0000000000001], [223632000000, 988.2117350968107], [226051200000, 979.5388014457244], [228729600000, 965.6829236636772], [231321600000, 935.1324238987725], [234000000000, 882.7054326770452], [236592000000, 845.2627648344788], [239270400000, 618.1066032164853], [241948800000, 373.658874785528], [244540800000, 283.3105609031978], [247219200000, 240], [249811200000, 267.29062040796197], [252489600000, 458.20667482838076], [255168000000, 843.2943173232236], [257587200000, 1021.3605263445141], [260265600000, 1294.6939352668437], [262857600000, 1486.2069198797105], [265536000000, 1490.642042083278], [268128000000, 1435.6856146155947], [270806400000, 1373.108986491101], [273484800000, 1287.2509813134566], [276076800000, 1290.851836819887], [278755200000, 1239.3542527670807], [281347200000, 1232.9815047308384], [284025600000, 1222.5966849749004], [286704000000, 1238.064150407632], [289123200000, 1292.2123944162895], [291801600000, 1435.1459264016394], [294393600000, 1550.246646506078], [297072000000, 1626.6152242547976], [299664000000, 1599.5829258951428], [302342400000, 1491.0407619604357], [305020800000, 1346.4918092887258], [307612800000, 1220.5616665430962], [310291200000, 1188.280403124109], [312883200000, 1250.7880333265157], [315561600000, 1317.3233744492197], [318240000000, 1536.1054675287262], [320745600000, 1824.5874055610384], [323424000000, 1956.260619406819], [326016000000, 2115.719776464171], [328694400000, 2020.6670735760822], [331286400000, 1922.5272531499909], [333964800000, 1799.6006383710696], [336643200000, 1655.289095794577], [339235200000, 1517.0474914237195], [341913600000, 1461.8665109484934], [344505600000, 1452.3257716347462], [347184000000, 1484.0168443702325], [349862400000, 1589.882940880852], [352281600000, 1727.8305785039588], [354960000000, 1851.982657031244], [357552000000, 1960.2606404149876], [360230400000, 1892.2222890295457], [362822400000, 1836.293862922134], [365500800000, 1608.4896079907996], [368179200000, 1423.6135748352829], [370771200000, 1300.6251673100091], [373449600000, 1241.4994332560282], [376041600000, 1484.4326511968413], [378720000000, 1807.6519615133905], [381398400000, 1896.5384497329762], [383817600000, 2000], [386496000000, 2100], [389088000000, 2300], [391766400000, 2317.827169596444], [394358400000, 2187.3578334552903], [397036800000, 2163.2798276013855], [399715200000, 2138.8872982703283], [402307200000, 1975], [404985600000, 1849.9999999999998], [407577600000, 1850], [410256000000, 1850], [412934400000, 1900], [415353600000, 2000], [418032000000, 2100], [420624000000, 2300], [423302400000, 2420], [425894400000, 2447], [428572800000, 2270], [431251200000, 2150], [433843200000, 1975], [436521600000, 1850], [439113600000, 1850], [441792000000, 1850], [444470400000, 1900], [446976000000, 1996.7366353712937], [449654400000, 2100], [452246400000, 2217.6119610357614], [454924800000, 2183.315329659132], [457516800000, 2124.2652814817266], [460195200000, 1988.7868550547462], [462873600000, 1846.178027656452], [465465600000, 1677.0721804104764], [468144000000, 1618.8523412622096], [470736000000, 1748.4765700179128], [473414400000, 1805.5635507719837], [476092800000, 1770.9771345936147], [478512000000, 1809.8860900484096], [481190400000, 1857.643171527426], [483782400000, 2004.203540804162], [486460800000, 1927.0879556408458], [489052800000, 1881.3097191761183], [491731200000, 1654.963462040902], [494409600000, 1420.1547245661209], [497001600000, 1297.5195075091797], [499680000000, 1238.1677681621838], [502272000000, 1207.4196903446966], [504950400000, 1213.9046199715642], [507628800000, 1333.870893698451], [510048000000, 1807.3715808190948], [512726400000, 2100], [515318400000, 2122.0582931275612], [517996800000, 2018.8355583014436], [520588800000, 1907.6219344278315], [523267200000, 1731.8326611765056], [525945600000, 1556.9252356790869], [528537600000, 1390.8888005568406], [531216000000, 1344.9046245179309], [533808000000, 1326.161035322114], [536486400000, 1319.823878879729], [539164800000, 1332.0209244539349], [541584000000, 1416.2972834835932], [544262400000, 1627.4877688653478], [546854400000, 1773.91089482176], [549532800000, 1730.7704630734631], [552124800000, 1575.2627902448135], [554803200000, 1349.6877801189758], [557481600000, 1138.3250361381008], [560073600000, 975.3621008325935], [562752000000, 931.0732453871975], [565344000000, 902.9062516242365], [568022400000, 1054.9853084352046], [570700800000, 1136.2033490443732], [573206400000, 1220.6893959079243], [575884800000, 1301.1862872228864], [578476800000, 1387.6654142292969], [581155200000, 1332.9122283539843], [583747200000, 1315.3619416416934], [586425600000, 1188.3356471212708], [589104000000, 1067.4671062822724], [591696000000, 975.257134975171], [594374400000, 946.947504132714], [596966400000, 969.0471179298986], [599644800000, 986.8520447139873], [602323200000, 1009.7055777258051], [604742400000, 1037.0453557163496], [607420800000, 1379.6700143747942], [610012800000, 1578.9969248799312], [612691200000, 1470.0547678410076], [615283200000, 1362.1182372380538], [617961600000, 1213.274824220901], [620640000000, 1069.9279774498987], [623232000000, 977.9363394485201], [625910400000, 994.4638664987278], [628502400000, 997.7399316119863], [631180800000, 992.6299527906516], [633859200000, 1058.3996525751272], [636278400000, 1085.1586617213727], [638956800000, 1173.1718643415388], [641548800000, 1212.492069081621], [644227200000, 1180.04931564201], [646819200000, 1198.876865381159], [649497600000, 1137.0269819053544], [652176000000, 1027.20273312275], [654768000000, 935.0942226267782], [657446400000, 906.8080500343825], [660038400000, 892.8814005242438], [662716800000, 877.265392499676], [665395200000, 861.4829208794825], [667814400000, 866.9572557463691], [670492800000, 941.3890731370889], [673084800000, 982.927653027946], [675763200000, 1006.499970461156], [678355200000, 1002.502499237974], [681033600000, 916.2836178326651], [683712000000, 797.5447803274828], [686304000000, 706.1042543674918], [688982400000, 678.0513749488299], [691574400000, 669.0605085516528], [694252800000, 664.0920958242637], [696931200000, 669.1953786777815], [699436800000, 808.0664035991263], [702115200000, 955.6465454275558], [704707200000, 1189.136041056044], [707385600000, 1182.2309050512117], [709977600000, 1157.9822196205469], [712656000000, 1070.8204206796702], [715334400000, 951.7616983042537], [717926400000, 822.4786590992961], [720604800000, 794.331625618805], [723196800000, 780.4149082059811], [725875200000, 799.9866242860874], [728553600000, 857.9608472560632], [730972800000, 995.4604367821547], [733651200000, 1373.4561461344451], [736243200000, 1569.0977918339224], [738921600000, 1738.5439690567564], [741513600000, 1853.1621824686076], [744192000000, 1836.196719617101], [746870400000, 1726.0999071285794], [749462400000, 1590.1101213847512], [752140800000, 1529.8679088242632], [754732800000, 1520.13400109385], [757411200000, 1538.0821586654072], [760089600000, 1525.0856260911855], [762508800000, 1559.0154236678857], [765187200000, 1626.9519450916891], [767779200000, 1671.9005096368248], [770457600000, 1676.143914626903], [773049600000, 1521.6496761492392], [775728000000, 1349.693609439764], [778406400000, 1174.9955650386382], [780998400000, 1000.0000000000002], [783676800000, 971.8444437642389], [786268800000, 958.0537940241618], [788947200000, 944.0670895317306], [791625600000, 1344.794174264436], [794044800000, 1598.4891153592107], [796723200000, 2031.8177661671352], [799315200000, 2269.8850203844954], [801993600000, 2345.197510560271], [804585600000, 2330.394740403572], [807264000000, 2270], [809942400000, 2150], [812534400000, 1975], [815212800000, 1850], [817804800000, 1821.3404988755242], [820483200000, 1849.9999999999998], [823161600000, 1900.0000000000002], [825667200000, 2000], [828345600000, 2100], [830937600000, 2277.874768623321], [833616000000, 2233.7378084315387], [836208000000, 2167.9045966860995], [838886400000, 2030.4709514792798], [841564800000, 1882.6709737402327], [844156800000, 1746.3030725752562], [846835200000, 1683.1836106281428], [849427200000, 1718.7523386996495], [852105600000, 1850], [854784000000, 1900], [857203200000, 1990.4408221828824], [859881600000, 2081.9979369023627], [862473600000, 2181.835960533022], [865152000000, 1995.1943746765191], [867744000000, 1826.051641943439], [870422400000, 1675.7708753408806], [873100800000, 1499.0493027943965], [875692800000, 1372.5132921958161], [878371200000, 1313.0181752940593], [880963200000, 1305.7442956403372], [883641600000, 1338.303383583556], [886320000000, 1648.993893672297], [888739200000, 1971.535626741254], [891417600000, 2100], [894009600000, 2295.5724488973256], [896688000000, 2420], [899280000000, 2447], [901958400000, 2270], [904636800000, 2150], [907228800000, 1975], [909907200000, 1850], [912499200000, 1850], [915177600000, 1850], [917856000000, 1900], [920275200000, 2000], [922953600000, 2100], [925545600000, 2299.8805487206073], [928224000000, 2339.7516610243606], [930816000000, 2360.4623153939197], [933494400000, 2270], [936172800000, 2135.8543367652987], [938764800000, 1975], [941443200000, 1850], [944035200000, 1850], [946713600000, 1850], [949392000000, 1900], [951897600000, 2000], [954576000000, 2100], [957168000000, 2300], [959846400000, 2291.5704881582233], [962438400000, 2306.4403357574006], [965116800000, 2176.0114758811656], [967795200000, 1952.718735207226], [970387200000, 1787.0663336744585], [973065600000, 1727.1691603279064], [975657600000, 1695.146197174043], [978336000000, 1684.5246575410883], [981014400000, 1684.3206286737843], [983433600000, 1714.6361222097707], [986112000000, 1885.069971602401], [988704000000, 1995.296480873559], [991382400000, 1971.1821062705046], [993974400000, 1869.6224106561203], [996652800000, 1657.17077427362], [999331200000, 1436.3275598212047], [1001923200000, 1317.317870279849], [1004601600000, 1196.2351185400337], [1007193600000, 1242.3368008674663], [1009872000000, 1318.2433663607098], [1012550400000, 1548.237545192829], [1014969600000, 1658.1475547133687], [1017648000000, 1760.6720261758296], [1020240000000, 1967.8028205256621], [1022918400000, 1876.8571584932176], [1025510400000, 1778.611080261363], [1028188800000, 1629.0725748919385], [1030867200000, 1452.9809747679346], [1033459200000, 1326.7004870294516], [1036137600000, 1271.8252775709693], [1038729600000, 1237.1460398640081], [1041408000000, 1420.648456176811], [1044086400000, 1733.0280000157452], [1046505600000, 1844.5114571915171], [1049184000000, 1999.1725071305304], [1051776000000, 2199.076571541786], [1054454400000, 2265.0538969575814], [1057046400000, 2340.27860553425], [1059724800000, 2224.203144422022], [1062403200000, 2092.6227315881606], [1064995200000, 1882.7824451038534]]
                    }, {
                        "period_months": ["Oct", "Nov", "Dec", "Jan"],
                        "annual_period": "Sacramento River Index<br>Wet",
                        "computed_statistics": [{
                            "statistic": "Averages",
                            "statistically_computed_time_series_monthly": [["Jan", 1780.9918602466162], ["Oct", 1538.6272230820243], ["Nov", 1569.7103914034963], ["Dec", 1667.3712097301986]],
                            "statistic_aggregate": 1631.1771420477794
                        }, {
                            "statistic": "Minimums",
                            "statistically_computed_time_series_monthly": [["Jan", 1333.870893698451], ["Oct", 971.8444437642389], ["Nov", 958.0537940241618], ["Dec", 944.0670895317306]],
                            "statistic_aggregate": 1054.6898753961418
                        }],
                        "aggregate_ts": [[1927, 1207.9015864682501], [1938, 1223.6226622220552], [1941, 1467.4163767385758], [1942, 1862.5], [1943, 1862.5], [1952, 1605.2500848540813], [1953, 1861.3286626537474], [1956, 1533.643761700855], [1958, 1851.670916066974], [1963, 1544.8349651994881], [1965, 1554.341764202363], [1967, 1700.119351402682], [1969, 1492.4729146977686], [1970, 1862.5], [1971, 1560.8129395204119], [1974, 1823.1054717639774], [1975, 1853.688944287468], [1982, 1607.530623924809], [1983, 1862.5], [1984, 1862.5], [1986, 1248.340743044224], [1995, 1054.6898753961418], [1996, 1855.3351247188812], [1997, 1787.9839873319481], [1998, 1401.5149370475624], [1999, 1862.5]],
                        "month_period": "Oct - Jan",
                        "discrete_ts": [[-1362240000000, 960.1326498735675], [-1359648000000, 1116.7589119637319], [-1356969600000, 1315.183579704499], [-1354291200000, 1439.531204331202], [-1351872000000, 1677.7192079715692], [-1349193600000, 1861.5459264712615], [-1346601600000, 2088.4247493615694], [-1343923200000, 2077.8308532631518], [-1341331200000, 2103.245767861831], [-1338652800000, 1981.0513926104738], [-1335974400000, 1841.3641872928786], [-1333382400000, 1704.082788894543], [-1015084800000, 978.8381422693712], [-1012492800000, 1126.4522949568352], [-1009814400000, 1345.8281291439516], [-1007136000000, 1443.3720825180628], [-1004716800000, 1600.305052060898], [-1002038400000, 1861.5814618503991], [-999446400000, 2169.145772342404], [-996768000000, 2342.985100062149], [-994176000000, 2287.9458277532203], [-991497600000, 2258.3797038243306], [-988819200000, 2149.9999999999995], [-986227200000, 1975], [-920390400000, 1354.9007080777938], [-917798400000, 1338.486465842741], [-915120000000, 1477.791260243608], [-912441600000, 1698.487072790161], [-910022400000, 1949.1775976454603], [-907344000000, 2100], [-904752000000, 2300], [-902073600000, 2420], [-899481600000, 2447], [-896803200000, 2270], [-894124800000, 2150], [-891532800000, 1975], [-888854400000, 1850], [-886262400000, 1850], [-883584000000, 1850], [-880905600000, 1900], [-878486400000, 2000], [-875808000000, 2078.5401342424584], [-873216000000, 2285.2581215952086], [-870537600000, 2344.7953085222416], [-867945600000, 2416.5226140655363], [-865267200000, 2270], [-862588800000, 2150], [-859996800000, 1974.9999999999995], [-857318400000, 1850.0000000000002], [-854726400000, 1850], [-852048000000, 1850], [-849369600000, 1900], [-846950400000, 2000], [-844272000000, 2100], [-841680000000, 2283.155661244806], [-839001600000, 2173.318610935777], [-836409600000, 2100.1013478625755], [-833731200000, 1955.632799582456], [-831052800000, 1810.9238053951467], [-828460800000, 1640.4268782084089], [-573321600000, 1510.5784833488797], [-570729600000, 1518.7352008707492], [-568051200000, 1666.1410492783], [-565372800000, 1725.5456059183966], [-562867200000, 1915.5637852595908], [-560188800000, 2081.5235453921127], [-557596800000, 2299.9999999999995], [-554918400000, 2410.963686814417], [-552326400000, 2447], [-549648000000, 2270], [-546969600000, 2150], [-544377600000, 1974.9999999999998], [-541699200000, 1850], [-539107200000, 1845.3146506149892], [-536428800000, 1850], [-533750400000, 1900], [-531331200000, 2000], [-528652800000, 2100], [-526060800000, 2300], [-523382400000, 2259.9112123923715], [-520790400000, 2353.448239660465], [-518112000000, 2270], [-515433600000, 2139.699638927823], [-512841600000, 1975.0000000000002], [-447091200000, 1317.6322586792123], [-444499200000, 1304.4166129956288], [-441820800000, 1612.5261751285796], [-439142400000, 1900], [-436636800000, 2000], [-433958400000, 2100], [-431366400000, 2300], [-428688000000, 2402.4665039813262], [-426096000000, 2321.222056407487], [-423417600000, 2270], [-420739200000, 2150], [-418147200000, 1974.9999999999995], [-383932800000, 1806.6836642678961], [-381340800000, 1850], [-378662400000, 1850], [-375984000000, 1900], [-373564800000, 2208.1742813390774], [-370886400000, 2100], [-368294400000, 2300], [-365616000000, 2420], [-363024000000, 2430.2546446549427], [-360345600000, 2270.0000000000005], [-357667200000, 2150], [-355075200000, 1975], [-226166400000, 1415.1934690590779], [-223574400000, 1463.804195426381], [-220896000000, 1629.6835230806355], [-218217600000, 1670.6586732318576], [-215798400000, 1950.8650133893266], [-213120000000, 2025.1571325707534], [-210528000000, 2282.3407241004584], [-207849600000, 2306.146565971952], [-205257600000, 2248.5028113332564], [-202579200000, 2195.9722115489176], [-199900800000, 2048.943122712088], [-197308800000, 1909.530320895925], [-163008000000, 1259.822385665771], [-160416000000, 1269.36018665332], [-157737600000, 1788.184484490361], [-155059200000, 1900], [-152640000000, 1987.636434425], [-149961600000, 2054.0129112455243], [-147369600000, 2277.9958455007545], [-144691200000, 2184.5943327184264], [-142099200000, 2113.845970510156], [-139420800000, 1977.3865602227438], [-136742400000, 1846.3686972546475], [-134150400000, 1713.0694784674263], [-99936000000, 1545.5628098369955], [-97344000000, 1626.0900536739698], [-94665600000, 1760.3403556767087], [-91987200000, 1868.484186423054], [-89568000000, 2000], [-86889600000, 2100], [-84297600000, 2205.250473305693], [-81619200000, 2310.2516540943643], [-79027200000, 2400.779247457748], [-76348800000, 2269.9999999999995], [-73670400000, 2138.5949781316126], [-71078400000, 1975], [-36777600000, 1428.1851001258094], [-34185600000, 1424.723189230479], [-31507200000, 1490.2312707283677], [-28828800000, 1626.752098706418], [-26409600000, 1744.5434492168465], [-23731200000, 1899.2516326181221], [-21139200000, 2220.8884938390192], [-18460800000, 2420], [-15868800000, 2340.1647498014195], [-13190400000, 2270], [-10512000000, 2150], [-7920000000, 1975], [-5241600000, 1850], [-2649600000, 1850], [28800000, 1850], [2707200000, 1900], [5126400000, 2000], [7804800000, 2100], [10396800000, 2161.386748718372], [13075200000, 2060.638672231599], [15667200000, 1916.8774545570625], [18345600000, 1772.7429555628744], [21024000000, 1596.96689142045], [23616000000, 1428.9257593522093], [26294400000, 1369.5210144772188], [28886400000, 1466.4634290854108], [31564800000, 1592.255731154013], [34243200000, 1815.0115833650048], [36662400000, 1944.9727355991427], [39340800000, 2100], [41932800000, 2268.42366063641], [44611200000, 2302.9967199961598], [47203200000, 2311.609626351704], [49881600000, 2270], [52560000000, 2150], [55152000000, 1975.0000000000002], [120988800000, 1692.42188705591], [123580800000, 1850], [126259200000, 1850], [128937600000, 1900], [131356800000, 2000], [134035200000, 2100], [136627200000, 2300], [139305600000, 2383.8642406856357], [141897600000, 2367.4432339418972], [144576000000, 2270], [147254400000, 2150], [149846400000, 1975], [152524800000, 1850], [155116800000, 1843.4598455521957], [157795200000, 1850], [160473600000, 1871.2959315976764], [162892800000, 1965.1356731414623], [165571200000, 2100], [168163200000, 2224.7267276111875], [170841600000, 2320.563198662463], [173433600000, 2431.4596258410074], [176112000000, 2270], [178790400000, 2149.9999999999995], [181382400000, 1975], [373449600000, 1241.4994332560282], [376041600000, 1484.4326511968413], [378720000000, 1807.6519615133905], [381398400000, 1896.5384497329762], [383817600000, 2000], [386496000000, 2100], [389088000000, 2300], [391766400000, 2317.827169596444], [394358400000, 2187.3578334552903], [397036800000, 2163.2798276013855], [399715200000, 2138.8872982703283], [402307200000, 1975], [404985600000, 1849.9999999999998], [407577600000, 1850], [410256000000, 1850], [412934400000, 1900], [415353600000, 2000], [418032000000, 2100], [420624000000, 2300], [423302400000, 2420], [425894400000, 2447], [428572800000, 2270], [431251200000, 2150], [433843200000, 1975], [436521600000, 1850], [439113600000, 1850], [441792000000, 1850], [444470400000, 1900], [446976000000, 1996.7366353712937], [449654400000, 2100], [452246400000, 2217.6119610357614], [454924800000, 2183.315329659132], [457516800000, 2124.2652814817266], [460195200000, 1988.7868550547462], [462873600000, 1846.178027656452], [465465600000, 1677.0721804104764], [499680000000, 1238.1677681621838], [502272000000, 1207.4196903446966], [504950400000, 1213.9046199715642], [507628800000, 1333.870893698451], [510048000000, 1807.3715808190948], [512726400000, 2100], [515318400000, 2122.0582931275612], [517996800000, 2018.8355583014436], [520588800000, 1907.6219344278315], [523267200000, 1731.8326611765056], [525945600000, 1556.9252356790869], [528537600000, 1390.8888005568406], [783676800000, 971.8444437642389], [786268800000, 958.0537940241618], [788947200000, 944.0670895317306], [791625600000, 1344.794174264436], [794044800000, 1598.4891153592107], [796723200000, 2031.8177661671352], [799315200000, 2269.8850203844954], [801993600000, 2345.197510560271], [804585600000, 2330.394740403572], [807264000000, 2270], [809942400000, 2150], [812534400000, 1975], [815212800000, 1850], [817804800000, 1821.3404988755242], [820483200000, 1849.9999999999998], [823161600000, 1900.0000000000002], [825667200000, 2000], [828345600000, 2100], [830937600000, 2277.874768623321], [833616000000, 2233.7378084315387], [836208000000, 2167.9045966860995], [838886400000, 2030.4709514792798], [841564800000, 1882.6709737402327], [844156800000, 1746.3030725752562], [846835200000, 1683.1836106281428], [849427200000, 1718.7523386996495], [852105600000, 1850], [854784000000, 1900], [857203200000, 1990.4408221828824], [859881600000, 2081.9979369023627], [862473600000, 2181.835960533022], [865152000000, 1995.1943746765191], [867744000000, 1826.051641943439], [870422400000, 1675.7708753408806], [873100800000, 1499.0493027943965], [875692800000, 1372.5132921958161], [878371200000, 1313.0181752940593], [880963200000, 1305.7442956403372], [883641600000, 1338.303383583556], [886320000000, 1648.993893672297], [888739200000, 1971.535626741254], [891417600000, 2100], [894009600000, 2295.5724488973256], [896688000000, 2420], [899280000000, 2447], [901958400000, 2270], [904636800000, 2150], [907228800000, 1975], [909907200000, 1850], [912499200000, 1850], [915177600000, 1850], [917856000000, 1900], [920275200000, 2000], [922953600000, 2100], [925545600000, 2299.8805487206073], [928224000000, 2339.7516610243606], [930816000000, 2360.4623153939197], [933494400000, 2270], [936172800000, 2135.8543367652987], [938764800000, 1975]]
                    }]
                }, {
                    "annual_filters": [{
                        "period_months": ["Dec", "Jan", "Feb"],
                        "annual_period": "Long Term<br>",
                        "computed_statistics": [{
                            "statistic": "Averages",
                            "statistically_computed_time_series_monthly": [["Jan", 1463.1903795124244], ["Feb", 1574.4217954436658], ["Dec", 1400.7085019067779]],
                            "statistic_aggregate": 1481.7161086962456
                        }, {
                            "statistic": "Minimums",
                            "statistically_computed_time_series_monthly": [["Jan", 382.80021625926156], ["Feb", 385.65028705529244], ["Dec", 341.7657018054722]],
                            "statistic_aggregate": 385.7211407552627
                        }],
                        "aggregate_ts": [[1922, 1760.1087037104808], [1923, 1651.41663709303], [1924, 1212.2748112945756], [1925, 792.8967987109619], [1926, 1061.035576848499], [1927, 1477.4779973357565], [1928, 1771.2158699858944], [1929, 1127.8548503010222], [1930, 978.6415632264811], [1931, 783.095640580828], [1932, 415.2902439992528], [1933, 385.7211407552627], [1934, 484.878829880266], [1935, 396.97276534093857], [1936, 624.6520240767901], [1937, 661.066154897126], [1938, 1463.168421240971], [1939, 1862.1448801097401], [1940, 1229.8485315381274], [1941, 1708.4853102264096], [1942, 1916.6666666666667], [1943, 1916.6666666666667], [1944, 1562.7553946614923], [1945, 1355.987316746552], [1946, 1533.6642383867254], [1947, 1410.3871160528372], [1948, 1289.25094661036], [1949, 1418.5706461387545], [1950, 1340.6850920806464], [1951, 1842.3810230039414], [1952, 1769.083480152096], [1953, 1916.6666666666667], [1954, 1916.6666666666667], [1955, 1688.7869289681232], [1956, 1837.5087250428599], [1957, 1899.7789543763986], [1958, 1986.0580937796924], [1959, 1915.1114771587243], [1960, 1380.817288176965], [1961, 1380.6792517899692], [1962, 1367.550284816296], [1963, 1750.40240323394], [1964, 1873.6869396622035], [1965, 1891.9403063051204], [1966, 1847.01789681268], [1967, 1876.2748473665877], [1968, 1916.6666666666667], [1969, 1620.508939550544], [1970, 1916.6666666666667], [1971, 1784.0800167060534], [1972, 1916.6666666666667], [1973, 1856.2695883974854], [1974, 1916.6666666666667], [1975, 1895.4772015797128], [1976, 1750.7208792206895], [1977, 989.2501788475116], [1978, 774.2871728320396], [1979, 1250.9577432662738], [1980, 1559.3387491796614], [1981, 1600.5767879183477], [1982, 1901.3968037487891], [1983, 1916.6666666666667], [1984, 1915.5788784570977], [1985, 1795.4755918046694], [1986, 1451.7156981630367], [1987, 1356.0473622724192], [1988, 1137.2926844625008], [1989, 1011.200992718714], [1990, 1045.3960890290507], [1991, 868.5685230418426], [1992, 713.7846260337237], [1993, 884.4693027747684], [1994, 1540.7277361414926], [1995, 1295.7834597184592], [1996, 1916.6666666666667], [1997, 1913.480274060961], [1998, 1652.944301332369], [1999, 1916.6666666666667], [2000, 1916.6666666666667], [2001, 1694.4938028082145], [2002, 1508.209488755636], [2003, 1666.062637794691]],
                        "month_period": "Dec - Feb",
                        "discrete_ts": [[-1522684800000, 2000], [-1520006400000, 1781.2779198935496], [-1517414400000, 1753.6804118542636], [-1514736000000, 1755.0900871780748], [-1512057600000, 1751.3772037354065], [-1509638400000, 1773.8588202179612], [-1506960000000, 1823.377987689489], [-1504368000000, 1941.4609885062093], [-1501689600000, 2020.0440770669159], [-1499097600000, 2066.1119064423083], [-1496419200000, 1954.1479165086514], [-1493740800000, 1805.2042476400295], [-1491148800000, 1661.2773908219176], [-1488470400000, 1606.7084586644032], [-1485878400000, 1608.382709207328], [-1483200000000, 1626.1605153984813], [-1480521600000, 1652.712911272988], [-1478102400000, 1675.3764846076203], [-1475424000000, 1727.773694560605], [-1472832000000, 1849.1059153816168], [-1470153600000, 1807.3192820072009], [-1467561600000, 1768.4308190291438], [-1464883200000, 1629.2816148891263], [-1462204800000, 1452.0332137551318], [-1459612800000, 1324.9955098605428], [-1456934400000, 1273.0393133452374], [-1454342400000, 1241.216962645222], [-1451664000000, 1228.6474814190387], [-1448985600000, 1174.3709080719398], [-1446480000000, 1233.8060443927482], [-1443801600000, 1226.1576898292942], [-1441209600000, 1196.8250792215842], [-1438531200000, 1114.946371920779], [-1435939200000, 1056.6768368615362], [-1433260800000, 955.040820999469], [-1430582400000, 753.6694060036803], [-1427990400000, 656.1455568884302], [-1425312000000, 648.5460301669519], [-1422720000000, 725.1923884395601], [-1420041600000, 767.6329276917554], [-1417363200000, 670.2045698666918], [-1414944000000, 940.8528985744387], [-1412265600000, 1079.0381733857957], [-1409673600000, 1377.3215105073523], [-1406995200000, 1362.6265824847649], [-1404403200000, 1288.797127048902], [-1401724800000, 1178.2775759846065], [-1399046400000, 1083.3623095951573], [-1396454400000, 1013.3093774665689], [-1393776000000, 990.4506876951313], [-1391184000000, 987.8705286434057], [-1388505600000, 1008.918715195414], [-1385827200000, 1013.6200188926917], [-1383408000000, 1160.5679964573915], [-1380729600000, 1274.7702893077444], [-1378137600000, 1496.197791645647], [-1375459200000, 1393.2008811548578], [-1372867200000, 1325.1603310981627], [-1370188800000, 1194.4008823490317], [-1367510400000, 1079.9959417808032], [-1364918400000, 983.9731555439773], [-1362240000000, 960.1326498735675], [-1359648000000, 1116.7589119637319], [-1356969600000, 1315.183579704499], [-1354291200000, 1439.531204331202], [-1351872000000, 1677.7192079715692], [-1349193600000, 1861.5459264712615], [-1346601600000, 2088.4247493615694], [-1343923200000, 2077.8308532631518], [-1341331200000, 2103.245767861831], [-1338652800000, 1981.0513926104738], [-1335974400000, 1841.3641872928786], [-1333382400000, 1704.082788894543], [-1330704000000, 1643.8958905010575], [-1328112000000, 1671.9601707210104], [-1325433600000, 1696.2755811122006], [-1322755200000, 1753.9439039710292], [-1320249600000, 1863.4281248744537], [-1317571200000, 2069.8537970945586], [-1314979200000, 2231.2259550071644], [-1312300800000, 2169.0932773296686], [-1309708800000, 2076.4311194212682], [-1307030400000, 1924.6657563707197], [-1304352000000, 1715.3406266376276], [-1301760000000, 1483.4519856483066], [-1299081600000, 1312.8712571238086], [-1296489600000, 1253.1201636390144], [-1293811200000, 1246.431380294254], [-1291132800000, 1054.9794698826697], [-1288713600000, 1082.1537007261425], [-1286035200000, 1133.189692136364], [-1283443200000, 1148.7704779670175], [-1280764800000, 1192.2902611082984], [-1278172800000, 1182.447988151475], [-1275494400000, 1092.3775450983082], [-1272816000000, 968.4198004622617], [-1270224000000, 871.7413135764752], [-1267545600000, 841.9139214238518], [-1264953600000, 826.2504542244756], [-1262275200000, 955.855283662534], [-1259596800000, 939.8727049121168], [-1257177600000, 1040.1967011047927], [-1254499200000, 1167.5788383251254], [-1251907200000, 1267.951509581913], [-1249228800000, 1179.1026690309377], [-1246636800000, 1141.7113883064756], [-1243958400000, 1050.0604163349622], [-1241280000000, 926.7889882995834], [-1238688000000, 832.6001104423804], [-1236009600000, 803.1584822209412], [-1233417600000, 785.3389361530847], [-1230739200000, 768.9844730084858], [-1228060800000, 779.8352761829938], [-1225641600000, 800.4671725510046], [-1222963200000, 860.031549172891], [-1220371200000, 883.1785257024842], [-1217692800000, 843.8397306717202], [-1215100800000, 820.670881588105], [-1212422400000, 658.9188386546883], [-1209744000000, 588.5524789966629], [-1207152000000, 500], [-1204473600000, 456.9261914946402], [-1201881600000, 410.24212392816486], [-1199203200000, 413.2055872261009], [-1196524800000, 405.33072152159593], [-1194019200000, 427.3344232500616], [-1191340800000, 580.110307632881], [-1188748800000, 669.8908310903988], [-1186070400000, 693.2401864531871], [-1183478400000, 700], [-1180800000000, 599.794976667157], [-1178121600000, 491.08564972783944], [-1175529600000, 426.0489077995353], [-1172851200000, 402.80953452501484], [-1170259200000, 391.600194902158], [-1167580800000, 387.2772609676166], [-1164902400000, 384.23587424287905], [-1162483200000, 385.65028705529244], [-1159804800000, 500.00000000000006], [-1157212800000, 651.9983122706526], [-1154534400000, 639.7495940556751], [-1151942400000, 758.5797426077723], [-1149264000000, 690.4580101475688], [-1146585600000, 625.4964507721056], [-1143993600000, 545.8252780346211], [-1141315200000, 499.99999999999994], [-1138723200000, 404.1041138607815], [-1136044800000, 413.9918286455671], [-1133366400000, 475.25394951902734], [-1130947200000, 565.3907114762035], [-1128268800000, 722.4179065542122], [-1125676800000, 806.3666639525917], [-1122998400000, 762.5559530297812], [-1120406400000, 727.2215353846308], [-1117728000000, 633.9284178860213], [-1115049600000, 412.3185365261456], [-1112457600000, 272.8109780826167], [-1109779200000, 240], [-1107187200000, 305.07353805621494], [-1104508800000, 341.7657018054722], [-1101830400000, 382.80021625926156], [-1099411200000, 466.35237795808195], [-1096732800000, 527.5588104112819], [-1094140800000, 741.5000907839534], [-1091462400000, 779.0394389095509], [-1088870400000, 786.565819023345], [-1086192000000, 702.3096033839132], [-1083513600000, 581.9454544770608], [-1080921600000, 518.6581211469536], [-1078243200000, 504.65401569456435], [-1075651200000, 499.39386637429476], [-1072972800000, 502.48119810091646], [-1070294400000, 615.5882926168746], [-1067788800000, 755.8865815125791], [-1065110400000, 872.5181454312384], [-1062518400000, 1049.1404727584884], [-1059840000000, 979.5303238091273], [-1057248000000, 942.1973141782462], [-1054569600000, 891.7434328153151], [-1051891200000, 796.3735251842928], [-1049299200000, 715.611659060015], [-1046620800000, 685.5292512816377], [-1044028800000, 674.759687268816], [-1041350400000, 665.1738540490086], [-1038672000000, 657.0160151882537], [-1036252800000, 661.0085954541157], [-1033574400000, 766.0877551038473], [-1030982400000, 991.4719916480095], [-1028304000000, 1137.5337589970086], [-1025712000000, 1242.417033210354], [-1023033600000, 1176.537900689841], [-1020355200000, 1081.1581175115175], [-1017763200000, 999.7469964242515], [-1015084800000, 978.8381422693712], [-1012492800000, 1126.4522949568352], [-1009814400000, 1345.8281291439516], [-1007136000000, 1443.3720825180628], [-1004716800000, 1600.305052060898], [-1002038400000, 1861.5814618503991], [-999446400000, 2169.145772342404], [-996768000000, 2342.985100062149], [-994176000000, 2287.9458277532203], [-991497600000, 2258.3797038243306], [-988819200000, 2149.9999999999995], [-986227200000, 1975], [-983548800000, 1850], [-980956800000, 1849.9999999999998], [-978278400000, 1850], [-975600000000, 1861.1571671160832], [-973180800000, 1875.2774732131365], [-970502400000, 1983.2145393876838], [-967910400000, 2058.507240661381], [-965232000000, 1953.0821845671883], [-962640000000, 1851.6018002881897], [-959961600000, 1619.5513992716624], [-957283200000, 1375.575410553875], [-954691200000, 1143.0160695673062], [-952012800000, 1030.2597226684816], [-949420800000, 939.3860371724832], [-946742400000, 1003.6413151749547], [-944064000000, 1180.0124902532855], [-941558400000, 1505.8917891861422], [-938880000000, 1808.0757492054472], [-936288000000, 2034.3993789250414], [-933609600000, 1971.502365511925], [-931017600000, 1855.7960305040394], [-928339200000, 1707.0209981250784], [-925660800000, 1531.0564987448201], [-923068800000, 1407.7414988330247], [-920390400000, 1354.9007080777938], [-917798400000, 1338.486465842741], [-915120000000, 1477.791260243608], [-912441600000, 1698.487072790161], [-910022400000, 1949.1775976454603], [-907344000000, 2100], [-904752000000, 2300], [-902073600000, 2420], [-899481600000, 2447], [-896803200000, 2270], [-894124800000, 2150], [-891532800000, 1975], [-888854400000, 1850], [-886262400000, 1850], [-883584000000, 1850], [-880905600000, 1900], [-878486400000, 2000], [-875808000000, 2078.5401342424584], [-873216000000, 2285.2581215952086], [-870537600000, 2344.7953085222416], [-867945600000, 2416.5226140655363], [-865267200000, 2270], [-862588800000, 2150], [-859996800000, 1974.9999999999995], [-857318400000, 1850.0000000000002], [-854726400000, 1850], [-852048000000, 1850], [-849369600000, 1900], [-846950400000, 2000], [-844272000000, 2100], [-841680000000, 2283.155661244806], [-839001600000, 2173.318610935777], [-836409600000, 2100.1013478625755], [-833731200000, 1955.632799582456], [-831052800000, 1810.9238053951467], [-828460800000, 1640.4268782084089], [-825782400000, 1583.0498075411322], [-823190400000, 1563.3481282303221], [-820512000000, 1551.4776864152889], [-817833600000, 1549.824109894487], [-815328000000, 1586.9643876747014], [-812649600000, 1648.8764145922105], [-810057600000, 1688.084247116321], [-807379200000, 1746.5857249883015], [-804787200000, 1697.7670848605617], [-802108800000, 1562.0190595213658], [-799430400000, 1356.0359235904748], [-796838400000, 1230.6450065466317], [-794160000000, 1170.8958180390327], [-791568000000, 1200.9021461401219], [-788889600000, 1268.5950581095], [-786211200000, 1317.0731685737105], [-783792000000, 1482.293723556446], [-781113600000, 1536.1463926975127], [-778521600000, 1673.6756571862545], [-775843200000, 1638.1914234749438], [-773251200000, 1578.7286112414731], [-770572800000, 1435.2377352099325], [-767894400000, 1314.727781697121], [-765302400000, 1186.7391616993718], [-762624000000, 1182.1171581798772], [-760032000000, 1237.1010993067373], [-757353600000, 1428.3252334878025], [-754675200000, 1558.8276930541163], [-752256000000, 1613.8397886182574], [-749577600000, 1736.0966449765817], [-746985600000, 1954.179482968568], [-744307200000, 1949.495681539694], [-741715200000, 1883.4532073000112], [-739036800000, 1751.9603430825946], [-736358400000, 1580.074281546491], [-733766400000, 1414.503589227336], [-731088000000, 1361.5524083091002], [-728496000000, 1370.047891375701], [-725817600000, 1390.9796145345913], [-723139200000, 1390.0896534250564], [-720720000000, 1450.0920801988643], [-718041600000, 1569.141537527109], [-715449600000, 1656.4301367957157], [-712771200000, 1566.3096981981807], [-710179200000, 1558.686017191306], [-707500800000, 1417.9574046398056], [-704822400000, 1246.4140874348693], [-702230400000, 1122.0522776600983], [-699552000000, 1138.0590286073518], [-696960000000, 1151.9725556729607], [-694281600000, 1151.3263037633462], [-691603200000, 1346.9684338173456], [-689097600000, 1369.4581022503878], [-686419200000, 1397.7868601833281], [-683827200000, 1584.9601658449133], [-681148800000, 1579.2708519226426], [-678556800000, 1658.8698255686163], [-675878400000, 1623.16890440377], [-673200000000, 1496.4658604552071], [-670608000000, 1449.5105015648498], [-667929600000, 1403.3765416117853], [-665337600000, 1410.2778435636649], [-662659200000, 1412.328045577411], [-659980800000, 1405.746830779409], [-657561600000, 1437.6370620594428], [-654883200000, 1653.8513002716847], [-652291200000, 1903.8577882414297], [-649612800000, 1904.8779589209141], [-647020800000, 1854.2977940760622], [-644342400000, 1709.3209513287693], [-641664000000, 1535.4432468446612], [-639072000000, 1408.1935310716099], [-636393600000, 1353.6420125275204], [-633801600000, 1323.635593000406], [-631123200000, 1307.8115251808376], [-628444800000, 1329.016767868543], [-626025600000, 1385.2269831925585], [-623347200000, 1506.290593182369], [-620755200000, 1664.3050868440525], [-618076800000, 1673.686358140288], [-615484800000, 1655.176886075836], [-612806400000, 1547.8836677389743], [-610128000000, 1402.1213457938682], [-607536000000, 1275.8244235577165], [-604857600000, 1345.2812464943854], [-602265600000, 1460.9624688441356], [-599587200000, 1718.5833139788476], [-596908800000, 1808.5597550329765], [-594489600000, 2000], [-591811200000, 2100], [-589219200000, 2285.6210802982428], [-586540800000, 2217.5091071736506], [-583948800000, 2133.2400239159238], [-581270400000, 1985.6232395973063], [-578592000000, 1764.3155817750212], [-576000000000, 1594.8513736444622], [-573321600000, 1510.5784833488797], [-570729600000, 1518.7352008707492], [-568051200000, 1666.1410492783], [-565372800000, 1725.5456059183966], [-562867200000, 1915.5637852595908], [-560188800000, 2081.5235453921127], [-557596800000, 2299.9999999999995], [-554918400000, 2410.963686814417], [-552326400000, 2447], [-549648000000, 2270], [-546969600000, 2150], [-544377600000, 1974.9999999999998], [-541699200000, 1850], [-539107200000, 1845.3146506149892], [-536428800000, 1850], [-533750400000, 1900], [-531331200000, 2000], [-528652800000, 2100], [-526060800000, 2300], [-523382400000, 2259.9112123923715], [-520790400000, 2353.448239660465], [-518112000000, 2270], [-515433600000, 2139.699638927823], [-512841600000, 1975.0000000000002], [-510163200000, 1850], [-507571200000, 1850.0000000000002], [-504892800000, 1850], [-502214400000, 1900.0000000000002], [-499795200000, 2000], [-497116800000, 2100], [-494524800000, 2300], [-491846400000, 2271.5030161556515], [-489254400000, 2203.9284707403995], [-486576000000, 2067.065926783014], [-483897600000, 1854.7149140411384], [-481305600000, 1692.863816871314], [-478627200000, 1604.9914122974592], [-476035200000, 1625.8888175989666], [-473356800000, 1666.9705269470671], [-470678400000, 1685.2655131310003], [-468259200000, 1714.1247468263025], [-465580800000, 1744.2625702022733], [-462988800000, 1790.4049529957765], [-460310400000, 1804.3706726705818], [-457718400000, 1789.739017439078], [-455040000000, 1681.0911841096988], [-452361600000, 1504.2761531315343], [-449769600000, 1377.12351822381], [-447091200000, 1317.6322586792123], [-444499200000, 1304.4166129956288], [-441820800000, 1612.5261751285796], [-439142400000, 1900], [-436636800000, 2000], [-433958400000, 2100], [-431366400000, 2300], [-428688000000, 2402.4665039813262], [-426096000000, 2321.222056407487], [-423417600000, 2270], [-420739200000, 2150], [-418147200000, 1974.9999999999995], [-415468800000, 1850], [-412876800000, 1849.9999999999998], [-410198400000, 1849.9999999999998], [-407520000000, 1849.336863129196], [-405100800000, 2000.0000000000002], [-402422400000, 2100], [-399830400000, 2218.3702490417722], [-397152000000, 2234.431188934835], [-394560000000, 2213.9086518272306], [-391881600000, 2069.73159988299], [-389203200000, 1925.6784213154615], [-386611200000, 1761.6258936629063], [-383932800000, 1806.6836642678961], [-381340800000, 1850], [-378662400000, 1850], [-375984000000, 1900], [-373564800000, 2208.1742813390774], [-370886400000, 2100], [-368294400000, 2300], [-365616000000, 2420], [-363024000000, 2430.2546446549427], [-360345600000, 2270.0000000000005], [-357667200000, 2150], [-355075200000, 1975], [-352396800000, 1850], [-349804800000, 1849.9999999999998], [-347126400000, 1845.3344314761728], [-344448000000, 1900], [-342028800000, 2000.0000000000002], [-339350400000, 2100], [-336758400000, 2271.801367418244], [-334080000000, 2170.725303667815], [-331488000000, 2053.05568815621], [-328809600000, 1825.4420296257313], [-326131200000, 1643.149071644791], [-323539200000, 1448.5693716909464], [-320860800000, 1389.6792380986888], [-318268800000, 1353.7340360012167], [-315590400000, 1335.1730002469928], [-312912000000, 1335.3754681879914], [-310406400000, 1471.9033960959105], [-307728000000, 1659.9483037303985], [-305136000000, 1781.2377151455137], [-302457600000, 1726.2709209823952], [-299865600000, 1722.7798681783931], [-297187200000, 1594.3194664669206], [-294508800000, 1420.457762059304], [-291916800000, 1293.1404197257407], [-289238400000, 1235.4572562609392], [-286646400000, 1222.1695919124277], [-283968000000, 1287.4794351518224], [-281289600000, 1331.6261117229838], [-278870400000, 1522.9322084951007], [-276192000000, 1620.5704098741398], [-273600000000, 1810.6559765816169], [-270921600000, 1838.7485240490385], [-268329600000, 1873.6470167253551], [-265651200000, 1727.1627430886988], [-262972800000, 1554.3067508451816], [-260380800000, 1387.8205706452818], [-257702400000, 1335.1380487376973], [-255110400000, 1313.0363796367806], [-252432000000, 1323.5275480995992], [-249753600000, 1340.6342902915464], [-247334400000, 1438.4890160577422], [-244656000000, 1497.0098445619867], [-242064000000, 1822.8615483102767], [-239385600000, 1788.9830278095676], [-236793600000, 1756.0843020055493], [-234115200000, 1607.9045655795728], [-231436800000, 1438.4829411570968], [-228844800000, 1313.4652992649212], [-226166400000, 1415.1934690590779], [-223574400000, 1463.804195426381], [-220896000000, 1629.6835230806355], [-218217600000, 1670.6586732318576], [-215798400000, 1950.8650133893266], [-213120000000, 2025.1571325707534], [-210528000000, 2282.3407241004584], [-207849600000, 2306.146565971952], [-205257600000, 2248.5028113332564], [-202579200000, 2195.9722115489176], [-199900800000, 2048.943122712088], [-197308800000, 1909.530320895925], [-194630400000, 1846.3826722413748], [-192038400000, 1849.9999999999998], [-189360000000, 1850], [-186681600000, 1887.9723147183158], [-184176000000, 1883.088504268295], [-181497600000, 1912.4108301411948], [-178905600000, 1947.071960840798], [-176227200000, 1839.2845925422828], [-173635200000, 1809.2139617778168], [-170956800000, 1597.3840765732348], [-168278400000, 1428.507015229122], [-165686400000, 1311.070693439766], [-163008000000, 1259.822385665771], [-160416000000, 1269.36018665332], [-157737600000, 1788.184484490361], [-155059200000, 1900], [-152640000000, 1987.636434425], [-149961600000, 2054.0129112455243], [-147369600000, 2277.9958455007545], [-144691200000, 2184.5943327184264], [-142099200000, 2113.845970510156], [-139420800000, 1977.3865602227438], [-136742400000, 1846.3686972546475], [-134150400000, 1713.0694784674263], [-131472000000, 1652.5048995124068], [-128880000000, 1736.4424836579053], [-126201600000, 1773.6720829469834], [-123523200000, 1850.9631268348069], [-121104000000, 1916.4184806562496], [-118425600000, 2100], [-115833600000, 2300], [-113155200000, 2300.6042666254175], [-110563200000, 2225.687726289895], [-107884800000, 2009.9996487874926], [-105206400000, 1796.4671466925795], [-102614400000, 1634.5098689340603], [-99936000000, 1545.5628098369955], [-97344000000, 1626.0900536739698], [-94665600000, 1760.3403556767087], [-91987200000, 1868.484186423054], [-89568000000, 2000], [-86889600000, 2100], [-84297600000, 2205.250473305693], [-81619200000, 2310.2516540943643], [-79027200000, 2400.779247457748], [-76348800000, 2269.9999999999995], [-73670400000, 2138.5949781316126], [-71078400000, 1975], [-68400000000, 1850], [-65808000000, 1850], [-63129600000, 1850], [-60451200000, 1900], [-57945600000, 2000], [-55267200000, 2100], [-52675200000, 2191.009019304407], [-49996800000, 2077.487701506115], [-47404800000, 1971.0970102032252], [-44726400000, 1820.5758791420346], [-42048000000, 1647.91583283241], [-39456000000, 1481.7815168854245], [-36777600000, 1428.1851001258094], [-34185600000, 1424.723189230479], [-31507200000, 1490.2312707283677], [-28828800000, 1626.752098706418], [-26409600000, 1744.5434492168465], [-23731200000, 1899.2516326181221], [-21139200000, 2220.8884938390192], [-18460800000, 2420], [-15868800000, 2340.1647498014195], [-13190400000, 2270], [-10512000000, 2150], [-7920000000, 1975], [-5241600000, 1850], [-2649600000, 1850], [28800000, 1850], [2707200000, 1900], [5126400000, 2000], [7804800000, 2100], [10396800000, 2161.386748718372], [13075200000, 2060.638672231599], [15667200000, 1916.8774545570625], [18345600000, 1772.7429555628744], [21024000000, 1596.96689142045], [23616000000, 1428.9257593522093], [26294400000, 1369.5210144772188], [28886400000, 1466.4634290854108], [31564800000, 1592.255731154013], [34243200000, 1815.0115833650048], [36662400000, 1944.9727355991427], [39340800000, 2100], [41932800000, 2268.42366063641], [44611200000, 2302.9967199961598], [47203200000, 2311.609626351704], [49881600000, 2270], [52560000000, 2150], [55152000000, 1975.0000000000002], [57830400000, 1850], [60422400000, 1849.9999999999998], [63100800000, 1850], [65779200000, 1900], [68284800000, 2000.0000000000002], [70963200000, 2100], [73555200000, 2233.747818808155], [76233600000, 2163.0551072842095], [78825600000, 2112.663296194655], [81504000000, 1964.1649917152063], [84182400000, 1790.0463124887483], [86774400000, 1621.7781439663013], [89452800000, 1568.128956171881], [92044800000, 1604.0208042958207], [94723200000, 1707.9864349820823], [97401600000, 1860.8223302103736], [99820800000, 2000], [102499200000, 2100], [105091200000, 2287.7040645885572], [107769600000, 2336.913207084849], [110361600000, 2283.3535459803284], [113040000000, 2143.5123270242116], [115718400000, 1920.3471698105482], [118310400000, 1752.2474051961096], [120988800000, 1692.42188705591], [123580800000, 1850], [126259200000, 1850], [128937600000, 1900], [131356800000, 2000], [134035200000, 2100], [136627200000, 2300], [139305600000, 2383.8642406856357], [141897600000, 2367.4432339418972], [144576000000, 2270], [147254400000, 2150], [149846400000, 1975], [152524800000, 1850], [155116800000, 1843.4598455521957], [157795200000, 1850], [160473600000, 1871.2959315976764], [162892800000, 1965.1356731414623], [165571200000, 2100], [168163200000, 2224.7267276111875], [170841600000, 2320.563198662463], [173433600000, 2431.4596258410074], [176112000000, 2270], [178790400000, 2149.9999999999995], [181382400000, 1975], [184060800000, 1850], [186652800000, 1850], [189331200000, 1850], [192009600000, 1685.5225696775158], [194515200000, 1716.6400679845533], [197193600000, 1764.9300920801936], [199785600000, 1846.2416164706385], [202464000000, 1820.655675440109], [205056000000, 1770.4368358461827], [207734400000, 1551.7441818722475], [210412800000, 1355.2434632932006], [213004800000, 1230.4553132281221], [215683200000, 1171.520794981765], [218275200000, 1026.0359645827386], [220953600000, 1000.0000000000001], [223632000000, 988.2117350968107], [226051200000, 979.5388014457244], [228729600000, 965.6829236636772], [231321600000, 935.1324238987725], [234000000000, 882.7054326770452], [236592000000, 845.2627648344788], [239270400000, 618.1066032164853], [241948800000, 373.658874785528], [244540800000, 283.3105609031978], [247219200000, 240], [249811200000, 267.29062040796197], [252489600000, 458.20667482838076], [255168000000, 843.2943173232236], [257587200000, 1021.3605263445141], [260265600000, 1294.6939352668437], [262857600000, 1486.2069198797105], [265536000000, 1490.642042083278], [268128000000, 1435.6856146155947], [270806400000, 1373.108986491101], [273484800000, 1287.2509813134566], [276076800000, 1290.851836819887], [278755200000, 1239.3542527670807], [281347200000, 1232.9815047308384], [284025600000, 1222.5966849749004], [286704000000, 1238.064150407632], [289123200000, 1292.2123944162895], [291801600000, 1435.1459264016394], [294393600000, 1550.246646506078], [297072000000, 1626.6152242547976], [299664000000, 1599.5829258951428], [302342400000, 1491.0407619604357], [305020800000, 1346.4918092887258], [307612800000, 1220.5616665430962], [310291200000, 1188.280403124109], [312883200000, 1250.7880333265157], [315561600000, 1317.3233744492197], [318240000000, 1536.1054675287262], [320745600000, 1824.5874055610384], [323424000000, 1956.260619406819], [326016000000, 2115.719776464171], [328694400000, 2020.6670735760822], [331286400000, 1922.5272531499909], [333964800000, 1799.6006383710696], [336643200000, 1655.289095794577], [339235200000, 1517.0474914237195], [341913600000, 1461.8665109484934], [344505600000, 1452.3257716347462], [347184000000, 1484.0168443702325], [349862400000, 1589.882940880852], [352281600000, 1727.8305785039588], [354960000000, 1851.982657031244], [357552000000, 1960.2606404149876], [360230400000, 1892.2222890295457], [362822400000, 1836.293862922134], [365500800000, 1608.4896079907996], [368179200000, 1423.6135748352829], [370771200000, 1300.6251673100091], [373449600000, 1241.4994332560282], [376041600000, 1484.4326511968413], [378720000000, 1807.6519615133905], [381398400000, 1896.5384497329762], [383817600000, 2000], [386496000000, 2100], [389088000000, 2300], [391766400000, 2317.827169596444], [394358400000, 2187.3578334552903], [397036800000, 2163.2798276013855], [399715200000, 2138.8872982703283], [402307200000, 1975], [404985600000, 1849.9999999999998], [407577600000, 1850], [410256000000, 1850], [412934400000, 1900], [415353600000, 2000], [418032000000, 2100], [420624000000, 2300], [423302400000, 2420], [425894400000, 2447], [428572800000, 2270], [431251200000, 2150], [433843200000, 1975], [436521600000, 1850], [439113600000, 1850], [441792000000, 1850], [444470400000, 1900], [446976000000, 1996.7366353712937], [449654400000, 2100], [452246400000, 2217.6119610357614], [454924800000, 2183.315329659132], [457516800000, 2124.2652814817266], [460195200000, 1988.7868550547462], [462873600000, 1846.178027656452], [465465600000, 1677.0721804104764], [468144000000, 1618.8523412622096], [470736000000, 1748.4765700179128], [473414400000, 1805.5635507719837], [476092800000, 1770.9771345936147], [478512000000, 1809.8860900484096], [481190400000, 1857.643171527426], [483782400000, 2004.203540804162], [486460800000, 1927.0879556408458], [489052800000, 1881.3097191761183], [491731200000, 1654.963462040902], [494409600000, 1420.1547245661209], [497001600000, 1297.5195075091797], [499680000000, 1238.1677681621838], [502272000000, 1207.4196903446966], [504950400000, 1213.9046199715642], [507628800000, 1333.870893698451], [510048000000, 1807.3715808190948], [512726400000, 2100], [515318400000, 2122.0582931275612], [517996800000, 2018.8355583014436], [520588800000, 1907.6219344278315], [523267200000, 1731.8326611765056], [525945600000, 1556.9252356790869], [528537600000, 1390.8888005568406], [531216000000, 1344.9046245179309], [533808000000, 1326.161035322114], [536486400000, 1319.823878879729], [539164800000, 1332.0209244539349], [541584000000, 1416.2972834835932], [544262400000, 1627.4877688653478], [546854400000, 1773.91089482176], [549532800000, 1730.7704630734631], [552124800000, 1575.2627902448135], [554803200000, 1349.6877801189758], [557481600000, 1138.3250361381008], [560073600000, 975.3621008325935], [562752000000, 931.0732453871975], [565344000000, 902.9062516242365], [568022400000, 1054.9853084352046], [570700800000, 1136.2033490443732], [573206400000, 1220.6893959079243], [575884800000, 1301.1862872228864], [578476800000, 1387.6654142292969], [581155200000, 1332.9122283539843], [583747200000, 1315.3619416416934], [586425600000, 1188.3356471212708], [589104000000, 1067.4671062822724], [591696000000, 975.257134975171], [594374400000, 946.947504132714], [596966400000, 969.0471179298986], [599644800000, 986.8520447139873], [602323200000, 1009.7055777258051], [604742400000, 1037.0453557163496], [607420800000, 1379.6700143747942], [610012800000, 1578.9969248799312], [612691200000, 1470.0547678410076], [615283200000, 1362.1182372380538], [617961600000, 1213.274824220901], [620640000000, 1069.9279774498987], [623232000000, 977.9363394485201], [625910400000, 994.4638664987278], [628502400000, 997.7399316119863], [631180800000, 992.6299527906516], [633859200000, 1058.3996525751272], [636278400000, 1085.1586617213727], [638956800000, 1173.1718643415388], [641548800000, 1212.492069081621], [644227200000, 1180.04931564201], [646819200000, 1198.876865381159], [649497600000, 1137.0269819053544], [652176000000, 1027.20273312275], [654768000000, 935.0942226267782], [657446400000, 906.8080500343825], [660038400000, 892.8814005242438], [662716800000, 877.265392499676], [665395200000, 861.4829208794825], [667814400000, 866.9572557463691], [670492800000, 941.3890731370889], [673084800000, 982.927653027946], [675763200000, 1006.499970461156], [678355200000, 1002.502499237974], [681033600000, 916.2836178326651], [683712000000, 797.5447803274828], [686304000000, 706.1042543674918], [688982400000, 678.0513749488299], [691574400000, 669.0605085516528], [694252800000, 664.0920958242637], [696931200000, 669.1953786777815], [699436800000, 808.0664035991263], [702115200000, 955.6465454275558], [704707200000, 1189.136041056044], [707385600000, 1182.2309050512117], [709977600000, 1157.9822196205469], [712656000000, 1070.8204206796702], [715334400000, 951.7616983042537], [717926400000, 822.4786590992961], [720604800000, 794.331625618805], [723196800000, 780.4149082059811], [725875200000, 799.9866242860874], [728553600000, 857.9608472560632], [730972800000, 995.4604367821547], [733651200000, 1373.4561461344451], [736243200000, 1569.0977918339224], [738921600000, 1738.5439690567564], [741513600000, 1853.1621824686076], [744192000000, 1836.196719617101], [746870400000, 1726.0999071285794], [749462400000, 1590.1101213847512], [752140800000, 1529.8679088242632], [754732800000, 1520.13400109385], [757411200000, 1538.0821586654072], [760089600000, 1525.0856260911855], [762508800000, 1559.0154236678857], [765187200000, 1626.9519450916891], [767779200000, 1671.9005096368248], [770457600000, 1676.143914626903], [773049600000, 1521.6496761492392], [775728000000, 1349.693609439764], [778406400000, 1174.9955650386382], [780998400000, 1000.0000000000002], [783676800000, 971.8444437642389], [786268800000, 958.0537940241618], [788947200000, 944.0670895317306], [791625600000, 1344.794174264436], [794044800000, 1598.4891153592107], [796723200000, 2031.8177661671352], [799315200000, 2269.8850203844954], [801993600000, 2345.197510560271], [804585600000, 2330.394740403572], [807264000000, 2270], [809942400000, 2150], [812534400000, 1975], [815212800000, 1850], [817804800000, 1821.3404988755242], [820483200000, 1849.9999999999998], [823161600000, 1900.0000000000002], [825667200000, 2000], [828345600000, 2100], [830937600000, 2277.874768623321], [833616000000, 2233.7378084315387], [836208000000, 2167.9045966860995], [838886400000, 2030.4709514792798], [841564800000, 1882.6709737402327], [844156800000, 1746.3030725752562], [846835200000, 1683.1836106281428], [849427200000, 1718.7523386996495], [852105600000, 1850], [854784000000, 1900], [857203200000, 1990.4408221828824], [859881600000, 2081.9979369023627], [862473600000, 2181.835960533022], [865152000000, 1995.1943746765191], [867744000000, 1826.051641943439], [870422400000, 1675.7708753408806], [873100800000, 1499.0493027943965], [875692800000, 1372.5132921958161], [878371200000, 1313.0181752940593], [880963200000, 1305.7442956403372], [883641600000, 1338.303383583556], [886320000000, 1648.993893672297], [888739200000, 1971.535626741254], [891417600000, 2100], [894009600000, 2295.5724488973256], [896688000000, 2420], [899280000000, 2447], [901958400000, 2270], [904636800000, 2150], [907228800000, 1975], [909907200000, 1850], [912499200000, 1850], [915177600000, 1850], [917856000000, 1900], [920275200000, 2000], [922953600000, 2100], [925545600000, 2299.8805487206073], [928224000000, 2339.7516610243606], [930816000000, 2360.4623153939197], [933494400000, 2270], [936172800000, 2135.8543367652987], [938764800000, 1975], [941443200000, 1850], [944035200000, 1850], [946713600000, 1850], [949392000000, 1900], [951897600000, 2000], [954576000000, 2100], [957168000000, 2300], [959846400000, 2291.5704881582233], [962438400000, 2306.4403357574006], [965116800000, 2176.0114758811656], [967795200000, 1952.718735207226], [970387200000, 1787.0663336744585], [973065600000, 1727.1691603279064], [975657600000, 1695.146197174043], [978336000000, 1684.5246575410883], [981014400000, 1684.3206286737843], [983433600000, 1714.6361222097707], [986112000000, 1885.069971602401], [988704000000, 1995.296480873559], [991382400000, 1971.1821062705046], [993974400000, 1869.6224106561203], [996652800000, 1657.17077427362], [999331200000, 1436.3275598212047], [1001923200000, 1317.317870279849], [1004601600000, 1196.2351185400337], [1007193600000, 1242.3368008674663], [1009872000000, 1318.2433663607098], [1012550400000, 1548.237545192829], [1014969600000, 1658.1475547133687], [1017648000000, 1760.6720261758296], [1020240000000, 1967.8028205256621], [1022918400000, 1876.8571584932176], [1025510400000, 1778.611080261363], [1028188800000, 1629.0725748919385], [1030867200000, 1452.9809747679346], [1033459200000, 1326.7004870294516], [1036137600000, 1271.8252775709693], [1038729600000, 1237.1460398640081], [1041408000000, 1420.648456176811], [1044086400000, 1733.0280000157452], [1046505600000, 1844.5114571915171], [1049184000000, 1999.1725071305304], [1051776000000, 2199.076571541786], [1054454400000, 2265.0538969575814], [1057046400000, 2340.27860553425], [1059724800000, 2224.203144422022], [1062403200000, 2092.6227315881606], [1064995200000, 1882.7824451038534]]
                    }, {
                        "period_months": ["Dec", "Jan", "Feb"],
                        "annual_period": "Sacramento River Index<br>Wet",
                        "computed_statistics": [{
                            "statistic": "Averages",
                            "statistically_computed_time_series_monthly": [["Jan", 1767.3334340099996], ["Feb", 1932.3466804208845], ["Dec", 1653.2837045291706]],
                            "statistic_aggregate": 1789.4114804359149
                        }, {
                            "statistic": "Minimums",
                            "statistically_computed_time_series_monthly": [["Jan", 1333.870893698451], ["Feb", 1598.4891153592107], ["Dec", 944.0670895317306]],
                            "statistic_aggregate": 1295.7834597184592
                        }],
                        "aggregate_ts": [[1927, 1477.4779973357565], [1938, 1463.168421240971], [1941, 1708.4853102264096], [1942, 1916.6666666666667], [1943, 1916.6666666666667], [1952, 1769.083480152096], [1953, 1916.6666666666667], [1956, 1837.5087250428599], [1958, 1986.0580937796924], [1963, 1750.40240323394], [1965, 1891.9403063051204], [1967, 1876.2748473665877], [1969, 1620.508939550544], [1970, 1916.6666666666667], [1971, 1784.0800167060534], [1974, 1916.6666666666667], [1975, 1895.4772015797128], [1982, 1901.3968037487891], [1983, 1916.6666666666667], [1984, 1915.5788784570977], [1986, 1451.7156981630367], [1995, 1295.7834597184592], [1996, 1916.6666666666667], [1997, 1913.480274060961], [1998, 1652.944301332369], [1999, 1916.6666666666667]],
                        "month_period": "Dec - Feb",
                        "discrete_ts": [[-1362240000000, 960.1326498735675], [-1359648000000, 1116.7589119637319], [-1356969600000, 1315.183579704499], [-1354291200000, 1439.531204331202], [-1351872000000, 1677.7192079715692], [-1349193600000, 1861.5459264712615], [-1346601600000, 2088.4247493615694], [-1343923200000, 2077.8308532631518], [-1341331200000, 2103.245767861831], [-1338652800000, 1981.0513926104738], [-1335974400000, 1841.3641872928786], [-1333382400000, 1704.082788894543], [-1015084800000, 978.8381422693712], [-1012492800000, 1126.4522949568352], [-1009814400000, 1345.8281291439516], [-1007136000000, 1443.3720825180628], [-1004716800000, 1600.305052060898], [-1002038400000, 1861.5814618503991], [-999446400000, 2169.145772342404], [-996768000000, 2342.985100062149], [-994176000000, 2287.9458277532203], [-991497600000, 2258.3797038243306], [-988819200000, 2149.9999999999995], [-986227200000, 1975], [-920390400000, 1354.9007080777938], [-917798400000, 1338.486465842741], [-915120000000, 1477.791260243608], [-912441600000, 1698.487072790161], [-910022400000, 1949.1775976454603], [-907344000000, 2100], [-904752000000, 2300], [-902073600000, 2420], [-899481600000, 2447], [-896803200000, 2270], [-894124800000, 2150], [-891532800000, 1975], [-888854400000, 1850], [-886262400000, 1850], [-883584000000, 1850], [-880905600000, 1900], [-878486400000, 2000], [-875808000000, 2078.5401342424584], [-873216000000, 2285.2581215952086], [-870537600000, 2344.7953085222416], [-867945600000, 2416.5226140655363], [-865267200000, 2270], [-862588800000, 2150], [-859996800000, 1974.9999999999995], [-857318400000, 1850.0000000000002], [-854726400000, 1850], [-852048000000, 1850], [-849369600000, 1900], [-846950400000, 2000], [-844272000000, 2100], [-841680000000, 2283.155661244806], [-839001600000, 2173.318610935777], [-836409600000, 2100.1013478625755], [-833731200000, 1955.632799582456], [-831052800000, 1810.9238053951467], [-828460800000, 1640.4268782084089], [-573321600000, 1510.5784833488797], [-570729600000, 1518.7352008707492], [-568051200000, 1666.1410492783], [-565372800000, 1725.5456059183966], [-562867200000, 1915.5637852595908], [-560188800000, 2081.5235453921127], [-557596800000, 2299.9999999999995], [-554918400000, 2410.963686814417], [-552326400000, 2447], [-549648000000, 2270], [-546969600000, 2150], [-544377600000, 1974.9999999999998], [-541699200000, 1850], [-539107200000, 1845.3146506149892], [-536428800000, 1850], [-533750400000, 1900], [-531331200000, 2000], [-528652800000, 2100], [-526060800000, 2300], [-523382400000, 2259.9112123923715], [-520790400000, 2353.448239660465], [-518112000000, 2270], [-515433600000, 2139.699638927823], [-512841600000, 1975.0000000000002], [-447091200000, 1317.6322586792123], [-444499200000, 1304.4166129956288], [-441820800000, 1612.5261751285796], [-439142400000, 1900], [-436636800000, 2000], [-433958400000, 2100], [-431366400000, 2300], [-428688000000, 2402.4665039813262], [-426096000000, 2321.222056407487], [-423417600000, 2270], [-420739200000, 2150], [-418147200000, 1974.9999999999995], [-383932800000, 1806.6836642678961], [-381340800000, 1850], [-378662400000, 1850], [-375984000000, 1900], [-373564800000, 2208.1742813390774], [-370886400000, 2100], [-368294400000, 2300], [-365616000000, 2420], [-363024000000, 2430.2546446549427], [-360345600000, 2270.0000000000005], [-357667200000, 2150], [-355075200000, 1975], [-226166400000, 1415.1934690590779], [-223574400000, 1463.804195426381], [-220896000000, 1629.6835230806355], [-218217600000, 1670.6586732318576], [-215798400000, 1950.8650133893266], [-213120000000, 2025.1571325707534], [-210528000000, 2282.3407241004584], [-207849600000, 2306.146565971952], [-205257600000, 2248.5028113332564], [-202579200000, 2195.9722115489176], [-199900800000, 2048.943122712088], [-197308800000, 1909.530320895925], [-163008000000, 1259.822385665771], [-160416000000, 1269.36018665332], [-157737600000, 1788.184484490361], [-155059200000, 1900], [-152640000000, 1987.636434425], [-149961600000, 2054.0129112455243], [-147369600000, 2277.9958455007545], [-144691200000, 2184.5943327184264], [-142099200000, 2113.845970510156], [-139420800000, 1977.3865602227438], [-136742400000, 1846.3686972546475], [-134150400000, 1713.0694784674263], [-99936000000, 1545.5628098369955], [-97344000000, 1626.0900536739698], [-94665600000, 1760.3403556767087], [-91987200000, 1868.484186423054], [-89568000000, 2000], [-86889600000, 2100], [-84297600000, 2205.250473305693], [-81619200000, 2310.2516540943643], [-79027200000, 2400.779247457748], [-76348800000, 2269.9999999999995], [-73670400000, 2138.5949781316126], [-71078400000, 1975], [-36777600000, 1428.1851001258094], [-34185600000, 1424.723189230479], [-31507200000, 1490.2312707283677], [-28828800000, 1626.752098706418], [-26409600000, 1744.5434492168465], [-23731200000, 1899.2516326181221], [-21139200000, 2220.8884938390192], [-18460800000, 2420], [-15868800000, 2340.1647498014195], [-13190400000, 2270], [-10512000000, 2150], [-7920000000, 1975], [-5241600000, 1850], [-2649600000, 1850], [28800000, 1850], [2707200000, 1900], [5126400000, 2000], [7804800000, 2100], [10396800000, 2161.386748718372], [13075200000, 2060.638672231599], [15667200000, 1916.8774545570625], [18345600000, 1772.7429555628744], [21024000000, 1596.96689142045], [23616000000, 1428.9257593522093], [26294400000, 1369.5210144772188], [28886400000, 1466.4634290854108], [31564800000, 1592.255731154013], [34243200000, 1815.0115833650048], [36662400000, 1944.9727355991427], [39340800000, 2100], [41932800000, 2268.42366063641], [44611200000, 2302.9967199961598], [47203200000, 2311.609626351704], [49881600000, 2270], [52560000000, 2150], [55152000000, 1975.0000000000002], [120988800000, 1692.42188705591], [123580800000, 1850], [126259200000, 1850], [128937600000, 1900], [131356800000, 2000], [134035200000, 2100], [136627200000, 2300], [139305600000, 2383.8642406856357], [141897600000, 2367.4432339418972], [144576000000, 2270], [147254400000, 2150], [149846400000, 1975], [152524800000, 1850], [155116800000, 1843.4598455521957], [157795200000, 1850], [160473600000, 1871.2959315976764], [162892800000, 1965.1356731414623], [165571200000, 2100], [168163200000, 2224.7267276111875], [170841600000, 2320.563198662463], [173433600000, 2431.4596258410074], [176112000000, 2270], [178790400000, 2149.9999999999995], [181382400000, 1975], [373449600000, 1241.4994332560282], [376041600000, 1484.4326511968413], [378720000000, 1807.6519615133905], [381398400000, 1896.5384497329762], [383817600000, 2000], [386496000000, 2100], [389088000000, 2300], [391766400000, 2317.827169596444], [394358400000, 2187.3578334552903], [397036800000, 2163.2798276013855], [399715200000, 2138.8872982703283], [402307200000, 1975], [404985600000, 1849.9999999999998], [407577600000, 1850], [410256000000, 1850], [412934400000, 1900], [415353600000, 2000], [418032000000, 2100], [420624000000, 2300], [423302400000, 2420], [425894400000, 2447], [428572800000, 2270], [431251200000, 2150], [433843200000, 1975], [436521600000, 1850], [439113600000, 1850], [441792000000, 1850], [444470400000, 1900], [446976000000, 1996.7366353712937], [449654400000, 2100], [452246400000, 2217.6119610357614], [454924800000, 2183.315329659132], [457516800000, 2124.2652814817266], [460195200000, 1988.7868550547462], [462873600000, 1846.178027656452], [465465600000, 1677.0721804104764], [499680000000, 1238.1677681621838], [502272000000, 1207.4196903446966], [504950400000, 1213.9046199715642], [507628800000, 1333.870893698451], [510048000000, 1807.3715808190948], [512726400000, 2100], [515318400000, 2122.0582931275612], [517996800000, 2018.8355583014436], [520588800000, 1907.6219344278315], [523267200000, 1731.8326611765056], [525945600000, 1556.9252356790869], [528537600000, 1390.8888005568406], [783676800000, 971.8444437642389], [786268800000, 958.0537940241618], [788947200000, 944.0670895317306], [791625600000, 1344.794174264436], [794044800000, 1598.4891153592107], [796723200000, 2031.8177661671352], [799315200000, 2269.8850203844954], [801993600000, 2345.197510560271], [804585600000, 2330.394740403572], [807264000000, 2270], [809942400000, 2150], [812534400000, 1975], [815212800000, 1850], [817804800000, 1821.3404988755242], [820483200000, 1849.9999999999998], [823161600000, 1900.0000000000002], [825667200000, 2000], [828345600000, 2100], [830937600000, 2277.874768623321], [833616000000, 2233.7378084315387], [836208000000, 2167.9045966860995], [838886400000, 2030.4709514792798], [841564800000, 1882.6709737402327], [844156800000, 1746.3030725752562], [846835200000, 1683.1836106281428], [849427200000, 1718.7523386996495], [852105600000, 1850], [854784000000, 1900], [857203200000, 1990.4408221828824], [859881600000, 2081.9979369023627], [862473600000, 2181.835960533022], [865152000000, 1995.1943746765191], [867744000000, 1826.051641943439], [870422400000, 1675.7708753408806], [873100800000, 1499.0493027943965], [875692800000, 1372.5132921958161], [878371200000, 1313.0181752940593], [880963200000, 1305.7442956403372], [883641600000, 1338.303383583556], [886320000000, 1648.993893672297], [888739200000, 1971.535626741254], [891417600000, 2100], [894009600000, 2295.5724488973256], [896688000000, 2420], [899280000000, 2447], [901958400000, 2270], [904636800000, 2150], [907228800000, 1975], [909907200000, 1850], [912499200000, 1850], [915177600000, 1850], [917856000000, 1900], [920275200000, 2000], [922953600000, 2100], [925545600000, 2299.8805487206073], [928224000000, 2339.7516610243606], [930816000000, 2360.4623153939197], [933494400000, 2270], [936172800000, 2135.8543367652987], [938764800000, 1975]]
                    }]
                }]
            }], "scenario_name": "NewScenarioRun 3840481", "scenario_color": "#FF3333FF"
        }, {"ts_list": [], "scenario_name": "CalSim3", "scenario_color": "#03C98CFF"}],
        "gui_link_title": "S1",
        "first_record": -1522684800000,
        "units": "TAF",
        "is_instantaneous": true
    });
}