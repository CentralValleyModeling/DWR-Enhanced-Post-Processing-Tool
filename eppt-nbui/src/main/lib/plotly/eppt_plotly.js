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

function buildModeBarButtons(graphDiv, tsDescriptor) {
    let syncPlots = {
        name: 'Toggle Sync X-Axis',
        icon: {
            'width': 477.859,
            'height': 477.859,
            // 'viewBox' : "0 0 477.859 477.859",
            'path': "M472.863,175.662L353.396,56.195c-6.666-6.664-17.472-6.662-24.136,0.004c-3.199,3.2-4.996,7.538-4.997,12.063v51.2 H204.796c-9.426,0-17.067,7.641-17.067,17.067c0,9.426,7.641,17.067,17.067,17.067H341.33c9.426,0,17.067-7.641,17.067-17.067 V109.46l78.268,78.268l-78.268,78.268v-27.068c0-9.426-7.641-17.067-17.067-17.067H153.596v-51.2 c-0.002-9.426-7.645-17.065-17.07-17.063c-4.524,0.001-8.863,1.798-12.063,4.997L4.997,278.062 c-6.663,6.665-6.663,17.468,0,24.132l119.467,119.467c3.2,3.201,7.54,5,12.066,5.001c2.243,0.007,4.466-0.434,6.536-1.297 c6.376-2.644,10.532-8.867,10.53-15.77v-51.2h119.467c9.426,0,17.067-7.641,17.067-17.067s-7.641-17.067-17.067-17.067H136.53 c-9.426,0-17.067,7.641-17.067,17.067v27.068l-78.268-78.268l78.268-78.268v27.068c0,9.426,7.641,17.067,17.067,17.067h187.733 v51.2c0.002,9.426,7.645,17.065,17.07,17.063c4.524-0.001,8.863-1.798,12.063-4.997l119.467-119.467 C479.525,193.129,479.525,182.326,472.863,175.662z",
        },
        click: () => {
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
    let properties = {
        name: 'Properties',
        icon: {
            'width': 24,
            'height': 24,
            'path': 'M12,8A4,4 0 0,1 16,12A4,4 0 0,1 12,16A4,4 0 0,1 8,12A4,4 0 0,1 12,8M12,10A2,2 0 0,0 10,12A2,2 0 0,0 12,14A2,2 0 0,0 14,12A2,2 0 0,0 12,10M10,22C9.75,22 9.54,21.82 9.5,21.58L9.13,18.93C8.5,18.68 7.96,18.34 7.44,17.94L4.95,18.95C4.73,19.03 4.46,18.95 4.34,18.73L2.34,15.27C2.21,15.05 2.27,14.78 2.46,14.63L4.57,12.97L4.5,12L4.57,11L2.46,9.37C2.27,9.22 2.21,8.95 2.34,8.73L4.34,5.27C4.46,5.05 4.73,4.96 4.95,5.05L7.44,6.05C7.96,5.66 8.5,5.32 9.13,5.07L9.5,2.42C9.54,2.18 9.75,2 10,2H14C14.25,2 14.46,2.18 14.5,2.42L14.87,5.07C15.5,5.32 16.04,5.66 16.56,6.05L19.05,5.05C19.27,4.96 19.54,5.05 19.66,5.27L21.66,8.73C21.79,8.95 21.73,9.22 21.54,9.37L19.43,11L19.5,12L19.43,13L21.54,14.63C21.73,14.78 21.79,15.05 21.66,15.27L19.66,18.73C19.54,18.95 19.27,19.04 19.05,18.95L16.56,17.95C16.04,18.34 15.5,18.68 14.87,18.93L14.5,21.58C14.46,21.82 14.25,22 14,22H10M11.25,4L10.88,6.61C9.68,6.86 8.62,7.5 7.85,8.39L5.44,7.35L4.69,8.65L6.8,10.2C6.4,11.37 6.4,12.64 6.8,13.8L4.68,15.36L5.43,16.66L7.86,15.62C8.63,16.5 9.68,17.14 10.87,17.38L11.24,20H12.76L13.13,17.39C14.32,17.14 15.37,16.5 16.14,15.62L18.57,16.66L19.32,15.36L17.2,13.81C17.6,12.64 17.6,11.37 17.2,10.2L19.31,8.65L18.56,7.35L16.15,8.39C15.38,7.5 14.32,6.86 13.12,6.62L12.75,4H11.25Z',
        },
        click: () => {
            var modal = document.getElementById("tsDescriptor");
            var span = document.getElementById("closeModal");
            modal.style.display = "block";
            span.onclick = function () {
                modal.style.display = "none";
            }
            window.onclick = function(event){
                if(event.target === modal){
                    modal.style.display = "none";
                }
            };
            let modalBody = document.getElementById("modal-body");
            modalBody.innerHTML = '';
            for(let i = 0; i < tsDescriptor.length; i++){
                let description = tsDescriptor[i];
                let outerDiv = document.createElement("div");
                outerDiv.classList.add('modalOuter');
                outerDiv.style.color = description['scenario_color'];
                modalBody.appendChild(outerDiv);
                let scenarioHeader = document.createElement("div");
                scenarioHeader.classList.add('scenarioHeader');
                outerDiv.appendChild(scenarioHeader);
                scenarioHeader.innerHTML += '<b>Scenario:</b> ' + description['scenario_name'] + " (" + description['scenario_model'] + ')';
                let timeSeriesMetadata = description['time_series_metadata'];
                for(let j = 0; j < timeSeriesMetadata.length; j++){
                    let tsDiv = document.createElement("div");
                    tsDiv.classList.add('metadata');
                    outerDiv.appendChild(tsDiv);
                    let metadata = timeSeriesMetadata[j];
                    tsDiv.innerHTML += '<b>Filename:</b> ' + metadata['time_series_filename'] + ' <b>B-Part:</b> '
                        + metadata['b_part'] + ' <b>C-Part:</b> ' + metadata['c_part'] + ' <b>F-Part:</b> ' + metadata['f_part'];
                }
            }
        }
    };
    return [['zoom2d', 'pan2d', 'zoomIn2d', 'zoomOut2d', 'resetScale2d'],
        ['toggleSpikelines', 'hoverClosestCartesian', 'hoverCompareCartesian'], [properties, syncPlots]];
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

function plotData(layout, dataList, tsDescriptor) {
    let main = document.getElementById("main");
    let plots = [];
    for (let i = 0; i < dataList.length; i++) {
        let plot = document.createElement("div");
        plots.push(plot);
        plot.id = 'plot' + i;
        main.appendChild(plot);
        Plotly.newPlot(plot.id, dataList[i], layout[i], {
            displaylogo: false,
            modeBarButtons: buildModeBarButtons(plot.id, tsDescriptor),
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
        try {
            let width = plot.offsetWidth;
            let height = plot.offsetHeight;
            console.log("Downloading plot with width: " + width + " and height: " + height);
            //javaObj instantiated from JavaFX
            if (javaObj) {
                console.log("Downloading through Java");
                javaObj.interruptFunction(format, JSON.stringify(plot.data), JSON.stringify(plot.layout), width, height);
            } else {
                console.log("Downloading through Javascript");
                Plotly.downloadImage(plot, {format: format, height: height, width: width});
            }
        } catch (err) {
            console.log(err);
            console.error(err);
        }
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
        "ts_descriptor": [{
            "scenario_name": "DCR_Baseline_4.0",
            "time_series_metadata": [{
                "c_part": "STORAGE",
                "f_part": "L2015A",
                "b_part": "S_SLUIS_CVP",
                "time_series_filename": "DCR_Baseline_4.0_512_DV (CalSim3)"
            }],
            "scenario_color": "#A3F244FF",
            "scenario_model": "CalSim3"
        }, {
            "scenario_name": "DCR_Baseline_5.0",
            "time_series_metadata": [{
                "c_part": "STORAGE",
                "f_part": "L2015A",
                "b_part": "S_SLUIS_CVP",
                "time_series_filename": "DCR_Baseline_5.0_DV (CalSim3)"
            }],
            "scenario_color": "#CF79DDFF",
            "scenario_model": "CalSim3"
        }],
        "last_record": 1443686400000,
        "scenario_run_data": [{
            "ts_list": [{
                "ts_name": "DCR_Baseline_4.0_512_DV (CalSim3)", "monthly_filters": [{
                    "annual_filters": [{
                        "period_months": ["Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep"],
                        "annual_period": "Long Term<br>",
                        "computed_statistics": [{
                            "statistic": "Averages",
                            "statistically_computed_time_series_monthly": [["Jan", 714.4686999606196], ["Feb", 800.9075304532923], ["Mar", 865.068932483966], ["Apr", 806.3127460584102], ["May", 612.6463300180801], ["Jun", 418.060481446147], ["Jul", 242.9352253798352], ["Aug", 155.32717566524886], ["Sep", 225.93612648252636], ["Oct", 306.54441168463495], ["Nov", 422.40148090418774], ["Dec", 585.0604384917052]],
                            "statistic_aggregate": 512.9724649190545
                        }],
                        "aggregate_ts": [[1922, 567.0493180603443], [1923, 648.577724372253], [1924, 514.6219604885965], [1925, 422.33304923783606], [1926, 487.7239302955472], [1927, 471.3387090771419], [1928, 453.86941405504757], [1929, 509.138298723232], [1930, 527.4402070581751], [1931, 380.1995591761076], [1932, 539.0678849314128], [1933, 359.788752405004], [1934, 422.4407368878206], [1935, 329.56967735199436], [1936, 448.4541927293237], [1937, 502.28047605279886], [1938, 651.0101692359123], [1939, 588.1393098662387], [1940, 329.9024468782089], [1941, 533.6709832804418], [1942, 523.143697479194], [1943, 505.3185412575164], [1944, 470.26482510552995], [1945, 552.3155066305181], [1946, 599.1726809930682], [1947, 569.7141500795261], [1948, 334.4895915378283], [1949, 546.0891914652158], [1950, 408.8100957324509], [1951, 551.7166705957635], [1952, 626.8742191974467], [1953, 581.737773573297], [1954, 424.148845597448], [1955, 521.5625003271529], [1956, 538.3434283298358], [1957, 460.76611194969547], [1958, 672.9878558190801], [1959, 462.06221037150135], [1960, 401.42890227495053], [1961, 382.39905091932854], [1962, 400.1508879420898], [1963, 570.442084978784], [1964, 451.7813822165979], [1965, 518.0557159325796], [1966, 485.1391121119477], [1967, 634.9325069164396], [1968, 521.6088324137711], [1969, 738.5724756329092], [1970, 623.4066210673345], [1971, 463.1150059009114], [1972, 446.3747469872314], [1973, 525.0218308743904], [1974, 541.1854513772788], [1975, 451.69382113454384], [1976, 505.03989228671884], [1977, 320.7113948300993], [1978, 497.0921548106102], [1979, 500.2674903182574], [1980, 623.904889093552], [1981, 475.24492225846126], [1982, 622.1828491479894], [1983, 880.2873652454126], [1984, 648.4629621694213], [1985, 441.65817912986364], [1986, 535.8308252149141], [1987, 496.2467984647281], [1988, 483.00848610997053], [1989, 463.43369219389757], [1990, 331.2151893407081], [1991, 488.3594094236776], [1992, 475.7735862955192], [1993, 534.1202792205023], [1994, 579.0334957349099], [1995, 537.2501508057959], [1996, 527.2474539567727], [1997, 516.5562470260605], [1998, 695.083956650839], [1999, 640.395643531404], [2000, 449.307284427166], [2001, 505.28471419919856], [2002, 451.27488758077783], [2003, 475.00933520640615], [2004, 428.9125169307178], [2005, 659.8896169450855], [2006, 746.9018909427683], [2007, 629.1653448434713], [2008, 565.4963540110388], [2009, 334.46758656875045], [2010, 564.3489085120149], [2011, 700.6789444249024], [2012, 571.6258156413779], [2013, 425.24382037331566], [2014, 303.5103502873305], [2015, 398.4438933501169]],
                        "month_period": "Oct - Sep",
                        "discrete_ts": [[-1522684800000, 300], [-1520006400000, 280.67104813865416], [-1517414400000, 290.27777777777766], [-1514736000000, 519.6744209237903], [-1512057600000, 713.9268130441114], [-1509638400000, 901.9612245654865], [-1506960000000, 971.9999999999994], [-1504368000000, 932.8123788690258], [-1501689600000, 768.1966041457136], [-1499097600000, 597.5307743381462], [-1496419200000, 343.79328898875394], [-1493740800000, 217.4767607377831], [-1491148800000, 266.27072519488866], [-1488470400000, 426.0479285277062], [-1485878400000, 629.8894746232977], [-1483200000000, 867.0830431023815], [-1480521600000, 972.0000000000001], [-1478102400000, 972], [-1475424000000, 971.9999999999998], [-1472832000000, 970.5252759855313], [-1470153600000, 722.2271427309577], [-1467561600000, 515.9816290043225], [-1464883200000, 282.11664769101264], [-1462204800000, 177.16819957049432], [-1459612800000, 275.8933512313325], [-1456934400000, 374.860858338558], [-1454342400000, 521.9905754815389], [-1451664000000, 504.8912873871818], [-1448985600000, 647.4003521543701], [-1446480000000, 679.2066449173345], [-1443801600000, 731.8559712186578], [-1441209600000, 816.1715885914501], [-1438531200000, 670.9443087680718], [-1435939200000, 457.82414943902785], [-1433260800000, 378.57336350371304], [-1430582400000, 166.2822407622729], [-1427990400000, 225.46218530098156], [-1425312000000, 288.43265767219236], [-1422720000000, 295.3812697586289], [-1420041600000, 382.3031647708058], [-1417363200000, 520.2436330594688], [-1414944000000, 658.4588416909454], [-1412265600000, 746.7987362238405], [-1409673600000, 717.6893267091262], [-1406995200000, 554.9904835485049], [-1404403200000, 388.9701755619887], [-1401724800000, 207.37489919632117], [-1399046400000, 101.81939315471506], [-1396454400000, 205.53400950749503], [-1393776000000, 303.0344252838864], [-1391184000000, 495.65223897595564], [-1388505600000, 550.2445649790321], [-1385827200000, 686.2787245478725], [-1383408000000, 803.0888848132522], [-1380729600000, 865.983767047109], [-1378137600000, 908.092053886267], [-1375459200000, 624.1083340009952], [-1372867200000, 318.95533908470156], [-1370188800000, 103.10708780947667], [-1367510400000, 45], [-1364918400000, 149.14174311801762], [-1362240000000, 149.14174311801762], [-1359648000000, 367.27423240081333], [-1356969600000, 541.1777477467612], [-1354291200000, 647.0177678514375], [-1351872000000, 809.5054270040075], [-1349193600000, 933.0495667914182], [-1346601600000, 833.9403813949697], [-1343923200000, 578.8046886246354], [-1341331200000, 382.7927897923574], [-1338652800000, 172.8758353087534], [-1335974400000, 81.8525109043387], [-1333382400000, 158.63181798819332], [-1330704000000, 183.33333333333348], [-1328112000000, 366.66666666666697], [-1325433600000, 575.4272804765241], [-1322755200000, 704.6862509760914], [-1320249600000, 759.6893839679059], [-1317571200000, 903.1112020026087], [-1314979200000, 767.545774393315], [-1312300800000, 523.7447572895055], [-1309708800000, 317.61657064932473], [-1307030400000, 152.2834791787528], [-1304352000000, 57.20338816595922], [-1301760000000, 135.12488156058282], [-1299081600000, 135.12488156058285], [-1296489600000, 267.60648198592895], [-1293811200000, 476.0429553903089], [-1291132800000, 620.1341304862345], [-1288713600000, 707.35625677028], [-1286035200000, 818.7524768249853], [-1283443200000, 861.3679706114009], [-1280764800000, 674.4888210090535], [-1278172800000, 501.93438116163895], [-1275494400000, 359.4925220491089], [-1272816000000, 294.1609419781982], [-1270224000000, 393.197764851063], [-1267545600000, 353.99211055779466], [-1264953600000, 361.1815674004169], [-1262275200000, 525.3648895201839], [-1259596800000, 719.3661417589063], [-1257177600000, 799.7751565832011], [-1254499200000, 908.6178816261078], [-1251907200000, 968.2545597927646], [-1249228800000, 787.226580073585], [-1246636800000, 459.0344634889161], [-1243958400000, 219.18795465132308], [-1241280000000, 88.23402530289768], [-1238688000000, 139.04715394200483], [-1236009600000, 177.17341821803166], [-1233417600000, 222.75369549795576], [-1230739200000, 263.26752109098675], [-1228060800000, 376.0178237997167], [-1225641600000, 454.9408023927834], [-1222963200000, 521.584807117766], [-1220371200000, 528.0497326600411], [-1217692800000, 468.5192521137802], [-1215100800000, 341.77266198861275], [-1212422400000, 359.1473893584361], [-1209744000000, 416.16219661229746], [-1207152000000, 433.0054092628827], [-1204473600000, 427.06025222353276], [-1201881600000, 428.4921111752773], [-1199203200000, 663.8713727465843], [-1196524800000, 829.7231889849156], [-1194019200000, 972.000000000001], [-1191340800000, 972], [-1188748800000, 834.324312064349], [-1186070400000, 618.4971040646953], [-1183478400000, 440.7370904754745], [-1180800000000, 150.8048857472495], [-1178121600000, 45.00000000000057], [-1175529600000, 86.30430169487278], [-1172851200000, 131.53217645813504], [-1170259200000, 188.98305055370025], [-1167580800000, 327.5860657952742], [-1164902400000, 486.3522771369339], [-1162483200000, 575.0233604604629], [-1159804800000, 647.7974890126134], [-1157212800000, 569.7677790744308], [-1154534400000, 468.5613115574115], [-1151942400000, 350.80937522505906], [-1149264000000, 121.63430108683706], [-1146585600000, 160.18928368309298], [-1143993600000, 289.2285588160968], [-1141315200000, 246.09447339534233], [-1138723200000, 251.5014443895171], [-1136044800000, 411.29862634905004], [-1133366400000, 574.1602553694373], [-1130947200000, 741.9883908951011], [-1128268800000, 718.7329662365247], [-1125676800000, 603.345055104934], [-1122998400000, 484.30282498408775], [-1120406400000, 374.69086519922666], [-1117728000000, 274.0260786676239], [-1115049600000, 232.91590258715846], [-1112457600000, 156.2319594758442], [-1109779200000, 112.35947179518342], [-1107187200000, 281.8401895416834], [-1104508800000, 287.7310800848122], [-1101830400000, 479.33838653216077], [-1099411200000, 446.410166370331], [-1096732800000, 635.311231024892], [-1094140800000, 618.8271649663312], [-1091462400000, 426.66808218752686], [-1088870400000, 297.5763515173736], [-1086192000000, 158.82727439455354], [-1083513600000, 61.502222654636995], [-1080921600000, 148.44450715444677], [-1078243200000, 200.73102982012992], [-1075651200000, 290.98703408440605], [-1072972800000, 407.2353691512375], [-1070294400000, 502.16310941521596], [-1067788800000, 722.1461037112445], [-1065110400000, 865.877368692419], [-1062518400000, 795.380306212016], [-1059840000000, 612.7959284109965], [-1057248000000, 427.23151900069263], [-1054569600000, 238.4134405744454], [-1051891200000, 126.89531930726311], [-1049299200000, 191.59378437181837], [-1046620800000, 243.4924531755128], [-1044028800000, 282.78320237061894], [-1041350400000, 475.02640648378264], [-1038672000000, 531.1263766871539], [-1036252800000, 711.0693358577354], [-1033574400000, 927.6759253625415], [-1030982400000, 894.9999421176182], [-1028304000000, 724.9939314139492], [-1025712000000, 605.6842991753513], [-1023033600000, 310.7415181442861], [-1020355200000, 128.29118058205952], [-1017763200000, 191.4811412629772], [-1015084800000, 175.03104053156412], [-1012492800000, 350.0590699498221], [-1009814400000, 550.8118848705818], [-1007136000000, 728.424789616228], [-1004716800000, 923.8943265777923], [-1002038400000, 972], [-999446400000, 972.0000000000001], [-996768000000, 857.701116659172], [-994176000000, 752.8162264366803], [-991497600000, 592.5074404787504], [-988819200000, 441.27619758567744], [-986227200000, 495.5999381246792], [-983548800000, 650.1928017817462], [-980956800000, 604.5547989065232], [-978278400000, 802.620050310636], [-975600000000, 881.8599625539465], [-973180800000, 927.5079796929352], [-970502400000, 971.9999999999999], [-967910400000, 884.4200788120638], [-965232000000, 632.3558337775659], [-962640000000, 356.20180722550344], [-959961600000, 163.0409177221321], [-957283200000, 44.999999999999915], [-954691200000, 137.9174876118127], [-952012800000, 164.94262650748655], [-949420800000, 219.6089513412367], [-946742400000, 208.10245366897973], [-944064000000, 419.13512646637605], [-941558400000, 606.0179626029975], [-938880000000, 755.571423669323], [-936288000000, 712.1590533644153], [-933609600000, 443.6516768310624], [-931017600000, 207.48485994616868], [-928339200000, 44.999999999999964], [-925660800000, 45], [-923068800000, 132.1552281404604], [-920390400000, 200.25895212541295], [-917798400000, 366.66666666666686], [-915120000000, 588.2028677489837], [-912441600000, 758.7944023286171], [-910022400000, 971.9999999999995], [-907344000000, 972.0000000000001], [-904752000000, 972.0000000000001], [-902073600000, 717.5845099396031], [-899481600000, 488.1873953197787], [-896803200000, 197.3237163572389], [-894124800000, 59.34834770750707], [-891532800000, 111.68494117149329], [-888854400000, 255.56969001051357], [-886262400000, 366.66666666666646], [-883584000000, 571.6488640872142], [-880905600000, 758.2046713047182], [-878486400000, 870.8898647136772], [-875808000000, 962.9934982282259], [-873216000000, 911.6595492950282], [-870537600000, 697.1919326501966], [-867945600000, 508.6047838924634], [-865267200000, 216.56508174870126], [-862588800000, 50.01558025806283], [-859996800000, 107.71418689485992], [-857318400000, 239.57239554973114], [-854726400000, 366.66666666666646], [-852048000000, 521.3172972744476], [-849369600000, 731.7796128718663], [-846950400000, 818.2080656929064], [-844272000000, 971.9999999999999], [-841680000000, 932.7024307324141], [-839001600000, 691.205372293747], [-836409600000, 460.8129154516315], [-833731200000, 191.55754124164542], [-831052800000, 45.00000000000001], [-828460800000, 93.00019731514068], [-825782400000, 195.2801628809157], [-823190400000, 366.6666666666668], [-820512000000, 550.0000000000002], [-817833600000, 688.8433491616499], [-815328000000, 808.3953869827625], [-812649600000, 877.8868073414621], [-810057600000, 805.0758247328367], [-807379200000, 563.8014638222371], [-804787200000, 361.0841076944007], [-802108800000, 195.96898494020766], [-799430400000, 78.89139410684452], [-796838400000, 151.28375293637546], [-794160000000, 254.12113310849978], [-791568000000, 458.9463042750991], [-788889600000, 687.4392714723139], [-786211200000, 784.6176574126545], [-783792000000, 940.7655679110643], [-781113600000, 972.0000000000182], [-778521600000, 830.0891881563075], [-775843200000, 626.2909652973246], [-773251200000, 437.9091915582355], [-770572800000, 240.38885408684254], [-767894400000, 157.58574091563872], [-765302400000, 237.6322053722193], [-762624000000, 412.58680931479387], [-760032000000, 609.4231997911294], [-757353600000, 844.8211327819565], [-754675200000, 972], [-752256000000, 971.9999999999999], [-749577600000, 971.9999999999994], [-746985600000, 835.6672642191977], [-744307200000, 632.2339837481478], [-741715200000, 415.9474384047678], [-739036800000, 238.1341756207205], [-736358400000, 110.07386561944087], [-733766400000, 175.1843024166643], [-731088000000, 334.41522563859337], [-728496000000, 561.4635609928371], [-725817600000, 794.025716288718], [-723139200000, 847.6072433021442], [-720720000000, 909.0164181411675], [-718041600000, 972.0000000000006], [-715449600000, 876.6202390443933], [-712771200000, 625.2263561135828], [-710179200000, 385.4702805752232], [-707500800000, 232.71223148610653], [-704822400000, 112.0736582115369], [-702230400000, 185.93887116001], [-699552000000, 237.33484340728103], [-696960000000, 340.18722226492775], [-694281600000, 444.6757089277772], [-691603200000, 402.50976831892257], [-689097600000, 382.3752924580034], [-686419200000, 533.3273705251454], [-683827200000, 487.2777759227247], [-681148800000, 345.58555920971475], [-678556800000, 259.11750726368905], [-675878400000, 141.4284114739688], [-673200000000, 162.0933987029392], [-670608000000, 277.9622399788455], [-667929600000, 456.31537096004575], [-665337600000, 598.9757762078477], [-662659200000, 676.6957973784462], [-659980800000, 822.7621230907754], [-657561600000, 934.0875244275084], [-654883200000, 971.9999999999998], [-652291200000, 799.1482965790864], [-649612800000, 579.1698288256742], [-647020800000, 338.49260001512323], [-644342400000, 154.0236760871522], [-641664000000, 70.13695419148567], [-639072000000, 151.26234981944523], [-636393600000, 183.33333333333346], [-633801600000, 366.66666666666663], [-631123200000, 473.70738610452776], [-628444800000, 609.6494944058962], [-626025600000, 665.5098344590828], [-623347200000, 768.6043040937216], [-620755200000, 650.208400114732], [-618076800000, 438.207863622244], [-615484800000, 323.8328986005017], [-612806400000, 171.46699950403166], [-610128000000, 72.75669683042723], [-607536000000, 181.77727105424609], [-604857600000, 368.0365714835442], [-602265600000, 576.2693220718924], [-599587200000, 823.5642014436372], [-596908800000, 968.8223851100395], [-594489600000, 971.9999999999998], [-591811200000, 971.9999999999997], [-589219200000, 846.6512538642037], [-586540800000, 558.6250803387885], [-583948800000, 312.75904233254073], [-581270400000, 69.47703543056984], [-578592000000, 44.99999999999999], [-576000000000, 107.39515507394698], [-573321600000, 205.64620169408568], [-570729600000, 366.6666666666665], [-568051200000, 574.1505430318252], [-565372800000, 801.8998897908539], [-562867200000, 964.7824062217849], [-560188800000, 971.9999999999999], [-557596800000, 948.0514116777142], [-554918400000, 805.5491158242783], [-552326400000, 728.0806412110009], [-549648000000, 487.352246231898], [-546969600000, 311.6415564276364], [-544377600000, 356.6699515916156], [-541699200000, 479.23770308657976], [-539107200000, 593.7131006353452], [-536428800000, 825.0630907551952], [-533750400000, 927.0101135531804], [-531331200000, 960.4990040330117], [-528652800000, 972.0000000000259], [-526060800000, 874.5634487751151], [-523382400000, 628.9343669391161], [-520790400000, 412.2171676604203], [-518112000000, 154.79727266130118], [-515433600000, 45.00000000000002], [-512841600000, 107.81801478027306], [-510163200000, 240.5189919045322], [-507571200000, 366.6666666666667], [-504892800000, 550.0000000000001], [-502214400000, 642.0548903933824], [-499795200000, 677.1160609745721], [-497116800000, 818.220531746357], [-494524800000, 670.407762704003], [-491846400000, 437.73270303371083], [-489254400000, 287.5173609386856], [-486576000000, 128.04945212032416], [-483897600000, 90.93112736359541], [-481305600000, 180.5705993235468], [-478627200000, 183.33333333333337], [-476035200000, 366.6666666666665], [-473356800000, 599.8671934083889], [-470678400000, 817.3707107995477], [-468259200000, 940.9097211414227], [-465580800000, 934.3940306269984], [-462988800000, 880.179882781096], [-460310400000, 649.8240583418603], [-457718400000, 354.7239925599186], [-455040000000, 248.82829694919923], [-452361600000, 110.82048208911888], [-449769600000, 171.8316352282836], [-447091200000, 181.70569687747982], [-444499200000, 345.2157292898999], [-441820800000, 588.8814801701756], [-439142400000, 830.3352135812489], [-436636800000, 971.9999999999999], [-433958400000, 971.9999999999995], [-431366400000, 936.9506378880925], [-428688000000, 698.6808466173973], [-426096000000, 464.6862103818726], [-423417600000, 219.11183165578956], [-420739200000, 93.37853372959195], [-418147200000, 157.1749597664818], [-415468800000, 314.708305534459], [-412876800000, 366.6666666666666], [-410198400000, 550.0000000000002], [-407520000000, 701.064530397212], [-405100800000, 713.4148970738751], [-402422400000, 793.3273390678338], [-399830400000, 679.8212914948874], [-397152000000, 514.3465098419954], [-394560000000, 341.8890541615055], [-391881600000, 198.006802683222], [-389203200000, 133.1417955413532], [-386611200000, 222.80615093333623], [-383932800000, 403.5313590358606], [-381340800000, 570.2304485136691], [-378662400000, 736.8054689496754], [-375984000000, 872.2231467128154], [-373564800000, 971.9999999999995], [-370886400000, 971.9999999999999], [-368294400000, 972.0000000000003], [-365616000000, 839.2739344393038], [-363024000000, 726.4602981206316], [-360345600000, 451.74218670026653], [-357667200000, 252.6527565084619], [-355075200000, 306.93467084827734], [-352396800000, 424.6101611518794], [-349804800000, 439.8748246923319], [-347126400000, 581.800435669303], [-344448000000, 676.9784775783613], [-342028800000, 757.2341309935927], [-339350400000, 805.5934354526979], [-336758400000, 727.4867937111246], [-334080000000, 494.1585954319298], [-331488000000, 292.36649427682244], [-328809600000, 127.03725831739692], [-326131200000, 45.00000000000011], [-323539200000, 172.60591718257592], [-320860800000, 190.56124539486763], [-318268800000, 256.7351155201269], [-315590400000, 438.72290797684616], [-312912000000, 595.2884273418883], [-310406400000, 716.6806852090446], [-307728000000, 782.372101361884], [-305136000000, 788.0153560558815], [-302457600000, 552.4265669617466], [-299865600000, 262.5832168246488], [-297187200000, 77.61792328841022], [-294508800000, 45], [-291916800000, 111.1432813640615], [-289238400000, 145.3502400167447], [-286646400000, 293.3927242925359], [-283968000000, 398.2020564955284], [-281289600000, 563.0724520419321], [-278870400000, 615.1932222649812], [-276192000000, 708.950942392527], [-273600000000, 725.1405309856635], [-270921600000, 546.1398019955304], [-268329600000, 288.43497257188795], [-265651200000, 115.90436001214815], [-262972800000, 56.7258660129435], [-260380800000, 132.28144194951946], [-257702400000, 197.8201256787581], [-255110400000, 341.2063355785726], [-252432000000, 331.1166661790175], [-249753600000, 487.2432936241871], [-247334400000, 686.3472267269747], [-244656000000, 784.6154026518942], [-242064000000, 668.8385463137837], [-239385600000, 473.8829199725755], [-236793600000, 317.77557173382695], [-234115200000, 181.3877858669135], [-231436800000, 114.48518241320164], [-228844800000, 217.09159856537224], [-226166400000, 390.283621315389], [-223574400000, 583.7577160810367], [-220896000000, 776.542517116807], [-218217600000, 867.9075928562954], [-215798400000, 971.9999999999998], [-213120000000, 972.0000000000002], [-210528000000, 921.5163596826434], [-207849600000, 641.0369194189742], [-205257600000, 403.22380410767795], [-202579200000, 162.09742902219776], [-199900800000, 44.99999999999999], [-197308800000, 109.93906014438703], [-194630400000, 258.8868081975768], [-192038400000, 453.1235346058555], [-189360000000, 580.1621131203941], [-186681600000, 708.1056934257042], [-184176000000, 710.437965272025], [-181497600000, 766.9478116082054], [-178905600000, 667.2667890522864], [-176227200000, 466.5923513183618], [-173635200000, 283.5159882108625], [-170956800000, 165.597671269909], [-168278400000, 134.793534362217], [-165686400000, 225.94632615577655], [-163008000000, 250.09110115192993], [-160416000000, 453.5666235336714], [-157737600000, 659.4155884731897], [-155059200000, 844.8388232520242], [-152640000000, 971.9999999999999], [-149961600000, 972.0000000000003], [-147369600000, 909.0243550255034], [-144691200000, 577.3589723927405], [-142099200000, 335.7750925371972], [-139420800000, 89.30422516777637], [-136742400000, 44.999999999999986], [-134150400000, 108.293809656922], [-131472000000, 183.33333333333317], [-128880000000, 404.4169450000681], [-126201600000, 635.4698268661745], [-123523200000, 777.636214907784], [-121104000000, 871.7666766067473], [-118425600000, 907.6560876487744], [-115833600000, 797.8022433859393], [-113155200000, 541.3667093214426], [-110563200000, 322.98449298106243], [-107884800000, 169.74292004529119], [-105206400000, 62.0545807459736], [-102614400000, 147.43931450078244], [-99936000000, 152.26507074811616], [-97344000000, 346.80272037471565], [-94665600000, 578.0129382045432], [-91987200000, 735.0801252157918], [-89568000000, 845.4872774549282], [-86889600000, 960.2477342724237], [-84297600000, 972.0000000000001], [-81619200000, 827.4430932474949], [-79027200000, 741.8688166126105], [-76348800000, 593.6664044197146], [-73670400000, 410.49368169984973], [-71078400000, 455.8222207470873], [-68400000000, 583.1530650330296], [-65808000000, 580.1537661098678], [-63129600000, 746.8381889918486], [-60451200000, 857.406087023656], [-57945600000, 865.5056370103424], [-55267200000, 913.641533487218], [-52675200000, 755.5039717015511], [-49996800000, 487.9529777220236], [-47404800000, 238.96223881255716], [-44726400000, 56.74872079309492], [-42048000000, 45], [-39456000000, 128.43980228006387], [-36777600000, 163.63196895153465], [-34185600000, 345.3330348879538], [-31507200000, 563.6646149268191], [-28828800000, 823.2765609726093], [-26409600000, 971.9999999999999], [-23731200000, 972], [-21139200000, 972], [-18460800000, 971.9999999999999], [-15868800000, 913.822376757311], [-13190400000, 805.0251279005593], [-10512000000, 650.5570824930857], [-7920000000, 709.5589407050388], [-5241600000, 861.4428910858929], [-2649600000, 899.0949478858147], [28800000, 972.0000000000001], [2707200000, 971.9999999999999], [5126400000, 971.9999999999999], [7804800000, 971.9999999999998], [10396800000, 802.5439575848823], [13075200000, 515.7105638390385], [15667200000, 285.19914215577364], [18345600000, 71.20256345001093], [21024000000, 45.000000000000014], [23616000000, 112.6853868066013], [26294400000, 158.8706229251306], [28886400000, 370.7237424030655], [31564800000, 602.8666312886522], [34243200000, 710.2966209086802], [36662400000, 755.3782276159217], [39340800000, 811.7497291454201], [41932800000, 694.8337422028816], [44611200000, 522.5482710381798], [47203200000, 357.7486633062023], [49881600000, 203.93896838686763], [52560000000, 143.83381864176008], [55152000000, 224.5910329481751], [57830400000, 372.6904999036935], [60422400000, 366.6666666666668], [63100800000, 593.4410688538615], [65779200000, 720.6005770504793], [68284800000, 756.61289002165], [70963200000, 784.8981637745642], [73555200000, 682.7831563823639], [76233600000, 455.96868004918906], [78825600000, 259.3287883350883], [81504000000, 132.50169484542093], [84182400000, 61.64625825029611], [86774400000, 169.35851971350326], [89452800000, 209.7575281311021], [92044800000, 442.53045102046275], [94723200000, 641.612009852472], [97401600000, 838.8133414821593], [99820800000, 971.9999999999999], [102499200000, 972], [105091200000, 821.9042692536881], [107769600000, 590.4619858011783], [110361600000, 378.29321674443867], [113040000000, 193.45456466221097], [115718400000, 83.20173907838738], [118310400000, 156.23286446658483], [120988800000, 331.6312271885621], [123580800000, 523.4240343054065], [126259200000, 719.4680678793266], [128937600000, 918.6464651168321], [131356800000, 972.0000000000005], [134035200000, 971.9999999999998], [136627200000, 853.808921867938], [139305600000, 565.9054478973843], [141897600000, 361.40475713112005], [144576000000, 138.91785180809038], [147254400000, 45.000000000000036], [149846400000, 92.01864333268608], [152524800000, 244.68339414457742], [155116800000, 366.6666666666668], [157795200000, 581.2556983488128], [160473600000, 607.7840326255497], [162892800000, 696.4261379291145], [165571200000, 860.6204858821261], [168163200000, 763.5640919409967], [170841600000, 498.5344310843516], [173433600000, 334.84753046380956], [176112000000, 169.68585219254632], [178790400000, 114.11361336025033], [181382400000, 182.14391897572375], [184060800000, 353.9740159414803], [186652800000, 437.0951841867467], [189331200000, 643.5340635492786], [192009600000, 733.333333333333], [194515200000, 773.8524366302336], [197193600000, 804.3426269494878], [199785600000, 846.1276047879843], [202464000000, 621.241103938424], [205056000000, 346.6036965591675], [207734400000, 202.51659471805385], [210412800000, 117.3102791872178], [213004800000, 180.54776765921824], [215683200000, 165.78767577112217], [218275200000, 258.7852018970098], [220953600000, 388.1778028455145], [223632000000, 508.2116206234489], [226051200000, 385.9109571892783], [228729600000, 426.4483386225901], [231321600000, 395.11459691334375], [234000000000, 377.1244870086641], [236592000000, 229.8264783228076], [239270400000, 276.49855738079395], [241948800000, 201.96464710690603], [244540800000, 234.68637427971296], [247219200000, 190.7817373773613], [249811200000, 213.56253669467213], [252489600000, 285.7277164736466], [255168000000, 530.0107442983715], [257587200000, 773.2680723161153], [260265600000, 971.9999999999999], [262857600000, 972.0000000000001], [265536000000, 774.8073247299735], [268128000000, 625.8123856360246], [270806400000, 298.48675280011713], [273484800000, 127.19283014848504], [276076800000, 201.45575725255506], [278755200000, 250.9840496128165], [281347200000, 421.85706921012235], [284025600000, 550.0000000000003], [286704000000, 751.1034378270973], [289123200000, 894.0881544189223], [291801600000, 971.9999999999999], [294393600000, 846.9669200184027], [297072000000, 589.9903228315416], [299664000000, 345.06862667567003], [302342400000, 159.90241296172707], [305020800000, 77.27062608028452], [307612800000, 143.97826418250403], [310291200000, 301.92111575768484], [312883200000, 495.35065285337754], [315561600000, 707.0057295912063], [318240000000, 953.2575743721565], [320745600000, 972.0000000000001], [323424000000, 972.0000000000001], [326016000000, 905.2105903611157], [328694400000, 717.5465628747892], [331286400000, 541.267057653482], [333964800000, 364.3545011706319], [336643200000, 250.1302964531982], [339235200000, 306.81458803498253], [341913600000, 424.89283866228095], [344505600000, 368.48358236231184], [347184000000, 565.8593446765109], [349862400000, 754.7036165142446], [352281600000, 772.6219866014976], [354960000000, 940.669308762425], [357552000000, 835.7841415303183], [360230400000, 566.5021893067146], [362822400000, 254.7955702306319], [365500800000, 52.768334267218734], [368179200000, 45], [370771200000, 120.85815418738063], [373449600000, 244.19953961383024], [376041600000, 448.95126139366505], [378720000000, 589.8429471284514], [381398400000, 753.32203063225], [383817600000, 876.6055829561701], [386496000000, 972.0000000000002], [389088000000, 972], [391766400000, 832.7636012487299], [394358400000, 726.4918013762308], [397036800000, 449.6150883156143], [399715200000, 260.40849627755335], [402307200000, 339.993840833377], [404985600000, 524.6332700044682], [407577600000, 752.2415478051673], [410256000000, 971.9999999999999], [412934400000, 972.0000000000006], [415353600000, 972], [418032000000, 971.9999999999998], [420624000000, 972], [423302400000, 972.0000000000001], [425894400000, 936.0607641163615], [428572800000, 819.2920210344649], [431251200000, 778.6611510285929], [433843200000, 920.5596289558962], [436521600000, 972], [439113600000, 971.9999999999992], [441792000000, 971.9999999999997], [444470400000, 972.0000000000001], [446976000000, 972.0000000000007], [449654400000, 972], [452246400000, 846.1557064795746], [454924800000, 549.7196769546313], [457516800000, 304.95977396791386], [460195200000, 92.9833427115546], [462873600000, 44.999999999999986], [465465600000, 110.73704591938188], [468144000000, 183.33333333333314], [470736000000, 386.1991801242605], [473414400000, 613.6480268325286], [476092800000, 735.2171404822506], [478512000000, 779.2685457510535], [481190400000, 890.5962613090717], [483782400000, 763.1829842355039], [486460800000, 519.3137946748196], [489052800000, 235.1734151583411], [491731200000, 59.45613765840242], [494409600000, 45], [497001600000, 89.50932999879917], [499680000000, 180.48218214529686], [502272000000, 357.1299877362526], [504950400000, 583.1653681804949], [507628800000, 645.2328007728685], [510048000000, 843.7206957848955], [512726400000, 972], [515318400000, 928.487369504121], [517996800000, 743.8791486975724], [520588800000, 567.5208205484535], [523267200000, 290.49270765336564], [525945600000, 117.79230253710375], [528537600000, 200.06651901854397], [531216000000, 201.435799984169], [533808000000, 366.6666666666669], [536486400000, 550.0000000000005], [539164800000, 687.656878153536], [541584000000, 766.364276243207], [544262400000, 914.2846150857758], [546854400000, 878.4804740427152], [549532800000, 663.5237921761784], [552124800000, 367.6802671634399], [554803200000, 244.32686118086403], [557481600000, 155.28106616377391], [560073600000, 159.2608847164105], [562752000000, 209.08200222731858], [565344000000, 299.34751532756115], [568022400000, 485.42252939251165], [570700800000, 641.9644749771657], [573206400000, 652.4599182960851], [575884800000, 656.0541783595827], [578476800000, 720.3595300497686], [581155200000, 580.0229369734188], [583747200000, 414.23689555502], [586425600000, 343.78090453461346], [589104000000, 351.67171198346813], [591696000000, 441.69923564313183], [594374400000, 379.38570238711185], [596966400000, 497.0058958724029], [599644800000, 501.48793853532777], [602323200000, 585.9965983329959], [604742400000, 711.3478098648371], [607420800000, 809.0400025864104], [610012800000, 787.9284083768323], [612691200000, 601.1528865845313], [615283200000, 332.40530517729826], [617961600000, 161.72316964811748], [620640000000, 45.000000000000156], [623232000000, 148.73058896090586], [625910400000, 224.07279391106263], [628502400000, 259.36184147446625], [631180800000, 284.57628911602376], [633859200000, 335.41810880288267], [636278400000, 351.1760691260212], [638956800000, 446.46351345801986], [641548800000, 397.762472383851], [644227200000, 384.70807308009495], [646819200000, 284.2934960162567], [649497600000, 218.5160823036453], [652176000000, 324.3613017022075], [654768000000, 463.8722307139654], [657446400000, 426.3525506882787], [660038400000, 473.09024318357876], [662716800000, 504.0161812204199], [665395200000, 476.3164342297839], [667814400000, 534.4362151666635], [670492800000, 750.1956652314497], [673084800000, 685.1093920638706], [675763200000, 531.4513271913755], [678355200000, 375.1348753712193], [681033600000, 323.6358496607419], [683712000000, 353.7976638897703], [686304000000, 426.7765151869797], [688982400000, 411.77145078960444], [691574400000, 477.0034045141717], [694252800000, 483.9009624469967], [696931200000, 442.89984708763666], [699436800000, 627.0926459341262], [702115200000, 739.5263112524299], [704707200000, 638.6097311568841], [707385600000, 466.45609597885766], [709977600000, 379.46385340584834], [712656000000, 337.22439022755884], [715334400000, 354.753006458925], [717926400000, 350.58133629319065], [720604800000, 393.2086252357945], [723196800000, 347.95818484079507], [725875200000, 556.7190109288999], [728553600000, 813.6646138941102], [730972800000, 972], [733651200000, 971.9999999999999], [736243200000, 814.9924918083652], [738921600000, 567.6430444841832], [741513600000, 391.16688994626537], [744192000000, 223.09955206794575], [746870400000, 147.81709899361417], [749462400000, 209.173838446054], [752140800000, 346.0313793016388], [754732800000, 453.59487186974684], [757411200000, 669.80828776031], [760089600000, 804.6531177143058], [762508800000, 902.1625932142015], [765187200000, 938.628646861334], [767779200000, 940.8612336392467], [770457600000, 809.5923837871791], [773049600000, 495.8945623583375], [775728000000, 293.58607956882014], [778406400000, 108.00025627924688], [780998400000, 185.58853646455253], [783676800000, 185.25230387797117], [786268800000, 270.7364719939244], [788947200000, 480.4224148443448], [791625600000, 500.69131198102286], [794044800000, 650.4934403899758], [796723200000, 855.0688711371673], [799315200000, 797.6431188461884], [801993600000, 735.7143489190778], [804585600000, 647.6008920528584], [807264000000, 528.3052414309639], [809942400000, 377.0020229003233], [812534400000, 418.071371295732], [815212800000, 531.8454613293145], [817804800000, 459.24373393491265], [820483200000, 635.7564416998729], [823161600000, 745.916641653373], [825667200000, 873.7757401986612], [828345600000, 972.0000000000002], [830937600000, 835.7934687980505], [833616000000, 636.2590687246093], [836208000000, 383.4541797457234], [838886400000, 109.95234327209972], [841564800000, 45], [844156800000, 97.97236812465474], [846835200000, 255.406621251439], [849427200000, 448.85117728569327], [852105600000, 696.7842955081695], [854784000000, 943.3675421828632], [857203200000, 972], [859881600000, 972], [862473600000, 888.4100023742511], [865152000000, 565.4216545180179], [867744000000, 268.4368423601433], [870422400000, 44.99999999999999], [873100800000, 44.999999999999986], [875692800000, 97.99682883214832], [878371200000, 176.22544688123722], [880963200000, 229.9661032311809], [883641600000, 451.36749236302114], [886320000000, 668.3696036095624], [888739200000, 972.0000000000001], [891417600000, 972], [894009600000, 972.0000000000001], [896688000000, 968.4472901790294], [899280000000, 906.7117175295917], [901958400000, 768.2326387267109], [904636800000, 602.8014736125815], [907228800000, 652.885713677154], [909907200000, 788.3384434297346], [912499200000, 962.4774367821218], [915177600000, 972], [917856000000, 972.0000000000002], [920275200000, 971.9999999999998], [922953600000, 972], [925545600000, 852.2762326913287], [928224000000, 567.6702178226676], [930816000000, 335.2841806300695], [933494400000, 134.7709145753135], [936172800000, 46.48933968405965], [938764800000, 109.4409567615522], [941443200000, 183.33333333333536], [944035200000, 366.66666666666663], [946713600000, 550.0000000000005], [949392000000, 668.4101147840751], [951897600000, 799.6671769068007], [954576000000, 909.1905549026028], [957168000000, 802.3492813370893], [959846400000, 536.3876961931823], [962438400000, 294.6997792958989], [965116800000, 110.77315141677335], [967795200000, 45], [970387200000, 125.20965828956693], [973065600000, 311.4296340481863], [975657600000, 366.6666666666669], [978336000000, 550], [981014400000, 741.5037541914724], [983433600000, 864.8333542411841], [986112000000, 964.6461272576381], [988704000000, 880.6971942289398], [991382400000, 586.5765566225061], [993974400000, 262.5490902303056], [996652800000, 211.04565065345392], [999331200000, 130.06848157113336], [1001923200000, 193.40006067889578], [1004601600000, 229.43782469751193], [1007193600000, 426.00536852962915], [1009872000000, 656.2261444387378], [1012550400000, 774.9019283375752], [1014969600000, 734.2363180798018], [1017648000000, 814.2727697004739], [1020240000000, 728.5277932904502], [1022918400000, 516.1785988256659], [1025510400000, 258.8255676021905], [1028188800000, 105.60313385817855], [1030867200000, 45.000000000000625], [1033459200000, 126.08320360911799], [1036137600000, 267.5647463828416], [1038729600000, 408.7981027509921], [1041408000000, 623.3590512467329], [1044086400000, 717.6527762199671], [1046505600000, 822.6180297829083], [1049184000000, 878.0468344867213], [1051776000000, 796.7508630472383], [1054454400000, 565.1689912792618], [1057046400000, 316.69209817277925], [1059724800000, 117.75487605133323], [1062403200000, 56.466489584760964], [1064995200000, 129.23916347133746], [1067673600000, 271.3516197940668], [1070265600000, 366.66666666666646], [1072944000000, 567.9287358604975], [1075622400000, 674.3995738768517], [1078128000000, 807.5678813935061], [1080806400000, 832.0814316335474], [1083398400000, 701.8038228931711], [1086076800000, 463.6771874785254], [1088668800000, 218.51429892206033], [1091347200000, 66.4394933708195], [1094025600000, 45.00000000000001], [1096617600000, 131.51949127890106], [1099296000000, 331.8503572490482], [1101888000000, 524.4165473393064], [1104566400000, 718.1425145705947], [1107244800000, 950.3599389352122], [1109664000000, 972.0000000005588], [1112342400000, 971.9999999999997], [1114934400000, 948.9766851940988], [1117612800000, 808.3886898619345], [1120204800000, 710.5478855073693], [1122883200000, 408.84955507770394], [1125561600000, 256.49318100456946], [1128153600000, 316.65004860063044], [1130832000000, 446.7259020457246], [1133424000000, 548.5012296479257], [1136102400000, 715.2776770499479], [1138780800000, 949.7894690317577], [1141200000000, 971.9999999999998], [1143878400000, 972.0000000000002], [1146470400000, 972.0000000000001], [1149148800000, 971.9999999999998], [1151740800000, 862.0611312793407], [1154419200000, 600.2912113637628], [1157097600000, 454.11258370955926], [1159689600000, 498.0634871852023], [1162368000000, 639.6863090833118], [1164960000000, 740.0170956817373], [1167638400000, 930.929404144138], [1170316800000, 972.0000000000002], [1172736000000, 966.6352794914312], [1175414400000, 971.9999999999999], [1178006400000, 916.6666666666665], [1180684800000, 692.5593396322736], [1183276800000, 361.6856259613227], [1185955200000, 153.88428326123244], [1188633600000, 45.00000005168228], [1191225600000, 158.92013414785984], [1193904000000, 301.85663115317186], [1196496000000, 430.0173048065427], [1199174400000, 532.0277859323685], [1201852800000, 735.999330103846], [1204358400000, 841.8714837383066], [1207036800000, 896.2077491620737], [1209628800000, 896.9694890876114], [1212307200000, 729.6439514465658], [1214899200000, 469.80731358820185], [1217577600000, 326.99321591183116], [1220256000000, 269.09942883120266], [1222848000000, 355.46256437074356], [1225526400000, 310.142901248207], [1228118400000, 366.42925471284593], [1230796800000, 396.7277846897113], [1233475200000, 381.72163228225605], [1235894400000, 481.74810085019715], [1238572800000, 578.6829783641374], [1241164800000, 451.76524170050874], [1243843200000, 298.7310238871129], [1246435200000, 215.31170294616325], [1249113600000, 130.15585147308184], [1251792000000, 147.24912030066207], [1254384000000, 254.94544637012163], [1257062400000, 366.43524812074054], [1259654400000, 498.02333094558776], [1262332800000, 695.2221550611923], [1265011200000, 905.6951048183589], [1267430400000, 971.9999999999999], [1270108800000, 972.0000000000001], [1272700800000, 913.4841556916266], [1275379200000, 664.0165769246609], [1277971200000, 414.5868900304235], [1280649600000, 181.77284141460885], [1283328000000, 64.61021810293346], [1285920000000, 124.34038103404518], [1288598400000, 272.58575708372985], [1291190400000, 458.04596428837704], [1293868800000, 702.3825562463616], [1296547200000, 891.9843710426852], [1298966400000, 971.9999999999991], [1301644800000, 972], [1304236800000, 972.0000000000001], [1306915200000, 848.5490886064492], [1309507200000, 778.0009750951854], [1312185600000, 601.5910770764831], [1314864000000, 450.3068028249494], [1317456000000, 488.70074083460867], [1320134400000, 634.1122977212915], [1322726400000, 598.5988344736796], [1325404800000, 742.5494829607504], [1328083200000, 848.7356160291855], [1330588800000, 855.8635737800132], [1333267200000, 958.9442437514409], [1335859200000, 910.0776767260602], [1338537600000, 635.9022136919299], [1341129600000, 355.50629256418193], [1343808000000, 163.1064434754604], [1346486400000, 49.41728101273482], [1349078400000, 106.69583150980661], [1351756800000, 211.87985494124968], [1354348800000, 297.2535368689754], [1357027200000, 523.7217480307453], [1359705600000, 621.7866032567023], [1362124800000, 645.7318048757876], [1364803200000, 711.11305458515], [1367395200000, 756.2977454388866], [1370073600000, 604.8259373761582], [1372665600000, 354.3743872816873], [1375344000000, 202.51451743637935], [1378022400000, 61.014050121459405], [1380614400000, 112.41260426660718], [1383292800000, 180.52245799132754], [1385884800000, 250.3336828287863], [1388563200000, 291.87482217450116], [1391241600000, 229.02250915667688], [1393660800000, 274.7990720887095], [1396339200000, 415.37533877114294], [1398931200000, 493.3758531341463], [1401609600000, 446.02576172510703], [1404201600000, 319.2454297501574], [1406880000000, 283.9924575794133], [1409558400000, 160.76317695444345], [1412150400000, 296.793641293554], [1414828800000, 247.0437698790916], [1417420800000, 262.96085432922456], [1420099200000, 514.8709778047104], [1422777600000, 486.90850435631023], [1425196800000, 602.0102770883674], [1427875200000, 521.8321091038041], [1430467200000, 535.6187132533386], [1433145600000, 455.4405548807796], [1435737600000, 328.359061737539], [1438416000000, 348.9085054353796], [1441094400000, 219.5711191255374], [1443686400000, 257.8022732073203]]
                    }, {
                        "period_months": ["Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep"],
                        "annual_period": "Sacramento River Index<br>Wet",
                        "computed_statistics": [{
                            "statistic": "Averages",
                            "statistically_computed_time_series_monthly": [["Jan", 811.2287345042109], ["Feb", 913.3392877618088], ["Mar", 956.8400118024218], ["Apr", 904.3698076061751], ["May", 720.3364070352504], ["Jun", 555.2718111202984], ["Jul", 340.1971231697662], ["Aug", 222.95871633283727], ["Sep", 283.93105093509973], ["Oct", 343.37584681796693], ["Nov", 486.0518301960353], ["Dec", 673.3743239313671]],
                            "statistic_aggregate": 596.3109767609616
                        }],
                        "aggregate_ts": [[1927, 471.3387090771419], [1938, 651.0101692359123], [1941, 533.6709832804418], [1942, 523.143697479194], [1943, 505.3185412575164], [1952, 626.8742191974467], [1953, 581.737773573297], [1956, 538.3434283298358], [1958, 672.9878558190801], [1963, 570.442084978784], [1965, 518.0557159325796], [1967, 634.9325069164396], [1969, 738.5724756329092], [1970, 623.4066210673345], [1971, 463.1150059009114], [1974, 541.1854513772788], [1975, 451.69382113454384], [1982, 622.1828491479894], [1983, 880.2873652454126], [1984, 648.4629621694213], [1986, 535.8308252149141], [1995, 537.2501508057959], [1996, 527.2474539567727], [1997, 516.5562470260605], [1998, 695.083956650839], [1999, 640.395643531404], [2006, 746.9018909427683], [2011, 700.6789444249024]],
                        "month_period": "Oct - Sep",
                        "discrete_ts": [[-1362240000000, 149.14174311801762], [-1359648000000, 367.27423240081333], [-1356969600000, 541.1777477467612], [-1354291200000, 647.0177678514375], [-1351872000000, 809.5054270040075], [-1349193600000, 933.0495667914182], [-1346601600000, 833.9403813949697], [-1343923200000, 578.8046886246354], [-1341331200000, 382.7927897923574], [-1338652800000, 172.8758353087534], [-1335974400000, 81.8525109043387], [-1333382400000, 158.63181798819332], [-1015084800000, 175.03104053156412], [-1012492800000, 350.0590699498221], [-1009814400000, 550.8118848705818], [-1007136000000, 728.424789616228], [-1004716800000, 923.8943265777923], [-1002038400000, 972], [-999446400000, 972.0000000000001], [-996768000000, 857.701116659172], [-994176000000, 752.8162264366803], [-991497600000, 592.5074404787504], [-988819200000, 441.27619758567744], [-986227200000, 495.5999381246792], [-920390400000, 200.25895212541295], [-917798400000, 366.66666666666686], [-915120000000, 588.2028677489837], [-912441600000, 758.7944023286171], [-910022400000, 971.9999999999995], [-907344000000, 972.0000000000001], [-904752000000, 972.0000000000001], [-902073600000, 717.5845099396031], [-899481600000, 488.1873953197787], [-896803200000, 197.3237163572389], [-894124800000, 59.34834770750707], [-891532800000, 111.68494117149329], [-888854400000, 255.56969001051357], [-886262400000, 366.66666666666646], [-883584000000, 571.6488640872142], [-880905600000, 758.2046713047182], [-878486400000, 870.8898647136772], [-875808000000, 962.9934982282259], [-873216000000, 911.6595492950282], [-870537600000, 697.1919326501966], [-867945600000, 508.6047838924634], [-865267200000, 216.56508174870126], [-862588800000, 50.01558025806283], [-859996800000, 107.71418689485992], [-857318400000, 239.57239554973114], [-854726400000, 366.66666666666646], [-852048000000, 521.3172972744476], [-849369600000, 731.7796128718663], [-846950400000, 818.2080656929064], [-844272000000, 971.9999999999999], [-841680000000, 932.7024307324141], [-839001600000, 691.205372293747], [-836409600000, 460.8129154516315], [-833731200000, 191.55754124164542], [-831052800000, 45.00000000000001], [-828460800000, 93.00019731514068], [-573321600000, 205.64620169408568], [-570729600000, 366.6666666666665], [-568051200000, 574.1505430318252], [-565372800000, 801.8998897908539], [-562867200000, 964.7824062217849], [-560188800000, 971.9999999999999], [-557596800000, 948.0514116777142], [-554918400000, 805.5491158242783], [-552326400000, 728.0806412110009], [-549648000000, 487.352246231898], [-546969600000, 311.6415564276364], [-544377600000, 356.6699515916156], [-541699200000, 479.23770308657976], [-539107200000, 593.7131006353452], [-536428800000, 825.0630907551952], [-533750400000, 927.0101135531804], [-531331200000, 960.4990040330117], [-528652800000, 972.0000000000259], [-526060800000, 874.5634487751151], [-523382400000, 628.9343669391161], [-520790400000, 412.2171676604203], [-518112000000, 154.79727266130118], [-515433600000, 45.00000000000002], [-512841600000, 107.81801478027306], [-447091200000, 181.70569687747982], [-444499200000, 345.2157292898999], [-441820800000, 588.8814801701756], [-439142400000, 830.3352135812489], [-436636800000, 971.9999999999999], [-433958400000, 971.9999999999995], [-431366400000, 936.9506378880925], [-428688000000, 698.6808466173973], [-426096000000, 464.6862103818726], [-423417600000, 219.11183165578956], [-420739200000, 93.37853372959195], [-418147200000, 157.1749597664818], [-383932800000, 403.5313590358606], [-381340800000, 570.2304485136691], [-378662400000, 736.8054689496754], [-375984000000, 872.2231467128154], [-373564800000, 971.9999999999995], [-370886400000, 971.9999999999999], [-368294400000, 972.0000000000003], [-365616000000, 839.2739344393038], [-363024000000, 726.4602981206316], [-360345600000, 451.74218670026653], [-357667200000, 252.6527565084619], [-355075200000, 306.93467084827734], [-226166400000, 390.283621315389], [-223574400000, 583.7577160810367], [-220896000000, 776.542517116807], [-218217600000, 867.9075928562954], [-215798400000, 971.9999999999998], [-213120000000, 972.0000000000002], [-210528000000, 921.5163596826434], [-207849600000, 641.0369194189742], [-205257600000, 403.22380410767795], [-202579200000, 162.09742902219776], [-199900800000, 44.99999999999999], [-197308800000, 109.93906014438703], [-163008000000, 250.09110115192993], [-160416000000, 453.5666235336714], [-157737600000, 659.4155884731897], [-155059200000, 844.8388232520242], [-152640000000, 971.9999999999999], [-149961600000, 972.0000000000003], [-147369600000, 909.0243550255034], [-144691200000, 577.3589723927405], [-142099200000, 335.7750925371972], [-139420800000, 89.30422516777637], [-136742400000, 44.999999999999986], [-134150400000, 108.293809656922], [-99936000000, 152.26507074811616], [-97344000000, 346.80272037471565], [-94665600000, 578.0129382045432], [-91987200000, 735.0801252157918], [-89568000000, 845.4872774549282], [-86889600000, 960.2477342724237], [-84297600000, 972.0000000000001], [-81619200000, 827.4430932474949], [-79027200000, 741.8688166126105], [-76348800000, 593.6664044197146], [-73670400000, 410.49368169984973], [-71078400000, 455.8222207470873], [-36777600000, 163.63196895153465], [-34185600000, 345.3330348879538], [-31507200000, 563.6646149268191], [-28828800000, 823.2765609726093], [-26409600000, 971.9999999999999], [-23731200000, 972], [-21139200000, 972], [-18460800000, 971.9999999999999], [-15868800000, 913.822376757311], [-13190400000, 805.0251279005593], [-10512000000, 650.5570824930857], [-7920000000, 709.5589407050388], [-5241600000, 861.4428910858929], [-2649600000, 899.0949478858147], [28800000, 972.0000000000001], [2707200000, 971.9999999999999], [5126400000, 971.9999999999999], [7804800000, 971.9999999999998], [10396800000, 802.5439575848823], [13075200000, 515.7105638390385], [15667200000, 285.19914215577364], [18345600000, 71.20256345001093], [21024000000, 45.000000000000014], [23616000000, 112.6853868066013], [26294400000, 158.8706229251306], [28886400000, 370.7237424030655], [31564800000, 602.8666312886522], [34243200000, 710.2966209086802], [36662400000, 755.3782276159217], [39340800000, 811.7497291454201], [41932800000, 694.8337422028816], [44611200000, 522.5482710381798], [47203200000, 357.7486633062023], [49881600000, 203.93896838686763], [52560000000, 143.83381864176008], [55152000000, 224.5910329481751], [120988800000, 331.6312271885621], [123580800000, 523.4240343054065], [126259200000, 719.4680678793266], [128937600000, 918.6464651168321], [131356800000, 972.0000000000005], [134035200000, 971.9999999999998], [136627200000, 853.808921867938], [139305600000, 565.9054478973843], [141897600000, 361.40475713112005], [144576000000, 138.91785180809038], [147254400000, 45.000000000000036], [149846400000, 92.01864333268608], [152524800000, 244.68339414457742], [155116800000, 366.6666666666668], [157795200000, 581.2556983488128], [160473600000, 607.7840326255497], [162892800000, 696.4261379291145], [165571200000, 860.6204858821261], [168163200000, 763.5640919409967], [170841600000, 498.5344310843516], [173433600000, 334.84753046380956], [176112000000, 169.68585219254632], [178790400000, 114.11361336025033], [181382400000, 182.14391897572375], [373449600000, 244.19953961383024], [376041600000, 448.95126139366505], [378720000000, 589.8429471284514], [381398400000, 753.32203063225], [383817600000, 876.6055829561701], [386496000000, 972.0000000000002], [389088000000, 972], [391766400000, 832.7636012487299], [394358400000, 726.4918013762308], [397036800000, 449.6150883156143], [399715200000, 260.40849627755335], [402307200000, 339.993840833377], [404985600000, 524.6332700044682], [407577600000, 752.2415478051673], [410256000000, 971.9999999999999], [412934400000, 972.0000000000006], [415353600000, 972], [418032000000, 971.9999999999998], [420624000000, 972], [423302400000, 972.0000000000001], [425894400000, 936.0607641163615], [428572800000, 819.2920210344649], [431251200000, 778.6611510285929], [433843200000, 920.5596289558962], [436521600000, 972], [439113600000, 971.9999999999992], [441792000000, 971.9999999999997], [444470400000, 972.0000000000001], [446976000000, 972.0000000000007], [449654400000, 972], [452246400000, 846.1557064795746], [454924800000, 549.7196769546313], [457516800000, 304.95977396791386], [460195200000, 92.9833427115546], [462873600000, 44.999999999999986], [465465600000, 110.73704591938188], [499680000000, 180.48218214529686], [502272000000, 357.1299877362526], [504950400000, 583.1653681804949], [507628800000, 645.2328007728685], [510048000000, 843.7206957848955], [512726400000, 972], [515318400000, 928.487369504121], [517996800000, 743.8791486975724], [520588800000, 567.5208205484535], [523267200000, 290.49270765336564], [525945600000, 117.79230253710375], [528537600000, 200.06651901854397], [783676800000, 185.25230387797117], [786268800000, 270.7364719939244], [788947200000, 480.4224148443448], [791625600000, 500.69131198102286], [794044800000, 650.4934403899758], [796723200000, 855.0688711371673], [799315200000, 797.6431188461884], [801993600000, 735.7143489190778], [804585600000, 647.6008920528584], [807264000000, 528.3052414309639], [809942400000, 377.0020229003233], [812534400000, 418.071371295732], [815212800000, 531.8454613293145], [817804800000, 459.24373393491265], [820483200000, 635.7564416998729], [823161600000, 745.916641653373], [825667200000, 873.7757401986612], [828345600000, 972.0000000000002], [830937600000, 835.7934687980505], [833616000000, 636.2590687246093], [836208000000, 383.4541797457234], [838886400000, 109.95234327209972], [841564800000, 45], [844156800000, 97.97236812465474], [846835200000, 255.406621251439], [849427200000, 448.85117728569327], [852105600000, 696.7842955081695], [854784000000, 943.3675421828632], [857203200000, 972], [859881600000, 972], [862473600000, 888.4100023742511], [865152000000, 565.4216545180179], [867744000000, 268.4368423601433], [870422400000, 44.99999999999999], [873100800000, 44.999999999999986], [875692800000, 97.99682883214832], [878371200000, 176.22544688123722], [880963200000, 229.9661032311809], [883641600000, 451.36749236302114], [886320000000, 668.3696036095624], [888739200000, 972.0000000000001], [891417600000, 972], [894009600000, 972.0000000000001], [896688000000, 968.4472901790294], [899280000000, 906.7117175295917], [901958400000, 768.2326387267109], [904636800000, 602.8014736125815], [907228800000, 652.885713677154], [909907200000, 788.3384434297346], [912499200000, 962.4774367821218], [915177600000, 972], [917856000000, 972.0000000000002], [920275200000, 971.9999999999998], [922953600000, 972], [925545600000, 852.2762326913287], [928224000000, 567.6702178226676], [930816000000, 335.2841806300695], [933494400000, 134.7709145753135], [936172800000, 46.48933968405965], [938764800000, 109.4409567615522], [1130832000000, 446.7259020457246], [1133424000000, 548.5012296479257], [1136102400000, 715.2776770499479], [1138780800000, 949.7894690317577], [1141200000000, 971.9999999999998], [1143878400000, 972.0000000000002], [1146470400000, 972.0000000000001], [1149148800000, 971.9999999999998], [1151740800000, 862.0611312793407], [1154419200000, 600.2912113637628], [1157097600000, 454.11258370955926], [1159689600000, 498.0634871852023], [1288598400000, 272.58575708372985], [1291190400000, 458.04596428837704], [1293868800000, 702.3825562463616], [1296547200000, 891.9843710426852], [1298966400000, 971.9999999999991], [1301644800000, 972], [1304236800000, 972.0000000000001], [1306915200000, 848.5490886064492], [1309507200000, 778.0009750951854], [1312185600000, 601.5910770764831], [1314864000000, 450.3068028249494], [1317456000000, 488.70074083460867]]
                    }]
                }]
            }], "scenario_name": "DCR_Baseline_4.0", "scenario_color": "#A3F244FF"
        }, {
            "ts_list": [{
                "ts_name": "DCR_Baseline_5.0_DV (CalSim3)", "monthly_filters": [{
                    "annual_filters": [{
                        "period_months": ["Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep"],
                        "annual_period": "Long Term<br>",
                        "computed_statistics": [{
                            "statistic": "Averages",
                            "statistically_computed_time_series_monthly": [["Jan", 708.7704356523816], ["Feb", 800.2482146021382], ["Mar", 865.7386360009242], ["Apr", 808.1286494953808], ["May", 614.0802728847694], ["Jun", 417.87315339224995], ["Jul", 243.5625354301398], ["Aug", 154.45974234770742], ["Sep", 223.83704156359144], ["Oct", 301.4127453545793], ["Nov", 417.2875489398823], ["Dec", 578.5220353385704]],
                            "statistic_aggregate": 511.1600842501929
                        }],
                        "aggregate_ts": [[1922, 566.8781159883885], [1923, 648.816394030266], [1924, 555.945652485108], [1925, 428.96112351846136], [1926, 513.8878129312441], [1927, 465.72811811308367], [1928, 453.83132205862626], [1929, 513.374207565085], [1930, 533.2090866180624], [1931, 362.7877953833429], [1932, 532.0629878153064], [1933, 344.2858309927767], [1934, 405.4946751441801], [1935, 340.8144193750908], [1936, 403.9831596044551], [1937, 487.9121681050927], [1938, 650.6965373557415], [1939, 585.5251954935027], [1940, 325.3732551860126], [1941, 533.3483039967355], [1942, 525.0217913840183], [1943, 505.95545733419476], [1944, 468.61423141219626], [1945, 551.5306112807385], [1946, 604.1585083530405], [1947, 575.7289913137381], [1948, 332.0389858540787], [1949, 542.2067065499251], [1950, 427.518675659904], [1951, 547.623825966241], [1952, 626.2704467170876], [1953, 577.9310117455706], [1954, 423.06759069193987], [1955, 518.7872420695747], [1956, 536.7269080967388], [1957, 458.22943420387725], [1958, 669.2765780385847], [1959, 460.4709703254034], [1960, 399.4082054504816], [1961, 383.2058033598774], [1962, 389.93594841465045], [1963, 568.3065156282618], [1964, 441.18769317030666], [1965, 509.5344407717652], [1966, 484.8789959256863], [1967, 627.5701143060577], [1968, 519.2127730830304], [1969, 758.0588101726945], [1970, 622.6818260851719], [1971, 463.15867010245415], [1972, 443.71100136070635], [1973, 502.9689683535794], [1974, 540.849205363438], [1975, 453.82805417296316], [1976, 505.5835452210717], [1977, 305.697148741504], [1978, 503.49503934323644], [1979, 500.5260690313067], [1980, 600.706447302994], [1981, 475.2593808914348], [1982, 618.4672625898953], [1983, 882.8931124136516], [1984, 648.4709714463395], [1985, 438.94779338407733], [1986, 536.1409208772901], [1987, 493.42002480733953], [1988, 482.0174267483672], [1989, 443.96751585603874], [1990, 491.92450827300974], [1991, 399.98032263439495], [1992, 477.01377111645985], [1993, 480.6433658426049], [1994, 575.6356585233436], [1995, 539.6695238645699], [1996, 529.4662268516191], [1997, 514.6442604572045], [1998, 694.1101708132366], [1999, 640.4743432245762], [2000, 448.77505689466483], [2001, 504.32822317179006], [2002, 451.25888403123514], [2003, 472.0601134397977], [2004, 429.4613595031133], [2005, 614.0884914140998], [2006, 748.8463680006304], [2007, 627.8555726180114], [2008, 563.4732288256091], [2009, 334.5891473114857], [2010, 558.8488393106577], [2011, 700.6986122995786], [2012, 571.70152763968], [2013, 435.1314198701596], [2014, 293.6690984599148], [2015, 402.5360119935994]],
                        "month_period": "Oct - Sep",
                        "discrete_ts": [[-1522684800000, 300], [-1520006400000, 278.82000947501155], [-1517414400000, 290.27777777777806], [-1514736000000, 519.6744209237904], [-1512057600000, 713.9268130441114], [-1509638400000, 901.9612245654863], [-1506960000000, 971.9999999999997], [-1504368000000, 932.7783699998306], [-1501689600000, 768.1490908191614], [-1499097600000, 597.4934418182132], [-1496419200000, 343.7671850197421], [-1493740800000, 217.45921777335258], [-1491148800000, 266.22984064418466], [-1488470400000, 426.00324729184416], [-1485878400000, 629.8450612975386], [-1483200000000, 867.0434669154768], [-1480521600000, 972], [-1478102400000, 972], [-1475424000000, 972.0000000000001], [-1472832000000, 970.5217503077325], [-1470153600000, 722.1340275885436], [-1467561600000, 519.1634891432226], [-1464883200000, 282.08174457161164], [-1462204800000, 177.13835151775066], [-1459612800000, 275.8655897294709], [-1456934400000, 382.44503430661814], [-1454342400000, 536.9654152220039], [-1451664000000, 550.0000000000001], [-1448985600000, 692.9411354144072], [-1446480000000, 724.8443642862593], [-1443801600000, 780.9871429534599], [-1441209600000, 858.2387182376409], [-1438531200000, 731.3886857656511], [-1435939200000, 524.1973781833259], [-1433260800000, 431.9309467089952], [-1430582400000, 203.3756285739053], [-1427990400000, 254.03338016902862], [-1425312000000, 304.56308089120324], [-1422720000000, 332.95039727391725], [-1420041600000, 390.3066361536645], [-1417363200000, 528.8346915611942], [-1414944000000, 662.3631620142966], [-1412265600000, 750.5887945847961], [-1409673600000, 721.2629136434225], [-1406995200000, 558.069294586044], [-1404403200000, 388.4884009897838], [-1401724800000, 208.48528909640606], [-1399046400000, 103.72075768153837], [-1396454400000, 197.9000637452698], [-1393776000000, 310.0233957378994], [-1391184000000, 478.8755355430545], [-1388505600000, 605.7768303880355], [-1385827200000, 741.6609279590201], [-1383408000000, 858.0720314291829], [-1380729600000, 913.2088300882755], [-1378137600000, 952.4335226114588], [-1375459200000, 658.373078616184], [-1372867200000, 338.13865950995284], [-1370188800000, 108.1836189610297], [-1367510400000, 45], [-1364918400000, 156.90732433083573], [-1362240000000, 156.90732433083565], [-1359648000000, 374.0051957181023], [-1356969600000, 531.3423552374406], [-1354291200000, 635.2933356582537], [-1351872000000, 794.4376631741345], [-1349193600000, 919.2298840994243], [-1346601600000, 821.7294781981964], [-1343923200000, 569.1232464222228], [-1341331200000, 376.7099976244466], [-1338652800000, 170.74309677883966], [-1335974400000, 82.72253407030719], [-1333382400000, 156.49330604480002], [-1330704000000, 183.33333333333334], [-1328112000000, 366.6666666666671], [-1325433600000, 573.699481449513], [-1322755200000, 703.575094094423], [-1320249600000, 760.2014431639444], [-1317571200000, 903.5986454568146], [-1314979200000, 768.0016848678248], [-1312300800000, 524.1165342671328], [-1309708800000, 317.8774501537089], [-1307030400000, 152.42209982483868], [-1304352000000, 57.27870564853006], [-1301760000000, 135.20472577678447], [-1299081600000, 135.52521112442435], [-1296489600000, 271.0504222488486], [-1293811200000, 477.4906630407223], [-1291132800000, 621.5673573740162], [-1288713600000, 708.7684701049982], [-1286035200000, 819.5280010323294], [-1283443200000, 861.2918515443733], [-1280764800000, 674.3473098695448], [-1278172800000, 512.4912216279475], [-1275494400000, 370.04558166189673], [-1272816000000, 304.68357710119574], [-1270224000000, 403.7008240507231], [-1267545600000, 364.48136569572466], [-1264953600000, 370.2117178790977], [-1262275200000, 531.338404619524], [-1259596800000, 725.8790923281529], [-1257177600000, 806.7254555849204], [-1254499200000, 915.5826096670133], [-1251907200000, 972.0000000000001], [-1249228800000, 793.9803476355885], [-1246636800000, 464.64555101246003], [-1243958400000, 223.56332135018855], [-1241280000000, 90.44263977900002], [-1238688000000, 139.65853386507945], [-1236009600000, 173.87813134530776], [-1233417600000, 211.96910781788208], [-1230739200000, 248.02795438052976], [-1228060800000, 357.29860586019566], [-1225641600000, 435.84557091054967], [-1222963200000, 503.36537180149344], [-1220371200000, 511.90129499426325], [-1217692800000, 457.0584381360366], [-1215100800000, 340.5900905623302], [-1212422400000, 365.6267688248357], [-1209744000000, 351.3782981417225], [-1207152000000, 396.51391182496815], [-1204473600000, 396.8153856614436], [-1201881600000, 417.87018625735584], [-1199203200000, 616.6264235545419], [-1196524800000, 783.6988241423386], [-1194019200000, 972.0000000000003], [-1191340800000, 972], [-1188748800000, 834.5856836706768], [-1186070400000, 618.722934012547], [-1183478400000, 424.27827280668663], [-1180800000000, 146.2535832743867], [-1178121600000, 64.43884734861284], [-1175529600000, 137.46571305508655], [-1172851200000, 57.6348445099459], [-1170259200000, 115.46440086706343], [-1167580800000, 284.13050174149157], [-1164902400000, 473.45802313739733], [-1162483200000, 561.9063988462083], [-1159804800000, 636.3120332528076], [-1157212800000, 560.9553345183192], [-1154534400000, 461.1644376825446], [-1151942400000, 344.2863393142679], [-1149264000000, 143.67146874895474], [-1146585600000, 183.9172497941564], [-1143993600000, 308.52893950016346], [-1141315200000, 266.44780383100925], [-1138723200000, 270.4194207511556], [-1136044800000, 374.7212461433434], [-1133366400000, 537.7528825237657], [-1130947200000, 707.5262412324454], [-1128268800000, 696.4651034519113], [-1125676800000, 584.2444075835979], [-1122998400000, 469.3457197436155], [-1120406400000, 357.15960645573915], [-1117728000000, 275.13537567239985], [-1115049600000, 201.67181219110867], [-1112457600000, 125.04648215006962], [-1109779200000, 82.55389068231857], [-1107187200000, 203.08205176016372], [-1104508800000, 209.71892821833603], [-1101830400000, 401.51651118545755], [-1099411200000, 571.6952107263804], [-1096732800000, 752.2708907417967], [-1094140800000, 726.8641655207417], [-1091462400000, 490.63095200301666], [-1088870400000, 310.81687397351175], [-1086192000000, 152.56958732949641], [-1083513600000, 65.01518492408043], [-1080921600000, 123.03878543578931], [-1078243200000, 173.32133286089288], [-1075651200000, 254.25130645857007], [-1072972800000, 345.44184937812827], [-1070294400000, 430.1668000890044], [-1067788800000, 636.4843356595024], [-1065110400000, 787.2383514420812], [-1062518400000, 727.7406030995712], [-1059840000000, 558.7588129496435], [-1057248000000, 387.40835066801776], [-1054569600000, 218.58579623957093], [-1051891200000, 121.86198136452097], [-1049299200000, 206.538395043958], [-1046620800000, 216.25205314584284], [-1044028800000, 253.47203978844834], [-1041350400000, 443.42599937885103], [-1038672000000, 510.4531139604801], [-1036252800000, 695.7820007249082], [-1033574400000, 913.3235333171017], [-1030982400000, 882.3720226220341], [-1028304000000, 715.1798184143082], [-1025712000000, 599.6888923850406], [-1023033600000, 307.38560460299504], [-1020355200000, 126.60690558228454], [-1017763200000, 191.00403333881815], [-1015084800000, 172.81949596140953], [-1012492800000, 348.20426965932114], [-1009814400000, 549.4172129070807], [-1007136000000, 729.043586737346], [-1004716800000, 925.186413403843], [-1002038400000, 971.9999999999998], [-999446400000, 972.0000000000001], [-996768000000, 857.6312085750293], [-994176000000, 752.7465205998355], [-991497600000, 592.4613135222265], [-988819200000, 441.2519682778492], [-986227200000, 495.59645862495665], [-983548800000, 650.209632955593], [-980956800000, 598.9960657818505], [-978278400000, 795.7578118515852], [-975600000000, 872.8602554630523], [-973180800000, 917.179838029904], [-970502400000, 971.9999999999999], [-967910400000, 885.5545819589329], [-965232000000, 633.2852545822442], [-962640000000, 357.3796288342711], [-959961600000, 163.87509568708205], [-957283200000, 45.000000000000014], [-954691200000, 134.2041807775167], [-952012800000, 163.62098116524623], [-949420800000, 216.79102836484796], [-946742400000, 199.60745879498052], [-944064000000, 410.6185177961663], [-941558400000, 597.4595873082535], [-938880000000, 747.8503569251509], [-936288000000, 705.3807084298562], [-933609600000, 438.429021455782], [-931017600000, 203.75327454047172], [-928339200000, 45.00000000000005], [-925660800000, 45], [-923068800000, 130.96812745139613], [-920390400000, 195.39447458191597], [-917798400000, 366.66666666666686], [-915120000000, 588.436523781235], [-912441600000, 759.2720749809165], [-910022400000, 972.0000000000001], [-907344000000, 972.0000000000001], [-904752000000, 972.0000000000001], [-902073600000, 717.6085636308063], [-899481600000, 488.22948330479346], [-896803200000, 197.38438083103102], [-894124800000, 59.427318962389144], [-891532800000, 111.76016122107126], [-888854400000, 267.39169127058796], [-886262400000, 366.6666666666665], [-883584000000, 571.6488640872144], [-880905600000, 758.2311547340103], [-878486400000, 870.9340100750456], [-875808000000, 963.0508836843071], [-873216000000, 911.7350272278693], [-870537600000, 697.2780717135329], [-867945600000, 511.2377391665454], [-865267200000, 219.18272629164778], [-862588800000, 52.61099323569843], [-859996800000, 110.29366845509449], [-857318400000, 242.14859520716283], [-854726400000, 366.66666666666663], [-852048000000, 521.3391726693181], [-849369600000, 733.8767546781205], [-846950400000, 820.3118570682742], [-844272000000, 972.0000000000001], [-841680000000, 932.757848940258], [-839001600000, 691.2831822405572], [-836409600000, 461.16336213736565], [-833731200000, 191.90571768426003], [-831052800000, 45], [-828460800000, 93.0123307183536], [-825782400000, 195.2898683463874], [-823190400000, 366.6666666666667], [-820512000000, 550.0000000000003], [-817833600000, 688.8368903030649], [-815328000000, 808.400014153898], [-812649600000, 878.1621012578546], [-810057600000, 805.3532234181926], [-807379200000, 564.0423251930641], [-804787200000, 361.2598593262709], [-802108800000, 196.0783861478706], [-799430400000, 69.74369734572849], [-796838400000, 139.53774478735664], [-794160000000, 252.89629458143258], [-791568000000, 460.41530441975516], [-788889600000, 688.9118171604807], [-786211200000, 776.3488633705225], [-783792000000, 932.4695148793327], [-781113600000, 972.000000000018], [-778521600000, 830.132561308128], [-775843200000, 626.3484534185974], [-773251200000, 439.24900709281405], [-770572800000, 241.72409777514042], [-767894400000, 158.91054209336707], [-765302400000, 238.9608792692741], [-762624000000, 422.89107012006], [-760032000000, 619.7346243865883], [-757353600000, 855.1279606939502], [-754675200000, 972], [-752256000000, 971.9999999999995], [-749577600000, 971.9999999999998], [-746985600000, 835.8043284231514], [-744307200000, 632.3768581663014], [-741715200000, 420.85358938752074], [-739036800000, 242.9915808204672], [-736358400000, 112.45629180555409], [-733766400000, 191.66579643289236], [-731088000000, 338.73732487876083], [-728496000000, 550.4935363281868], [-725817600000, 783.0677957193694], [-723139200000, 894.4228144243876], [-720720000000, 916.6666666666664], [-718041600000, 972.0000000000001], [-715449600000, 876.6391572769791], [-712771200000, 625.2582564831615], [-710179200000, 386.2720181223889], [-707500800000, 233.51894495971746], [-704822400000, 134.04910759247565], [-702230400000, 197.62227331276395], [-699552000000, 237.71329662012153], [-696960000000, 336.8448946183265], [-694281600000, 434.1336113580062], [-691603200000, 395.8568523005416], [-689097600000, 380.81301180963766], [-686419200000, 531.024888071331], [-683827200000, 485.1155738050781], [-681148800000, 343.86331790375766], [-678556800000, 258.05813268860027], [-675878400000, 140.85754285632248], [-673200000000, 162.0515351943467], [-670608000000, 278.1351730228747], [-667929600000, 462.7015433640741], [-665337600000, 575.6903857133709], [-662659200000, 709.7636750127343], [-659980800000, 784.240455679031], [-657561600000, 897.7406757980625], [-654883200000, 972.0000000000001], [-652291200000, 798.9721568699407], [-649612800000, 579.0342628651289], [-647020800000, 338.4120465702324], [-644342400000, 154.00561564771036], [-641664000000, 76.3136143532222], [-639072000000, 157.60604672559413], [-636393600000, 202.63563356025696], [-633801600000, 366.6666666666667], [-631123200000, 509.7541974078622], [-628444800000, 643.7099024072634], [-626025600000, 699.5866466691239], [-623347200000, 799.2995029155192], [-620755200000, 677.2043389631799], [-618076800000, 459.48332796846813], [-615484800000, 338.9867604963907], [-612806400000, 179.1979396014592], [-610128000000, 70.51566624313831], [-607536000000, 183.18352501951983], [-604857600000, 356.547130170452], [-602265600000, 564.1842598728272], [-599587200000, 810.5632772588166], [-596908800000, 954.7755355089525], [-594489600000, 971.9999999999998], [-591811200000, 972.0000000000002], [-589219200000, 846.6733480617626], [-586540800000, 558.6615699100719], [-583948800000, 312.78949690465896], [-581270400000, 70.88582285307774], [-578592000000, 44.99999999999999], [-576000000000, 107.40547105427348], [-573321600000, 196.69524991472215], [-570729600000, 366.6666666666664], [-568051200000, 574.6567360733538], [-565372800000, 802.4215268329044], [-562867200000, 965.3303774358257], [-560188800000, 972.0000000000001], [-557596800000, 948.0526387337896], [-554918400000, 805.5490596543714], [-552326400000, 728.08058519158], [-549648000000, 487.3748954951372], [-546969600000, 311.68440331409187], [-544377600000, 356.7332212926087], [-541699200000, 479.3200542324257], [-539107200000, 582.6951630506007], [-536428800000, 814.0716020772121], [-533750400000, 915.5665455384365], [-531331200000, 949.0341096468328], [-528652800000, 972.0000000000003], [-526060800000, 874.5714172781647], [-523382400000, 628.700777114801], [-520790400000, 411.89425553493044], [-518112000000, 154.484523390867], [-515433600000, 45.00000000000002], [-512841600000, 107.83369308257603], [-510163200000, 240.55735604360657], [-507571200000, 366.66666666666663], [-504892800000, 550.0000000000001], [-502214400000, 642.958810976361], [-499795200000, 678.0331673068696], [-497116800000, 820.6494245916697], [-494524800000, 673.3353091805997], [-491846400000, 440.0465344984889], [-489254400000, 288.91556676701634], [-486576000000, 125.26604594653314], [-483897600000, 80.42640469828919], [-481305600000, 169.95580162717835], [-478627200000, 183.33333333333331], [-476035200000, 366.66666666666663], [-473356800000, 599.7470087487013], [-470678400000, 817.1708297610713], [-468259200000, 940.4804042585251], [-465580800000, 931.0305372331272], [-462988800000, 875.2882100822238], [-460310400000, 645.7902429148666], [-457718400000, 351.5913459330291], [-455040000000, 247.35653295287977], [-452361600000, 110.63214299529749], [-449769600000, 156.35964995517534], [-447091200000, 171.15960758563975], [-444499200000, 342.3192151712794], [-441820800000, 586.1499185504535], [-439142400000, 827.7917441527213], [-436636800000, 972.0000000000002], [-433958400000, 972], [-431366400000, 936.9079622669566], [-428688000000, 698.49070997934], [-426096000000, 464.52846700305923], [-423417600000, 218.98881009640274], [-420739200000, 93.28439327361232], [-418147200000, 157.10206908140034], [-415468800000, 314.6540946220753], [-412876800000, 366.66666666666674], [-410198400000, 550.0000000000002], [-407520000000, 701.0662184895949], [-405100800000, 713.4346522399674], [-402422400000, 793.3514926076848], [-399830400000, 679.8472895512715], [-397152000000, 514.3750854687468], [-394560000000, 332.8569137808135], [-391881600000, 194.9429902222773], [-389203200000, 123.89235235571195], [-386611200000, 213.66545444171672], [-383932800000, 394.42649808577846], [-381340800000, 558.2824766028673], [-378662400000, 724.9298538060516], [-375984000000, 860.3520905953976], [-373564800000, 972.0000000000001], [-370886400000, 971.9999999999998], [-368294400000, 972], [-365616000000, 839.2896537570307], [-363024000000, 726.4953769297668], [-360345600000, 451.79635871494463], [-357667200000, 252.72351491176747], [-355075200000, 307.02311305941157], [-352396800000, 424.714276113627], [-349804800000, 437.46113984759296], [-347126400000, 578.1665549317537], [-344448000000, 674.2502739747023], [-342028800000, 754.5200565180412], [-339350400000, 803.1822348172752], [-336758400000, 725.3324300123613], [-334080000000, 492.44622156072563], [-331488000000, 291.23096052263776], [-328809600000, 126.56682330211734], [-326131200000, 45], [-323539200000, 172.7806723040069], [-320860800000, 187.21502867725565], [-318268800000, 261.68224143328905], [-315590400000, 439.1648393598103], [-312912000000, 595.7696361230846], [-310406400000, 717.7522113885034], [-307728000000, 783.4715673327466], [-305136000000, 778.7809569790787], [-302457600000, 545.0915708859508], [-299865600000, 259.0801319894439], [-297187200000, 76.94830080676023], [-294508800000, 45], [-291916800000, 102.9419804298565], [-289238400000, 139.09606650624696], [-286646400000, 299.42958741159396], [-283968000000, 401.72632564184437], [-281289600000, 566.9328341092773], [-278870400000, 620.1131048623904], [-276192000000, 713.3959933662546], [-273600000000, 725.9850591758304], [-270921600000, 546.8548538056308], [-268329600000, 290.0428078447538], [-265651200000, 117.24487663978474], [-262972800000, 53.227543520152764], [-260380800000, 124.42058743476905], [-257702400000, 193.3746264225164], [-255110400000, 324.9214016655871], [-252432000000, 314.8101326359177], [-249753600000, 474.10494087273537], [-247334400000, 673.7600442387113], [-244656000000, 773.0598417900447], [-242064000000, 658.7123117640728], [-239385600000, 465.8176628095611], [-236793600000, 312.0061548082678], [-234115200000, 178.50307740413382], [-231436800000, 103.25333002050002], [-228844800000, 206.90785654375705], [-226166400000, 380.6834856250776], [-223574400000, 578.4971308115445], [-220896000000, 771.6579120187472], [-218217600000, 862.9519993740532], [-215798400000, 972], [-213120000000, 972.0000000000001], [-210528000000, 921.3194739605929], [-207849600000, 640.7300825696123], [-205257600000, 402.9758275722088], [-202579200000, 161.90107299948727], [-199900800000, 45], [-197308800000, 109.96120260781804], [-194630400000, 250.68413111551325], [-192038400000, 444.9318649677939], [-189360000000, 565.0152589596638], [-186681600000, 692.9640872466971], [-184176000000, 694.967126472487], [-181497600000, 752.372375235309], [-178905600000, 654.6710578172069], [-176227200000, 456.5409174887491], [-173635200000, 277.80926889995675], [-170956800000, 163.76026751765977], [-168278400000, 128.0836838551299], [-165686400000, 212.45227846751405], [-163008000000, 227.16782223175213], [-160416000000, 430.9569737166972], [-157737600000, 637.3071523119876], [-155059200000, 823.0965386947942], [-152640000000, 953.6372260933041], [-149961600000, 972.0000000000025], [-147369600000, 908.5683392274403], [-144691200000, 577.0113532138519], [-142099200000, 338.83456455329434], [-139420800000, 92.49407829960025], [-136742400000, 44.999999999999986], [-134150400000, 108.33924091845769], [-131472000000, 183.33333333333348], [-128880000000, 404.42941142188215], [-126201600000, 635.5000940373736], [-123523200000, 777.8873202031863], [-121104000000, 872.1430902615679], [-118425600000, 908.0004405625496], [-115833600000, 798.0719432453772], [-113155200000, 541.3826777204742], [-110563200000, 322.3447501139866], [-107884800000, 169.00049810246], [-105206400000, 61.686317225225764], [-102614400000, 144.76807488081903], [-99936000000, 151.97233784373944], [-97344000000, 330.104587929494], [-94665600000, 560.3673408715997], [-91987200000, 717.153391614106], [-89568000000, 827.5393275314434], [-86889600000, 942.9913422233515], [-84297600000, 972.0000000000001], [-81619200000, 827.3107039444579], [-79027200000, 741.736739672294], [-76348800000, 593.5506695270449], [-73670400000, 410.38817779577334], [-71078400000, 455.7267527193878], [-68400000000, 583.066607532898], [-65808000000, 575.8315594043769], [-63129600000, 742.7114520071748], [-60451200000, 853.2520719254716], [-57945600000, 861.3599998735673], [-55267200000, 909.9416104981377], [-52675200000, 752.321027991183], [-49996800000, 485.46783103984023], [-47404800000, 237.44722888433918], [-44726400000, 56.271518783545275], [-42048000000, 45], [-39456000000, 127.88236905583118], [-36777600000, 229.7161057053381], [-34185600000, 409.94279016650705], [-31507200000, 615.2040632070231], [-28828800000, 874.8798539284066], [-26409600000, 972.000000000002], [-23731200000, 972], [-21139200000, 972], [-18460800000, 971.9999999999999], [-15868800000, 913.822376757311], [-13190400000, 805.0251279005595], [-10512000000, 650.5527634958978], [-7920000000, 709.5626409112891], [-5241600000, 861.4531836158018], [-2649600000, 889.7791205973415], [28800000, 971.9999999999997], [2707200000, 972], [5126400000, 971.9999999999999], [7804800000, 971.9999999999998], [10396800000, 802.5304714306495], [13075200000, 515.7069592687363], [15667200000, 285.50492492447387], [18345600000, 71.515630048554], [21024000000, 45.000000000000014], [23616000000, 112.6916231365061], [26294400000, 158.86826107675378], [28886400000, 370.72710767466003], [31564800000, 602.8807212407735], [34243200000, 710.4029084912612], [36662400000, 755.4931553941861], [39340800000, 811.8377736041803], [41932800000, 694.9039829463231], [44611200000, 522.6150092431708], [47203200000, 357.759478621082], [49881600000, 203.96356819347554], [52560000000, 143.84735193723057], [55152000000, 224.6047228063529], [57830400000, 372.7107019820315], [60422400000, 366.66666666666674], [63100800000, 593.4433752858843], [65779200000, 720.2975132492796], [68284800000, 756.3069961939292], [70963200000, 784.6347598601124], [73555200000, 683.0060273376649], [76233600000, 456.384256335966], [78825600000, 258.88275436441097], [81504000000, 131.39632400134903], [84182400000, 44.999999999999986], [86774400000, 155.80264105118164], [89452800000, 145.18555291283138], [92044800000, 377.9831452950588], [94723200000, 575.6535142494537], [97401600000, 771.400735371388], [99820800000, 971.9999999999998], [102499200000, 972], [105091200000, 821.0541969032922], [107769600000, 589.805244423256], [110361600000, 377.8881304320802], [113040000000, 193.31136543621142], [115718400000, 83.11473391214366], [118310400000, 156.23100130723708], [120988800000, 330.19717802451], [123580800000, 522.0273482009675], [126259200000, 718.0932547071716], [128937600000, 918.5951526026024], [131356800000, 972], [134035200000, 971.9999999999999], [136627200000, 853.8365718584273], [139305600000, 565.9694812601729], [141897600000, 361.46454925143024], [144576000000, 138.98279186232767], [147254400000, 45.00000000000003], [149846400000, 92.0241365936472], [152524800000, 244.32778369306862], [155116800000, 366.6666666666669], [157795200000, 581.2657255422589], [160473600000, 612.9450869225965], [162892800000, 701.5881619961134], [165571200000, 865.3998774638565], [168163200000, 767.8706013295206], [170841600000, 501.9530622570971], [173433600000, 337.10905290095627], [176112000000, 170.71968493085916], [178790400000, 114.22512133457099], [181382400000, 181.86582503799252], [184060800000, 353.4756437757344], [186652800000, 430.3916494365783], [189331200000, 632.8124894768528], [192009600000, 745.8220479899564], [194515200000, 783.0393792714489], [197193600000, 810.4261487612985], [199785600000, 849.9072662410522], [202464000000, 623.4876338683466], [205056000000, 347.70966769999364], [207734400000, 202.53878621119947], [210412800000, 110.9874004576686], [213004800000, 176.40442946273006], [215683200000, 180.83162996855324], [218275200000, 250.09704949330487], [220953600000, 368.4199335966095], [223632000000, 512.7264399974925], [226051200000, 423.60305769391863], [228729600000, 411.7174236284018], [231321600000, 342.4127308951844], [234000000000, 324.79306386592737], [236592000000, 177.82597464594116], [239270400000, 224.94515613924196], [241948800000, 207.7869072691392], [244540800000, 243.2064177043338], [247219200000, 199.27576524994458], [249811200000, 220.6494847291936], [252489600000, 306.92859035027857], [255168000000, 551.0898869146922], [257587200000, 794.3389551912567], [260265600000, 971.9999999999999], [262857600000, 972.0000000000001], [265536000000, 774.6527647825594], [268128000000, 625.152404872281], [270806400000, 298.0467656242878], [273484800000, 126.76255846391894], [276076800000, 201.04329594042494], [278755200000, 244.75637330292614], [281347200000, 434.1144263853544], [284025600000, 550.0000000000001], [286704000000, 751.1141442144382], [289123200000, 894.1100261829419], [291801600000, 972], [294393600000, 846.2421927688515], [297072000000, 588.9690762067901], [299664000000, 344.2620447265071], [302342400000, 159.326948953317], [305020800000, 77.31507864935332], [307612800000, 144.10251698520094], [310291200000, 278.7393291211642], [312883200000, 462.53559998112746], [315561600000, 595.7751730096047], [318240000000, 842.0702617963784], [320745600000, 971.9999999999997], [323424000000, 972], [326016000000, 905.1808774293036], [328694400000, 717.5184325367321], [331286400000, 541.2605897393801], [333964800000, 364.37037212031765], [336643200000, 250.16607469048986], [339235200000, 306.86065721142955], [341913600000, 424.96671653582774], [344505600000, 370.47785229953], [347184000000, 567.890193342452], [349862400000, 756.7497904036038], [352281600000, 774.6691698401829], [354960000000, 942.5870481675723], [357552000000, 837.4916344405603], [360230400000, 567.6273681467912], [362822400000, 256.329510537835], [365500800000, 53.777753234509795], [368179200000, 45], [370771200000, 105.54553374835189], [373449600000, 234.4172657240493], [376041600000, 439.12419733186925], [378720000000, 580.9335078335997], [381398400000, 746.4859418733845], [383817600000, 869.5839642648324], [386496000000, 972.0000000000001], [389088000000, 972], [391766400000, 833.0209806865422], [394358400000, 725.7457477278026], [397036800000, 448.967160353232], [399715200000, 259.80630872061664], [402307200000, 339.52207656281496], [404985600000, 524.2460521235814], [407577600000, 751.8544680268756], [410256000000, 971.9999999999998], [412934400000, 972.0000000000001], [415353600000, 972], [418032000000, 971.9999999999998], [420624000000, 972], [423302400000, 972.0000000000001], [425894400000, 944.0911674938593], [428572800000, 827.302124219163], [431251200000, 786.6502153732041], [433843200000, 928.5733217271353], [436521600000, 972.0000000000001], [439113600000, 971.9999999999994], [441792000000, 971.9999999999998], [444470400000, 972.0000000000002], [446976000000, 972.0000000000005], [449654400000, 971.9999999999999], [452246400000, 846.161348091007], [454924800000, 549.7399025772661], [457516800000, 304.9784902840194], [460195200000, 93.02086247218914], [462873600000, 44.999999999999986], [465465600000, 110.7510539315927], [468144000000, 183.33333333333334], [470736000000, 386.2196144778688], [473414400000, 613.6832283247679], [476092800000, 735.2557147895477], [478512000000, 779.3190284055447], [481190400000, 891.5035628636957], [483782400000, 763.9724404257294], [486460800000, 519.9273907827852], [489052800000, 235.26375005628398], [491731200000, 59.3158576149088], [494409600000, 45], [497001600000, 54.57959953446265], [499680000000, 162.79958817482535], [502272000000, 378.42630454940746], [504950400000, 582.2651781501295], [507628800000, 644.3618213843248], [510048000000, 842.7977341039186], [512726400000, 971.9999999999999], [515318400000, 928.5172184621517], [517996800000, 743.8879142457022], [520588800000, 568.698433963751], [523267200000, 291.27778326356406], [525945600000, 118.18484034220286], [528537600000, 200.4742338875032], [531216000000, 201.43579998416868], [533808000000, 366.66666666666606], [536486400000, 543.278104647338], [539164800000, 680.6748408281412], [541584000000, 759.3968332132306], [544262400000, 907.8048229495141], [546854400000, 874.6814653363606], [549532800000, 660.4681246603752], [552124800000, 365.5485652581334], [554803200000, 243.23187733136723], [557481600000, 159.14975251733577], [560073600000, 158.7034442954433], [562752000000, 208.58728036078284], [565344000000, 300.28955107384513], [568022400000, 486.6108429067506], [570700800000, 643.9105471004966], [573206400000, 653.7005239902828], [575884800000, 655.7063469687307], [578476800000, 721.1660843991264], [581155200000, 580.0729514031757], [583747200000, 414.0970337668521], [586425600000, 337.3639940352518], [589104000000, 345.14020781079546], [591696000000, 437.5637571643164], [594374400000, 404.46577021480516], [596966400000, 516.7204869761604], [599644800000, 521.17668914862], [602323200000, 601.4388823834322], [604742400000, 585.7817032541392], [607420800000, 694.2404626491644], [610012800000, 682.4842720541271], [612691200000, 532.798034924423], [615283200000, 317.2662695416164], [617961600000, 199.8047208976933], [620640000000, 77.75296794131931], [623232000000, 193.67993028696475], [625910400000, 286.68615169738405], [628502400000, 378.74893314083073], [631180800000, 467.4038222401732], [633859200000, 536.7112362494775], [636278400000, 591.562381579529], [638956800000, 669.5163109444801], [641548800000, 739.9389368129033], [644227200000, 661.1076158571508], [646819200000, 461.81449446316503], [649497600000, 334.6021025277485], [652176000000, 339.6787227330017], [654768000000, 435.3233910302735], [657446400000, 369.90519200020805], [660038400000, 398.80581505139213], [662716800000, 403.1378453277328], [665395200000, 351.66859897979396], [667814400000, 350.2754940885972], [670492800000, 575.9758693693781], [673084800000, 537.232028906972], [675763200000, 414.553717906778], [678355200000, 297.7498748306491], [681033600000, 282.8525847513558], [683712000000, 366.71355531540246], [686304000000, 450.89329508447986], [688982400000, 445.3194825080509], [691574400000, 512.5947370465356], [694252800000, 539.5973187725986], [696931200000, 504.98326415052594], [699436800000, 702.6634673340664], [702115200000, 807.1220533324453], [704707200000, 704.7980278467187], [707385600000, 492.5842886964662], [709977600000, 289.3889508894114], [712656000000, 245.9920404963335], [715334400000, 242.26178049003954], [717926400000, 236.85984183432626], [720604800000, 250.942706270178], [723196800000, 249.95100218561464], [725875200000, 404.71846992287584], [728553600000, 658.8131905190164], [730972800000, 873.3828667760214], [733651200000, 972], [736243200000, 815.1058587112825], [738921600000, 568.8062291153615], [741513600000, 392.50591860066913], [744192000000, 223.5350028728609], [746870400000, 148.28226429151474], [749462400000, 209.67688084586425], [752140800000, 346.57112628115806], [754732800000, 450.49061317041054], [757411200000, 666.7355369718701], [760089600000, 801.6267208283435], [762508800000, 899.1601825127365], [765187200000, 934.2834691402162], [767779200000, 935.245264465373], [770457600000, 805.077865157854], [773049600000, 493.17278522226337], [775728000000, 292.6440940709798], [778406400000, 104.03617799475303], [780998400000, 178.5840664641654], [783676800000, 161.16710130166143], [786268800000, 274.79704057967723], [788947200000, 484.8016220580855], [791625600000, 505.2359567707008], [794044800000, 655.4359103272178], [796723200000, 859.7467762460327], [799315200000, 802.6551847799378], [801993600000, 740.7736293822671], [804585600000, 652.6473653347622], [807264000000, 533.3988550751259], [809942400000, 382.1352065919645], [812534400000, 423.239637927405], [815212800000, 537.0463228083084], [817804800000, 464.4819888868978], [820483200000, 640.9931593951621], [823161600000, 751.1917016181037], [825667200000, 879.0818899176384], [828345600000, 972.0000000000003], [830937600000, 835.8355689357218], [833616000000, 636.3348120247101], [836208000000, 383.5504645219475], [838886400000, 110.0828068845298], [841564800000, 45], [844156800000, 97.99600722640936], [846835200000, 255.47657770816028], [849427200000, 441.9976054781504], [852105600000, 689.9327153854108], [854784000000, 936.5182050006094], [857203200000, 972.0000000000001], [859881600000, 971.9999999999999], [862473600000, 888.4425312728281], [865152000000, 565.812396192019], [867744000000, 265.5246613531526], [870422400000, 44.99999999999995], [873100800000, 44.999999999999986], [875692800000, 98.0264330961231], [878371200000, 176.23114371111055], [880963200000, 242.63447998913327], [883641600000, 464.07094603006095], [886320000000, 681.1010720957042], [888739200000, 922.0411497173103], [891417600000, 971.9999999999998], [894009600000, 972.0000000000001], [896688000000, 968.4543125110202], [899280000000, 906.6950999655138], [901958400000, 768.2160686999746], [904636800000, 602.8414409647263], [907228800000, 653.0363360742856], [909907200000, 788.5232674423761], [912499200000, 962.6980880534851], [915177600000, 972.0000000000001], [917856000000, 972], [920275200000, 971.9999999999998], [922953600000, 972], [925545600000, 852.311318602011], [928224000000, 567.723995930368], [930816000000, 335.3699764145738], [933494400000, 134.87417421060903], [936172800000, 46.6093331415558], [938764800000, 109.58196489993546], [941443200000, 183.3333333333336], [944035200000, 366.6666666666667], [946713600000, 550.0000000000001], [949392000000, 668.4443123831945], [951897600000, 799.725607926378], [954576000000, 909.270990477571], [957168000000, 802.4841984668343], [959846400000, 536.5289073198612], [962438400000, 291.27185709315677], [965116800000, 107.3531525864425], [967795200000, 45], [970387200000, 125.22165648253879], [973065600000, 311.46000044426256], [975657600000, 366.66666666666686], [978336000000, 550], [981014400000, 741.5226706368244], [983433600000, 864.8619858905398], [986112000000, 964.6959925213821], [988704000000, 880.7686513641245], [991382400000, 586.6589411756676], [993974400000, 261.8363472252065], [996652800000, 211.07523351532492], [999331200000, 130.10427725892916], [1001923200000, 182.28791136255228], [1004601600000, 229.78174861629202], [1007193600000, 426.36350882448215], [1009872000000, 656.6005160694696], [1012550400000, 778.9214756727445], [1014969600000, 736.2746900902539], [1017648000000, 817.4696183194684], [1020240000000, 729.8423254584935], [1022918400000, 516.3634980372293], [1025510400000, 258.32840267748213], [1028188800000, 104.71149246542014], [1030867200000, 45], [1033459200000, 115.44933214348666], [1036137600000, 256.95145505069183], [1038729600000, 406.42527318013555], [1041408000000, 620.9588009315868], [1044086400000, 715.4413335259827], [1046505600000, 820.2884723455415], [1049184000000, 875.3034645091283], [1051776000000, 794.3242406699894], [1054454400000, 563.2232080138031], [1057046400000, 313.64566159848596], [1059724800000, 115.50270143064152], [1062403200000, 54.802924471233034], [1064995200000, 127.85382555035228], [1067673600000, 270.1656683996412], [1070265600000, 366.66666666666606], [1072944000000, 568.1513967483041], [1075622400000, 674.8586609155611], [1078128000000, 808.6114057003006], [1080806400000, 833.019686682141], [1083398400000, 702.8558742866812], [1086076800000, 465.150036901935], [1088668800000, 220.0083772392162], [1091347200000, 67.61344501958068], [1094025600000, 45], [1096617600000, 131.43509547733234], [1099296000000, 188.59857283515274], [1101888000000, 381.24562543246105], [1104566400000, 575.8809016392389], [1107244800000, 808.2192819605151], [1109664000000, 972.0000000000152], [1112342400000, 971.9999999999999], [1114934400000, 949.0196725810308], [1117612800000, 808.2192237819243], [1120204800000, 710.3929097023755], [1122883200000, 416.009514294975], [1125561600000, 263.64302848868175], [1128153600000, 323.83316625282714], [1130832000000, 453.91901736964553], [1133424000000, 553.7918775303802], [1136102400000, 720.5820722880909], [1138780800000, 955.0914869918543], [1141200000000, 971.9999999999999], [1143878400000, 972.0000000000002], [1146470400000, 972.0000000000001], [1149148800000, 972], [1151740800000, 862.061131279341], [1154419200000, 600.3372587054978], [1157097600000, 454.1917197894715], [1159689600000, 498.1818520532835], [1162368000000, 639.8368408353532], [1164960000000, 729.1764338072356], [1167638400000, 920.1208678476313], [1170316800000, 972.0000000000002], [1172736000000, 966.6231722972036], [1175414400000, 971.9999999999998], [1178006400000, 916.6666666666665], [1180684800000, 693.189115148336], [1183276800000, 364.15358472995626], [1185955200000, 156.34769693351512], [1188633600000, 45.23235900237912], [1191225600000, 158.92013414785984], [1193904000000, 301.8759691872079], [1196496000000, 425.59880981462504], [1199174400000, 528.2265819012556], [1201852800000, 732.2714681377018], [1204358400000, 838.1753020626115], [1207036800000, 892.8203037458053], [1209628800000, 894.4351540042512], [1212307200000, 727.559551742692], [1214899200000, 468.5809718756806], [1217577600000, 326.49764913909695], [1220256000000, 269.45184858827594], [1222848000000, 356.1851357081052], [1225526400000, 305.64198425320114], [1228118400000, 363.5118377320606], [1230796800000, 388.68102621969103], [1233475200000, 386.10413311096534], [1235894400000, 487.51487364924697], [1238572800000, 583.8991589972707], [1241164800000, 456.3158065871555], [1243843200000, 302.33026183984936], [1246435200000, 217.78942570915953], [1249113600000, 131.39471285457998], [1251792000000, 147.51666304611717], [1254384000000, 244.36988373853129], [1257062400000, 363.36675220677193], [1259654400000, 495.4250080762349], [1262332800000, 664.7550009582983], [1265011200000, 875.2337793059187], [1267430400000, 972.0000000000001], [1270108800000, 972.0000000000001], [1272700800000, 913.4834252612168], [1275379200000, 664.0702198553582], [1277971200000, 414.68262213546745], [1280649600000, 181.89811173899065], [1283328000000, 64.76076855966112], [1285920000000, 124.51038362997383], [1288598400000, 272.78007409940363], [1291190400000, 458.26506213899876], [1293868800000, 702.6016407706231], [1296547200000, 891.18952365976], [1298966400000, 972.0000000000007], [1301644800000, 972], [1304236800000, 972.0000000000001], [1306915200000, 848.5813633833111], [1309507200000, 778.0558966860976], [1312185600000, 601.6665540850437], [1314864000000, 450.41295927126504], [1317456000000, 488.8302735004391], [1320134400000, 634.2661170544661], [1322726400000, 595.0046802208232], [1325404800000, 736.6500781898545], [1328083200000, 842.833157984567], [1330588800000, 858.8268348550838], [1333267200000, 961.7007565791961], [1335859200000, 912.5895274715256], [1338537600000, 637.862448389398], [1341129600000, 357.41664452644727], [1343808000000, 164.29925243884065], [1346486400000, 50.000005451713776], [1349078400000, 108.96882851424343], [1351756800000, 200.54552098710369], [1354348800000, 297.5645529062236], [1357027200000, 523.9754036522211], [1359705600000, 621.9513440965422], [1362124800000, 692.9883573546591], [1364803200000, 736.2109219151707], [1367395200000, 774.6485837946628], [1370073600000, 619.5201946879188], [1372665600000, 365.08216612264135], [1375344000000, 207.81873871231144], [1378022400000, 67.01547349864907], [1380614400000, 114.25578071381177], [1383292800000, 172.18867417864914], [1385884800000, 236.50100237632998], [1388563200000, 278.1784343043837], [1391241600000, 214.4033865008602], [1393660800000, 257.7807317819053], [1396339200000, 400.1838099466484], [1398931200000, 480.2723917525111], [1401609600000, 441.7485015617144], [1404201600000, 317.01157741008353], [1406880000000, 278.79421277498284], [1409558400000, 155.12589632373542], [1412150400000, 291.8405626071732], [1414828800000, 242.97156164399073], [1417420800000, 275.3216021238508], [1420099200000, 527.7258569237673], [1422777600000, 500.05569278699465], [1425196800000, 616.8620009849045], [1427875200000, 531.8205894423788], [1430467200000, 541.3923481484171], [1433145600000, 459.89132403472485], [1435737600000, 328.9824522963945], [1438416000000, 348.18464575896286], [1441094400000, 214.63240193415257], [1443686400000, 242.59166784465404]]
                    }, {
                        "period_months": ["Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep"],
                        "annual_period": "Sacramento River Index<br>Wet",
                        "computed_statistics": [{
                            "statistic": "Averages",
                            "statistically_computed_time_series_monthly": [["Jan", 811.324300713782], ["Feb", 910.147973591696], ["Mar", 956.556542711916], ["Apr", 904.7028705682833], ["May", 720.6465624205842], ["Jun", 555.9630273757684], ["Jul", 340.95833325027235], ["Aug", 223.54918632347733], ["Sep", 284.53126115384634], ["Oct", 342.30919389328915], ["Nov", 486.55335664738845], ["Dec", 673.0224776208386]],
                            "statistic_aggregate": 596.029448650827
                        }],
                        "aggregate_ts": [[1927, 465.72811811308367], [1938, 650.6965373557415], [1941, 533.3483039967355], [1942, 525.0217913840183], [1943, 505.95545733419476], [1952, 626.2704467170876], [1953, 577.9310117455706], [1956, 536.7269080967388], [1958, 669.2765780385847], [1963, 568.3065156282618], [1965, 509.5344407717652], [1967, 627.5701143060577], [1969, 758.0588101726945], [1970, 622.6818260851719], [1971, 463.15867010245415], [1974, 540.849205363438], [1975, 453.82805417296316], [1982, 618.4672625898953], [1983, 882.8931124136516], [1984, 648.4709714463395], [1986, 536.1409208772901], [1995, 539.6695238645699], [1996, 529.4662268516191], [1997, 514.6442604572045], [1998, 694.1101708132366], [1999, 640.4743432245762], [2006, 748.8463680006304], [2011, 700.6986122995786]],
                        "month_period": "Oct - Sep",
                        "discrete_ts": [[-1362240000000, 156.90732433083565], [-1359648000000, 374.0051957181023], [-1356969600000, 531.3423552374406], [-1354291200000, 635.2933356582537], [-1351872000000, 794.4376631741345], [-1349193600000, 919.2298840994243], [-1346601600000, 821.7294781981964], [-1343923200000, 569.1232464222228], [-1341331200000, 376.7099976244466], [-1338652800000, 170.74309677883966], [-1335974400000, 82.72253407030719], [-1333382400000, 156.49330604480002], [-1015084800000, 172.81949596140953], [-1012492800000, 348.20426965932114], [-1009814400000, 549.4172129070807], [-1007136000000, 729.043586737346], [-1004716800000, 925.186413403843], [-1002038400000, 971.9999999999998], [-999446400000, 972.0000000000001], [-996768000000, 857.6312085750293], [-994176000000, 752.7465205998355], [-991497600000, 592.4613135222265], [-988819200000, 441.2519682778492], [-986227200000, 495.59645862495665], [-920390400000, 195.39447458191597], [-917798400000, 366.66666666666686], [-915120000000, 588.436523781235], [-912441600000, 759.2720749809165], [-910022400000, 972.0000000000001], [-907344000000, 972.0000000000001], [-904752000000, 972.0000000000001], [-902073600000, 717.6085636308063], [-899481600000, 488.22948330479346], [-896803200000, 197.38438083103102], [-894124800000, 59.427318962389144], [-891532800000, 111.76016122107126], [-888854400000, 267.39169127058796], [-886262400000, 366.6666666666665], [-883584000000, 571.6488640872144], [-880905600000, 758.2311547340103], [-878486400000, 870.9340100750456], [-875808000000, 963.0508836843071], [-873216000000, 911.7350272278693], [-870537600000, 697.2780717135329], [-867945600000, 511.2377391665454], [-865267200000, 219.18272629164778], [-862588800000, 52.61099323569843], [-859996800000, 110.29366845509449], [-857318400000, 242.14859520716283], [-854726400000, 366.66666666666663], [-852048000000, 521.3391726693181], [-849369600000, 733.8767546781205], [-846950400000, 820.3118570682742], [-844272000000, 972.0000000000001], [-841680000000, 932.757848940258], [-839001600000, 691.2831822405572], [-836409600000, 461.16336213736565], [-833731200000, 191.90571768426003], [-831052800000, 45], [-828460800000, 93.0123307183536], [-573321600000, 196.69524991472215], [-570729600000, 366.6666666666664], [-568051200000, 574.6567360733538], [-565372800000, 802.4215268329044], [-562867200000, 965.3303774358257], [-560188800000, 972.0000000000001], [-557596800000, 948.0526387337896], [-554918400000, 805.5490596543714], [-552326400000, 728.08058519158], [-549648000000, 487.3748954951372], [-546969600000, 311.68440331409187], [-544377600000, 356.7332212926087], [-541699200000, 479.3200542324257], [-539107200000, 582.6951630506007], [-536428800000, 814.0716020772121], [-533750400000, 915.5665455384365], [-531331200000, 949.0341096468328], [-528652800000, 972.0000000000003], [-526060800000, 874.5714172781647], [-523382400000, 628.700777114801], [-520790400000, 411.89425553493044], [-518112000000, 154.484523390867], [-515433600000, 45.00000000000002], [-512841600000, 107.83369308257603], [-447091200000, 171.15960758563975], [-444499200000, 342.3192151712794], [-441820800000, 586.1499185504535], [-439142400000, 827.7917441527213], [-436636800000, 972.0000000000002], [-433958400000, 972], [-431366400000, 936.9079622669566], [-428688000000, 698.49070997934], [-426096000000, 464.52846700305923], [-423417600000, 218.98881009640274], [-420739200000, 93.28439327361232], [-418147200000, 157.10206908140034], [-383932800000, 394.42649808577846], [-381340800000, 558.2824766028673], [-378662400000, 724.9298538060516], [-375984000000, 860.3520905953976], [-373564800000, 972.0000000000001], [-370886400000, 971.9999999999998], [-368294400000, 972], [-365616000000, 839.2896537570307], [-363024000000, 726.4953769297668], [-360345600000, 451.79635871494463], [-357667200000, 252.72351491176747], [-355075200000, 307.02311305941157], [-226166400000, 380.6834856250776], [-223574400000, 578.4971308115445], [-220896000000, 771.6579120187472], [-218217600000, 862.9519993740532], [-215798400000, 972], [-213120000000, 972.0000000000001], [-210528000000, 921.3194739605929], [-207849600000, 640.7300825696123], [-205257600000, 402.9758275722088], [-202579200000, 161.90107299948727], [-199900800000, 45], [-197308800000, 109.96120260781804], [-163008000000, 227.16782223175213], [-160416000000, 430.9569737166972], [-157737600000, 637.3071523119876], [-155059200000, 823.0965386947942], [-152640000000, 953.6372260933041], [-149961600000, 972.0000000000025], [-147369600000, 908.5683392274403], [-144691200000, 577.0113532138519], [-142099200000, 338.83456455329434], [-139420800000, 92.49407829960025], [-136742400000, 44.999999999999986], [-134150400000, 108.33924091845769], [-99936000000, 151.97233784373944], [-97344000000, 330.104587929494], [-94665600000, 560.3673408715997], [-91987200000, 717.153391614106], [-89568000000, 827.5393275314434], [-86889600000, 942.9913422233515], [-84297600000, 972.0000000000001], [-81619200000, 827.3107039444579], [-79027200000, 741.736739672294], [-76348800000, 593.5506695270449], [-73670400000, 410.38817779577334], [-71078400000, 455.7267527193878], [-36777600000, 229.7161057053381], [-34185600000, 409.94279016650705], [-31507200000, 615.2040632070231], [-28828800000, 874.8798539284066], [-26409600000, 972.000000000002], [-23731200000, 972], [-21139200000, 972], [-18460800000, 971.9999999999999], [-15868800000, 913.822376757311], [-13190400000, 805.0251279005595], [-10512000000, 650.5527634958978], [-7920000000, 709.5626409112891], [-5241600000, 861.4531836158018], [-2649600000, 889.7791205973415], [28800000, 971.9999999999997], [2707200000, 972], [5126400000, 971.9999999999999], [7804800000, 971.9999999999998], [10396800000, 802.5304714306495], [13075200000, 515.7069592687363], [15667200000, 285.50492492447387], [18345600000, 71.515630048554], [21024000000, 45.000000000000014], [23616000000, 112.6916231365061], [26294400000, 158.86826107675378], [28886400000, 370.72710767466003], [31564800000, 602.8807212407735], [34243200000, 710.4029084912612], [36662400000, 755.4931553941861], [39340800000, 811.8377736041803], [41932800000, 694.9039829463231], [44611200000, 522.6150092431708], [47203200000, 357.759478621082], [49881600000, 203.96356819347554], [52560000000, 143.84735193723057], [55152000000, 224.6047228063529], [120988800000, 330.19717802451], [123580800000, 522.0273482009675], [126259200000, 718.0932547071716], [128937600000, 918.5951526026024], [131356800000, 972], [134035200000, 971.9999999999999], [136627200000, 853.8365718584273], [139305600000, 565.9694812601729], [141897600000, 361.46454925143024], [144576000000, 138.98279186232767], [147254400000, 45.00000000000003], [149846400000, 92.0241365936472], [152524800000, 244.32778369306862], [155116800000, 366.6666666666669], [157795200000, 581.2657255422589], [160473600000, 612.9450869225965], [162892800000, 701.5881619961134], [165571200000, 865.3998774638565], [168163200000, 767.8706013295206], [170841600000, 501.9530622570971], [173433600000, 337.10905290095627], [176112000000, 170.71968493085916], [178790400000, 114.22512133457099], [181382400000, 181.86582503799252], [373449600000, 234.4172657240493], [376041600000, 439.12419733186925], [378720000000, 580.9335078335997], [381398400000, 746.4859418733845], [383817600000, 869.5839642648324], [386496000000, 972.0000000000001], [389088000000, 972], [391766400000, 833.0209806865422], [394358400000, 725.7457477278026], [397036800000, 448.967160353232], [399715200000, 259.80630872061664], [402307200000, 339.52207656281496], [404985600000, 524.2460521235814], [407577600000, 751.8544680268756], [410256000000, 971.9999999999998], [412934400000, 972.0000000000001], [415353600000, 972], [418032000000, 971.9999999999998], [420624000000, 972], [423302400000, 972.0000000000001], [425894400000, 944.0911674938593], [428572800000, 827.302124219163], [431251200000, 786.6502153732041], [433843200000, 928.5733217271353], [436521600000, 972.0000000000001], [439113600000, 971.9999999999994], [441792000000, 971.9999999999998], [444470400000, 972.0000000000002], [446976000000, 972.0000000000005], [449654400000, 971.9999999999999], [452246400000, 846.161348091007], [454924800000, 549.7399025772661], [457516800000, 304.9784902840194], [460195200000, 93.02086247218914], [462873600000, 44.999999999999986], [465465600000, 110.7510539315927], [499680000000, 162.79958817482535], [502272000000, 378.42630454940746], [504950400000, 582.2651781501295], [507628800000, 644.3618213843248], [510048000000, 842.7977341039186], [512726400000, 971.9999999999999], [515318400000, 928.5172184621517], [517996800000, 743.8879142457022], [520588800000, 568.698433963751], [523267200000, 291.27778326356406], [525945600000, 118.18484034220286], [528537600000, 200.4742338875032], [783676800000, 161.16710130166143], [786268800000, 274.79704057967723], [788947200000, 484.8016220580855], [791625600000, 505.2359567707008], [794044800000, 655.4359103272178], [796723200000, 859.7467762460327], [799315200000, 802.6551847799378], [801993600000, 740.7736293822671], [804585600000, 652.6473653347622], [807264000000, 533.3988550751259], [809942400000, 382.1352065919645], [812534400000, 423.239637927405], [815212800000, 537.0463228083084], [817804800000, 464.4819888868978], [820483200000, 640.9931593951621], [823161600000, 751.1917016181037], [825667200000, 879.0818899176384], [828345600000, 972.0000000000003], [830937600000, 835.8355689357218], [833616000000, 636.3348120247101], [836208000000, 383.5504645219475], [838886400000, 110.0828068845298], [841564800000, 45], [844156800000, 97.99600722640936], [846835200000, 255.47657770816028], [849427200000, 441.9976054781504], [852105600000, 689.9327153854108], [854784000000, 936.5182050006094], [857203200000, 972.0000000000001], [859881600000, 971.9999999999999], [862473600000, 888.4425312728281], [865152000000, 565.812396192019], [867744000000, 265.5246613531526], [870422400000, 44.99999999999995], [873100800000, 44.999999999999986], [875692800000, 98.0264330961231], [878371200000, 176.23114371111055], [880963200000, 242.63447998913327], [883641600000, 464.07094603006095], [886320000000, 681.1010720957042], [888739200000, 922.0411497173103], [891417600000, 971.9999999999998], [894009600000, 972.0000000000001], [896688000000, 968.4543125110202], [899280000000, 906.6950999655138], [901958400000, 768.2160686999746], [904636800000, 602.8414409647263], [907228800000, 653.0363360742856], [909907200000, 788.5232674423761], [912499200000, 962.6980880534851], [915177600000, 972.0000000000001], [917856000000, 972], [920275200000, 971.9999999999998], [922953600000, 972], [925545600000, 852.311318602011], [928224000000, 567.723995930368], [930816000000, 335.3699764145738], [933494400000, 134.87417421060903], [936172800000, 46.6093331415558], [938764800000, 109.58196489993546], [1130832000000, 453.91901736964553], [1133424000000, 553.7918775303802], [1136102400000, 720.5820722880909], [1138780800000, 955.0914869918543], [1141200000000, 971.9999999999999], [1143878400000, 972.0000000000002], [1146470400000, 972.0000000000001], [1149148800000, 972], [1151740800000, 862.061131279341], [1154419200000, 600.3372587054978], [1157097600000, 454.1917197894715], [1159689600000, 498.1818520532835], [1288598400000, 272.78007409940363], [1291190400000, 458.26506213899876], [1293868800000, 702.6016407706231], [1296547200000, 891.18952365976], [1298966400000, 972.0000000000007], [1301644800000, 972], [1304236800000, 972.0000000000001], [1306915200000, 848.5813633833111], [1309507200000, 778.0558966860976], [1312185600000, 601.6665540850437], [1314864000000, 450.41295927126504], [1317456000000, 488.8302735004391]]
                    }]
                }]
            }], "scenario_name": "DCR_Baseline_5.0", "scenario_color": "#CF79DDFF"
        }],
        "gui_link_title": "CVP San Luis Reservoir Storage",
        "first_record": -1522684800000,
        "units": "TAF",
        "is_instantaneous": true
    });
}
