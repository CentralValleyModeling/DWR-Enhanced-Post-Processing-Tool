/*
 * Enhanced Post Processing Tool (EPPT) Copyright (c) 2019.
 *
 * EPPT is copyrighted by the State of California, Department of Water Resources. It is licensed
 * under the GNU General Public License, version 2. This means it can be
 * copied, distributed, and modified freely, but you may not restrict others
 * in their ability to copy, distribute, and modify it. See the license below
 * for more details.
 *
 * GNU General Public License
 */

var FORMATTER = '';

function getHeaders(data) {
    let firstIndexData = data[0]['primary_data']['statistically_computed_time_series_wyt'][0];
    let headers = [''];
    for (let j = 0; j < firstIndexData.length; j++) {
        let waterYearPeriod = firstIndexData[j]['water_year_period'];
        headers.push('<b>' + getAcronym(waterYearPeriod) + '</b>');
    }
    return headers;
}

function buildTable(data) {
    let headers = getHeaders(data);
    let values = [];
    let colors = [];
    for (let i = 0; i < data.length; i++) {
        let scenarioArr = values[0];
        if (!scenarioArr) {
            scenarioArr = [];
            values[0] = scenarioArr;
        }
        colors.push(data[i]['scenario_color']);
        scenarioArr.push(data[i]['scenario_name']);
        let timeSeries = data[i]['primary_data']['statistically_computed_time_series_wyt'][0];
        let k = 0;
        for (; k < timeSeries.length; k++) {
            let val = timeSeries[k]['water_year_period_values'];
            let periodValues = values[1 + k];
            if (!periodValues) {
                periodValues = [];
                values[1 + k] = periodValues;
            }
            periodValues.push(val);
        }
    }
    return {
        type: 'table',
        header: {
            values: headers,
            align: "center",
            line: {width: 1, color: 'black'},
            font: PLOTLY_FONT
        },
        cells: {
            format: ['',',.3r%'],
            values: values,
            line: {color: "black", width: 1},
            align: ["left", "center"],
            font: {family: PLOTLY_FONT['family'], color: [colors]}
        },
        domain: {x: [0, 1], y: [0, 0.3]}
    };
}

function getAcronym(text) {
    let matches = text.match(/\b(\w)/g);
    return matches.join('');
}

function buildMatrixData(data, tableValues) {
    let header = [['Scenario'], [data['scenario_run_data'][0]['primary_data']['units'][0]]];
    let scenarios = [];
    let volumes = [];
    let colors = [];
    let headerColors = [];
    let format = [];
    for (let i = 0; i < data['scenario_run_data'].length; i++) {
        let scenarioName = data['scenario_run_data'][i]['scenario_name'];
        scenarios.push('<b>' + scenarioName + '</b>');
        headerColors.push(data['scenario_run_data'][i]['scenario_color']);
        volumes.push(tableValues[i]);
        if (i < data['scenario_run_data'].length - 1) {
            header.push(['Difference from<br>' + scenarioName]);
        }
    }
    let headerFormat = [];
    let valueFormat = [];
    for(let i = 0; i < header.length - 1; i++){
        headerFormat.push('');
        valueFormat.push(FORMATTER);
    }
    format.push(headerFormat);
    format.push(valueFormat);

    colors.push(headerColors);
    let values = [scenarios, volumes];
    for (let i = 1; i < volumes.length; i++) {
        let diffValues = [];
        let diffFormat = [];
        for (let j = 0; j < data['scenario_run_data'].length; j++) {
            if (j < i) {
                diffValues.push('');
                diffFormat.push('');
            } else {
                let diff = tableValues[j] - tableValues[i - 1];
                diffValues.push(diff);
            }
        }
        values.push(diffValues);
        format.push(diffFormat);
    }
    return {
        type: 'table',
        header: {
            values: header,
            align: "center",
            line: {width: 1, color: 'black'},
            font: {family: PLOTLY_FONT['family'], size: 11}
        },
        cells: {
            format: ['',',.3r%'],
            values: values,
            align: ['left', 'center'],
            height: 8,
            line: {color: "black", width: 1},
            font: {family: PLOTLY_FONT['family'], color: colors, size: [9, 11]}
        }
    };
}

function tabulateLongTermPeriod(data) {
    let layout = {
        font: PLOTLY_FONT,
        bgcolor: 'red',
        title: {
            text: '<b>Long Term</b>',
            bgcolor: 'red'
        },
        height: tableHeight,
        margin: {
            l: 10,
            r: 10,
            b: 20,
            t: 45,
            pad: 4
        }
    };
    let longTermData = [];
    for (let i = 0; i < data['scenario_run_data'].length; i++) {
        longTermData[i] = data['scenario_run_data'][i]['primary_data']['statistically_computed_time_series_yearly'][0];
    }
    let tableData = buildMatrixData(data, longTermData);
    Plotly.newPlot('water-year-matrix0', [tableData], layout, {
        displaylogo: false,
        displayModeBar: false,
        responsive: false
    });
    let elementid = "water-year-matrix0";
    $("#" + elementid).mousedown((ev) => {
        if (ev.which === 3) {
            openContextMenu("#" + elementid, ev, () => plotlyCopyToClipboard(elementid), plotlyExportFunction(document.getElementById(elementid)));
        }
    });
}

function tabulateWaterYearPeriod(waterYearPeriod, data, tableValues, i) {
    let layout = {
        font: PLOTLY_FONT,
        bgcolor: 'red',
        title: {
            text: '<b>' + waterYearPeriod.toUpperCase() + ' (' + getAcronym(waterYearPeriod) + ')' + '</b>',
            bgcolor: 'red'
        },
        height: tableHeight,
        margin: {
            l: 10,
            r: 10,
            b: 20,
            t: 45,
            pad: 4
        }
    };
    let tableData = buildMatrixData(data, tableValues[i]);
    Plotly.newPlot('water-year-matrix' + i, [tableData], layout, {
        displaylogo: false,
        displayModeBar: false,
        responsive: false
    });
    let elementid = "water-year-matrix" + i;
    $("#" + elementid).mousedown((ev) => {
        if (ev.which === 3) {
            openContextMenu("#" + elementid, ev, () => plotlyCopyToClipboard(elementid), plotlyExportFunction(document.getElementById(elementid)));
        }
    });
}

var tableHeight = 300;

function plot(data) {
    let title = document.getElementById('title');
    while (title.firstChild) {
        title.removeChild(title.firstChild);
    }
    let element = document.createElement('h2');
    title.appendChild(element);
    element.appendChild(document.createTextNode(data['gui_link_title']));
    element.appendChild(document.createElement('br'));
    element.appendChild(document.createTextNode(data['month_period_title']));
    element.appendChild(document.createElement('br'));
    element.appendChild(document.createTextNode(data['statistics']));
    tableHeight = 100 + data['scenario_run_data'].length * 35;
    let tableValues = buildTable(data['scenario_run_data'])['cells']['values'];
    tabulateLongTermPeriod(data);
    for (let i = 1; i < tableValues.length; i++) {
        let arr = data['scenario_run_data'][0]['primary_data']['statistically_computed_time_series_wyt'][0][i - 1];
        if (arr) {
            let waterYearPeriod = arr['water_year_period'];
            tabulateWaterYearPeriod(waterYearPeriod, data, tableValues, i);
        }
    }
}

function plotlyCopyToClipboard(element) {
    let plot = document.getElementById(element);
    let layout = plot.layout;
    let data1 = plot.data;
    var text = layout['title']['text'];
    if (layout['yaxis'] && layout['yaxis']['title']) {
        text += '\nScenario\t' + layout['yaxis']['title']['text'];
    }
    text += '\n';
    let datum = data1[data1.length - 1];
    for (let i = 0; i < datum['header']['values'].length; i++) {
        let name = datum['header']['values'][i] + "";
        text += name + '\t';
    }
    text += '\n';
    let values = datum['cells']['values'];
    let rows = values[0].length;
    for (let j = 0; j < rows; j++) {
        for (let k = 0; k < values.length; k++) {
            let val = values[k][j] + '\t';
            text += val;
        }
        text += '\n';
    }
    copyTextToClipboard(text.replace(/<br>/g, ' ').replace(/<b>/g, '').replace(/<\/b>/g, ''));
}
