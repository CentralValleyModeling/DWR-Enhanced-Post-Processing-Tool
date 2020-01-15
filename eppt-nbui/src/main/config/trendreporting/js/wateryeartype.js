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

// plot();

function getHeaders(data, allData) {
    let firstIndexData = data[0]['statistically_computed_time_series_wyt'];
    let headers = [''];
    for (let j = 0; j < firstIndexData.length; j++) {
        let waterYearPeriod = firstIndexData[j]['water_year_period'];
        headers.push('<b>' + getAcronym(waterYearPeriod) + '</b>');
    }
    return headers;
}

function buildTable(data, allData) {
    let headers = getHeaders(data, allData);
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
        let timeSeries = data[i]['statistically_computed_time_series_wyt'];
        let k = 0;
        for (; k < timeSeries.length; k++) {
            let val = timeSeries[k]['water_year_period_values'];
            let periodValues = values[1 + k];
            if (!periodValues) {
                periodValues = [];
                values[1 + k] = periodValues;
            }
            periodValues.push(Math.round(val));
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

function getPlotlySeries(allData) {
    let series = [];
    let datum = allData['scenario_run_data'];
    for (let i = 0; i < datum.length; i++) {
        let data = datum[i];
        let timeSeries = data['statistically_computed_time_series_wyt'];
        let xarr = [];
        for (let j = 0; j < timeSeries.length; j++) {
            let waterYearPeriod = timeSeries[j]['water_year_period'];
            xarr.push(getAcronym(waterYearPeriod));
        }
        let yarr = [];
        for (let k = 0; k < timeSeries.length; k++) {
            let val = timeSeries[k]['water_year_period_values'];
            yarr.push(val);
        }
        series.push({
            type: 'bar',
            name: data['scenario_name'],
            x: xarr,
            y: yarr,
            marker: {
                color: datum[i]['scenario_color']
            }
        });
    }
    series.push(buildTable(datum, allData));
    return series;
}

function plotGraph(data) {
    let layout = {
        font: PLOTLY_FONT,
        barmode: 'group',
        yaxis: {
            title: {
                text: data['units'],
            },
            domain: [0.35, 1.0],
            gridcolor: '#CCCCCC'
        },
        xaxis: {
            gridcolor: '#CCCCCC'
        },
        showlegend: false,
        title: {
            text: data['month_period_title'] + '<br>' + data['gui_link_title'] + '<br>(' + data['statistics'] + ')',
            font: {
                size: 20,
            }
        }
    };

    Plotly.newPlot('tester', getPlotlySeries(data), layout, {
        displaylogo: false,
        modeBarButtonsToRemove: ['toImage', 'sendDataToCloud', 'editInChartStudio', 'lasso2d', 'select2d', 'resetScale2d'],
        scrollZoom: true,
        responsive: true,
        grid: {rows: 2, columns: 1, pattern: 'independent'}
    });
    $("#tester").mousedown((ev) => {
        if (ev.which === 3) {
            openContextMenu('#tester', ev, () => plotlyCopyToClipboard('tester'), plotlyExportFunction(document.getElementById('tester')));
        }
    });
}

function buildMatrixData(data, tableValues) {
    let header = [['Scenario'], [data['units']]];
    let scenarios = [];
    let volumes = [];
    let colors = [];
    let headerColors = [];
    for (let i = 0; i < data['scenario_run_data'].length; i++) {
        let scenarioName = data['scenario_run_data'][i]['scenario_name'];
        scenarios.push('<b>' + scenarioName + '</b>');
        headerColors.push(data['scenario_run_data'][i]['scenario_color']);
        volumes.push(tableValues[i]);
        if (i < data['scenario_run_data'].length - 1) {
            header.push(['Difference from<br>' + scenarioName]);
        }
    }
    colors.push(headerColors);
    let values = [scenarios, volumes];
    for (let i = 1; i < volumes.length; i++) {
        let diffValues = [];
        for (let j = 0; j < data['scenario_run_data'].length; j++) {
            if (j < i) {
                diffValues.push('');
            } else {
                let diff = tableValues[j] - tableValues[i - 1];
                diffValues.push(Math.round(diff));
            }
        }
        values.push(diffValues);
    }
    return {
        type: 'table',
        header: {
            values: header,
            align: "center",
            line: {width: 1, color: 'black'},
            font: {family: PLOTLY_FONT['family'], size: 8}
        },
        cells: {
            values: values,
            align: ['left', 'center'],
            height: 8,
            line: {color: "black", width: 1},
            font: {family: PLOTLY_FONT['family'], color: colors, size: [8, 11]}
        }
    };
}

function tabulateWaterYearPeriod(waterYearPeriod, data, tableValues, i) {
    let layout = {
        font: PLOTLY_FONT,
        bgcolor: 'red',
        title: {
            text: waterYearPeriod.toUpperCase() + ' (' + getAcronym(waterYearPeriod) + ')',
            bgcolor: 'red'
        },
        height: 250,
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
        responsive: true
    });
    let elementid = "water-year-matrix" + i;
    $("#" + elementid).mousedown((ev) => {
        if (ev.which === 3) {
            openContextMenu("#" + elementid, ev, () => plotlyCopyToClipboard(elementid), plotlyExportFunction(document.getElementById(elementid)));
        }
    });
}

function plotMatrix(data) {
    let tableValues = buildTable(data['scenario_run_data'], data)['cells']['values'];
    for (let i = 1; i < tableValues.length; i++) {
        let arr = data['scenario_run_data'][0]['statistically_computed_time_series_wyt'][i - 1];
        if (arr) {
            let waterYearPeriod = arr['water_year_period'];
            tabulateWaterYearPeriod(waterYearPeriod, data, tableValues, i);
        }
    }
}

function plot(data) {
    plotGraph(data);
    plotMatrix(data);
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
    for (var i = 0; i < datum['header']['values'].length; i++) {
        var name = datum['header']['values'][i] + "";
        text += name + '\t';
    }
    text += '\n';
    let values = datum['cells']['values'];
    var rows = values[0].length;
    for (var j = 0; j < rows; j++) {
        for (var k = 0; k < values.length; k++) {
            var val = values[k][j] + '\t';
            text += val;
        }
        text += '\n';
    }
    copyTextToClipboard(text.replace(/<br>/g, ' ').replace(/<b>/g, '').replace(/<\/b>/g, ''));
}