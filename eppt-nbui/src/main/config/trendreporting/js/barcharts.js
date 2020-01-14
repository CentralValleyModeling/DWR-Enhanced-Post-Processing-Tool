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

function getHeaders(data) {
    let firstIndexData = data[0]['statistically_computed_time_series_wyt'];
    let headers = ['','<b>Long Term</b>'];
    for (let j = 0; j < firstIndexData.length; j++) {
        let waterYearPeriod = firstIndexData[j]['water_year_period'];
        headers.push('<b>' + getAcronym(waterYearPeriod) + '</b>');
    }
    return headers;
}

function buildTable(data, allData) {
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
        let longTerm = values[1];
        if (!longTerm) {
            longTerm = [];
            values[1] = longTerm;
        }
        longTerm[i] = Math.round(data[i]['statistically_computed_time_series_yearly']);
        let timeSeries = data[i]['statistically_computed_time_series_wyt'];
        for (let k = 0; k < timeSeries.length; k++) {
            let val = timeSeries[k]['water_year_period_values'];
            let periodValues = values[2 + k];
            if (!periodValues) {
                periodValues = [];
                values[2 + k] = periodValues;
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
    return text;
}

function getPlotlySeries(allData) {
    let series = [];
    let datum = allData['scenario_run_data'];
    for (let i = 0; i < datum.length; i++) {
        series.push(buildBarsForScenario(datum[i], datum[i]['scenario_color']));
    }
    series.push(buildTable(datum, allData));
    return series;
}

function buildBarsForScenario(data, color) {
    let timeSeries = data['statistically_computed_time_series_wyt'];
    let xarr = ['Long Term'];
    for (let j = 0; j < timeSeries.length; j++) {
        let waterYearPeriod = timeSeries[j]['water_year_period'];
        xarr.push(getAcronym(waterYearPeriod));
    }
    let yarr = [data['statistically_computed_time_series_yearly']];
    for (let k = 0; k < timeSeries.length; k++) {
        let val = timeSeries[k]['water_year_period_values'];
        yarr.push(val);
    }
    return {
        type: 'bar',
        name: data['scenario_name'],
        x: xarr,
        y: yarr,
        marker: {
            color: color
        }
    };
}

function plot(data) {
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
            text: data['month_period_title'] + ' ' + data['gui_link_title'] + '<br>(' + data['statistics'] + ')',
            font: {
                size: 20,
            }
        }
    };

    Plotly.newPlot('tester', getPlotlySeries(data), layout, {
        displaylogo: false,
        modeBarButtonsToRemove: ['toImage', 'sendDataToCloud', 'editInChartStudio', 'lasso2d', 'select2d', 'resetScale2d'],
        scrollZoom: true,
        responsive: false,
        grid: {rows: 2, columns: 1, pattern: 'independent'}
    });
    $("#tester").mousedown((ev) => {
        if (ev.which === 3) {
            openContextMenu('#tester', ev, () => plotlyCopyToClipboard('tester'), plotlyExportFunction(document.getElementById('tester')));
        }
    });
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