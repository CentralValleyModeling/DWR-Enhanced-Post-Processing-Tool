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

function getPlotlyBoxValuesForAll(tsc) {
    var datum = [];
    // All data
    for (var k = 0; k < tsc.length; k++) {
        datum.push(tsc[k][1]);
    }
    return datum;
}

function getPlotlyBoxValuesForPeriodAvg(tsc) {
    var datum = [];
    for (var i = 0; i < tsc.length; i++) {
        datum.push(tsc[i][1]);
    }
    return datum;
}

function getPlotlyBoxValues(dataOnly, periodMonths) {
    var y = [];
    var x = [];
    if (periodMonths.length > 1) {
        let values = getPlotlyBoxValuesForAll(dataOnly);
        y = y.concat(values);
        x = x.concat(new Array(values.length).fill('All', 0, values.length));
    }
    for (var i = 0; i < periodMonths.length; i++) {
        let filteredData = filterDataForMonth(dataOnly, periodMonths[i]);
        y = y.concat(filteredData);
        x = x.concat(new Array(filteredData.length).fill(periodMonths[i], 0, filteredData.length));
    }
    if (periodMonths.length > 1) {
        let values = getPlotlyBoxValuesForPeriodAvg(periodSeries);
        y = y.concat(values);
        x = x.concat(new Array(values.length).fill(periodMonths[0] + '-' + periodMonths[periodMonths.length - 1], 0, values.length));
    }
    return {
        y: y,
        x: x,
    };
}

function filterDataForMonth(data, month) {
    var retval = [];
    for (var i = 0; i < data.length; i++) {
        let dateMillis = data[i][0];
        var tsDate = new Date(dateMillis);
        tsDate.setMonth(tsDate.getMonth() - 1);
        var compareMonth = new Date(Date.parse(month + "1, 2012"));
        if (tsDate.getMonth() === compareMonth.getMonth()) {
            retval.push(data[i][1]);
        }
    }
    return retval;
}

function getPlotlySeries(datum, periodMonths) {
    var series = [];
    for (var i = 0; i < datum.length; i++) {
        var timeSeries = datum[i]['primary_data']['full_time_series'][0];
        var data = getPlotlyBoxValues(timeSeries, periodMonths);
        series.push({
            y: data['y'],
            x: data['x'],
            type: 'box',
            name: datum[i]['scenario_name'],
            marker: {
                color: datum[i]['scenario_color']
            },
            boxpoints: false,
            boxmean: true
        });
    }
    return series;
}

function plot(data) {
    FORMATTER = getD3Formatter(data['scenario_run_data'][0]['full_time_series']);
    let datum = data['scenario_run_data'];
    let periodMonths = data['period_months'];
    var layout = {
        font: {
            family: 'Lucida Grande", "Lucida Sans Unicode", "Verdana", "Arial", "Helvetica", "sans-serif',
            color: 'black',
        },
        title: {
            text: data['gui_link_title'] + '<br>' + data['month_period_title'],
            font: {
                size: 20,
            }
        },
        legend: {
            orientation: 'h',
            xanchor: 'center',
            x: 0.5,
            font: {
                size: 10,
            }
        },
        yaxis: {
            tickformat: ',.3r%',
            title: {
                text: data['units']
            },
            tickformat: FORMATTER,
            rangemode: 'tozero',
            zeroline: false,
            gridcolor: '#DDDDDD'
        },
        xaxis: {
            automargin: true
        },
        boxmode: 'group'
    };
    Plotly.newPlot('tester', getPlotlySeries(datum, periodMonths), layout, {
            displaylogo: false,
        modeBarButtons: buildModeBarButtons('tester'),
            scrollZoom: true,
            responsive: true
        }
    );
    $("#tester").mousedown((ev) => {
        if (ev.which === 3) {
            openContextMenu('#tester', ev, plotlyCopyToClipboard, plotlyExportFunction(document.getElementById("tester")));
        }
    });
}


function plotlyCopyToClipboard() {
    let plot = document.getElementById("tester");
    let layout = plot.layout;
    let data1 = plot.data;
    let calcdata = plot.calcdata;
    var text = layout['title']['text'] + '\n' + 'Scenario\tGroup\t' + layout['yaxis']['title']['text'] + '\n\t';
    text += '\n';
    for (var j = 0; j < calcdata.length; j++) {
        let calcdatum = calcdata[j];
        text += calcdatum[0]['trace']['name'] + '\t';
        text += '\tmin\tmax\tmedian\tmean\tq1\tq3\tsd';
        text += '\n';
        for (var k = 0; k < calcdatum.length; k++) {
            let boxdatum = calcdatum[k];
            text += '\t';
            if (calcdatum[0]['trace']) {
                let boxName = calcdatum[0]['trace']['x'][boxdatum.pts[0]['i']];
                text += boxName;
            }
            text += '\t' + boxdatum.min + '\t' + boxdatum.max + '\t' + boxdatum.med + '\t';
            text += boxdatum.mean + '\t' + boxdatum.q1 + '\t' + boxdatum.q3 + '\t' + boxdatum.sd;
            text += '\n';
        }
        text += '\n';
    }
    copyTextToClipboard(text);
}
