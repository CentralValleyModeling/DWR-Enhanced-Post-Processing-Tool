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

function getAggregatePlotlySeries(datum) {
    let series = [];

    function plotSeries(allTimeSeries, lineName, lineColor, primary) {
        for (let m = 0; m < allTimeSeries['period_filtered_time_series'].length; m++) {
            let timeSeries = allTimeSeries['period_filtered_time_series'][m];
            let dataOnly = [];
            for (var j = 0; j < timeSeries.length; j++) {
                dataOnly.push(timeSeries[j][1]);
            }
            dataOnly.sort((a, b) => a - b);
            let xData = [];
            let yData = [];
            for (var k = 0; k < dataOnly.length; k++) {
                let exceedance = 1 - ((k + 0.5) / dataOnly.length);
                xData.push(exceedance);
                yData.push(dataOnly[k]);
            }
            let dash;
            if (primary) {
                dash = PLOTLY_LINE_DASH_STYLES[m % PLOTLY_LINE_DASH_STYLES.length];
            } else {
                dash = PLOTLY_LINE_DASH_STYLES[1];
            }
            let name = lineName;
            if (allTimeSeries['data_suffix'][m]) {
                name = " " + allTimeSeries['data_suffix'][m];
            }
            series.push({
                x: xData,
                y: yData,
                marker: {
                    color: lineColor
                },
                line: {
                    dash: dash
                },
                name: name
            });
        }
    }

    for (var i = 0; i < datum.length; i++) {
        let primarySeries = datum[i]['primary_data'];
        let scenarioName = datum[i]['scenario_name'];
        let lineColor = datum[i]['scenario_color'];
        plotSeries(primarySeries, scenarioName, lineColor, true);
        let secondarySeries = datum[i]['secondary_data'];
        plotSeries(secondarySeries, scenarioName, lineColor, false);
    }
    return series;
}

function plot(data) {
    let units = data['scenario_run_data'][0]['primary_data']['units'][0];
    let datum = data['scenario_run_data'];
    var layout = {
        font: PLOTLY_FONT,
        xaxis: {
            tickformat: ',.0%',
            range: [1, 0],
            zeroline: false,
            gridcolor: '#CCCCCC'
        },
        yaxis: {
            title: {
                text: units,
            },
            autorange: true,
            zeroline: false,
            gridcolor: '#CCCCCC'
        },
        showlegend: true,
        legend: {
            orientation: 'h',
            xanchor: 'center',
            x: 0.5,
            font: {
                size: 15,
            }
        },
        title: {
            text: data['month_period_title'] + ' ' + data['gui_link_title'],
            font: {
                size: 20,
            }
        }
    };
    Plotly.newPlot('container_aggregate_tester', getAggregatePlotlySeries(datum), layout, {
        displaylogo: false,
        modeBarButtons: buildModeBarButtons("container_aggregate_tester"),
        scrollZoom: true,
        responsive: true,
    });
    $("#container_aggregate_tester").mousedown((ev) => {
        if (ev.which === 3) {
            openContextMenu('#container_aggregate_tester', ev, plotlyAggregateCopyToClipboard, plotlyExportFunction(document.getElementById("container_aggregate_tester")));
        }
    })
}

function plotlyAggregateCopyToClipboard() {
    let plot = document.getElementById("container_aggregate_tester");
    let layout = plot.layout;
    let data1 = plot.data;
    var text = layout['title']['text'] + '\n' + 'Percent\t' + layout['yaxis']['title']['text'] + '\n';
    for (var i = 0; i < data1.length; i++) {
        text += '\t' + data1[i]['name']
    }
    text += '\n';
    let datum = data1[0];
    let xarr = datum['x'];
    for (var j = 0; j < xarr.length; j++) {
        text += (xarr[j] * 100);
        for (var k = 0; k < data1.length; k++) {
            let yarr = data1[k]['y'];
            text += '\t' + yarr[j]
        }
        text += '\n';
    }
    copyTextToClipboard(text);
}
