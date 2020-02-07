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


function getPlotlySeries(datum) {
    let categoryKeys = [];
    let series = new Array(datum.length);
    for (var i = 0; i < datum.length; i++) {
        let dataSeries = datum[i]['primary_data']['statistically_computed_time_series_monthly'][0];
        let yData = [];
        for (var j = 0; j < dataSeries.length; j++) {
            yData.push(dataSeries[j][1]);
            categoryKeys[j] = dataSeries[j][0];
        }
        series[i] = {
            x: categoryKeys,
            y: yData,
            type: 'scatter',
            marker: {
                color: datum[i]['scenario_color']
            },
            name: datum[i]['scenario_name'],
        };
    }
    return series;
}

function plot(data) {

    let datum = data['scenario_run_data'];

    var layout = {
        font: PLOTLY_FONT,
        yaxis: {
            tickformat: ',.3r',
            title: {
                text: data['units']
            },
            gridcolor: '#CCCCCC'
        },
        xaxis:{
            gridcolor: '#CCCCCC'
        },
        legend: {
            orientation: 'h',
            xanchor: 'center',
            x: 0.5
        },
        showlegend: true,
        title: {
            text: data['month_period_title'] + ' ' + data['gui_link_title'] + '<br>(' + data['statistics'] + ')',
            font: {
                family: PLOTLY_FONT['family'],
                size: 20,
                color: 'black',
            }
        }
    };
    Plotly.newPlot('tester', getPlotlySeries(datum), layout, {
        displaylogo: false,
        modeBarButtons: buildModeBarButtons('tester'),
        scrollZoom: true
    });
    $("#tester").mousedown((ev) => {
        if (ev.which === 3) {
            openContextMenu('#tester', ev, plotlyCopyToClipboard, plotlyExportFunction(document.getElementById("tester")));
        }
    })
}

function plotlyCopyToClipboard() {
    let plot = document.getElementById("tester");
    let layout = plot.layout;
    let data1 = plot.data;
    var text = layout['title']['text'] + '\n' + 'Month\t' + layout['yaxis']['title']['text'] + '\n';
    for (var i = 0; i < data1.length; i++) {
        text += '\t' + data1[i]['name']
    }
    text += '\n';
    let datum = data1[0];
    let xarr = datum['x'];
    for (var j = 0; j < xarr.length; j++) {
        text += (xarr[j]);
        for (var k = 0; k < data1.length; k++) {
            let yarr = data1[k]['y'];
            text += '\t' + yarr[j];
        }
        text += '\n';
    }
    copyTextToClipboard(text);
}
