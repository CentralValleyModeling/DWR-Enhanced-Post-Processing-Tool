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

function getPlotlyMonthlySeries(datum) {
    var series = [];
    for (var i = 0; i < datum.length; i++) {
        let timeSeries = datum[i]['full_time_series'];
        let x = [];
        let y = [];
        for (var j = 0; j < timeSeries.length; j++) {
            x.push(new Date(timeSeries[j][0]));
            y.push(timeSeries[j][1]);
        }
        series.push({
            name: datum[i]['scenario_name'],
            x: x,
            y: y,
            line: {color: datum[i]['scenario_color']}
        });
    }
    return series;
}

function plotMonthly(data) {
    var datum = data['scenario_run_data'];
    var layout = {
        font: PLOTLY_FONT,
        yaxis: {
            title: {
                text: data['units'],
            },
            gridcolor: '#CCCCCC'
        },
        xaxis:{
            gridcolor: '#CCCCCC'
        },
        showlegend: true,
        legend: {
            orientation: 'h',
            xanchor: 'center',
            x: 0.5,
            font: {
                size: 10,
            }
        },
        title: {
            text: data['gui_link_title'],
            font: {
                size: 20,
            }
        }
    };
    Plotly.newPlot('container_monthly_tester', getPlotlyMonthlySeries(datum), layout, {
        displaylogo: false,
        modeBarButtonsToRemove: ['toImage', 'sendDataToCloud', 'editInChartStudio', 'lasso2d', 'select2d', 'resetScale2d'],
        scrollZoom: true,
        responsive: true
    });
    $("#container_monthly_tester").mousedown((ev) => {
        if (ev.which === 3) {
            openContextMenu('#container_monthly_tester', ev, plotlyCopyToClipboardMonthly, plotlyExportFunction(document.getElementById("container_monthly_tester")));
        }
    });
}

function plotlyCopyToClipboardMonthly() {
    let plot = document.getElementById("container_monthly_tester");
    let layout = plot.layout;
    let data1 = plot.data;
    var text = layout['title']['text'] + '\n' + 'Date\t' + layout['yaxis']['title']['text'] + '\n';
    for (var i = 0; i < data1.length; i++) {
        text += '\t' + data1[i]['name']
    }
    text += '\n';
    let datum = data1[0];
    let xarr = datum['x'];
    for (var j = 0; j < xarr.length; j++) {
        text += xarr[j];
        for (var k = 0; k < data1.length; k++) {
            let yarr = data1[k]['y'];
            text += '\t' + yarr[j];
        }
        text += '\n';
    }
    copyTextToClipboard(text);
}
