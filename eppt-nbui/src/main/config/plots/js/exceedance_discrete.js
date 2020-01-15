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
function getDiscretePlotlySeries(datum) {
    let series = new Array(datum.length);
    for (var i = 0; i < datum.length; i++) {
        let timeSeries = datum[i]['full_time_series'];
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
        series[i] = {
            x: xData,
            y: yData,
            marker: {
                color: datum[i]['scenario_color']
            },
            name: datum[i]['scenario_name']
        };
    }
    return series;
}

function plot(data) {
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
                text: data['units'],
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
            text: data['gui_link_title'],
            font: {
                size: 20,
            }
        }
    };
    Plotly.newPlot('container_discrete_tester', getDiscretePlotlySeries(datum), layout, {
        displaylogo: false,
        modeBarButtons: buildModeBarButtons(),
        scrollZoom: true,
        responsive: true,
    });
    $("#container_discrete_tester").mousedown((ev) => {
        if (ev.which === 3) {
            openContextMenu('#container_discrete_tester', ev, plotlyDiscreteCopyToClipboard, plotlyExportFunction(document.getElementById("container_discrete_tester")));
        }
    })
}

function plotlyDiscreteCopyToClipboard() {
    let plot = document.getElementById("container_discrete_tester");
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
