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
            mode: 'lines',
            line: {color: datum[i]['scenario_color']}
        });
    }
    return series;
}

function getPlotlyScatterSeries(datum) {
    var series = [];
    for (var i = 1; i < datum.length; i++) {
        let baseTimeSeries = datum[0]['full_time_series'];
        let altTimeSeries = datum[i]['full_time_series'];
        let x = [];
        let y = [];
        for (var j = 0; j < altTimeSeries.length; j++) {
            x.push(baseTimeSeries[j][1]);
            y.push(altTimeSeries[j][1]);
        }
        series.push({
            name: datum[i]['scenario_name'],
            x: x,
            y: y,
            type: 'scatter',
            mode: 'markers',
            marker:{
                color: datum[i]['scenario_color']
            }
        });
    }
    return series;
}

function buildLineLayout(data) {
    return {
        font: PLOTLY_FONT,
        yaxis: {
            title: {
                text: data['units'],
            },
            gridcolor: '#CCCCCC'
        },
        xaxis: {
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
}

function buildScatterLayout(data) {
    return {
        font: PLOTLY_FONT,
        yaxis: {
            title: {
                text: data['units'],
            },
            gridcolor: '#CCCCCC'
        },
        xaxis: {
            title: {
                text: data['scenario_run_data'][0]['scenario_name'] + ' - ' +  data['units'],
            },
            hovertemplate: '<i>Price</i>: $%{y:.2f}' +
                '<br><b>X</b>: %{x}<br>' +
                '<b>%{text}</b>',
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
}

function plot(data) {
    var datum = data['scenario_run_data'];
    var layout = buildLineLayout(data);
    let plotlyMonthlySeries = getPlotlyMonthlySeries(datum);
    let modeBar;
    if (data['comparison_mode'] === 'COMPARISON' && datum.length > 1) {
        modeBar = buildModeBarButtonsWithScatter('container_discrete_tester', data);
    } else {
        modeBar = buildModeBarButtons();
    }
    Plotly.newPlot('container_discrete_tester', plotlyMonthlySeries, layout, {
        displaylogo: false,
        modeBarButtons: modeBar,
        scrollZoom: true,
        responsive: true
    });
    $("#container_discrete_tester").mousedown((ev) => {
        if (ev.which === 3) {
            openContextMenu('#container_discrete_tester', ev, plotlyCopyToClipboardMonthly, plotlyExportFunction(document.getElementById("container_discrete_tester")));
        }
    });
}

function plotlyCopyToClipboardMonthly() {
    let plot = document.getElementById("container_discrete_tester");
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

function buildModeBarButtonsWithScatter(graphDiv, data) {
    let scatterButton = {
        name: 'Scatter Plot',
        // icon: '../img/scatter-plot.svg',
        icon: {
            'width': 1500,
            'height': 1500,
            'path': 'm 1206.5226,551.534 c 0,61.97227 -53.8873,112.21073 -120.3606,112.21073 -66.4733,0 -120.36063,-50.23846 -120.36063,-112.21073 0,-61.97228 53.88733,-112.21073 120.36063,-112.21073 66.4733,0 120.3606,50.23845 120.3606,112.21073 z m 788.8696,-625.174074 c 0,61.972271 -53.8873,112.210727 -120.3606,112.210727 -66.4733,0 -120.3606,-50.238456 -120.3606,-112.210727 0,-61.972296 53.8873,-112.210746 120.3606,-112.210746 66.4733,0 120.3606,50.23845 120.3606,112.210746 z M 1656.0623,351.15768 c 0,61.97228 -53.8873,112.21073 -120.3607,112.21073 -66.4733,0 -120.3606,-50.23845 -120.3606,-112.21073 0,-61.97227 53.8873,-112.21072 120.3606,-112.21072 66.4734,0 120.3607,50.23845 120.3607,112.21072 z M 1357.3359,-33.564819 c 0,61.972271 -53.8873,112.210727 -120.3606,112.210727 -66.4733,0 -120.3606,-50.238456 -120.3606,-112.210727 0,-61.972281 53.8873,-112.210731 120.3606,-112.210731 66.4733,0 120.3606,50.23845 120.3606,112.210731 z M 1099.2131,1120.6027 c 0,61.9723 -53.8873,112.2107 -120.3606,112.2107 -66.47334,0 -120.36063,-50.2384 -120.36063,-112.2107 0,-61.9722 53.88729,-112.2107 120.36063,-112.2107 66.4733,0 120.3606,50.2385 120.3606,112.2107 z M 785.98547,885.49454 c 0,61.97228 -53.88728,112.21072 -120.36062,112.21072 -66.47334,0 -120.36062,-50.23844 -120.36062,-112.21072 0,-61.97228 53.88728,-112.21073 120.36062,-112.21073 66.47334,0 120.36062,50.23845 120.36062,112.21073 z M 491.60949,1139.3045 c 0,61.9723 -53.88729,112.2107 -120.36062,112.2107 -66.47336,0 -120.36064,-50.2384 -120.36064,-112.2107 0,-61.9723 53.88728,-112.2108 120.36064,-112.2108 66.47333,0 120.36062,50.2385 120.36062,112.2108 z M 219.25741,1451.2417 H 2013.1081 q 18.0849,0 30.5721,11.1064 12.4872,11.1064 12.4872,27.766 0,15.8663 -12.4872,27.3694 -12.4872,11.503 -30.5721,11.503 H 177.05931 q -18.0849,0 -30.57213,-11.503 Q 134,1505.9804 134,1490.1141 V -190.12754 q 0,-15.86632 12.48718,-27.36939 12.48723,-11.50306 30.57213,-11.50306 17.2237,0 29.71093,11.50306 12.48717,11.50307 12.48717,27.36939',
            // 'transform': 'matrix(1 0 0 -1 0 850)'
        },
        click: () => toggleScatterPlot()
    };
    let lineButton = {
        name: 'Line Plot',
        icon: {
            'width': 1500,
            'height': 1000,
            'path': 'M233 -251h2083q21 0 35.5 -14t14.5 -35q0 -20 -14.5 -34.5t-35.5 -14.5h-2132q-21 0 -35.5 14.5t-14.5 34.5v2118q0 20 14.5 34.5t35.5 14.5q20 0 34.5 -14.5t14.5 -34.5v-1642l235 241q21 21 48 21q25 0 45 -17l211 -176l435 1316q15 46 59 46q45 0 65 -35l310 -560\n' +
                'l179 517q8 21 26 34l391 273q18 12 40 12q28 0 48 -19.5t20 -48.5q0 -37 -29 -57l-373 -261l-224 -640q-16 -47 -59 -47q-46 0 -66 36l-306 557l-416 -1263q-7 -20 -24.5 -34t-41.5 -14q-14 0 -24 4.5t-20 12.5l-240 201l-289 -294v-232z',
            'transform': 'matrix(1 0 0 -1 0 850)'
        },
        click: () => toggleScatterPlot()
    };
    var scatter = false;
    function toggleScatterPlot() {
        if (!scatter) {
            scatter = true;
            var datum = data['scenario_run_data'];
            let modeBar = buildModeBarButtons();
            var layout = buildScatterLayout(data);
            let plotlyMonthlySeries = getPlotlyScatterSeries(datum);
            modeBar[1].push(lineButton);
            Plotly.react(graphDiv, plotlyMonthlySeries, layout, {
                displaylogo: false,
                modeBarButtons: modeBar,
                scrollZoom: true,
                responsive: true
            });
        } else {
            scatter = false;
            let modeBar = buildModeBarButtons();
            var datum = data['scenario_run_data'];
            var layout = buildLineLayout(data);
            let plotlyMonthlySeries = getPlotlyMonthlySeries(datum);
            modeBar[1].push(scatterButton);
            Plotly.react(graphDiv, plotlyMonthlySeries, layout, {
                displaylogo: false,
                modeBarButtons: modeBar,
                scrollZoom: true,
                responsive: true
            });
        }
    }

    let modeBar = buildModeBarButtons();
    modeBar[1].push(scatterButton);
    return modeBar;
}
