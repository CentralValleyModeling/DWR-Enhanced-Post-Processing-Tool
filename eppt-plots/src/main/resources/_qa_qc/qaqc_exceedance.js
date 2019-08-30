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
function test() {
    return plot({
        "scenario_run_data": [
            {
                "data": [[.01, 9999], [.20, 9999], [.25, 999], [.29, 99], [.40, 50], [.90, 9], [.95, 2]],
                "name": "Base",
                "thresholds": [
                    {
                        "data": [[.09, 8888], [.24, 887], [.45, 250], [.55, 87], [.66, 45], [.78, 8], [.96, 2.5]],
                        name: 'Regulatory Value'
                    },
                    {
                        "data": [[.01, 9999], [.20, 9999], [.25, 999], [.29, 99], [.40, 50], [.90, 9], [.95, 2]],
                        name: 'Operational Standard'
                    }
                ]
            },
            {
                "data": [[.20, 6000], [.25, 5000], [.29, 500], [.40, 50], [.90, 5], [.99, 1]],
                "name": "Alt",
                "thresholds": [
                    {
                        "data": [[.09, 5858], [.24, 585], [.45, 258], [.55, 85], [.66, 58], [.78, 8], [.96, 5]],
                        name: 'Regulatory Value'
                    },
                    {
                        "data": [[.01, 8787], [.20, 7878], [.25, 787], [.29, 87], [.40, 78], [.90, 8], [.95, 7]],
                        name: 'Operational Standard'
                    }
                ]
            },
        ],
        "y_axis": "TAF",
        "x_axis": "Exceedance Probability",
        "title": "Exceedance (All Months)"
    });
}

function getSeries(datum) {
    let series = new Array(datum.length);
    for (var i = 0; i < datum.length; i++) {
        var timeSeries = datum[i]['data'];
        series[i] = {
            name: datum[i]['name'],
            data: timeSeries,
        };
    }
    return series;
}

function buildScenarioLine(datum, i) {
    let dataSeries = datum['data'];
    let yData = [];
    for (var j = 0; j < dataSeries.length; j++) {
        yData.push(dataSeries[j][1]);
    }
    let xData = [];
    for (var k = 0; k < dataSeries.length; k++) {
        xData.push(dataSeries[k][0]);
    }
    return {
        x: xData,
        y: yData,
        type: 'scatter',
        marker: {
            color: PLOTLY_COLORS[i]
        },
        name: datum['name'],
        legendgroup: datum['name'],
        textfont: {
            color: 'white'
        },
    };
}

function buildThresholdLines(datum, color) {
    let thresholds = datum['thresholds'];
    let thresholdLines = [];
    for (var i = 0; i < thresholds.length; i++) {
        let dataSeries = thresholds[i]['data'];
        let yData = [];
        for (var j = 0; j < dataSeries.length; j++) {
            yData.push(dataSeries[j][1]);
        }
        let xData = [];
        for (var k = 0; k < dataSeries.length; k++) {
            xData.push(dataSeries[k][0]);
        }
        let scenarioLine = {
            x: xData,
            y: yData,
            type: 'scatter',
            mode: 'lines',
            legendgroup: datum['name'],
            marker: {
                color: color
            },
            line: {
                dash: THRESHOLD_LINE_DASH[i % THRESHOLD_LINE_DASH.length],
                width: 1
            },
            name: thresholds[i]['name'],
            textfont: {
                color: 'white'
            },
        };
        thresholdLines.push(scenarioLine);
    }
    return thresholdLines;
}

function getPlotlySeries(datum) {
    let series = [];
    for (var i = 0; i < datum.length; i++) {
        series.push(buildScenarioLine(datum[i], i));
        Array.prototype.push.apply(series, buildThresholdLines(datum[i], PLOTLY_COLORS[i]));
    }
    return series;
}

function plot(data) {
    let datum = data['scenario_run_data'];
    let series = getSeries(datum);
    let container = document.createElement('div');
    var layout = {
        font: PLOTLY_FONT,
        // legend: {
        //     orientation: 'v',
        //     xanchor: 'center',
        //     x: .8,
        //     y: .5
        // },
        xaxis: {
            showgrid: false,
            tickformat: ',.0%',
            title: {
                text: data['x_axis'],
            },
            reversed: true,
            range: [1.01, 0.001]
        },
        yaxis: {
            type: 'log',
            dtick: 1,
            autorange: true,
            title: {
                text: data['y_axis'],
            }
        },
        showlegend: true,
        modebar: {
            orientation: 'v',
            bgcolor: 'rgba(0,0,0,0.5)'
        },
        title: {
            text: data['title'],
            font: {
                size: 20,
            }
        }
    };
    let newPlot = Plotly.newPlot('tester', getPlotlySeries(datum), layout, {
        displaylogo: false,
        modeBarButtonsToRemove: ['toImage', 'sendDataToCloud', 'editInChartStudio', 'lasso2d', 'select2d', 'toggleSpikelines', 'resetScale2d'],
        scrollZoom: true
    });
    let elementsByClassName = document.getElementsByClassName("main-svg");
    let toImage = Plotly.toImage(newPlot,{format:'svg'}).resolve();
    let chart = Highcharts.chart('container', {
        chart: {
            renderTo: container
        },
        title: {
            text: data['title']
        },
        xAxis: {
            min: 0,
            max: 100,
            reversed: 'true',
            title: {
                text: data['x_axis'],
                align: 'middle'
            },
            labels: {
                overflow: 'justify',
                formatter: function () {
                    return this.value + "%";
                }
            }
        },
        yAxis: {
            type: 'logarithmic',
            minRange: 0.1,
            title: {
                text: data['y_axis'],
                align: 'middle'
            },
            labels: {
                overflow: 'justify'
            }
        },
        legend: {
            enabled: true
        },
        credits: {
            enabled: false
        },
        exporting: {enabled: false},
        series: series
    });
    return chart.getSVG();
}
