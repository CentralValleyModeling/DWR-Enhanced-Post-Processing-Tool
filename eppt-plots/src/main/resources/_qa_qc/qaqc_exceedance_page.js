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
                "month": "Jan",
                "base": {
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
                "alt":{
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
                }
            },{
                "month": "Feb",
                "base": {
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
                "alt":{
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
                }
            },
            {
                "month": "Mar",
                "base": {
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
                "alt": {
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
                }
            }, {
                "month": "April",
                "base": {
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
                "alt": {
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
                }
            },
            {
                "month": "May",
                "base": {
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
                "alt": {
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
                }
            },
            {
                "month": "June",
                "base": {
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
                "alt": {
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
                }
            }, {
                "month": "July",
                "base": {
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
                "alt": {
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
                }
            }, {
                "month": "Aug",
                "base": {
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
                "alt": {
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
                }
            }, {
                "month": "Sept",
                "base": {
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
                "alt": {
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
                }
            }, {
                "month": "Oct",
                "base": {
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
                "alt": {
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
                }
            }, {
                "month": "Nov",
                "base": {
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
                "alt": {
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
                }
            }, {
                "month": "Dec",
                "base": {
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
                "alt": {
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
                }
            }
        ],
        "y_axis": "TAF",
        "x_axis": "Exceedance Probability",
        "title": "Exceedance (All Months)"
    });
}

function buildScenarioLine(datum, color, xaxis, yaxis, month) {
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
        xaxis: 'x' + xaxis,
        yaxis: 'y' + yaxis,
        marker: {
            color: color
        },
        name: datum['name'] + ' (' + month + ')',
        legendgroup: datum['name'] + ' (' + month + ')',
        textfont: {
            color: 'white'
        },
    };
}

function buildThresholdLines(datum, color, axis, month) {
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
            xaxis: 'x' + axis,
            yaxis: 'y' + axis,
            legendgroup: datum['name'] + ' (' + month + ')',
            marker: {
                color: color
            },
            line: {
                dash: THRESHOLD_LINE_DASH[i % THRESHOLD_LINE_DASH.length],
                width: 1
            },
            name: thresholds[i]['name'] + ' (' + month + ')',
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
        let month = datum[i]['month'];
        let base = datum[i]['base'];
        let alt = datum[i]['alt'];
        let xaxis = i + 1;
        series.push(buildScenarioLine(base, PLOTLY_COLORS[0], xaxis, xaxis, month));
        Array.prototype.push.apply(series, buildThresholdLines(base, PLOTLY_COLORS[0], xaxis, month));
        if(alt){
            series.push(buildScenarioLine(alt, PLOTLY_COLORS[1], xaxis, xaxis, month));
            Array.prototype.push.apply(series, buildThresholdLines(alt, PLOTLY_COLORS[1], xaxis, month));
        }
    }
    return series;
}

function plot(data) {
    let datum = data['scenario_run_data'];
    var layout = {
        font: {
            family: 'Lucida Grande", "Lucida Sans Unicode", "Verdana", "Arial", "Helvetica", "sans-serif',
            color: 'black',
        },
        xaxis: {
            showgrid: false,
            tickformat: ',.0%',
            reversed: true,
            range: [1.01, 0.001],
        },
        xaxis2: {
            showgrid: false,
            tickformat: ',.0%',
            reversed: true,
            range: [1.01, 0.001],
        },
        xaxis3: {
            showgrid: false,
            tickformat: ',.0%',
            reversed: true,
            range: [1.01, 0.001],
        },
        xaxis4: {
            showgrid: false,
            tickformat: ',.0%',
            reversed: true,
            range: [1.01, 0.001],
        },
        xaxis5: {
            showgrid: false,
            tickformat: ',.0%',
            reversed: true,
            range: [1.01, 0.001],
        },
        xaxis6: {
            showgrid: false,
            tickformat: ',.0%',
            reversed: true,
            range: [1.01, 0.001],
        },
        xaxis7: {
            showgrid: false,
            tickformat: ',.0%',
            reversed: true,
            range: [1.01, 0.001],
        },
        xaxis8: {
            showgrid: false,
            tickformat: ',.0%',
            reversed: true,
            range: [1.01, 0.001],
        },
        xaxis9: {
            showgrid: false,
            tickformat: ',.0%',
            reversed: true,
            range: [1.01, 0.001],
        },
        xaxis10: {
            showgrid: false,
            tickformat: ',.0%',
            reversed: true,
            range: [1.01, 0.001],
        },
        xaxis11: {
            showgrid: false,
            tickformat: ',.0%',
            reversed: true,
            range: [1.01, 0.001],
        },
        xaxis12: {
            showgrid: false,
            tickformat: ',.0%',
            reversed: true,
            range: [1.01, 0.001],
        },
        yaxis: {
            type: 'log',
            dtick: 1,
            autorange: true,
            title: {
                text: data['y_axis'],
            },
            tickfont: {
                size: 10
            }
        },
        yaxis2: {
            type: 'log',
            dtick: 1,
            autorange: true,
            tickfont: {
                size: 10
            }
        },
        yaxis3: {
            type: 'log',
            dtick: 1,
            autorange: true,
            tickfont: {
                size: 10
            }
        },
        yaxis4: {
            type: 'log',
            dtick: 1,
            autorange: true,
            title: {
                text: data['y_axis'],
            },
            tickfont: {
                size: 10
            }
        },
        yaxis5: {
            type: 'log',
            dtick: 1,
            autorange: true,
            tickfont: {
                size: 10
            }
        },
        yaxis6: {
            type: 'log',
            dtick: 1,
            autorange: true,
            tickfont: {
                size: 10
            }
        },
        yaxis7: {
            type: 'log',
            dtick: 1,
            autorange: true,
            title: {
                text: data['y_axis'],
            },
            tickfont: {
                size: 10
            }
        },
        yaxis8: {
            type: 'log',
            dtick: 1,
            autorange: true,
            tickfont: {
                size: 10
            }
        },
        yaxis9: {
            type: 'log',
            dtick: 1,
            autorange: true,
            tickfont: {
                size: 10
            }
        },
        yaxis10: {
            type: 'log',
            dtick: 1,
            autorange: true,
            title: {
                text: data['y_axis'],
            },
            tickfont: {
                size: 10
            }
        },
        yaxis11: {
            type: 'log',
            dtick: 1,
            autorange: true,
            tickfont: {
                size: 10
            }
        },
        yaxis12: {
            type: 'log',
            dtick: 1,
            autorange: true,
            title: {
                text: data['y_axis'],
            },
            tickfont: {
                size: 10
            }
        },
        grid: {rows: 4, columns: 3, pattern: 'independent'},
        showlegend: false,
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

    var graphDiv = document.getElementById('tester');
    let newPlot = Plotly.newPlot( graphDiv, getPlotlySeries(datum), layout);
    Plotly.downloadImage(graphDiv, {format: 'svg', height:900, width:1800});
}
