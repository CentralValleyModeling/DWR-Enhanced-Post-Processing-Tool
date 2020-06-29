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
    let headers = [''];
    let periods = data[0]['ts_list'][0]['monthly_filters'][0]['annual_filters'];
    for (let j = 0; j < periods.length; j++) {
        let waterYearPeriod = periods[j]['annual_period'];
        headers.push('<b>' + waterYearPeriod + '</b>');
    }
    return headers;
}

function buildTable(data, monthlyIndex, statIndex) {
    let headers = getHeaders(data);
    let values = [];
    let colors = [];
    for (let scenarioIndex = 0; scenarioIndex < data.length; scenarioIndex++) {
        for (let tsIndex = 0; tsIndex < data[0]['ts_list'].length; tsIndex++) {
            let scenarioArr = values[0];
            if (!scenarioArr) {
                scenarioArr = [];
                values[0] = scenarioArr;
            }
            colors.push(data[scenarioIndex]['scenario_color']);
            if (data[scenarioIndex]['ts_list'][tsIndex]) {
                scenarioArr.push(data[scenarioIndex]['ts_list'][tsIndex]['ts_name']);
            }
        }
    }
    for (let annualIndex = 0; annualIndex < data[0]['ts_list'][0]['monthly_filters'][monthlyIndex]['annual_filters'].length; annualIndex++) {
        for (let scenarioIndex = 0; scenarioIndex < data.length; scenarioIndex++) {
            for (let tsIndex = 0; tsIndex < data[scenarioIndex]['ts_list'].length; tsIndex++) {
                let annual = data[scenarioIndex]['ts_list'][tsIndex]['monthly_filters'][monthlyIndex]['annual_filters'][annualIndex];
                let annualValues = values[annualIndex + 1];
                if (!annualValues) {
                    annualValues = [];
                    values[annualIndex + 1] = annualValues;
                }
                if (annual['computed_statistics'][statIndex]) {
                    annualValues.push(format(annual['computed_statistics'][statIndex]['statistic_aggregate']));
                } else {
                    annualValues.push(NaN);
                }
            }
        }
    }
    return {
        type: 'table',
        header: {
            values: headers,
            align: "center",
            line: {width: 1, color: 'black'},
            font: {family: PLOTLY_FONT['family'], size: [11, 8]}
        },
        cells: {
            values: values,
            line: {color: "black", width: 1},
            align: ["left", "center"],
            height: 25,
            font: {family: PLOTLY_FONT['family'], color: [colors], size: [11, 14]}
        },
        domain: {x: [0, 1], y: [0, 0.3]}
    };
}

function buildLayouts(datum, yaxis, title) {
    let layoutList = [];

    for (let statIndex = 0; statIndex < datum[0]['ts_list'][0]['monthly_filters'][0]['annual_filters'][0]['computed_statistics'].length; statIndex++) {
        for (let monthlyIndex = 0; monthlyIndex < datum[0]['ts_list'][0]['monthly_filters'].length; monthlyIndex++) {
            let monthPeriod = datum[0]['ts_list'][0]['monthly_filters'][monthlyIndex]['annual_filters'][0]['month_period'];
            let statistic = datum[0]['ts_list'][0]['monthly_filters'][monthlyIndex]['annual_filters'][0]['computed_statistics'][statIndex]['statistic'];
            layoutList.push({
                font: PLOTLY_FONT,
                barmode: 'grouped',
                height: 600,
                yaxis: {
                    title: {
                        text: yaxis,
                        standoff: 50
                    },
                    automargin: true,
                    tickformatstops: FORMATTER,
                    domain: [0.4, 1.0],
                    gridcolor: '#CCCCCC',
                    rangemode: 'tozero'
                },
                xaxis: {
                    gridcolor: '#CCCCCC'
                },
                bargroupgap: 0.05,
                showlegend: false,
                title: {
                    text: title + '<br>' + monthPeriod + '<br>' + statistic,
                    font: {
                        size: 20,
                    }
                },
                margin: {
                    l: 60,
                    r: 40,
                    b: 0,
                    t: 120
                }
            });
        }
    }
    return layoutList;
}

function plot(data) {
    var datum = data['scenario_run_data'];
    var layout = buildLayouts(datum, data['units'], data['gui_link_title']);
    let plotlyAggregateSeries = getPeriodGroupedPlotlySeries(datum);
    let numberOfRows = datum.length;
    for (let i = 0; i < layout.length; i++) {
        layout[i]['height'] = 400 + numberOfRows * 65;
    }
    plotData(layout, plotlyAggregateSeries, data['ts_descriptor']);
}


function plotScenarioGroupedForMonthStat(datum, monthlyIndex, statIndex) {
    let series = [];
    for (let annualIndex = 0; annualIndex < datum[0]['ts_list'][0]['monthly_filters'][monthlyIndex]['annual_filters'].length; annualIndex++) {
        for (let tsIndex = 0; tsIndex < datum[0]['ts_list'].length; tsIndex++) {
            let annualPeriodName = datum[0]['ts_list'][0]['monthly_filters'][monthlyIndex]['annual_filters'][annualIndex]['annual_period'];
            let x = [];
            let y = [];
            let color = [];
            let opacity = [];
            for (let i = 0; i < datum.length; i++) {
                for (let datumIndex = 0; datumIndex < datum.length; datumIndex++) {
                    if (i === datumIndex) {
                        for (let ts = 0; ts < datum[datumIndex]['ts_list'].length; ts++) {
                            if (tsIndex === ts) {
                                let tsList = datum[datumIndex]['ts_list'];
                                let annualFilters = tsList[ts]['monthly_filters'][monthlyIndex]['annual_filters'];
                                let annualData = annualFilters[annualIndex];
                                let statistics = annualData['computed_statistics'];
                                let value = statistics[statIndex];
                                if (tsIndex === 0) {
                                    opacity.push(1);
                                } else {
                                    opacity.push(.8);
                                }
                                color.push(datum[i]['scenario_color']);
                                x.push(datum[datumIndex]['scenario_name']);
                                y.push(value['statistic_aggregate']);
                            }
                        }
                    }
                }
            }
            series.push({
                type: 'bar',
                x: x,
                y: y,
                name: annualPeriodName + '<br>' + datum[0]['ts_list'][tsIndex]['ts_name'],
                marker: {
                    color: color,
                    opacity: opacity,
                    line: {
                        color: color,
                        width: 2
                    }
                }

            });
        }
    }
    return series;
}

function getScenarioGroupedPlotlySeries(datum) {
    let seriesList = [];
    for (let statIndex = 0; statIndex < datum[0]['ts_list'][0]['monthly_filters'][0]['annual_filters'][0]['computed_statistics'].length; statIndex++) {
        for (let monthlyIndex = 0; monthlyIndex < datum[0]['ts_list'][0]['monthly_filters'].length; monthlyIndex++) {
            seriesList.push(plotScenarioGroupedForMonthStat(datum, monthlyIndex, statIndex));
        }
    }
    return seriesList;
}

function plotPeriodGroupedForMonthStat(datum, monthlyIndex, statIndex) {
    let series = [];
    for (let tsIndex = 0; tsIndex < datum[0]['ts_list'].length; tsIndex++) {
        for (let i = 0; i < datum.length; i++) {
            let tsList = datum[i]['ts_list'];
            if (tsList[tsIndex]) {
                let annualFilters = tsList[tsIndex]['monthly_filters'][monthlyIndex]['annual_filters'];
                let x = [];
                let y = [];
                for (let j = 0; j < annualFilters.length; j++) {
                    let annualData = annualFilters[j];
                    let statistics = annualData['computed_statistics'];
                    let value = statistics[statIndex];
                    x.push(annualData['annual_period']);
                    if (value) {
                        y.push(value['statistic_aggregate']);
                    } else {
                        y.push(NaN);
                    }
                }
                series.push({
                    type: 'bar',
                    x: x,
                    y: y,
                    name: tsList[tsIndex]['ts_name'],
                    marker: {
                        color: datum[i]['scenario_color']
                    }
                });
            }
        }
    }
    series.push(buildTable(datum, monthlyIndex, statIndex));
    return series;
}

function getPeriodGroupedPlotlySeries(datum) {
    let seriesList = [];
    for (let statIndex = 0; statIndex < datum[0]['ts_list'][0]['monthly_filters'][0]['annual_filters'][0]['computed_statistics'].length; statIndex++) {
        for (let monthlyIndex = 0; monthlyIndex < datum[0]['ts_list'][0]['monthly_filters'].length; monthlyIndex++) {
            seriesList.push(plotPeriodGroupedForMonthStat(datum, monthlyIndex, statIndex));
        }
    }
    return seriesList;
}

function plotData2(layout, dataList, data) {
    let main = document.getElementById("main");
    let plots = [];
    for (let i = 0; i < dataList.length; i++) {
        let plot = document.createElement("div");
        plots.push(plot);
        plot.id = 'plot' + i;
        main.appendChild(plot);
        Plotly.newPlot(plot.id, dataList[i], layout[i], {
            displaylogo: false,
            modeBarButtons: buildModeBarButtonsWithScatter(i, plot.id, data),
            scrollZoom: true,
            responsive: true
        });
        $("#" + plot.id).mousedown((ev) => {
            if (ev.which === 3) {
                openContextMenu('#' + plot.id, ev, plotlyCopyToClipboard, plotlyExportFunction(plot));
            }
        });
        plot.on('plotly_relayout',
            function (eventdata) {
                if (eventdata['xaxis.range[0]']) {
                    for (let i = 0; i < plots.length; i++) {
                        if (plots[i].id != plot.id) {
                            let curentRange = plots[i]['_fullLayout']['xaxis']['range'];
                            if (curentRange[0] !== eventdata['xaxis.range[0]']
                                && curentRange[1] !== eventdata['xaxis.range[1]']) {
                                Plotly.relayout(plots[i].id, {
                                    'xaxis.range': [eventdata['xaxis.range[0]'], eventdata['xaxis.range[1]']]
                                });
                            }
                        }
                    }
                } else if (eventdata['xaxis.autorange']) {
                    for (let i = 0; i < plots.length; i++) {
                        if (plots[i].id != plot.id) {
                            let curentRange = plots[i]['_fullLayout']['xaxis']['range'];
                            if (curentRange !== plot['_fullLayout']['xaxis']['range']) {
                                Plotly.relayout(plots[i].id, {
                                    'xaxis.range': plot['_fullLayout']['xaxis']['range']
                                });
                            }
                        }
                    }
                }
            });
    }
}

function plotlyCopyToClipboard(element) {
    let plot = $(element)[0];
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

function buildModeBarButtonsWithScatter(index, graphDiv, data) {
    let scatterButton = {
        name: 'Group By Period',
        // icon: '../img/scatter-plot.svg',
        icon: {
            'width': 1500,
            'height': 1500,
            'path': 'm 1206.5226,551.534 c 0,61.97227 -53.8873,112.21073 -120.3606,112.21073 -66.4733,0 -120.36063,-50.23846 -120.36063,-112.21073 0,-61.97228 53.88733,-112.21073 120.36063,-112.21073 66.4733,0 120.3606,50.23845 120.3606,112.21073 z m 788.8696,-625.174074 c 0,61.972271 -53.8873,112.210727 -120.3606,112.210727 -66.4733,0 -120.3606,-50.238456 -120.3606,-112.210727 0,-61.972296 53.8873,-112.210746 120.3606,-112.210746 66.4733,0 120.3606,50.23845 120.3606,112.210746 z M 1656.0623,351.15768 c 0,61.97228 -53.8873,112.21073 -120.3607,112.21073 -66.4733,0 -120.3606,-50.23845 -120.3606,-112.21073 0,-61.97227 53.8873,-112.21072 120.3606,-112.21072 66.4734,0 120.3607,50.23845 120.3607,112.21072 z M 1357.3359,-33.564819 c 0,61.972271 -53.8873,112.210727 -120.3606,112.210727 -66.4733,0 -120.3606,-50.238456 -120.3606,-112.210727 0,-61.972281 53.8873,-112.210731 120.3606,-112.210731 66.4733,0 120.3606,50.23845 120.3606,112.210731 z M 1099.2131,1120.6027 c 0,61.9723 -53.8873,112.2107 -120.3606,112.2107 -66.47334,0 -120.36063,-50.2384 -120.36063,-112.2107 0,-61.9722 53.88729,-112.2107 120.36063,-112.2107 66.4733,0 120.3606,50.2385 120.3606,112.2107 z M 785.98547,885.49454 c 0,61.97228 -53.88728,112.21072 -120.36062,112.21072 -66.47334,0 -120.36062,-50.23844 -120.36062,-112.21072 0,-61.97228 53.88728,-112.21073 120.36062,-112.21073 66.47334,0 120.36062,50.23845 120.36062,112.21073 z M 491.60949,1139.3045 c 0,61.9723 -53.88729,112.2107 -120.36062,112.2107 -66.47336,0 -120.36064,-50.2384 -120.36064,-112.2107 0,-61.9723 53.88728,-112.2108 120.36064,-112.2108 66.47333,0 120.36062,50.2385 120.36062,112.2108 z M 219.25741,1451.2417 H 2013.1081 q 18.0849,0 30.5721,11.1064 12.4872,11.1064 12.4872,27.766 0,15.8663 -12.4872,27.3694 -12.4872,11.503 -30.5721,11.503 H 177.05931 q -18.0849,0 -30.57213,-11.503 Q 134,1505.9804 134,1490.1141 V -190.12754 q 0,-15.86632 12.48718,-27.36939 12.48723,-11.50306 30.57213,-11.50306 17.2237,0 29.71093,11.50306 12.48717,11.50307 12.48717,27.36939',
            // 'transform': 'matrix(1 0 0 -1 0 850)'
        },
        click: () => toggleScenarioGrouping()
    };
    let groupByScenarioButton = {
        name: 'Group By Scenario',
        icon: {
            'width': 1500,
            'height': 1000,
            'path': 'M233 -251h2083q21 0 35.5 -14t14.5 -35q0 -20 -14.5 -34.5t-35.5 -14.5h-2132q-21 0 -35.5 14.5t-14.5 34.5v2118q0 20 14.5 34.5t35.5 14.5q20 0 34.5 -14.5t14.5 -34.5v-1642l235 241q21 21 48 21q25 0 45 -17l211 -176l435 1316q15 46 59 46q45 0 65 -35l310 -560\n' +
                'l179 517q8 21 26 34l391 273q18 12 40 12q28 0 48 -19.5t20 -48.5q0 -37 -29 -57l-373 -261l-224 -640q-16 -47 -59 -47q-46 0 -66 36l-306 557l-416 -1263q-7 -20 -24.5 -34t-41.5 -14q-14 0 -24 4.5t-20 12.5l-240 201l-289 -294v-232z',
            'transform': 'matrix(1 0 0 -1 0 850)'
        },
        click: () => toggleScenarioGrouping()
    };
    var groupByScenario = false;

    function toggleScenarioGrouping() {
        if (!groupByScenario) {
            groupByScenario = true;
            let modeBar = buildModeBarButtons(graphDiv);
            var layout = buildLayouts(data['scenario_run_data'], data['units'], data['gui_link_title'])[index];
            let plotlyMonthlySeries = getScenarioGroupedPlotlySeries(data['scenario_run_data'])[index];
            modeBar[1].push(groupByScenarioButton);
            Plotly.react(graphDiv, plotlyMonthlySeries, layout, {
                displaylogo: false,
                modeBarButtons: modeBar,
                scrollZoom: true,
                responsive: true
            });
        } else {
            groupByScenario = false;
            let modeBar = buildModeBarButtons(graphDiv);
            var layout = buildLayouts(data['scenario_run_data'], data['units'], data['gui_link_title'])[index];
            let plotlyMonthlySeries = getPeriodGroupedPlotlySeries(data['scenario_run_data'])[index];
            modeBar[1].push(scatterButton);
            Plotly.react(graphDiv, plotlyMonthlySeries, layout, {
                displaylogo: false,
                modeBarButtons: modeBar,
                scrollZoom: true,
                responsive: true
            });
        }
    }

    let modeBar = buildModeBarButtons(graphDiv);
    modeBar[1].push(scatterButton);
    return modeBar;
}