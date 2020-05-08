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

function getPlotlyDiscreteSeries(datum) {
    let seriesList = [];
    for (let i = 0; i < datum.length; i++) {
        let tsList = datum[i]['ts_list'];
        for (let j = 0; j < tsList.length; j++) {
            let axis = 0;
            let monthlyFilters = tsList[j]['monthly_filters'];
            for (let k = 0; k < monthlyFilters.length; k++) {
                let annualFilters = monthlyFilters[k]['annual_filters'];
                for (let m = 0; m < annualFilters.length; m++) {
                    let timeSeries = annualFilters[m]['discrete_ts'];
                    let x = [];
                    let y = [];
                    let dataOnly = [];
                    for (let index = 0; index < timeSeries.length; index++) {
                        dataOnly.push(timeSeries[index][1]);
                    }
                    dataOnly.sort((a, b) => a - b);
                    for (let n = 0; n < dataOnly.length; n++) {
                        let exceedance = 1 - ((n + 0.5) / dataOnly.length);
                        x.push(exceedance);
                        y.push(dataOnly[n]);
                    }
                    let series = seriesList[axis];
                    if (!series) {
                        series = [];
                        seriesList.push(series);
                    }
                    series.push({
                        name: tsList[j]['ts_name'],
                        x: x,
                        y: y,
                        line: {
                            color: datum[i]['scenario_color'],
                            dash: PLOTLY_LINE_DASH_STYLES[j % PLOTLY_LINE_DASH_STYLES.length]
                        }
                    });
                    axis++;
                }
            }
        }
    }
    return seriesList;
}

function plot(data){
    plotDiscrete(data);
}

function plotDiscrete(data) {
    FORMATTER = getD3Formatter(data['scenario_run_data'][0]['ts_list'][0]['monthly_filters'][0]['annual_filters'][0]['discrete_ts']);
    var datum = data['scenario_run_data'];
    var layout = buildDiscreteLayouts(datum, data['units'], data['gui_link_title']);
    let plotlyDiscreteSeries = getPlotlyDiscreteSeries(datum);
    plotData(layout, plotlyDiscreteSeries);
}

function buildDiscreteLayouts(datum, yaxis, title) {
    let layoutList = [];
    for (let i = 0; i < datum.length; i++) {
        let tsList = datum[i]['ts_list'];
        for (let j = 0; j < tsList.length; j++) {
            let axis = 0;
            let monthlyFilters = tsList[j]['monthly_filters'];
            for (let k = 0; k < monthlyFilters.length; k++) {
                let annualFilters = monthlyFilters[k]['annual_filters'];
                for (let m = 0; m < annualFilters.length; m++) {
                    let series = layoutList[axis];
                    if (!series) {
                        let plotTitle = title;
                        if (annualFilters[m]['annual_period']) {
                            if (annualFilters[m]['annual_period'].indexOf('<br>') === annualFilters[m]['annual_period'].length - 4) {
                                plotTitle += '<br>' + annualFilters[m]['annual_period'].replace("<br>", "");
                            } else {
                                plotTitle += '<br>' + annualFilters[m]['annual_period'].replace("<br>", " - ");
                            }
                        }
                        if (annualFilters[m]['month_period']) {
                            plotTitle += '<br>' + annualFilters[m]['month_period'];
                        }
                        layoutList[axis] = {
                            font: PLOTLY_FONT,
                            yaxis: {
                                title: {
                                    text: yaxis,
                                },
                                tickformat: FORMATTER,
                                gridcolor: '#CCCCCC',
                                rangemode: 'tozero'
                            },
                            xaxis: {
                                gridcolor: '#CCCCCC',
                                tickformat: ',.0%',
                                range: [1, 0],
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
                                text: plotTitle,
                                font: {
                                    size: 20,
                                }
                            },
                            margin: {
                                l: 60,
                                r: 40,
                                b: 90,
                                t: 120
                            }
                        };
                    }
                    axis++;
                }
            }
        }
    }
    return layoutList;
}

function plotlyCopyToClipboard(element) {
    let plot = $(element)[0];
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
