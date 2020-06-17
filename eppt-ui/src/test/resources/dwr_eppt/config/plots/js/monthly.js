/*
 * Enhanced Post Processing Tool (EPPT) Copyright (c) 2020.
 *
 *  EPPT is copyrighted by the State of California, Department of Water Resources. It is licensed
 *  under the GNU General Public License, version 2. This means it can be
 *  copied, distributed, and modified freely, but you may not restrict others
 *  in their ability to copy, distribute, and modify it. See the license below
 *  for more details.
 *
 *  GNU General Public License
 */

function getPlotlyStatisticsSeries(datum) {
    let seriesList = [];
    for (let i = 0; i < datum.length; i++) {
        let tsList = datum[i]['ts_list'];
        for (let j = 0; j < tsList.length; j++) {
            let axis = 0;
            let monthlyFilters = tsList[j]['monthly_filters'];
            for (let k = 0; k < monthlyFilters.length; k++) {
                let annualFilters = monthlyFilters[k]['annual_filters'];
                for (let m = 0; m < annualFilters.length; m++) {
                    for (let statIndex = 0; statIndex < annualFilters[m]['computed_statistics'].length; statIndex++) {
                        let timeSeries = annualFilters[m]['computed_statistics'][statIndex]['statistically_computed_time_series_monthly'];
                        let x = [];
                        let y = [];
                        for (let periodMonth = 0; periodMonth < annualFilters[m]['period_months'].length; periodMonth++) {
                            for (let ts = 0; ts < timeSeries.length; ts++) {
                                if (annualFilters[m]['period_months'][periodMonth] === timeSeries[ts][0]) {
                                    x.push(timeSeries[ts][0]);
                                    y.push(timeSeries[ts][1]);
                                }
                            }
                        }
                        let series = seriesList[axis];
                        if (!series) {
                            series = [];
                            seriesList.push(series);
                        }
                        series.push({
                            name: tsList[j]['ts_name'] + '     ',
                            x: x,
                            y: y,
                            line: {
                                color: datum[i]['scenario_color'],
                                dash: PLOTLY_LINE_DASH_STYLES[j % PLOTLY_LINE_DASH_STYLES.length],
                                shape: 'hvh'
                            },
                            mode: 'lines+markers',
                            marker: {
                                size: 6,
                                color: darken(datum[i]['scenario_color'], 20)
                            }
                        });
                        axis++;
                    }
                }
            }
        }
    }
    return seriesList;
}

function buildLayouts(datum, yaxis, title) {
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
                        let annualFilter = annualFilters[m];
                        for (let statIndex = 0; statIndex < annualFilters[m]['computed_statistics'].length; statIndex++) {
                            let plotTitle = title;
                            if (annualFilter['annual_period']) {
                                if (annualFilter['annual_period'].indexOf('<br>') === annualFilter['annual_period'].length - 4) {
                                    plotTitle += '<br>' + annualFilter['annual_period'].replace("<br>", "");
                                } else {
                                    plotTitle += '<br>' + annualFilter['annual_period'].replace("<br>", " - ");
                                }
                            }
                            if (annualFilter['month_period']) {
                                plotTitle += '<br>' + annualFilter['month_period'] + '<br>' + annualFilter['computed_statistics'][statIndex]['statistic']
                            }
                            layoutList[axis] = {
                                font: PLOTLY_FONT,
                                yaxis: {
                                    title: {
                                        text: yaxis,
                                        standoff: 50
                                    },
                                    automargin: true,
                                    tickformatstops: FORMATTER,
                                    gridcolor: '#CCCCCC',
                                    rangemode: 'tozero'
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
                                    text: plotTitle,
                                    font: {
                                        size: 20,
                                    }
                                },
                                margin: {
                                    l: 60,
                                    r: 40,
                                    b: 20,
                                    t: 160
                                }
                            };
                            axis++;
                        }
                    }
                }
            }
        }
    }
    return layoutList;
}

function plot(data) {
    var datum = data['scenario_run_data'];
    var layout = buildLayouts(datum, data['units'], data['gui_link_title']);
    let plotlyAggregateSeries = getPlotlyStatisticsSeries(datum);
    plotData(layout, plotlyAggregateSeries, data['ts_descriptor']);
}

function plotlyCopyToClipboard(element) {
    let plot = $(element)[0];
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
        text += xarr[j];
        for (var k = 0; k < data1.length; k++) {
            let yarr = data1[k]['y'];
            text += '\t' + yarr[j];
        }
        text += '\n';
    }
    copyTextToClipboard(text);
}
