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


function getPlotlyMonthlySeries(datum, firstRecord, lastRecord, instantaneous) {
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
                    let hoverInfo = [];
                    let markerSize = [];
                    let startDate = new Date(firstRecord);
                    startDate.setMonth(startDate.getMonth() - 1);
                    let endDate = new Date(lastRecord);

                    var startingDataIndex = 0;
                    while (startDate <= endDate) {
                        let date = new Date(startDate);
                        date.setDate(date.getDate() - 1);
                        x.push(date);
                        if (timeSeries[startingDataIndex]) {
                            let dataDate = new Date(timeSeries[startingDataIndex][0]);
                            if (dataDate.getFullYear() === startDate.getFullYear() && dataDate.getMonth() === startDate.getMonth()) {
                                y.push(timeSeries[startingDataIndex][1]);
                                hoverInfo.push('all');
                                markerSize.push(4);
                                startingDataIndex++;
                            }
                        }
                        if (!instantaneous) {
                            if (!y[y.length - 2] && y[y.length - 1]) {
                                y[y.length - 2] = y[y.length - 1];
                            }
                            if (x.length > y.length) {
                                y.push(null);
                                hoverInfo.push('skip');
                                markerSize.push(0);
                            }
                        }
                        startDate.setMonth(startDate.getMonth() + 1);
                    }
                    let series = seriesList[axis];
                    if (!series) {
                        series = [];
                        seriesList.push(series);
                    }
                    let shape = 'vh';
                    if(instantaneous){
                        shape = 'linear';
                    }
                    series.push({
                        name: tsList[j]['ts_name'],
                        x: x,
                        y: y,
                        line: {
                            color: datum[i]['scenario_color'],
                            dash: PLOTLY_LINE_DASH_STYLES[j % PLOTLY_LINE_DASH_STYLES.length],
                            shape: shape
                        },
                        mode: 'lines+markers',
                        marker: {
                            size: markerSize,
                            color: darken(datum[i]['scenario_color'], 20)
                        },
                        hoverinfo: hoverInfo
                    });
                    axis++;
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
                        var updatemenus = [
                            {
                                buttons: [
                                    {
                                        args: ['xaxis.range', [new Date('31Dec1909 2359'), new Date('01Jan1920 0000')]],
                                        label: '1910\'s',
                                        method: 'relayout'
                                    },
                                    {
                                        args: ['xaxis.range', [new Date('31Dec1919 2359'), new Date('01Jan1930 0000')]],
                                        label: '1920\'s',
                                        method: 'relayout'
                                    }, {
                                        args: ['xaxis.range', [new Date('31Dec1929 2359'), new Date('01Jan1940 0000')]],
                                        label: '1930\'s',
                                        method: 'relayout'
                                    }, {
                                        args: ['xaxis.range', [new Date('31Dec1939 2359'), new Date('01Jan1950 0000')]],
                                        label: '1940\'s',
                                        method: 'relayout'
                                    }, {
                                        args: ['xaxis.range', [new Date('31Dec1949 2359'), new Date('01Jan1960 0000')]],
                                        label: '1950\'s',
                                        method: 'relayout'
                                    }, {
                                        args: ['xaxis.range', [new Date('31Dec1959 2359'), new Date('01Jan1970 0000')]],
                                        label: '1960\'s',
                                        method: 'relayout'
                                    }, {
                                        args: ['xaxis.range', [new Date('31Dec1969 2359'), new Date('01Jan1980 0000')]],
                                        label: '1970\'s',
                                        method: 'relayout'
                                    }, {
                                        args: ['xaxis.range', [new Date('31Dec1979 2359'), new Date('01Jan1990 0000')]],
                                        label: '1980\'s',
                                        method: 'relayout'
                                    }, {
                                        args: ['xaxis.range', [new Date('31Dec1989 2359'), new Date('01Jan2000 0000')]],
                                        label: '1990\'s',
                                        method: 'relayout'
                                    }, {
                                        args: ['xaxis.range', [new Date('31Dec1999 2359'), new Date('01Jan2010 0000')]],
                                        label: '2000\'s',
                                        method: 'relayout'
                                    }, {
                                        args: ['xaxis.range', [new Date('31Dec2009 2359'), new Date('01Jan2020 0000')]],
                                        label: '2010\'s',
                                        method: 'relayout'
                                    }
                                ],
                                direction: 'left',
                                pad: {'r': 10, 't': 10},
                                showactive: true,
                                type: 'buttons',
                                x: 0.1,
                                xanchor: 'left',
                                y: 1.1,
                                yanchor: 'top'
                            }
                        ];
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
                            // updatemenus: updatemenus,
                            xaxis: {
                                tickformat: '%b-%Y',
                                hoverformat: '%b-%Y',
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
                            height: 650,
                            title: {
                                text: plotTitle,
                                font: {
                                    size: 20,
                                }
                            },
                            margin: {
                                l: 60,
                                r: 40,
                                b: 100,
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

function plot(data) {
    FORMATTER = getD3Formatter(data['scenario_run_data'][0]['ts_list'][0]['monthly_filters'][0]['annual_filters'][0]['discrete_ts']);
    var datum = data['scenario_run_data'];
    var layout = buildLayouts(datum, data['units'], data['gui_link_title']);
    let plotlyMonthlySeries = getPlotlyMonthlySeries(datum, data['first_record'], data['last_record'], data['is_instantaneous']);
    plotData(layout, plotlyMonthlySeries);
}

function plotlyCopyToClipboard(element) {
    let plot = $(element)[0];
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
        let date = new Date(xarr[j]);
        date.setDate(date.getDate() - 1);
        text += date.getMonth() + '/' + date.getFullYear();
        for (var k = 0; k < data1.length; k++) {
            let yarr = data1[k]['y'];
            text += '\t' + yarr[j];
        }
        text += '\n';
    }
    copyTextToClipboard(text);
}




















