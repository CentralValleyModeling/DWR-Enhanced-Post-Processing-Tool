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

function getPlotlyAggregateSeries(datum) {
    let seriesList = [];
    for (let monthlyIndex = 0; monthlyIndex < datum[0]['ts_list'][0]['monthly_filters'].length; monthlyIndex++) {
        let series = [];
        for (let tsIndex = 0; tsIndex < datum[0]['ts_list'].length; tsIndex++) {
            for (let i = 0; i < datum.length; i++) {
                let tsList = datum[i]['ts_list'];
                if(tsList[tsIndex]) {
                    let annualFilters = tsList[tsIndex]['monthly_filters'][monthlyIndex]['annual_filters'];
                    let x = [];
                    let y = [];
                    for (let j = 0; j < annualFilters.length; j++) {
                        let annualData = annualFilters[j];
                        let timeSeries = annualData['aggregate_ts'];
                        for (var ts = 0; ts < timeSeries.length; ts++) {
                            x.push(annualData['annual_period']);
                            y.push(timeSeries[ts][1]);
                        }
                    }
                    series.push({
                        y: y,
                        x: x,
                        type: 'box',
                        name: tsList[tsIndex]['ts_name'],
                        marker: {
                            color: datum[i]['scenario_color']
                        },
                        line: {
                            width: 3 - tsIndex
                        },
                        boxmean: true,
                        boxpoints: false
                    });
                }
            }
        }
        seriesList.push(series);
    }
    return seriesList;
}

function buildAggregateLayouts(datum, yaxis, title) {
    let layoutList = [];
    for (let i = 0; i < 1; i++) {
        let tsList = datum[i]['ts_list'];
        for (let j = 0; j < 1; j++) {
            let axis = 0;
            let monthlyFilters = tsList[j]['monthly_filters'];
            for (let k = 0; k < monthlyFilters.length; k++) {
                let annualFilters = monthlyFilters[k]['annual_filters'];
                for (let m = 0; m < 1; m++) {
                    let series = layoutList[axis];
                    if (!series) {
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
                                gridcolor: '#CCCCCC'
                            },
                            showlegend: true,
                            legend: {
                                orientation: 'h',
                                xanchor: 'center',
                                y: -0.2,
                                x: 0.5,
                                font: {
                                    size: 10,
                                }
                            },
                            title: {
                                text: title + '<br>' + annualFilters[m]['month_period'],
                                font: {
                                    size: 20,
                                }
                            },
                            boxmode: 'group',
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

function plot(data){
    plotAggregate(data);
}

function plotAggregate(data) {
    FORMATTER = getD3Formatter(data['scenario_run_data'][0]['ts_list'][0]['monthly_filters'][0]['annual_filters'][0]['discrete_ts']);
    var datum = data['scenario_run_data'];
    var layout = buildAggregateLayouts(datum, data['units'], data['gui_link_title']);
    let plotlyAggregateSeries = getPlotlyAggregateSeries(datum);
    plotData(layout, plotlyAggregateSeries);
}

function plotlyCopyToClipboard(element) {
    let plot = $(element)[0];
    let layout = plot.layout;
    let data1 = plot.data;
    let calcdata = plot.calcdata;
    var text = layout['title']['text'] + '\n' + 'Scenario\tGroup\t' + layout['yaxis']['title']['text'] + '\n\t';
    text += '\n';
    for (var j = 0; j < calcdata.length; j++) {
        let calcdatum = calcdata[j];
        text += calcdatum[0]['trace']['name'] + '\t';
        text += '\tmin\tmax\tmedian\tmean\tq1\tq3\tsd';
        text += '\n';
        for (var k = 0; k < calcdatum.length; k++) {
            let boxdatum = calcdatum[k];
            text += '\t';
            if (calcdatum[0]['trace']) {
                let boxName = calcdatum[0]['trace']['x'][boxdatum.pts[0]['i']];
                text += boxName;
            }
            text += '\t' + boxdatum.min + '\t' + boxdatum.max + '\t' + boxdatum.med + '\t';
            text += boxdatum.mean + '\t' + boxdatum.q1 + '\t' + boxdatum.q3 + '\t' + boxdatum.sd;
            text += '\n';
        }
        text += '\n';
    }
    copyTextToClipboard(text);
}
