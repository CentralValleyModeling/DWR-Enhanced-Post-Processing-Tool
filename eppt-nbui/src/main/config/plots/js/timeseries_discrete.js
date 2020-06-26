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
            let annualFilters = tsList[j]['monthly_filters'][0]['annual_filters'];
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
                    if (!instantaneous) {
                        date.setMonth(date.getMonth() - 1);
                    }
                    if (timeSeries[startingDataIndex]) {
                        let dataDate = new Date(timeSeries[startingDataIndex][0]);
                        if(instantaneous){
                            dataDate.setMonth(dataDate.getMonth() - 1);
                        }
                        if (dataDate.getFullYear() === startDate.getFullYear() && dataDate.getMonth() === startDate.getMonth()) {
                            x.push(date);
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
                            x.push(date);
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
                if (instantaneous) {
                    shape = 'linear';
                }
                series.push({
                    name: tsList[j]['ts_name'] + '     ',
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
    return seriesList;
}

function buildDiscreteLayouts(datum, yaxis, title) {
    let layoutList = [];
    for (let i = 0; i < datum.length; i++) {
        let tsList = datum[i]['ts_list'];
        for (let j = 0; j < tsList.length; j++) {
            let axis = 0;
            let annualFilters = tsList[j]['monthly_filters'][0]['annual_filters'];
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
    return layoutList;
}

function plot(data) {
    plotDiscrete(data);
}

function plotDiscrete(data) {
    var datum = data['scenario_run_data'];
    var layout = buildDiscreteLayouts(datum, data['units'], data['gui_link_title']);
    let plotlyMonthlySeries = getPlotlyMonthlySeries(datum, data['first_record'], data['last_record'], data['is_instantaneous']);
    plotData(layout, plotlyMonthlySeries, data['ts_descriptor']);
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
    let xyVals = [];
    var foundDate = false;
    for (let k = 0; k < data1.length; k++) {
        let xarr = data1[k]['x'];
        let yarr = data1[k]['y'];
        let hoverinfo = data1[k]['hoverinfo'];
        for (let j = 0; j < xarr.length; j++) {
            let x;
            if (hoverinfo[j] !== 'skip') {

                if (Object.prototype.toString.call(xarr[j]) === '[object Date]') {
                    foundDate = true;
                    let date = new Date(xarr[j]);
                    x = date.setMonth(date.getMonth());
                    x = date;
                } else {
                    x = xarr[j];
                }
                if (!xyVals[x]) {
                    xyVals[x] = [];
                }
                xyVals[x].push(yarr[j]);
            }
        }
    }
    let keys = Object.keys(xyVals);
    if (foundDate) {
        keys.sort((a, b) => new Date(a).getTime() - new Date(b).getTime());
    } else {
        keys.sort();
    }
    for (let i = 0; i < keys.length; i++) {
        let key = keys[i];
        if (foundDate) {
            let date = new Date(key);
            text += (date.getMonth() + 1) + '/' + date.getFullYear();
        } else {
            text += key;
        }
        for (let j = 0; j < xyVals[key].length; j++) {
            text += '\t';
            if (xyVals[key][j]) {
                text += xyVals[key][j];
            }
        }
        text += '\n';
    }
    copyTextToClipboard(text);
}
