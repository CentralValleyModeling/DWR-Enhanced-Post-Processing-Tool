/*
 * Enhanced Post Processing Tool (EPPT) Copyright (c) 2026.
 *
 * EPPT is copyrighted by the State of California, Department of Water Resources. It is licensed
 * under the GNU General Public License, version 2. This means it can be
 * copied, distributed, and modified freely, but you may not restrict others
 * in their ability to copy, distribute, and modify it. See the license below
 * for more details.
 *
 * GNU General Public License
 */

function monthlyBoxChartKeys(datum) {
    let keys = [];
    let monthlyFilters = datum[0]['ts_list'][0]['monthly_filters'];
    for (let k = 0; k < monthlyFilters.length; k++) {
        for (let j = 0; j < monthlyFilters[k]['annual_filters'].length; j++) {
            keys.push({monthlyIndex: k, annualIndex: j});
        }
    }
    return keys;
}

function monthlyBoxValues(annualData) {
    let x = [];
    let y = [];
    let periodMonths = annualData['period_months'];
    let timeSeries = annualData['discrete_ts'];
    for (const element of timeSeries) {
        let date = new Date(element[0]);
        date.setMonth(date.getMonth() - 1);
        for (const periodMonthName of periodMonths) {
            let periodMonth = new Date(Date.parse('02 ' + periodMonthName + ' 1970 05:00:00 GMT')).getMonth();
            if (periodMonth === date.getMonth()) {
                x.push(periodMonthName);
                y.push(element[1]);
            }
        }
    }
    return {x: x, y: y};
}

function getPlotlyMonthlySeries(datum, keys) {
    let seriesList = [];
    for (const key of keys) {
        let series = [];
        for (let tsIndex = 0; tsIndex < datum[0]['ts_list'].length; tsIndex++) {
            for (const scenario of datum) {
                let tsList = scenario['ts_list'];
                if (tsList[tsIndex]) {
                    let annualData = tsList[tsIndex]['monthly_filters'][key.monthlyIndex]['annual_filters'][key.annualIndex];
                    let values = monthlyBoxValues(annualData);
                    series.push({
                        y: values.y,
                        x: values.x,
                        type: 'box',
                        name: tsList[tsIndex]['ts_name'] + '     ',
                        marker: {
                            color: scenario['scenario_color']
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

function buildMonthlyLayouts(datum, keys, yaxis, title) {
    let layoutList = [];
    for (const key of keys) {
        let annualData = datum[0]['ts_list'][0]['monthly_filters'][key.monthlyIndex]['annual_filters'][key.annualIndex];
        let plotTitle = title;
        if (annualData['annual_period']) {
            plotTitle += '<br>' + annualData['annual_period'].replace(/<br>$/, '').replace('<br>', ' - ');
        }
        plotTitle += '<br>' + annualData['month_period'];
        layoutList.push({
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
                gridcolor: '#CCCCCC',
                categoryorder: 'array',
                categoryarray: annualData['period_months']
            },
            showlegend: true,
            legend: {
                orientation: 'h',
                xanchor: 'center',
                x: 0.5,
                y: -0.2,
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
            boxmode: 'group',
            margin: {
                l: 60,
                r: 40,
                b: 90,
                t: 140
            }
        });
    }
    return layoutList;
}

function plot(data) {
    plotMonthly(data);
}

function plotMonthly(data) {
    let datum = data['scenario_run_data'];
    let keys = monthlyBoxChartKeys(datum);
    let layout = buildMonthlyLayouts(datum, keys, data['units'], data['gui_link_title']);
    let series = getPlotlyMonthlySeries(datum, keys);
    plotData(layout, series, data['ts_descriptor']);
}

function plotlyCopyToClipboard(plot) {
    let layout = plot.layout;
    let calcdata = plot.calcdata;
    let text = layout['title']['text'] + '\n' + 'Scenario\tMonth\t' + layout['yaxis']['title']['text'] + '\n\t';
    text += '\n';
    for (const calcdatum of calcdata) {
        text += calcdatum[0]['trace']['name'] + '\t';
        text += '\tmin\tmax\tmedian\tmean\tq1\tq3\tsd';
        text += '\n';
        for (const boxdatum of calcdatum) {
            text += '\t';
            if (calcdatum[0]['trace']) {
                text += calcdatum[0]['trace']['x'][boxdatum.pts[0]['i']];
            }
            text += '\t' + boxdatum.min + '\t' + boxdatum.max + '\t' + boxdatum.med + '\t';
            text += boxdatum.mean + '\t' + boxdatum.q1 + '\t' + boxdatum.q3 + '\t' + boxdatum.sd;
            text += '\n';
        }
        text += '\n';
    }
    copyTextToClipboard(text);
}
