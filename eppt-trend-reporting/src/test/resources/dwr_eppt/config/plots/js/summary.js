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

var FORMATTER = '';

function buildAnnualPeriodCells(data) {
    let retval = [];
    for (let annualIndex = 0; annualIndex < data[0]['ts_list'][0]['monthly_filters'][0]['annual_filters'].length; annualIndex++) {
        let periodName = data[0]['ts_list'][0]['monthly_filters'][0]['annual_filters'][annualIndex]['annual_period'];
        let name = periodName;
        if (periodName.indexOf('<br>') === periodName.length - 4) {
            name = periodName.replace("<br>", "");
        } else {
            name = periodName.replace("<br>", " - ");
        }
        retval.push(name);
        for (let tsIndex = 0; tsIndex < data[0]['ts_list'].length; tsIndex++) {
            for (let scenarioIndex = 1; scenarioIndex < data.length; scenarioIndex++) {
                retval.push('');
            }
            if (tsIndex > 0) {
                retval.push('');
            }
        }
    }
    return retval;
}

function buildScenarioCells(data) {
    let retval = [];
    for (let annualIndex = 0; annualIndex < data[0]['ts_list'][0]['monthly_filters'][0]['annual_filters'].length; annualIndex++) {
        for (let tsIndex = 0; tsIndex < data[0]['ts_list'].length; tsIndex++) {
            for (let scenarioIndex = 0; scenarioIndex < data.length; scenarioIndex++) {
                retval.push(data[scenarioIndex]['ts_list'][tsIndex]['ts_name']);
            }
        }
    }
    return retval;
}

function buildScenarioColors(data) {
    let retval = [];
    for (let annualIndex = 0; annualIndex < data[0]['ts_list'][0]['monthly_filters'][0]['annual_filters'].length; annualIndex++) {
        for (let tsIndex = 0; tsIndex < data[0]['ts_list'].length; tsIndex++) {
            for (let scenarioIndex = 0; scenarioIndex < data.length; scenarioIndex++) {
                retval.push(data[scenarioIndex]['scenario_color']);
            }
        }
    }
    return retval;
}

function buildScenarioValues(data, monthlyIndex, statIndex) {
    let retval = [];
    for (let annualIndex = 0; annualIndex < data[0]['ts_list'][0]['monthly_filters'][monthlyIndex]['annual_filters'].length; annualIndex++) {
        for (let tsIndex = 0; tsIndex < data[0]['ts_list'].length; tsIndex++) {
            for (let scenarioIndex = 0; scenarioIndex < data.length; scenarioIndex++) {
                let stat = data[scenarioIndex]['ts_list'][tsIndex]['monthly_filters'][monthlyIndex]['annual_filters'][annualIndex]['computed_statistics'];
                if (stat[statIndex]) {
                    retval.push(stat[statIndex]['statistic_aggregate']);
                } else {
                    retval.push(NaN);
                }
            }
        }
    }
    return retval;
}

function buildScenarioValuesDiff(data, diffIndex, monthlyIndex, statIndex) {
    let retval = [];
    for (let annualIndex = 0; annualIndex < data[0]['ts_list'][0]['monthly_filters'][monthlyIndex]['annual_filters'].length; annualIndex++) {
        for (let tsIndex = 0; tsIndex < data[0]['ts_list'].length; tsIndex++) {
            for (let scenarioIndex = 0; scenarioIndex < data.length; scenarioIndex++) {
                if (diffIndex < scenarioIndex) {
                    let stat = data[scenarioIndex]['ts_list'][tsIndex]['monthly_filters'][monthlyIndex]['annual_filters'][annualIndex]['computed_statistics'][statIndex];
                    if (stat) {
                        let currentValue = stat['statistic_aggregate'];
                        let diffValue = data[diffIndex]['ts_list'][tsIndex]['monthly_filters'][monthlyIndex]['annual_filters'][annualIndex]['computed_statistics'][statIndex]['statistic_aggregate'];
                        retval.push(currentValue - diffValue);
                    }else{
                        retval.push(NaN);
                    }
                } else {
                    retval.push('');
                }
            }
        }
    }
    return retval;
}

function buildScenarioValuesDiffFormat(data, diffIndex, monthlyIndex, statIndex) {
    let retval = [];
    for (let annualIndex = 0; annualIndex < data[0]['ts_list'][0]['monthly_filters'][monthlyIndex]['annual_filters'].length; annualIndex++) {
        for (let tsIndex = 0; tsIndex < data[0]['ts_list'].length; tsIndex++) {
            for (let scenarioIndex = 0; scenarioIndex < data.length; scenarioIndex++) {
                if (diffIndex < scenarioIndex) {
                    retval.push(FORMATTER);
                } else {
                    retval.push('');
                }
            }
        }
    }
    return retval;
}

function plot(data) {
    FORMATTER = getD3Formatter(data['scenario_run_data'][0]['ts_list'][0]['monthly_filters'][0]['annual_filters'][0]['discrete_ts']);
    var layout = buildLayouts(data['scenario_run_data'], data['units'], data['gui_link_title']);
    let plotlyAggregateSeries = getPlotlyData(data['scenario_run_data'], data['units']);
    let numberOfRows = plotlyAggregateSeries[0][0]['cells']['values'][0].length;
    var rowHeight = 35;
    if(data['scenario_run_data'][0]['ts_list'][0]['ts_name'].includes('<br>')){
        rowHeight *= 2;
    }

    for (let i = 0; i < layout.length; i++) {
        layout[i]['height'] = 165 + numberOfRows * rowHeight;
    }
    plotData(layout, plotlyAggregateSeries);
}

function getPlotlyData(datum, units) {
    let seriesList = [];
    for (let statIndex = 0; statIndex < datum[0]['ts_list'][0]['monthly_filters'][0]['annual_filters'][0]['computed_statistics'].length; statIndex++) {
        for (let monthlyIndex = 0; monthlyIndex < datum[0]['ts_list'][0]['monthly_filters'].length; monthlyIndex++) {
            seriesList.push([plotPeriodGroupedForMonthStat(datum, monthlyIndex, statIndex, units)]);
        }
    }
    return seriesList;
}

function plotPeriodGroupedForMonthStat(data, monthlyIndex, statIndex, units) {
    FORMATTER = getD3Formatter(data[0]['ts_list'][0]['monthly_filters'][0]['annual_filters'][0]['discrete_ts']);
    let header = [['<b>Period</b>'], ['<b>Scenario</b>'], ['<b>' + units] + '</b>'];
    let annualPeriods = buildAnnualPeriodCells(data);
    let scenarios = buildScenarioCells(data);
    let colors = buildScenarioColors(data);
    let format = [];
    let periodNameFormat = [];
    let scenarioFormat = [];
    let periodValues = buildScenarioValues(data, monthlyIndex, statIndex);
    let periodValuesFormat = [];
    let values = [annualPeriods, scenarios, periodValues];
    format.push(periodNameFormat);
    format.push(scenarioFormat);
    format.push(periodValuesFormat);
    for (let i = 0; i < data.length; i++) {
        let scenarioData = data[i];
        periodNameFormat.push('');
        scenarioFormat.push('');
        periodValuesFormat.push(FORMATTER);
        if (i !== data.length - 1) {
            header.push('<b>Diff From<br>' + scenarioData['scenario_name'] + '</b>');
            values.push(buildScenarioValuesDiff(data, i, monthlyIndex, statIndex));
        }
        format.push(buildScenarioValuesDiffFormat(data, i, monthlyIndex, statIndex));

    }

    return {
        type: 'table',
        header: {
            values: header,
            align: "center",
            font: {family: PLOTLY_FONT['family'], size: 18}
        },
        cells: {
            format: format,
            values: values,
            align: ['left', 'left', 'center'],
            height: 25,
            font: {family: PLOTLY_FONT['family'], color: ['', colors], size: [14, 14, 16]}
        }
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

function plotlyCopyToClipboard(element) {
    let plot = $(element)[0];
    let layout = plot.layout;
    let data1 = plot.data;
    var text = layout['title']['text'];
    text += '\n';
    let datum = data1[data1.length - 1];
    for (let i = 0; i < datum['header']['values'].length; i++) {
        let name = datum['header']['values'][i] + "";
        text += name + '\t';
    }
    text += '\n';
    let values = datum['cells']['values'];
    let rows = values[0].length;
    for (let j = 0; j < rows; j++) {
        for (let k = 0; k < values.length; k++) {
            let val = values[k][j] + '\t';
            text += val;
        }
        text += '\n';
    }
    copyTextToClipboard(text.replace(/<br>/g, ' ').replace(/<b>/g, '').replace(/<\/b>/g, ''));
}
