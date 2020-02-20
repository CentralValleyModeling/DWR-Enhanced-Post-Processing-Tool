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

function getHeaders(data) {
    let firstIndexData = data[0]['primary_data']['statistically_computed_time_series_wyt'][0];
    let headers = [''];
    for (let j = 0; j < firstIndexData.length; j++) {
        let waterYearPeriod = firstIndexData[j]['water_year_period'];
        headers.push('<b>' + getAcronym(waterYearPeriod) + '</b>');
    }
    return headers;
}

function getAcronym(text) {
    let matches = text.match(/\b(\w)/g);
    return matches.join('');
}

function buildAnnualPeriodCells(data) {
    let retval = ['Long Term'];
    for (let i = 1; i < data['scenario_run_data'].length; i++) {
        retval.push('');
    }

    if(data['scenario_run_data']){
        let scenarioData = data['scenario_run_data'][0];
        let scenarioPrimaryData = scenarioData['primary_data'];
        if(scenarioPrimaryData['statistically_computed_time_series_wyt']) {
            let wytStats = scenarioPrimaryData['statistically_computed_time_series_wyt'][0];
            for (let j = 0; j < wytStats.length; j++) {
                retval.push(wytStats[j]['water_year_period']);
                for (let i = 1; i < data['scenario_run_data'].length; i++) {
                    retval.push('');
                }
            }
        }
    }
    return retval;
}

function buildScenarioCells(data) {
    let retval = [];
    for (let i = 0; i < data['scenario_run_data'].length; i++) {
        retval.push(data['scenario_run_data'][i]['scenario_name']);
    }

    if(data['scenario_run_data']){
        let scenarioData = data['scenario_run_data'][0];
        let scenarioPrimaryData = scenarioData['primary_data'];
        if(scenarioPrimaryData['statistically_computed_time_series_wyt']) {
            let wytStats = scenarioPrimaryData['statistically_computed_time_series_wyt'][0];
            for (let j = 0; j < wytStats.length; j++) {
                for (let i = 0; i < data['scenario_run_data'].length; i++) {
                    retval.push(data['scenario_run_data'][i]['scenario_name']);
                }
            }
        }
    }
    return retval;
}

function buildScenarioColors(data) {
    let retval = [];
    for (let i = 0; i < data['scenario_run_data'].length; i++) {
        retval.push(data['scenario_run_data'][i]['scenario_color']);
    }

    if(data['scenario_run_data']){
        let scenarioData = data['scenario_run_data'][0];
        let scenarioPrimaryData = scenarioData['primary_data'];
        if(scenarioPrimaryData['statistically_computed_time_series_wyt']) {
            let wytStats = scenarioPrimaryData['statistically_computed_time_series_wyt'][0];
            for (let j = 0; j < wytStats.length; j++) {
                for (let i = 0; i < data['scenario_run_data'].length; i++) {
                    retval.push(data['scenario_run_data'][i]['scenario_color']);
                }
            }
        }
    }
    return retval;
}

function buildScenarioValues(data) {
    let retval = [];
    for (let i = 0; i < data['scenario_run_data'].length; i++) {
        let scenarioPrimaryData = data['scenario_run_data'][i]['primary_data'];
        let longTermValue = scenarioPrimaryData['statistically_computed_time_series_yearly'][0];
        retval.push(longTermValue);
    }

    if(data['scenario_run_data']){
        let scenarioData = data['scenario_run_data'][0];
        let scenarioPrimaryData = scenarioData['primary_data'];
        if(scenarioPrimaryData['statistically_computed_time_series_wyt']) {
            let wytStats = scenarioPrimaryData['statistically_computed_time_series_wyt'][0];
            for (let j = 0; j < wytStats.length; j++) {
                for (let i = 0; i < data['scenario_run_data'].length; i++) {
                    retval.push(data['scenario_run_data'][i]['primary_data']['statistically_computed_time_series_wyt'][0][j]['water_year_period_values']);
                }
            }
        }
    }
    return retval;
}

function buildScenarioValuesDiff(data, diffIndex) {
    let retval = [];
    for (let i = 0; i < data['scenario_run_data'].length; i++) {
        let currentValue = data['scenario_run_data'][i]['primary_data']['statistically_computed_time_series_yearly'][0];
        let diffValue = data['scenario_run_data'][diffIndex]['primary_data']['statistically_computed_time_series_yearly'][0];
        if(diffIndex < i){
            retval.push(currentValue - diffValue);
        }
        else{
            retval.push('');
        }
    }

    if(data['scenario_run_data']){
        let scenarioData = data['scenario_run_data'][0];
        let scenarioPrimaryData = scenarioData['primary_data'];
        if(scenarioPrimaryData['statistically_computed_time_series_wyt']) {
            let wytStats = scenarioPrimaryData['statistically_computed_time_series_wyt'][0];
            for (let j = 0; j < wytStats.length; j++) {
                for (let i = 0; i < data['scenario_run_data'].length; i++) {
                    let currentValue = data['scenario_run_data'][i]['primary_data']['statistically_computed_time_series_wyt'][0][j]['water_year_period_values'];
                    let diffValue = data['scenario_run_data'][diffIndex]['primary_data']['statistically_computed_time_series_wyt'][0][j]['water_year_period_values'];
                    if(diffIndex < i){
                        retval.push(currentValue - diffValue);
                    }
                    else{
                        retval.push('');
                    }
                }
            }
        }
    }
    return retval;
}

function buildScenarioValuesDiffFormat(data, diffIndex) {
    let retval = [];
    for (let i = 0; i < data['scenario_run_data'].length; i++) {
        if(diffIndex < i){
            retval.push(FORMATTER);
        }
        else{
            retval.push('');
        }
    }

    if(data['scenario_run_data']){
        let scenarioData = data['scenario_run_data'][0];
        let scenarioPrimaryData = scenarioData['primary_data'];
        if(scenarioPrimaryData['statistically_computed_time_series_wyt']) {
            let wytStats = scenarioPrimaryData['statistically_computed_time_series_wyt'][0];
            for (let j = 0; j < wytStats.length; j++) {
                for (let i = 0; i < data['scenario_run_data'].length; i++) {
                    if(diffIndex < i){
                        retval.push(FORMATTER);
                    }
                    else{
                        retval.push('');
                    }
                }
            }
        }
    }
    return retval;
}

function plot(data) {
    FORMATTER = getD3Formatter(data['scenario_run_data'][0]['primary_data']['full_time_series'][0]);
    let header = [['<b>Period</b>'], ['<b>Scenario</b>'], ['<b>' + data['scenario_run_data'][0]['primary_data']['units'][0]] + '</b>'];
    let annualPeriods = buildAnnualPeriodCells(data);
    let scenarios = buildScenarioCells(data);
    let colors = buildScenarioColors(data);
    let format = [];
    let periodNameFormat = [];
    let scenarioFormat = [];
    let periodValues = buildScenarioValues(data);
    let periodValuesFormat = [];
    let values = [annualPeriods, scenarios, periodValues];
    format.push(periodNameFormat);
    format.push(scenarioFormat);
    format.push(periodValuesFormat);
    for (let i = 0; i < data['scenario_run_data'].length; i++) {
        let scenarioData = data['scenario_run_data'][i];
        periodNameFormat.push('');
        scenarioFormat.push('');
        periodValuesFormat.push(FORMATTER);
        if (i !== data['scenario_run_data'].length - 1) {
            header.push('<b>Diff From<br>' + scenarioData['scenario_name'] + '</b>');
            values.push(buildScenarioValuesDiff(data, i));
        }
        format.push(buildScenarioValuesDiffFormat(data, i));

    }

    let tableData = {
        type: 'table',
        header: {
            values: header,
            align: "center",
            font: {family: PLOTLY_FONT['family']}
        },
        cells: {
            format: format,
            values: values,
            align: ['left', 'left', 'center'],
            font: {family: PLOTLY_FONT['family'], color: ['', colors]}
        }
    };

    let layout = {
        font: PLOTLY_FONT,
        title: {
            text: '<b>' + data['gui_link_title'] + '<br>' + data['month_period_title'] + '<br>' + data['statistics'] + '</b>'
        }
    };
    let elementid = "tester";
    Plotly.newPlot(elementid, [tableData], layout, {
        displaylogo: false,
        displayModeBar: false,
        responsive: false
    });
    $("#" + elementid).mousedown((ev) => {
        if (ev.which === 3) {
            openContextMenu("#" + elementid, ev, () => plotlyCopyToClipboard(elementid), plotlyExportFunction(document.getElementById(elementid)));
        }
    });
}

function plotlyCopyToClipboard(element) {
    let plot = document.getElementById(element);
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
