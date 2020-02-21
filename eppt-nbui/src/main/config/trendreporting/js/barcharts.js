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
var FORMATTER = '';

function getHeaders(data) {
    let firstIndexData = data[0]['primary_data']['statistically_computed_time_series_wyt'][0];
    let headers = ['', '<b>Long Term</b>'];
    for (let j = 0; j < firstIndexData.length; j++) {
        let waterYearPeriod = firstIndexData[j]['water_year_period'];
        headers.push('<b>' + getAcronym(waterYearPeriod) + '</b>');
    }
    return headers;
}

function buildTable(data) {
    let headers = getHeaders(data);
    let values = [];
    let colors = [];
    for (let i = 0; i < data.length; i++) {
        let scenarioArr = values[0];
        if (!scenarioArr) {
            scenarioArr = [];
            values[0] = scenarioArr;
        }
        colors.push(data[i]['scenario_color']);
        scenarioArr.push(data[i]['scenario_name']);
        let longTerm = values[1];
        if (!longTerm) {
            longTerm = [];
            values[1] = longTerm;
        }
        longTerm[i] = data[i]['primary_data']['statistically_computed_time_series_yearly'][0];
        let timeSeries = data[i]['primary_data']['statistically_computed_time_series_wyt'][0];
        for (let k = 0; k < timeSeries.length; k++) {
            let val = timeSeries[k]['water_year_period_values'];
            let periodValues = values[2 + k];
            if (!periodValues) {
                periodValues = [];
                values[2 + k] = periodValues;
            }
            periodValues.push(val);
        }
    }
    return {
        type: 'table',
        header: {
            values: headers,
            align: "center",
            line: {width: 1, color: 'black'},
            font: PLOTLY_FONT
        },
        cells: {
            format: ['', FORMATTER],
            values: values,
            line: {color: "black", width: 1},
            align: ["left", "center"],
            font: {family: PLOTLY_FONT['family'], color: [colors]}
        },
        domain: {x: [0, 1], y: [0, 0.3]}
    };
}

function getAcronym(text) {
    return text;
}

function getPeriodGroupedPlotlySeries(allData) {
    let series = [];
    let datum = allData['scenario_run_data'];
    for (let i = 0; i < datum.length; i++) {
        series.push(buildBarsForScenario(datum[i]['primary_data'], datum[i]['scenario_color'], datum[i]['scenario_name']));
    }
    series.push(buildTable(datum));
    return series;
}

function getScenarioGroupedPlotlySeries(allData) {
    let series = [];
    let datum = allData['scenario_run_data'];

    function buildLongTermGroupedBars() {
        let xarr = [];
        let yarr = [];
        let color = [];
        for (let i = 0; i < datum.length; i++) {
            color.push(datum[i]['scenario_color']);
            xarr.push(datum[i]['scenario_name']);
            yarr.push(datum[i]['primary_data']['statistically_computed_time_series_yearly'][0])
        }
        let longTermBars = {
            type: 'bar',
            name: 'Long Term',
            text: 'Long Term',
            hoverinfo:'y+name',
            textposition: 'outside',
            x: xarr,
            y: yarr,
            marker: {
                color: color
            }
        };
        return longTermBars;
    }
    series.push(buildLongTermGroupedBars());
    let periodCount = datum[0]['primary_data']['statistically_computed_time_series_wyt'][0].length;
    for(let i = 0; i < periodCount; i++){
        let name = datum[0]['primary_data']['statistically_computed_time_series_wyt'][0][i]['water_year_period'];
        let xarr = [];
        let yarr = [];
        let color = [];
        for (let j = 0; j < datum.length; j++) {
            color.push(datum[j]['scenario_color']);
            xarr.push(datum[j]['scenario_name']);
            yarr.push(datum[j]['primary_data']['statistically_computed_time_series_wyt'][0][i]['water_year_period_values'])
        }
        let bars = {
            type: 'bar',
            name: name,
            text: name,
            hoverinfo:'y+name',
            textposition: 'outside',
            x: xarr,
            y: yarr,
            marker: {
                color: color
            }
        };
        series.push(bars);
    }
    series.push(buildTable(datum));
    return series;
}

function buildBarsForScenario(data, color, name) {
    let timeSeries = data['statistically_computed_time_series_wyt'][0];
    let xarr = ['Long Term'];
    for (let j = 0; j < timeSeries.length; j++) {
        let waterYearPeriod = timeSeries[j]['water_year_period'];
        xarr.push(getAcronym(waterYearPeriod));
    }
    let yarr = [data['statistically_computed_time_series_yearly'][0]];
    for (let k = 0; k < timeSeries.length; k++) {
        let val = timeSeries[k]['water_year_period_values'];
        yarr.push(val);
    }
    return {
        type: 'bar',
        name: name,
        x: xarr,
        y: yarr,
        marker: {
            color: color
        }
    };
}

function buildLayout(data) {
    let layout = {
        font: PLOTLY_FONT,
        barmode: 'group',
        yaxis: {
            tickformat: FORMATTER,
            title: {
                text: data['units'],
            },
            domain: [0.35, 1.0],
            gridcolor: '#CCCCCC'
        },
        xaxis: {
            gridcolor: '#CCCCCC'
        },
        showlegend: false,
        title: {
            text: data['gui_link_title'] + '<br>' + data['month_period_title'] + '<br>' + data['statistics'],
            font: {
                size: 20,
            }
        }
    };
    return layout;
}

function plot(data) {
    FORMATTER = getD3Formatter(data['scenario_run_data'][0]['primary_data']['full_time_series'][0]);
    let layout = buildLayout(data);
    let modeBar = buildModeBarButtonsWithScatter('tester', data);
    Plotly.newPlot('tester', getPeriodGroupedPlotlySeries(data), layout, {
        displaylogo: false,
        modeBarButtons: modeBar,
        scrollZoom: true,
        responsive: false,
        grid: {rows: 2, columns: 1, pattern: 'independent'}
    });
    $("#tester").mousedown((ev) => {
        if (ev.which === 3) {
            openContextMenu('#tester', ev, () => plotlyCopyToClipboard('tester'), plotlyExportFunction(document.getElementById('tester')));
        }
    });
}

function plotlyCopyToClipboard(element) {
    let plot = document.getElementById(element);
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

function buildModeBarButtonsWithScatter(graphDiv, data) {
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
            var layout = buildLayout(data);
            let plotlyMonthlySeries = getScenarioGroupedPlotlySeries(data);
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
            var layout = buildLayout(data);
            let plotlyMonthlySeries = getPeriodGroupedPlotlySeries(data);
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