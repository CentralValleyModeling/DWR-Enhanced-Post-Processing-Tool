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
function getMonthlyUnits(data) {
    var units;
    if (data['taf']) {
        units = 'TAF';
    } else {
        units = 'CFS';
    }
    return units;
}

function getMonthlySeries(datum) {
    var series = new Array(datum.length);
    for (var i = 0; i < datum.length; i++) {
        series[i] = {
            name: datum[i]['scenario_name'],
            data: datum[i]['full_time_series']
        };
    }
    return series;
}

function getPlotlyMonthlySeries(datum) {
    var series = [];
    for (var i = 0; i < datum.length; i++) {
        let timeSeries = datum[i]['full_time_series'];
        let x = [];
        let y = [];
        for(var j =0; j < timeSeries.length; j++){
            x.push(new Date(timeSeries[j][0]));
            y.push(timeSeries[j][1]);
        }
        series.push({
            name:datum[i]['scenario_name'],
            x: x,
            y: y,
            line: {color: PLOTLY_COLORS[i]}
        });
    }
    return series;
}

function plotMonthly(data) {
    var datum = data['scenario_run_data'];
    var series = getMonthlySeries(datum);
    var units = getMonthlyUnits(data);
    var layout = {
        font: PLOTLY_FONT,
        yaxis: {
            title: {
                text: 'Volume (' + units + ')',
            }
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
            text: data['gui_link_title'],
            font: {
                size: 20,
            }
        }
    };
    Plotly.newPlot('container_monthly_tester', getPlotlyMonthlySeries(datum), layout, {
        displaylogo: false,
        modeBarButtonsToRemove: ['toImage', 'sendDataToCloud', 'editInChartStudio', 'lasso2d', 'select2d', 'resetScale2d'],
        scrollZoom: true,
        responsive: true
    });
}