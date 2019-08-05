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

function plotMonthly(data) {
    var datum = data['scenario_run_data'];
    var series = getMonthlySeries(datum);
    var units = getMonthlyUnits(data);
    var chart = Highcharts.chart('container_monthly', {
        chart: {
            type: 'line',
            zoomKey: 'shift',
            zoomType: 'xy',
            panning: 'true'
        },
        title: {
            text: data['gui_link_title']
        },
        xAxis: {
            type: 'datetime',
            title: {
                text: 'Date',
                align: 'middle'
            }
        },
        yAxis: {
            title: {
                text: 'Volume (' + units + ')',
                align: 'middle'
            },
            labels: {
                overflow: 'justify'
            }
        },
        legend: {
            enabled: true
        },
        series: series
    });
    postInit(chart);
}