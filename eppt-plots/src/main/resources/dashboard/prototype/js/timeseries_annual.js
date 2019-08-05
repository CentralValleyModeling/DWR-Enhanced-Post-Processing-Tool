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


function getAnnualUnits(data) {
    var units;
    if (data['taf']) {
        units = 'TAF';
    } else {
        units = 'CFS';
    }
    return units;
}

function getAnnualSeries(datum) {
    var series = new Array(datum.length);
    for (var i = 0; i < datum.length; i++) {
        series[i] = {
            name: datum[i]['scenario_name'],
            data: datum[i]['period_filtered_time_series']
        };
    }
    return series;
}

function plotAnnual(data) {
    var datum = data['scenario_run_data'];
    var series = getAnnualSeries(datum);
    var units = getAnnualUnits(data);
    var chart = Highcharts.chart('container_annual', {
        chart: {
            type: 'line',
            panKey: 'shift',
            zoomKey: 'ctrl',
            zoomType: 'xy',
            panning: true
        },
        title: {
            text: 'Annual Timeseries ' + data['month_period_title'] + ' ' + data['gui_link_title'],
        },
        xAxis: {
            type: 'datetime',
            title: {
                text: 'Date',
                align: 'middle'
            },
            dateTimeLabelFormats: {
                second: '%Y-%m-%d<br/>%H:%M:%S',
                minute: '%Y-%m-%d<br/>%H:%M',
                hour: '%Y-%m-%d<br/>%H:%M',
                day: '%Y<br/>%m-%d',
                week: '%Y<br/>%m-%d',
                month: '%Y-%m',
                year: '%Y'
            }
        },
        yAxis: {
            title: {
                text: 'Volume (' + units + ')',
                align: 'middle'
            },
            labels: {
                overflow: 'justify'
            },
        },
        series: series
    });
    postInit(chart);
}