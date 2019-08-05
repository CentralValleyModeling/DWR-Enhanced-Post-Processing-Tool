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

function getCategoryKeys(datum) {
    let categoryKeys = [];
    for (var i = 0; i < datum.length; i++) {
        let statisticSeries = datum[i]['statistically_computed_time_series'];
        for (var j = 0; j < statisticSeries.length; j++) {
            let month = statisticSeries[j][0];
            if (!categoryKeys.includes(month)) {
                categoryKeys.push(month);
            }
        }
    }
    return categoryKeys;
}

function getSeries(datum) {
    var series = new Array(datum.length);
    for (var i = 0; i < datum.length; i++) {
        series[i] = {
            name: datum[i]['scenario_name'],
            data: datum[i]['statistically_computed_time_series']
        };
    }
    return series;
}

function getUnits(data) {
    var units;
    if (data['taf']) {
        units = 'TAF';
    } else {
        units = 'CFS';
    }
    return units;
}

function plot(data) {

    let datum = data['scenario_run_data'];
    let categoryKeys = getCategoryKeys(datum);
    var series = getSeries(datum);
    var units = getUnits(data);

    var chart = Highcharts.chart('container', {
        chart: {
            type: 'line',
            zoomKey: 'shift',
            zoomType: 'xy',
            panning: true
        },
        title: {
            text: data['gui_link_title'] + ' (' + data['statistics'] + ')'
        },
        xAxis: {
            type: 'datetime',
            categories: categoryKeys,
            title: {
                text: 'Month',
                align: 'middle'
            }
        },
        yAxis: {
            title: {
                text: 'Volume (' + units  + ')',
                align: 'middle'
            },
            labels: {
                overflow: 'justify'
            },
            allowDecimals: false
        },
        tooltip: {
            valueSuffix: ' ' + units
        },
        legend: {
            enabled: true
        },
        series: series
    });
    postInit(chart);
}
