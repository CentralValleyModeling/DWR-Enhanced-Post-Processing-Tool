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
function test() {
    return plot({
        "scenario_run_data": [{
            "data": [[1520236800000, 6000], [1522911600000, 5000], [1528182000000, 500], [1533452400000, 50], [1536130800000, 5], [1538722800000, 1]],
            "name": "Alt"
        }, {
            "data": [[1515139200000, 9999], [1517817600000, 9999], [1520236800000, 999], [1522911600000, 99], [1525503600000, 50], [1528182000000, 9], [1530774000000, 2]],
            "name": "Base"
        }], "y_axis": "Y Axis Test", "x_axis": "X AXIS Test", "title": "Test Stack"
    });
}

function getSeries(datum) {
    let series = new Array(datum.length);
    for (var i = 0; i < datum.length; i++) {
        var timeSeries = datum[i]['data'];
        series[i] = {
            name: datum[i]['name'],
            data: timeSeries,
        };
    }
    return series;
}

function plot(data) {
    let datum = data['scenario_run_data'];
    let series = getSeries(datum);
    let container = document.createElement('div');
    let chart = Highcharts.chart('container', {
        chart: {
            type: 'line',
            renderTo: container
        },
        title: {
            text: data['title']
        },
        xAxis: {
            type: 'datetime',
            title: {
                text: data['x_axis'],
                align: 'middle'
            }
        },
        yAxis: {
            title: {
                text: data['y_axis'],
                align: 'middle'
            }
        },
        legend: {
            enabled: true
        },
        credits: {
            enabled: false
        },
        exporting: {enabled: false},
        series: series
    });
    return chart.getSVG();
}
