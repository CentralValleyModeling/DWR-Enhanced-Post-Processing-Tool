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
        "line_data": [[0, 1], [500, 10], [1600, 12], [2050, 12], [9500, 80], [15500, 100], [16500, 150]],
        "scatter_data": [
            {x: 1000, y: 21, z: 1, name: 1},
            {x: 2000, y: 60, z: 1, name: 2},
            {x: 3000, y: 10, z: 1, name: 3},
            {x: 4000, y: 50, z: 1, name: 4},
            {x: 5000, y: 120, z: 1, name: 5},
            {x: 6000, y: 65, z: 1, name: 6},
            {x: 7000, y: 23, z: 1, name: 7},
            {x: 8000, y: 5, z: 1, name: 8},
            {x: 9000, y: 2, z: 1, name: 9},
            {x: 10000, y: 130, z: 1, name: 10},
            {x: 11000, y: 101, z: 1, name: 11},
            {x: 12000, y: 120, z: 1, name: 12},
            {x: 13000, y: 115, z: 1, name: 13},
            {x: 14000, y: 80, z: 1, name: 14},
            {x: 15000, y: 82, z: 1, name: 15},
            {x: 16000, y: 12, z: 1, name: 16}
        ],
        "gui_link_title": "CVP SOD Delivery vs. Carryover",
        "taf": false,
        "statistics": "Averages"
    });
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

function getSeries(data) {
    let series = [{
        type: 'bubble',
        name: 'Percent Allocation',
        data: data['scatter_data'],
        dataLabels: {
            enabled: true,
            format: '{point.name}'
        },
        label: {
            enabled: true,
            connectorAllowed: true
        }
    }];
    if (data['line_data']) {
        series.push({
            type: 'line',
            name: 'Deliver-carryover curve (Input)',
            data: data['line_data'],
            label: {
                enabled: true,
                connectorAllowed: true
            }
        });
    }
    return series;
}

function plot(data) {
    let units = getUnits(data);
    let series = getSeries(data);
    let container = document.createElement('div');
    let chart = Highcharts.chart('container', {
        chart: {
            renderTo: container
        },
        title: {
            text: data['gui_link_title']
        },
        xAxis: {
            title: {
                text: 'Carryover',
                align: 'middle'
            }
        },
        yAxis: {
            title: {
                text: 'Delivery Target (' + units + ')',
                align: 'middle'
            }
        },
        legend: {
            enabled: false
        },
        series: series
    });
    return chart.getSVG();
}
