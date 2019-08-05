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

var chart = Highcharts.chart('container', {
    chart: {
        type: 'column',
        zoomType: 'xy'
    },
    title: {
        text: 'October-September Delta Outflow (Total) Averages'
    },
    xAxis: {
        type: 'category',
        gridLineWidth: 1
    },
    yAxis: {
        min: 0,
        title: {
            text: 'Volume (TAF)',
            align: 'middle'
        },
        labels: {
            overflow: 'justify'
        },
        scrollbar: {
            enabled: true
        }
    },
    tooltip: {
        valueSuffix: ' TAF'
    },
    plotOptions: {
        column: {
            dataLabels: {
                enabled: false
            }
        }
    },
    legend: {
        enabled: true
    },
    series: [{
        name: 'DCR 2017',
        data: [
            ['W', 28470],
            ['AN', 17355],
            ['BN', 9074],
            ['D', 7981],
            ['C', 5283],
            ['D & C', 6969]
        ]
    }, {
        name: 'COS Q0',
        data: [
            ['W', 28762],
            ['AN', 17537],
            ['BN', 9100],
            ['D', 7983],
            ['C', 5311],
            ['D & C', 6981]
        ]
    }, {
        name: 'VA Base 5/3',
        data: [
            ['W', 28708],
            ['AN', 17445],
            ['BN', 8943],
            ['D', 7813],
            ['C', 5152],
            ['D & C', 6815]
        ]
    }, {
        name: 'VA Model (B4.01)',
        data: [
            ['W', 28671],
            ['AN', 17492],
            ['BN', 9057],
            ['D', 7813],
            ['C', 5152],
            ['D & C', 6815]
        ]
    }]
});
postInit(chart);