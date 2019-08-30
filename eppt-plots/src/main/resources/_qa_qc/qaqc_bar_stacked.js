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

    TESTER = document.getElementById('tester');
    Plotly.plot( TESTER, [{
        x: [1, 2, 3, 4, 5],
        y: [1, 2, 4, 8, 16] }], {
        margin: { t: 0 } } );

    plot({
        "scenario_run_data": [
            {"data": [25], "name": "CVP"},
            {"data": [40], "name": "SVP"},
            {"data": [null, 40], "name": "LVP"},
            {"data": [null, 90], "name": "TVP"}
        ],
        "y_axis": "Y Axis Test",
        "x_axis": "X AXIS Test",
        "categories": ["First stack", "Second stack"],
        "title": "Test Stack"
    });
}

function plot(data) {
    let series = data['scenario_run_data'];
    let categories = data['categories'];
    let container = document.createElement('div');
    let chart = Highcharts.chart('container', {
        chart: {
            type: 'column',
            renderTo: container
        },
        title: {
            text: data['title']
        },
        xAxis: {
            title: {
                text: data['x_axis'],
                align: 'middle'
            },
            categories: categories
        },
        yAxis: {
            title: {
                text: data['y_axis'],
                align: 'middle'
            }
        },
        plotOptions: {
            column: {
                stacking: 'normal',
                dataLabels: {
                    enabled: true,
                    formatter: function () {
                        return Highcharts.numberFormat(this.y, 2);
                    }
                }
            }
        },
        legend: {
            enabled: true,
            layout: 'vertical',
            verticalAlign: 'middle',
            align: 'right'
        },
        credits: {
            enabled: false
        },
        exporting: {enabled: false},
        series: series
    });
    // return chart.getSVG();
}
