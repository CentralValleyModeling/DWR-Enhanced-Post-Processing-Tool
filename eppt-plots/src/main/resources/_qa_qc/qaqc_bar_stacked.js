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
        "categories": ["EXP1 VS EXP2", "SWP VS CVP"],
        "scenario_run_data": [
            {
                name: "EXP2",
                data: [220.3, 0],
            }, {
                name: "EXP1",
                data: [3621.6, 0],
            }, {
                name: "CVP",
                data: [0, 92.3],
            }, {
                name: "SWP",
                data: [0, 3713.9],
            }],
        "gui_link_title": "Trinity Reservoir Storage",
        "taf": false,
        "month_period_title": "January - December",
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

function plot(data) {
    let units = getUnits(data);
    let series = data['scenario_run_data'];
    let categories = data['categories'];
    let container = document.createElement('div');
    let chart = Highcharts.chart('container', {
        chart: {
            type: 'column',
            renderTo: container
        },
        title: {
            text: data['gui_link_title']
        },
        xAxis: {
            title: {
                text: 'Period',
                align: 'middle'
            },
            categories: categories
        },
        yAxis: {
            title: {
                text: 'Volume (' + units + ')',
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
        tooltip: {
            valueSuffix: ' ' + units
        },
        legend: {
            enabled: true,
            layout: 'vertical',
            verticalAlign: 'middle',
            align: 'right'
        },
        series: series
    });
    return chart.getSVG();
}
