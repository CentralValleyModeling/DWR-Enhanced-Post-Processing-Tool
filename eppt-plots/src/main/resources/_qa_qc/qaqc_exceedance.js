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
            "data": [["Jan", 1396.3445271643088], ["Feb", 1458.825340095423], ["Mar", 1569.013079128303], ["Apr", 1692.711896350799], ["May", 1841.6618916308717], ["Jun", 1835.029623785408], ["Jul", 1797.7170578847126], ["Aug", 1660.2221874591778], ["Sep", 1521.8104733953267], ["Oct", 1400.7485390792303], ["Nov", 1330.3366163520498], ["Dec", 1345.1052102615438]],
            "scenario_name": "Base",
        },{
            "data": [["Jan", 1396.3445271643088], ["Feb", 1458.825340095423], ["Mar", 1569.013079128303], ["Apr", 1692.711896350799], ["May", 1841.6618916308717], ["Jun", 1835.029623785408], ["Jul", 1797.7170578847126], ["Aug", 1660.2221874591778], ["Sep", 1521.8104733953267], ["Oct", 1400.7485390792303], ["Nov", 1330.3366163520498], ["Dec", 1345.1052102615438]],
            "scenario_name": "Alt",
        }],
        "title": "Trinity Reservoir Storage",
        "taf": false,
        "statistics": "Averages"
    });
}

function getSeries(datum) {
    let series = new Array(datum.length);
    for (var i = 0; i < datum.length; i++) {
        var timeSeries = datum[i]['data'];
        series[i] = {
            name: datum[i]['scenario_name'],
            data: timeSeries,
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
    let units = getUnits(data);
    let datum = data['scenario_run_data'];
    let series = getSeries(datum);
    let container = document.createElement('div');
    let chart = Highcharts.chart('container', {
        chart: {
            renderTo: container
        },
        title: {
            text: data['title']
        },
        xAxis: {
            min: 0,
            max: 100,
            reversed: 'true',
            title: {
                text: 'Probability of Exceedence',
                align: 'middle'
            },
            labels: {
                overflow: 'justify',
                formatter: function () {
                    return this.value + "%";
                }
            }
        },
        yAxis: {
            type: 'logarithmic',
            minRange: 0.1,
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
    return chart.getSVG();
}
