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

    var trace1 = {
        x: [1, 2, 3, 4, 5],
        y: [1, 6, 3, 6, 1],
        mode: 'markers+text',
        type: 'scatter',
        name: 'Team A',
        text: ['A-1', 'A-2', 'A-3', 'A-4', 'A-5'],
        textfont: {
            family: 'Raleway, sans-serif'
        },
        marker: {size: 100}
    };

    var trace2 = {
        x: [1.5, 2.5, 3.5, 4.5, 5.5],
        y: [4, 1, 7, 1, 4],
        mode: 'markers+text',
        type: 'scatter',
        name: 'Team B',
        text: ['B-a', 'B-b', 'B-c', 'B-d', 'B-e'],
        textfont: {
            family: 'Times New Roman'
        },
        marker: {size: 100}
    };

    var data = [trace1, trace2];

    var layout = {
        legend: {
            y: 0.5,
            yref: 'paper',
            font: {
                family: 'Arial, sans-serif',
                size: 20,
                color: 'grey',
            }
        },
        title: 'Data Labels on the Plot'
    };
    Plotly.newPlot('tester', data, layout, {
        displaylogo: false,
        modeBarButtonsToRemove: ['toImage', 'sendDataToCloud', 'editInChartStudio', 'lasso2d', 'select2d', 'toggleSpikelines', 'resetScale2d']
    });


    return plot({
        "line_data": {
            name: 'Deliver-carryover curve (Input)',
            data: [[0, 1], [500, 10], [1600, 12], [2050, 12], [9500, 80], [15500, 100], [16500, 150]]
        },
        "bubble_data": {
            name: 'Percent Allocation',
            data: [
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
            ]
        },
        "title": "CVP SOD Delivery vs. Carryover",
        "y_axis": "Delivery Target (CFS)",
        "x_axis": "Carryover"
    });
}

function getSeries(data) {
    let series = [{
        type: 'bubble',
        name: data['bubble_data']['name'],
        data: data['bubble_data']['data'],
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
            name: data['line_data']['name'],
            data: data['line_data']['data'],
            label: {
                enabled: true,
                connectorAllowed: true
            }
        });
    }
    return series;
}

function plot(data) {
    let series = getSeries(data);
    let container = document.createElement('div');
    let chart = Highcharts.chart('container', {
        chart: {
            renderTo: container
        },
        title: {
            text: data['title']
        },
        xAxis: {
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
            enabled: false
        },
        credits: {
            enabled: false
        },
        exporting: {enabled: false},
        series: series
    });
    return chart.getSVG();
}
