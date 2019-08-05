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


function plot(data) {
    var datum = data['scenario_run_data'];
    var seriesData = new Array(datum.length);
    for (var i = 0; i < datum.length; i++) {
        let timeSeries = datum[i]['statistically_computed_time_series'];
        let average = 0;
        for (var j = 0; j < timeSeries.length; j++) {
            average += timeSeries[j][1];
        }
        average /= timeSeries.length;
        seriesData[i] = {
            name: datum[i]['scenario_name'],
            y: average
        };
    }
    var units;
    if (data['taf']) {
        units = 'TAF';
    } else {
        units = 'CFS';
    }
    var chart = buildChart(data, units, seriesData);
    postInit(chart);
    buildTable(units, data, seriesData);
}


function buildChart(data, units, seriesData) {
    return Highcharts.chart('container', {
        chart: {
            type: 'column',
            zoomType: 'xy',
            zoomKey: 'shift'
        },
        title: {
            text: data['month_period_title'] + ' ' + data['gui_link_title'] + ' ' + data['statistics']
        },
        xAxis: {
            type: 'category',
            labels: {
                rotation: 55,
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
        tooltip: {
            valueSuffix: ' ' + units
        },
        plotOptions: {
            column: {
                dataLabels: {
                    enabled: true,
                    formatter: function () {
                        return Highcharts.numberFormat(this.y, 2);
                    }
                }
            }
        },
        legend: {
            enabled: false
        },
        series: [
            {
                name: "Models",
                colorByPoint: true,
                data: seriesData
            }],
    });
}

function buildTable(units, data, seriesData) {

    function buildScenarioDiffColumn(scenarioName) {
        return {
            title: 'Difference from <br/>' + scenarioName,
            field: scenarioName,
            headerSort: false,
            mutator: calculateDifference,
            mutatorParams: {scenarioIndex: m},
            formatter: "money",
            align: "right",
            widthGrow: 3
        };

    }

    function calculateDifference(value, data, type, params, column) {
        if (data['name'] !== column.getField() && params.scenarioIndex < data['id']) {
            let dataTable = column.getTable();
            let tableData = dataTable.getData();
            for (var i = 0; i < tableData.length; i++) {
                let row = tableData[i];
                if (row['name'] === column.getField()) {
                    return data['value'] - row['value'];
                }
            }
        }
    }

    function formatHeader(column, params) {
        let title = document.createElement('div');
        title.innerHTML = params['month_period'];
        title.style.fontSize = '13px';
        let location = document.createElement('div');
        location.innerHTML = params['location'];
        location.style.fontWeight = 'bold';
        location.style.fontSize = '18px';
        let statistics = document.createElement('div');
        statistics.innerHTML = params['statistics'];
        statistics.style.fontSize = '13px';
        title.appendChild(location);
        title.appendChild(statistics);
        return title;
    }

    var tableData = [];
    var scenarioColumns = [];
    scenarioColumns.push({title: 'Scenario', field: 'name', headerSort: false, widthGrow: 4});
    scenarioColumns.push({title: 'Volume (' + units + ')',field: 'value',headerSort: false,formatter: "money",align: "right",widthGrow: 3});
    for (var m = 0; m < seriesData.length; m++) {
        let scenarioName = seriesData[m]['name'];
        scenarioColumns.push(buildScenarioDiffColumn(scenarioName));
        tableData.push({id: m, name: seriesData[m]['name'], value: seriesData[m]['y']});
    }

    let table = new Tabulator("#table", {
        layout: 'fitColumns',
        selectable: false,
        columnVertAlign: 'center',
        columns: [
            {
                titleFormatter: formatHeader,
                titleFormatterParams: {month_period: data['month_period_title'], location: data['gui_link_title'], statistics: data['statistics']},
                headerSort: false,
                columns: scenarioColumns
            }
        ],
        data: tableData,
        rowFormatter: row => row.getElement().style.color = Highcharts.getOptions().colors[row.getData()['id']]
    });
    table.redraw();

}

