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
function postInit(chart) {
    chart.update({
        exporting: {
            fallbackToExportServer: false,
            menuItemDefinitions: {
                // Custom definition
                xZoom: {
                    onclick: function () {
                        this.update({chart: {zoomType: "x"}});
                    },
                    text: 'X Axis Zoom'
                },
                yZoom: {
                    onclick: function () {
                        this.update({chart: {zoomType: "y"}});
                    },
                    text: 'Y Axis Zoom'
                },
                xyZoom: {
                    onclick: function () {
                        this.update({chart: {zoomType: "xy"}});
                    },
                    text: 'Both Axis Zoom'
                },
                pan: {
                    onclick: function () {
                        this.update({chart: {zoomType: "", panning: true}});
                        this.pointer.zoomX = true;
                    },
                    text: 'Pan'
                }
            },
            buttons: {
                contextButton: {
                    symbol: 'menuball',
                    menuItems: ['xZoom', 'yZoom', 'xyZoom', 'pan']
                }
            }
        },
        credits: {
            enabled: false
        },
        tooltip: {
            valueDecimals: 2
        }
    });
}