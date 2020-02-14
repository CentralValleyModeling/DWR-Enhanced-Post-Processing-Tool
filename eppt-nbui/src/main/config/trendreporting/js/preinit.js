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

const PLOTLY_COLORS = ['rgb(76,126,238)', 'rgb(3,201,140)', 'rgb(214,46,130)', 'rgb(98,210,250)', 'rgb(163,242,68)', 'rgb(207,121,221)', 'rgb(128,244,225)'];
const PLOTLY_FONT = {
    family: 'Lucida Sans Unicode, Verdana, Arial, Helvetica, sans-serif',
    color: 'black',
};


function openContextMenu(id, e, copyToClipboard, plotlyExportToFormat) {
    // prevent the browsers default context menu form appearing.
    e.preventDefault();
    $.contextMenu({
        selector: id,
        build: function ($triggerElement, e) {
            return {
                items: {
                    "export": {
                        name: "Export",
                        items: {
                            "SVG": {
                                name: "To SVG",
                                callback: function (key, options) {
                                    plotlyExportToFormat('svg');
                                }
                            },
                            "PNG": {
                                name: "To PNG",
                                callback: function (key, options) {
                                    plotlyExportToFormat('png');
                                }
                            },
                            "JPEG": {
                                name: "To JPEG",
                                callback: function (key, options) {
                                    plotlyExportToFormat('jpeg');
                                }
                            },
                            "PDF": {
                                name: "To PDF",
                                callback: function (key, options) {
                                    plotlyExportToFormat('pdf');
                                }
                            }
                        }
                    },
                    "copy": {
                        name: "Copy Data",
                        callback: function (key, options) {
                            copyToClipboard();
                        }
                    },
                }
            }
        }
    });

}

var javaObj;

function plotlyExportFunction(plot) {
    return (format) => {
        var width = plot.offsetWidth;
        var height = plot.offsetHeight;
        Plotly.downloadImage(plot, {format: format, height: height, width: width});
        //javaObj instantiated from JavaFX
        if(javaObj){
            javaObj.interruptFunction(format, JSON.stringify(plot.data), JSON.stringify(plot.layout), width, height);
        }
    }
}

function copyTextToClipboard(text) {
    var textArea = document.createElement("textarea");
    textArea.value = text.replace(/<br>/g, ' ').replace(/<b>/g, '').replace(/<\/b>/g, '');
    document.body.appendChild(textArea);
    textArea.focus();
    textArea.select();
    document.execCommand('copy');
    document.body.removeChild(textArea);
}

