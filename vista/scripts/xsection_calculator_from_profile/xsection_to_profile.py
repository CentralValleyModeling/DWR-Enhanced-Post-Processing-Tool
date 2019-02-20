# Create a profile from a xsection as defined in dsm2
import sys
from gov.ca.dsm2.input.model import *

if __name__ == '__main__':
    if len(sys.argv) < 2:
        print "Usage: xsection_to_profile xsections.inp"
        sys.exit(1)
    infile = sys.argv[1]
    tables = Parser.parseModel(infile)
    xsect_layer_table = tables.getTableNamed("XSECT_LAYER")
    nxsects = xsectionTable.getValues().size()
    newLayer = True
    currentChannelDist = ""
    currentXSection = null;
    for i in range(nxsects):
        if not newLayer:
            channelDist = xsectionTable.getValue(i, "CHAN_NO") + "_" + xsectionTable.getValue(i, "DIST")
            newLayer = True
        if newLayer:
            xsection = XSection();
            xsection.setChannelId(channel.getId());
            xsection.setDistance(Double.parseDouble(xsectionTable
                                                    .getValue(i, "DIST")));
            currentChannelDist = xsectionTable.getValue(i, "CHAN_NO") + "_" + xsectionTable.getValue(i, "DIST");
            currentXSection = xsection;
            channel.addXSection(xsection);
            newLayer = false;
        layer = XSectionLayer()
        layer.setElevation(Double.parseDouble(xsectionTable.getValue(i, "ELEV")))
        layer.setArea(Double.parseDouble(xsectionTable.getValue(i, "AREA")))
        layer.setTopWidth(Double.parseDouble(xsectionTable.getValue(i, "WIDTH")))
        layer.setWettedPerimeter(Double.parseDouble(xsectionTable.getValue(i, "WET_PERIM")))
        currentXSection.addLayer(layer)
