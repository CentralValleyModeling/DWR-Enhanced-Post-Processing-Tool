from hec.heclib.dss import *
from hec.script import *
from javax.swing import *

if __name__ == '__main__':
    import sys

    dssfile = sys.argv[1]  # dssfile=r"D:\testin.DSS"
    outdssfile = sys.argv[2]  # outdssfile=r"D:\testout.dss"
    dss = HecDss.open(dssfile)
    outdss = HecDss.open(outdssfile, False)
    matches = dss.getCatalogedPathnames("C=FLOW")
    for m in matches:
        print "Match: %s" % m
        data = dss.get(m, "01JAN1972 2400", "31DEC1991 2400")
        path = DSSPathname(data.fullName)
        path.setFPart(path.fPart() + "-COPY")
        data.fullName = path.getPathname()
        data.fileName = outdssfile
        outdss.put(data)
    dss.done()
    outdss.done()
