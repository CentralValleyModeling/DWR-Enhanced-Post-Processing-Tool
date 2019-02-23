import csv
import sys
from vista.set import DataSetAttr, DataType, Constants, IrregularTimeSeries

import vdss
from vtimeseries import time


def parse_date(vals, date_col):
    try:
        return time(vals[date_col], 'MM/dd/yyyy HH:mm')
    except:
        return None


def parse_value(vals, val_col):
    try:
        return float(vals[val_col])
    except:
        return Constants.MISSING_VALUE


def write_its(pathname, tvals, yvals, units):
    attr = DataSetAttr(DataType.IRREGULAR_TIME_SERIES, '', units, 'TIME', 'INST-VAL')
    its = IrregularTimeSeries(pathname, tvals, yvals, None, attr)
    vdss.writedss(dssfilename, its.name, its)


if __name__ == '__main__':
    lines_to_skip = 5
    date_col = 4
    val_col = 5
    units = 'FEET'
    pathname = '/DES/MARTINEZ/STAGE//IR-DAY/EMP/'
    buffer_size = 100000;
    csvfilename = sys.argv[1]
    dssfilename = sys.argv[2]
    yvals = []
    tvals = []
    try:
        f = open(csvfilename, 'r')
        r = csv.reader(f)
        for i in range(lines_to_skip): r.next()
        field_names = r.next()
        count = 0
        for vals in r:
            t = parse_date(vals, date_col)
            v = parse_value(vals, val_col)
            if t != None:
                tvals.append(t)
                yvals.append(v)
            count = count + 1
            if (count % buffer_size == 0):
                write_its(pathname, tvals, yvals, units)
                yvals = []
                tvals = []
        write_its(pathname, tvals, yvals, units)
    finally:
        f.close()
    print "Done."
