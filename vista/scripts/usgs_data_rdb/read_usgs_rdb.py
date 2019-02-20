from httplib import *
from vista.set import DataSetAttr, DataType, IrregularTimeSeries
from vista.time import TimeFactory

from vdss import writedss


def read_ts(file):
    fh = open(file)
    line = fh.readline()
    while not line.startswith("DATE"):
        line = fh.readline()
    line = fh.readline()
    line = fh.readline()
    tf = TimeFactory.getInstance()
    timeinterval = 'IR-DAY'
    vals = []
    tvals = []
    ncount = 0
    while line != None or line.strip() != '':
        fields = line.split()
        if len(fields) < 4:
            print 'No data in line #%d: %s' % (ncount, line)
            break
        else:
            date, time = fields[0:2]
            vtime = tf.createTime(date + ' ' + time, 'MM/dd/yyyy HH:mm:ss')
            tvals.append(vtime.create(vtime))
            vals.append(float(fields[3]))
        ncount = ncount + 1
        line = fh.readline()
    fh.close()
    attr = DataSetAttr(DataType.IRREGULAR_TIME_SERIES, '', 'UMHOS/CM', 'TIME', 'INST-VAL')
    return IrregularTimeSeries("TIME SERIES", tvals, vals, None, attr)


if __name__ == '__main__':
    ts = read_ts('d:/data/usgs_ec/11455780.uv.95.lower.rdb')
    writedss('D:/data/usgs_ec/USGS_ECstations.dss', '/USGS/BENICIA/EC//IR-DAY/11455780-lower/', ts)
