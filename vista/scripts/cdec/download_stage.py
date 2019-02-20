import vdisplay

import cdec
from vutils import *

if __name__ == '__main__':
    station_name = 'MRZ'
    sensor_number = 1811
    start_date = "04/01/2013"
    end_date = "07/01/2014"
    rts = cdec.retrieve(station_name, sensor_number, start_date, end_date, verbose=1)
    if rts: vdisplay.plot(rts)
    writedss('z:/temp/cdec.dss', rts.name, rts)
