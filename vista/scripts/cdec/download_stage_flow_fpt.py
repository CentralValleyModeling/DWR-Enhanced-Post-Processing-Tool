import vdisplay

import cdec
from vutils import *

if __name__ == '__main__':
    station_name = 'FPT'
    start_date = "01/01/2008"
    end_date = "01/01/2012"
    sensor_number = 598  # stage
    rts = cdec.retrieve(station_name, sensor_number, start_date, end_date, verbose=1)
    if rts: vdisplay.plot(rts)
    writedss('z:/temp/fpt.dss', rts.name, rts)
    sensor_number = 599  # flow
    rts = cdec.retrieve(station_name, sensor_number, start_date, end_date, verbose=1)
    if rts: vdisplay.plot(rts)
    writedss('z:/temp/fpt.dss', rts.name, rts)
