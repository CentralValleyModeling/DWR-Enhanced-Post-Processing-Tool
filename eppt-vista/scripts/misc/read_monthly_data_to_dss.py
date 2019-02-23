from vdss import *


def read_monthly_data_from_text(filename):
    lines = open(filename, 'r').readlines()
    data = []
    for line in lines:
        fields = line.split()
        for f in fields:
            data.append(float(f))
    return data


if __name__ == '__main__':
    filename = 'ULDemandAdj.txt'
    data = read_monthly_data_from_text(filename)
    pathname = '/CALSIM/DD/DEPLETION//1MON/CALSIM30_10/'
    rts = RegularTimeSeries(pathname, '31OCT1921 2400', '1MON', data)
    rts = rts * 2.0
    writedss('output.dss', pathname, rts)
