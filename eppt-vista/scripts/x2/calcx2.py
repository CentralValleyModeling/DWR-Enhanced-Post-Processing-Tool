# calculate x2 based on ndo
from vista.set import RegularTimeSeries, DataReference

from vdss import opendss, findpath, writedss


def escape_regex(path):
    return path.replace("+", "\+")


ndofile = "ndo.dss"
ndopath = "/DELTA/NDO/FLOW-NDO//1DAY/BDO/"
twstr = "01JAN1989 0000 - 31JUL2014 0000"
ndo = DataReference.create(findpath(opendss(ndofile), ndopath)[0], twstr).getData().getYArray().tolist()
#
# X2 (km) = 10.16 + 0.945 * X2 (day-1) � 1.487 * log [Outflow (cfs)]
# [Kimmerer-Monismith daily X2]
# Steady X2 (km) = 185 � 27 * log [Outflow (cfs)]
# [steady-outflow to X2 relationship]
# Outflow (cfs) = 10 ^ ([185 � X2 (km)]/27)
# [daily X2 to equivalent outflow relationship]
x2_prev = 85  # initial value
x2vals = []
for day in range(1, len(ndo)):
    x2 = 10.16 + 0.945 * x2_prev - 1.487 * log(ndo[day])
    x2vals.append(x2)
#
x2path = "/DELTA/X2/DIST//1DAY/BDO/"
x2rts = RegularTimeSeries("/DELTA/X2/DIST//1DAY/BDO/", twstr.split("-")[0].strip(), '1DAY', x2vals)
writedss("x2.dss", x2path, x2rts)
#
