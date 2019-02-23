from vdisplay import plot
from vtidefile import opentidefile

from vutils import *


def get_avg_conc(tidefile, chan, twstr):
    tf = opentidefile(tidefile)
    if twstr != None:
        print 'Timewindow: %s' % twstr
        tw = timewindow(twstr)
    else:
        tw = None
    refs = tf.find(['', '^%s$' % chan, 'AVG CONC'])
    if refs and len(refs) == 1:
        print "Getting data %s" % (str(chan))
        if tw != None:
            ref = DataReference.create(refs[0], tw)
        else:
            ref = refs[0]
        return ref.data
    else:
        raise "No data found for %s in file %s" % (chan, tidefile)


if __name__ == '__main__':
    tidefile = "d:/tidefile_ec_problem/FOR8_DB0_CU8_DXC0_ec.h5"
    twstr = "01JUL2014 0000 - 01AUG2014 0000"
    chans = [291, 290, 436, 435, 434, 433]
    chan_concs = []
    for chan in chans:
        chan_concs.append(get_avg_conc(tidefile, chan, twstr))

    for conc in chan_concs:
        plot(conc)
#
