# vscript to calculate CCFB gate timings for priority 3 ops
from vista.set import IrregularTimeSeries
from vista.set import TimeSeriesMergeUtils

import vdss
import vtimeseries
from vtimeseries import timeinterval


# Step 1. Get the stage outside CCFB
def get_stage_data(dssfile, search_str):
    print 'Finding stage data for %s & %s' % (dssfile, search_str)
    pdss = vdss.opendss(dssfile)
    d = vdss.find(pdss, search_str)
    if len(d) < 0:
        print "No data found for : ", search_str
        exit(1)
    elif len(d) > 1:
        print "Too many data sets found for : ", search_str
        for x in d:
            print x.data
        exit(2)
    return d[0]


# Step 2. Find the HH, HL and LL, LH timings
def generate_hh_ll_ts(d):
    print 'Find Tidal {HH, LH}, {LL, LH} as two time series and writing it out to tidal_phase_amp.dss'
    dtidal_pts = vtimeseries.find_tidal_pt(d)
    b_part = d.pathname.getPart(Pathname.B_PART)
    c_part = d.pathname.getPart(Pathname.C_PART)
    # Irregular Time Series:  # HH, HL
    th = dtidal_pts[0].data
    # Irregular Time Series: # LH, LL
    tl = dtidal_pts[1].data
    print 'Writing Tidal Pts to HH and LL pathnames in tidal_phase_amp.dss'
    vdss.writedss('tidal_phase_amp.dss', '/planning/%s/%s//ir-mon/tidal-pts-hh/' % (b_part, c_part), th)
    vdss.writedss('tidal_phase_amp.dss', '/planning/%s/%s//ir-mon/tidal-pts-ll/' % (b_part, c_part), tl)
    return th, tl


# Step 3: calculate gate timings based on Priority 3 rule:
# look for HH timings and open gate 1 hour before HH timing
# look for LL and close gate 2 hours before LL and open gate 1 hour after LL
# Look for HL and close gate 2 hours after HL
def generate_ccfb_timings(th, tl):
    print 'Generating CCFB Priority 3 Gate Timings and writing it out to ccfb.dss'
    times = []  # times array for gates
    values = []  # 0 for close , 1 for open
    prev_e = th[0];
    ti_hour = timeinterval('1HOUR')
    for e in th[1:]:
        if e.y > prev_e.y:  # @ HH open 1 hour before HH
            hh_t = time(e.getXString())
            times.append(hh_t - ti_hour)
            values.append(1)
        else:
            hh_t = time(e.getXString())
            times.append(hh_t - ti_hour)
            values.append(1)
        prev_e = e;
    attr = DataSetAttr(DataType.IRREGULAR_TIME_SERIES, "TIME", "POS", "", "INST-VAL")  # "PER-AVER"
    # look for HH timings and open gate 1 hour before HH timing
    irr1 = IrregularTimeSeries('HH OPEN', times, values, None, attr)
    times = []  # times array for gates
    values = []  # 0 for close , 1 for open
    prev_e = tl[0];
    for e in tl[1:]:
        if e.y < prev_e.y:  # @ LL close 2 hour before, open 1 hour after
            times.append(time(e.getXString()) - 2 * ti_hour)
            values.append(0)
            times.append(time(e.getXString()) + ti_hour)
            values.append(1)
        else:
            times.append(time(e.getXString()) + 2 * ti_hour)  # close 2 hour after Hl
            values.append(0)
        prev_e = e;
    # look for LL and close gate 2 hours before LL and open gate 1 hour after LL
    # Look for HL and close gate 2 hours after HL
    irr2 = IrregularTimeSeries('LL OPEN CLOSE', times, values, None, attr)
    ccfb = TimeSeriesMergeUtils.merge([irr1, irr2], irr1.timeWindow.union(irr2.timeWindow))
    # clean up if prev value is same is current value remove it
    times = []
    values = []
    prev_e = ccfb[0]
    times.append(prev_e.x)
    values.append(prev_e.y)
    for e in ccfb[1:]:
        if abs(e.y - prev_e.y) < 1.0e-6:  # same value as last time then ignore it
            pass
        else:
            times.append(e.x)
            values.append(e.y)
        prev_e = e
    ccfb_cleaned = IrregularTimeSeries('CCFB cleaned', times, values, None, attr)
    print 'Writing CCFB gate open/close timings to priority-3 pathname in ccfb.dss'
    vdss.writedss('ccfb.dss', '/planning/ccfb/gate//ir-mon/priority-3/', ccfb_cleaned)
    return ccfb


#
if __name__ == '__main__':
    d = get_stage_data('CS3_NAA_94yr_CCFB_out_stage.dss', 'ROLD040/STAGE')
    th, tl = generate_hh_ll_ts(d)
    ccfb = generate_ccfb_timings(th, tl)
    print 'All Done'
