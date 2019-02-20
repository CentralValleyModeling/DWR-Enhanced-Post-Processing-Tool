# Process the calsim file to generate input and output csv file for dl4j training dataSet
import vdss
from vtimeseries import timeinterval


def get_data(dssfile, search_str):
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


def create_daily_gatepos(gate):
    """
    Converts number of days gate is open to a regular time series of 0 (closed) and 1 (open) assuming open in first part of month 
    """
    import jarray
    from vista.set import MultiIterator
    st = gate.getStartTime()
    et = gate.getEndTime()
    ndays = st.getNumberOfIntervalsTo(et, timeinterval("1DAY"))
    dailygate = RegularTimeSeries(d.name, str(st), "1DAY", jarray.zeros(ndays, 'd'))
    mts = MultiIterator([gate, dailygate])
    while (not mts.atEnd()):
        el = mts.getElement()
        if (el.getX(1) != el.getX(1)):  # is nan
            pass
        else:
            nopen = m
    return dailygate


if __name__ == '__main__':
    calsim_file = "d:/calsim_ann/ANN_data_full.dss"
    out_file = "d:/calism_ann/ANN_Input.dss"
    dxc = get_data(calsim_file, "/*/DXC")
    dxc = create_daily_gatepos(dxc)
    writedss(outfile, "/CALSIM-SMOOTH/DELTACC/POS//1DAY/ANN-INPUT-1/", dxc)
    # SAC
    sac = get_data(calsim_file, "/*/C_SAC041") + get_data(calsim_file, "/*/C_CSL004A") + get_data(calsim_file,
                                                                                                  "/*/C_MOK019") + get_data(
        calsim_file, "/*/C_C08")
    writedss(outfile, "/CALSIM-SMOOTH/SAC/FLOW//1DAY/ANN-INPUT-2/", sac)
    # EXPORTS
    exports = get_data(calsim_file, "/*/D_OMR027_CAA000") + get_data(calsim_file, "/*/D_OMR028_DMC003") + get_data(
        calsim_file, "/*/D408")
    writedss(outfile, "/CALSIM-SMOOTH/EXPORTS/FLOW//1DAY/ANN-INPUT-3/", exports)
    # DICU
    dicu = get_data(calsim_file, "/*/NET_DICU")
    writedss(outfile, "/CALSIM-SMOOTH/DICU/FLOW//1DAY/ANN-INPUT-4/", dicu)
    # SJR
    sjr = get_data(calsim_file, "/*/C639")
    writedss(outfile, "/CALSIM-SMOOTH/SJR/FLOW//1DAY/ANN-INPUT-5/", sjr)
    # VERNALIS EC
    vernalis_ec = get_data(calsim_file, "/*/VERNWQFINAL")
    writedss(outfile, "/CALSIM-SMOOTH/VERNALIS/EC//1DAY/ANN-INPUT-6/", vernalis_ec)
    # SF ASTRO
    sf_astro = get_data(calsim_file, "/*/san_francisco")
    writedss(outfile, "/CALSIM/SF_ASTRO/STAGE-MAX-MIN//1DAY/ASTRO_NAVD_20170607/", sf_astro)
    #
    emmaton = get_data(calsim_file, "/*/rsac092/ec")
    #
