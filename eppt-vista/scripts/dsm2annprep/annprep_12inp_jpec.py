from annprep import *


def create_dss_file(file, outfile):
    f = File(file)
    print 'Parsing %s' % f.absoluteFile
    basedir = f.parentFile.absoluteFile.parentFile.absolutePath  # directory off which relative pathnames are based
    tables = dsm2parser.parse(file)
    print 'Loading References'
    refmap1 = load_datarefs(tables.getTableNamed("BOUNDARY_FLOW"), basedir)
    refmap2 = load_datarefs(tables.getTableNamed("SOURCE_FLOW"), basedir)
    refmap3 = load_datarefs(tables.getTableNamed("SOURCE_FLOW_RESERVOIR"), basedir)
    refmap4 = load_datarefs(tables.getTableNamed("BOUNDARY_STAGE"), basedir)
    refmap = merge_two_dicts(refmap1, refmap2)
    refmap = merge_two_dicts(refmap, refmap3)
    refmap = merge_two_dicts(refmap, refmap4)
    print 'Writing References to ' + outfile
    # east side streams
    write_ref(outfile, refmap['calaveras'] + refmap['cosumnes'] + refmap['moke'])
    # north bay
    write_ref(outfile, refmap['north_bay'])
    # sac
    write_ref(outfile, refmap['sac'])
    # sjr
    write_ref(outfile, refmap['vernalis'])
    # yolo
    write_ref(outfile, refmap['yolo'])
    # ccc
    write_ref(outfile, refmap['ccc'] + refmap['ccw'] + refmap['cccoldr'])
    # cvp
    write_ref(outfile, refmap['cvp'])
    # swp
    write_ref(outfile, refmap['swp'])
    # channel diversions and returns
    print 'Summing up diversions, seepages and return flows'
    dicu_divseep = sum_all(refmap, 'dicu_div') + sum_all(refmap, 'dicu_seep')
    div_seep = flat_interpolate(dicu_divseep)
    vdss.writedss(outfile, str(dicu_divseep.pathname), div_seep)
    dicu_drain = sum_all(refmap, 'dicu_drain')
    drain = flat_interpolate(dicu_drain)
    vdss.writedss(outfile, str(dicu_drain.pathname), drain)
    # dxc gate
    print 'Interpolating gate positon to daily time series'
    refmap_gates = load_datarefs(tables.getTableNamed("OPRULE_TIME_SERIES"), basedir)
    dcc = flat_interpolate(refmap_gates['dcc_op'])
    vdss.writedss(outfile, dcc.name, dcc)
    # stage at mtz
    print 'Tidally filtering and averaging boundary stage to daily'
    refmap_mtz = refmap['mtz']
    mtz = per_avg(godin(refmap_mtz), '1DAY')
    write_ref(outfile, mtz)
    # jersey pt ec
    jpref = vdss.get_ref(basedir + '/output/historical_v81.dss', '//RSAN018/EC////')
    jpgodin = per_avg(godin(jpref), '1DAY')
    write_ref(outfile, jpgodin)
    return basedir


if __name__ == '__main__':
    if (len(sys.argv) < 2):
        print "Usage: annprep hydro_echo_file.inp"
        exit(1)
    file = sys.argv[1]
    outfile = 'ann.dss'
    create_dss_file(file, outfile)
    #
    import os

    basedir_name = os.path.basename(os.path.abspath('.'))
    csvfile = 'jpann' + basedir_name + '.csv'
    print 'Writing out csv file: %s' % csvfile
    twstr = '05JAN1999 2400 - 29MAR2012 2400'
    write_to_csv(get_basedir(file), outfile, csvfile, twstr)
    print 'Ann Prep Done.'
