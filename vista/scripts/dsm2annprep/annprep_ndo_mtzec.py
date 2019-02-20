from annprep import *

from vtimeseries import flat_interpolate, godin, per_avg, per_min, per_max


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
    # 
    inflows = refmap['calaveras'] + refmap['cosumnes'] + refmap['moke'] + refmap['sac'] + refmap['vernalis'] + refmap[
        'yolo']
    outflows = refmap['north_bay'] + refmap['ccc'] + refmap['ccw'] + refmap['cccoldr'] + refmap['cvp'] + refmap['swp']
    # channel diversions and returns
    print 'Summing up diversions, seepages and return flows'
    dicu_divseep = sum_all(refmap, 'dicu_div') + sum_all(refmap, 'dicu_seep')
    div_seep = flat_interpolate(dicu_divseep)
    dicu_drain = sum_all(refmap, 'dicu_drain')
    drain = flat_interpolate(dicu_drain)
    ndoref = inflows + outflows + vdss.gen_ref(div_seep) + vdss.gen_ref(drain)
    ndoref.pathname.setPart(0, 'ALL')
    ndoref.pathname.setPart(1, 'NDO')
    ndoref.pathname.setPart(2, 'FLOW')
    write_ref(outfile, ndoref)
    # stage at mtz
    print 'Tidally filtering and averaging boundary stage to daily'
    refmap_mtzstage = refmap['mtz']
    mtzmin = per_min(refmap_mtzstage, '1DAY')
    mtzmin.pathname.setPart(1, 'MTZ-MIN')
    mtzmax = per_max(refmap_mtzstage, '1DAY')
    mtzmax.pathname.setPart(1, 'MTZ-MAX')
    mtzstage = per_avg(godin(refmap_mtzstage), '1DAY')
    write_ref(outfile, mtzmin)
    write_ref(outfile, mtzmax)
    write_ref(outfile, mtzstage)
    # mtz ec
    rsac054ecref = vdss.get_ref(basedir + '/../../timeseries/hist_19902012.dss',
                                '/FILL+CHAN/RSAC054/EC//1HOUR/DWR-DMS-201203_CORRECTED/')
    rsac054ec = per_avg(godin(rsac054ecref), '1DAY')
    write_ref(outfile, rsac054ec)
    return ndoref, mtzmin, mtzmax, mtzstage, rsac054ec


if __name__ == '__main__':
    if (len(sys.argv) < 2):
        print "Usage: annprep hydro_echo_file.inp"
        exit(1)
    file = sys.argv[1]
    outfile_prefix = 'ann_ndo_mtzec'
    outfile = '%s.dss' % outfile_prefix
    refs = create_dss_file(file, outfile)
    #
    import os

    basedir_name = os.path.basename(os.path.abspath('.'))
    csvfile = '%s_' % outfile_prefix + basedir_name + '.csv'
    print 'Writing out csv file: %s' % csvfile
    twstr = '05JAN1999 2400 - 29MAR2012 2400'
    write_to_csv(get_basedir(file), outfile, csvfile, twstr)
    print 'Ann Prep Done.'
