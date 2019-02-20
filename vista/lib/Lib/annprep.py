from java.io import File
from vista.set import Pathname

import vdss
import vutils


def load_datarefs(table, basedir):
    nrows = table.getNumberOfRows()
    headers = table.getHeaders()
    refmap = {}
    for i in range(nrows):
        name = table.getValue(i, "NAME")
        if headers.contains("SIGN"):
            sign = table.getValue(i, "SIGN")
        else:
            sign = "1"
        file = table.getValue(i, "FILE")
        path = table.getValue(i, "PATH")
        if (file == "constant"): continue
        ref = vdss.get_ref(basedir + '/' + file, path)
        ref = float(sign) * ref
        refmap[name] = ref
    return refmap


def merge_two_dicts(x, y):
    z = x.copy()  # start with x's keys and values
    z.update(y)  # modifies z with y's keys and values & returns None
    return z


def write_ref(outfile, ref):
    vdss.writedss(outfile, str(ref.pathname), ref.data)


def sum_all(refmap, match):
    sum_ref = None
    for k in refmap.keys():
        if k.find(match) >= 0:
            if sum_ref == None:
                sum_ref = refmap[k]
            else:
                sum_ref = sum_ref + refmap[k]
    sum_ref.getPathname().setPart(Pathname.B_PART, 'ALL')
    return sum_ref


def write_to_csv(basedir, dssfile, csvfile, twstr):
    # read from dss file and write out to .csv file
    g = vutils.opendss(basedir + '/' + dssfile)
    refs = g.getAllDataReferences().tolist()
    refs.sort()
    write_refs_to_csv(refs, basedir, csvfile, twstr)


def write_refs_to_csv(refs, basedir, csvfile, twstr):
    from vtimeseries import timewindow
    from vista.set import DataReference
    tw = timewindow(twstr)
    refs = map(lambda x: DataReference.create(x, tw), refs)
    ts = map(lambda x: x.data, refs)
    from vista.set import MultiIterator
    iter = MultiIterator(ts)
    fh = open(basedir + '/' + csvfile, 'w')
    fh.write("Study: " + basedir)
    fh.write("\n")
    fh.write(str(reduce(lambda x, y: x + ',' + y, map(lambda x: x.name.split("/")[2], ts))))
    fh.write("\n")
    fh.write(str(reduce(lambda x, y: x + ',' + y, map(lambda x: x.name.split("/")[3], ts))))
    fh.write("\n")
    fh.write(str(reduce(lambda x, y: x + ',' + y, map(lambda x: x.attributes.YUnits, ts))))
    fh.write("\n")
    while not iter.atEnd():
        el = iter.element
        for i in range(el.dimension - 1):
            fh.write(str(el.getY(i)))
            if i == el.dimension - 2:
                fh.write("\n")
            else:
                fh.write(",")
        iter.advance()
    fh.close()


def get_basedir(file):
    f = File(file)
    print 'Parsing %s' % f.absoluteFile
    basedir = f.parentFile.absoluteFile.parentFile.absolutePath  # directory off which relative pathnames are based
    return basedir
