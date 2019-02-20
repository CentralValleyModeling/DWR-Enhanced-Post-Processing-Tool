import annprep
import dsm2parser
import os
import subprocess
from shutil import copyfile

import vutils


#   modify sac/sjr to 0.9*sac as input. run dsm2. extract results to ann.dss
#   modify swp/cvp to 1.1*swp as input. run dsm2. extract results to ann.dss
#   modify dxc to invert as input and then run dsm2
#   modify cd to 1.1*cd as input and then run dsm2.
def generate_dss_and_modify_table(basedir, table, flowname, ratio):
    nameIndex = table.headers.indexOf('NAME')
    fileIndex = table.headers.indexOf('FILE')
    pathIndex = table.headers.indexOf('PATH')
    for row in table.values:
        if row[nameIndex] == flowname:
            file = row[fileIndex]
            path = row[pathIndex]
            ref = annprep.get_ref(vutils.opendss(basedir + '/' + file), path)
            outfile = flowname + '.dss'
            newref = ref * ratio
            newref.pathname.setPart(1, ref.pathname.getPart(1))
            annprep.write_ref(basedir + '/' + outfile, newref)
            row[fileIndex] = outfile
            print "Writing out to " + outfile + " for boundary: " + flowname + " ratio: " + str(ratio)


def run_dsm2(study_dir):
    os.makedirs(study_dir + '/output')
    hydro_exe = os.path.join(os.path.dirname(os.path.abspath('hydro.inp')), '..\\..\\bin\\hydro.exe')
    qual_exe = os.path.join(os.path.dirname(os.path.abspath('hydro.inp')), '..\\..\\bin\\qual.exe')
    hydro_cmd_str = hydro_exe + ' ' + os.path.abspath('hydro.inp')
    qual_cmd_str = qual_exe + ' ' + os.path.abspath('qual_ec.inp')
    fh = open(study_dir + '/dsm2.bat', 'w')
    fh.write(hydro_cmd_str)
    fh.write("\n")
    fh.write(qual_cmd_str)
    fh.close()
    # os.system('start cmd /c "%s & %s"'%(hydro_cmd_str, qual_cmd_str))
    subprocess.Popen([study_dir + '/dsm2.bat'], shell=True, stdin=None, stdout=None, stderr=None, close_fds=True)


def setup_and_run_study_dir(study_dir, flow_name, ratio, table_name):
    """
    Sets up study dir for the change to the boundary flow name (should match the name in the BOUNDARY_FLOW table in dsm2 input.)
    The flow is changed by the ratio provided and written in the new study directory as <flow name>.dss file 
    and the table entry for it is modified to point to it.
    """
    orig_dir = os.getcwd()
    # make new directory
    parentdir = os.path.abspath('../')
    study_dir = parentdir + '/' + study_dir
    os.makedirs(study_dir)
    print 'Made study dir: ' + study_dir
    copyfile('output/hydro_echo_historical_v81.inp', study_dir + '/hydro.inp')
    copyfile('output/qual_ec_echo_historical_v81.inp', study_dir + '/qual_ec.inp')
    os.chdir(study_dir)
    # read input file, modify input data and write out input file to point to it.
    tables = dsm2parser.parse(os.path.abspath('hydro.inp'))
    flow_table = tables.getTableNamed(table_name)
    print table_name, flow_table
    # read the base sac flow and write out modified flow
    generate_dss_and_modify_table(os.path.abspath(study_dir), flow_table, flow_name, ratio)
    #
    fh = open(os.path.abspath('hydro.inp'), 'w')
    for table in tables.getTables():
        fh.write(table.toStringRepresentation())
        fh.write('\n')
    fh.close()
    # run model
    run_dsm2(study_dir)
    os.chdir(orig_dir)


if __name__ == '__main__':
    # setup_and_run_study_dir("sac90",'sac',0.9,'BOUNDARY_FLOW')
    setup_and_run_study_dir("cvp111", 'cvp', 1.11, 'SOURCE_FLOW')
    # setup_and_run_study_dir("vernalis90",'vernalis',0.9,'BOUNDARY_FLOW')
    setup_and_run_study_dir("swp111", 'swp', 1.11, 'SOURCE_FLOW_RESERVOIR')
