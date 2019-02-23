import os
import sys

sys.executable='jython'
import pydoc
import hecutils, interpolate, vdiff, vdisplay, vdss, vmath, vtidefile, vtimeseries, vutils
if __name__=='__main__':
    print 'Current working directory: ', os.getcwd()
    os.chdir('..\\doc\\pydocs')
    print 'Now working in directory: ', os.getcwd()
    modules = [hecutils, interpolate, vdiff, vdisplay, vdss, vmath, vtidefile, vtimeseries, vutils]
    for m in modules:
        pydoc.writedoc(m)
#
