import csv
import math
import os
import sys
from java.awt import *
from java.lang import Runnable
from javax.swing import *
from javax.swing.border import *
from vdisplay import plot
from vista.gui import VistaUtils
from vista.set import Pathname

import vtimeseries
import vutils
from vdss import writedss, set_part


class RunThis(Runnable):
    def __init__(self, runFunction):
        self._runFunction = runFunction

    def run(self):
        self._runFunction()


def list_pathnames(dssgroup):
    for ref in dssgroup:
        p = ref.pathname


def matches(path1, path2):
    if path1.getPart(Pathname.B_PART) == path2.getPart(Pathname.B_PART) and path1.getPart(
            Pathname.C_PART) == path2.getPart(Pathname.C_PART) and path1.getPart(Pathname.E_PART) == path2.getPart(
        Pathname.E_PART):
        return True
    else:
        return False


def set_timewindow(ref1, ref2, twOption="1"):
    """
    Set timewindow based on twOption of 1 for ref1 and 2 for ref2 else parse time window from twOption string in the format DDMMMYYYY HHmm-DDMMMYYYY HHmm
    """
    if twOption == "1":
        tw = ref1.getTimeWindow()
        ref2 = ref2.create(ref2, tw)
    elif twOption == "2":
        tw = ref2.getTimeWindow()
        ref1 = ref1.create(ref1, tw)
    else:
        tw = vtimeseries.timewindow(twOption)
        ref1 = ref1.create(ref1, tw)
        ref2 = ref2.create(ref2, tw)
    return ref1, ref2


def compare_dss_files(file1, file2, showPlot="Abs", outputFile=None, outputPathFile=None, twOption="1",
                      pathFilter=None):
    """
    Simply compares the files and outputs differences if any of those that differ and lists mismatching pathnames in either
    """
    g1 = vutils.opendss(file1)
    g2 = vutils.opendss(file2)
    if pathFilter:
        g1 = vutils.findpath(g1, pathFilter)
        g2 = vutils.findpath(g2, pathFilter)
    print 'Comparing %s to %s' % (file1, file2)
    print '%12s\t%32s' % ('DIFFERENCE', 'PATHNAME')
    if outputPathFile:
        opf_handle = open(outputPathFile, 'wb')
        opf = csv.writer(opf_handle, delimiter=",", quotechar='"', quoting=csv.QUOTE_MINIMAL)
        opf.writerow([file1, file2])
    no_diff = True
    for ref1 in g1:
        p1 = ref1.pathname
        found = False
        for ref2 in g2:
            p2 = ref2.pathname
            if matches(p1, p2):
                found = True
                ref1, ref2 = set_timewindow(ref1, ref2, twOption)
                diff = ref2.data - ref1.data
                absdiff = diff.createSlice(diff.getTimeWindow())
                vtimeseries.apply(absdiff, math.fabs)
                diff_total = vtimeseries.total(absdiff)
                if (diff_total > 1e-06):
                    no_diff = False
                    if showPlot and showPlot.find("Diff") >= 0:
                        graphs = plot(diff)
                        # for graph in graphs: graph.setExtendedState(JFrame.MAXIMIZED_BOTH)
                    else:
                        graphs = plot(ref1.data, ref2.data)
                        # for graph in graphs: graph.setExtendedState(JFrame.MAXIMIZED_BOTH)
                    print '%10.2f\t%32s' % (diff_total, p1)
                    if outputFile:
                        diffp = set_part(p1, 'DIFF-%s-%s' % (os.path.basename(file1), os.path.basename(file2)),
                                         Pathname.A_PART)
                        writedss(outputFile, str(diffp), diff)
                    if outputPathFile:
                        opf.writerow([p1, p1, diff_total])
                break
        if (not found):
            no_diff = False
            print 'No matching path: %s in file %s NOT found in file %s' % (p1, file1, file2)
            if outputPathFile: opf.writerow([p1, "", "M"])
    for ref2 in g2:
        p2 = ref2.pathname
        found = False
        for ref1 in g1:
            p1 = ref1.pathname
            if matches(p1, p2):
                found = True
                break
        if (not found):
            no_diff = False
            print 'No matching path: %s in file %s NOT found in file %s' % (p2, file2, file1)
            if outputPathFile: opf.writerow(["", p2, "M"])
    if no_diff:
        print 'NO DIFFERENCE ACROSS ENTIRE FILEs %s and %s' % (file1, file2)
    if outputPathFile: opf_handle.close()


#
def do_interactive():
    print 'Start interactive'
    p = JPanel()
    file1lbl, file2lbl, outputFilelbl, outputPathFilelbl = JLabel("File1"), JLabel("File2"), JLabel(
        "Output File"), JLabel("CSV File")
    file1TF, file2TF, outputFileTF, outputPathFileTF = JTextField(), JTextField(), JTextField(), JTextField()
    file1Btn, file2Btn, outputFileBtn, outputPathFileBtn = JButton("..."), JButton("..."), JButton("..."), JButton(
        "...")

    def selectFileAndSetTextField(event):
        if event.source == file1Btn:
            file = VistaUtils.getFilenameFromDialog(event.source, FileDialog.LOAD, "dss", "DSS File")
            file1TF.setText(file)
        elif event.source == file2Btn:
            file = VistaUtils.getFilenameFromDialog(event.source, FileDialog.LOAD, "dss", "DSS File")
            file2TF.setText(file)
        elif event.source == outputFileBtn:
            file = VistaUtils.getFilenameFromDialog(event.source, FileDialog.SAVE, "dss", "DSS File")
            outputFileTF.setText(file)
        elif event.source == outputPathFileBtn:
            file = VistaUtils.getFilenameFromDialog(event.source, FileDialog.SAVE, "csv", "CSV File")
            outputPathFileTF.setText(file)
        else:
            print 'Unknown type of event source', event.source

    file1Btn.actionPerformed = selectFileAndSetTextField
    file2Btn.actionPerformed = selectFileAndSetTextField
    outputFileBtn.actionPerformed = selectFileAndSetTextField
    outputPathFileBtn.actionPerformed = selectFileAndSetTextField
    # --Layout below
    layout = GridLayout(1, 3)
    p.setLayout(layout);

    def addWithLayout(left, right, center, p):
        ip = JPanel();
        ip.setLayout(BorderLayout())
        ip.add(left, BorderLayout.LINE_START);
        ip.add(right, BorderLayout.LINE_END);
        ip.add(center, BorderLayout.CENTER);
        p.add(ip)

    leftPanel = JPanel();
    leftPanel.setLayout(GridLayout(4, 1))
    leftPanel.add(file1lbl);
    leftPanel.add(file2lbl);
    leftPanel.add(outputFilelbl);
    leftPanel.add(outputPathFilelbl)
    rightPanel = JPanel();
    rightPanel.setLayout(GridLayout(4, 1))
    rightPanel.add(file1Btn);
    rightPanel.add(file2Btn);
    rightPanel.add(outputFileBtn);
    rightPanel.add(outputPathFileBtn)
    centerPanel = JPanel();
    centerPanel.setLayout(GridLayout(4, 1))
    centerPanel.add(file1TF);
    centerPanel.add(file2TF);
    centerPanel.add(outputFileTF);
    centerPanel.add(outputPathFileTF)
    addWithLayout(leftPanel, rightPanel, centerPanel, p)
    # --Layout above
    f = JFrame("DSS File Compare Tool")
    f.setLayout(BorderLayout())
    f.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
    f.contentPane.setLayout(BoxLayout(f.contentPane, BoxLayout.Y_AXIS))
    f.contentPane.add(p)
    # diff button
    doDiffBtn = JButton("Do Diff Check")

    def do_diff(event):
        f.setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR))
        doDiffBtn.enabled = False
        doDiffBtn.text = 'Running diff...'
        compare_dss_files(file1TF.text, file2TF.text, False, outputFileTF.text, outputPathFileTF.text)
        doDiffBtn.text = 'Do Diff Check'
        doDiffBtn.enabled = True
        f.setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR))
        f.setTitle("Done. Check output file for results")
        import sched, time
        s = sched.scheduler(time.time, time.sleep)

        def reset_title():
            f.setTitle("DSS File Compare Tool")

        s.enter(10, 1, reset_title, ())
        s.run()

    doDiffBtn.actionPerformed = do_diff
    f.contentPane.add(doDiffBtn)
    f.pack()
    f.setSize(720, 180)
    f.setLocationRelativeTo(None)
    f.setVisible(True)


#
def usage():
    print 'Usage: compare_dss_files [-i | --interactive] [-f | --filter=</A/B/C////>][-s|--show="Diff|Abs"] [-d | --differences] [-o|--output=<outputdssfile>] [-od|--output-paths=<output_path_file.csv>] [-t| --timewindow=1|2|01JAN1990 2400-01FEB1990 2400] file1.dss file2.dss'


if __name__ == '__main__':
    import getopt

    try:
        opts, args = getopt.getopt(sys.argv[1:], "ihf:s:o:d:t:",
                                   ["interactive", "help", "filter=", "show=", "output=", "differences=",
                                    "timewindow="])
    except err:
        # print help information and exit:
        print str(err)  # will print something like "option -a not recognized"
        usage()
        sys.exit(2)
    showPlot = None
    pathFilter = None
    outputFile = None
    outputPathFile = None
    interactive = False
    twOption = "1"
    for o, a in opts:
        if o in ("-h", "help"):
            usage()
            exit(0)
        elif o in ("-f", "--filter"):
            pathFilter = a
        elif o in ("-s", "--show"):
            showPlot = a
        elif o in ("-o", "--output"):
            outputFile = a
            print 'Writing paths that differ to %s' % outputFile
        elif o in ("-d", "--differences"):
            outputPathFile = a
            print 'Writing paths that differ to %s' % outputFile
        elif o in ("-i", "--interactive"):
            interactive = True
        elif o in ("-t", "--timewindow"):
            twOption = a
            print "Time Window set to %s" % twOption
        else:
            assert False, "unhandled option"
    if interactive:
        SwingUtilities.invokeLater(RunThis(do_interactive))
    else:
        if len(args) != 2:
            usage()
            sys.exit(3)
        file1 = args[0]
        file2 = args[1]
        compare_dss_files(file1, file2, showPlot, outputFile, outputPathFile, twOption, pathFilter)
        if not showPlot:
            sys.exit(0)
#
