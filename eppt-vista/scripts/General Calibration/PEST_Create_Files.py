import datetime
import glob
import os
import random
import re
import shutil
from vista.time import DefaultTimeFormat

from PEST_Calib_Functions import *
# from vista.db.dss import *
from vutils import *

#
if __name__ == '__main__':
    TF = TimeFactory.getInstance()
    filter = Constants.DEFAULT_FLAG_FILTER
    random.seed()
    # Create a .pst file (PEST Control File), a PEST Template File (.tpl),
    # and a .ins file (PEST Instruction File) for the PEST calibration of DSM2
    #
    sq = "'"
    dq = '"'
    bs = "\\"
    fs = '/'
    ##
    ## Basic/Most commonly changed settings
    ##
    # continue on DSM2 failure, or fail PEST run?
    contDSM2 = True
    # usable water years in the 2000s decade
    # with BN/DRY/CRIT year types, and their
    # desired end dates (start dates always 01OCT)
    WYEndDates = { \
        '2001': '01OCT2001', \
        '2002': '31DEC2002', \
        '2004': '01OCT2004', \
        '2005': '01OCT2005', \
        '2007': '01OCT2007', \
        '2008': '01OCT2008', \
        '2009': '01OCT2009' \
        }
    useWY = '2009'
    Calib = True
    #
    #     NOPTMAX = -1    # Jacobian (run on all params)
    #     NOPTMAX = 0     # Only one model run
    #     NOPTMAX = 1     # Sensitivities and one calib iter
    NOPTMAX = 15  # Calibration
    #
    # use grouped (tied/fixed) parameters?
    useParamGroups = False
    ##
    # Number of condor slaves
    NCondor = 70
    # Adjust the observed stage for bad datum?
    adjStage = True
    # Use uniform initial Manning's N for all channels?
    UniformInitialManning = True
    # use initial parameter values from prior Calibration directory?
    priorCalibDir = ''
    # priorCalibDir = 'z:/delta/calib/Condor-Calib3-MANN-EC/'
    # DSM2 runs: restart or cold start?
    useRestart = True
    #####
    ## Set several variables to None here to allow them to be overridden 
    ## in the args below
    # Directory to create the Condorized PEST calibration directory
    condorBaseDir = None
    # Directory for the Condorized PEST calibration directory
    condorDir = None
    # Parameter names and DERINCLB values 
    paramGrpDER = None
    # Which data types to use
    useObsDataTypes = None
    #####
    ## Allow a few to be read as command line arguments for batch scripts
    for arg in sys.argv[1:]:
        print 'Using argument', arg
        exec (arg)
    ##
    # what type of PEST parallel run?
    # typePEST = 'beo'
    typePEST = 'genie'
    ##
    # Observed Parameter Groups
    # Use either Width or Elev, not both
    if paramGrpDER == None:
        paramGrpDER = [ \
            ['MANN', 0.001] \
            #                     ,['DISP',10.0] \
            #                    ['LENGTH' ,50.0], \
            #                     ['GATECF' ,0.05] \
            #                     ['RESERCF' ,0.05] \
            #                    ['WIDTH' ,0.01], \
            #                     ,['ELEV' ,0.01] \
            #                     ['DIV-FLOW' ,0.1] \
            #                     ,['DRAIN-FLOW' ,0.1] \
            #                     ,['DRAIN-EC' ,0.1] \
        ]
    # Observed data types to use
    if useObsDataTypes == None:
        useObsDataTypes = [ \
            'EC'
            , 'FLOW'
            , 'STAGE'
        ]
    # compare obs/model data from this date
    cmpDataDate = '01JUN' + useWY + ' 0000'
    #
    if Calib:
        CVStr = 'Calib'
    else:
        CVStr = 'Valid'
    ###
    startDateStr = '01OCT' + str(int(useWY) - 1) + ' 0000'
    endDateStr = WYEndDates[useWY] + ' 0000'
    # 1 year warmup period start date
    warmupStartDateStr = '01OCT' + str(int(useWY) - 2) + ' 0000'
    warmupEndDateStr = startDateStr
    #
    paramGroups, paramDERINCLB = zip(*paramGrpDER)
    paramGroups = list(paramGroups)
    paramDERINCLB = list(paramDERINCLB)
    # DSM2 start run dates; these must match the DSM2 BaseRun dates
    # if a restart file is used
    # runLength is years
    StartDateObj_Hydro = TF.createTime(startDateStr)
    if not useRestart:
        StartDateObj_Hydro = StartDateObj_Hydro - TF.createTimeInterval('1YEAR')
    #
    EndDateObj_Hydro = TF.createTime(endDateStr)
    # Qual start/end dates are one day after/before Hydro
    StartDateObj_Qual = StartDateObj_Hydro + TF.createTimeInterval('1DAY')
    StartDateStr_Qual = StartDateObj_Qual.format()
    EndDateObj_Qual = EndDateObj_Hydro - TF.createTimeInterval('1DAY')
    # Comparison start and end dates must be within the run dates, 
    # and are used for observed and DSM2 comparison data. 
    # A delayed comparison date allows DSM2 to equilibrate with
    # its new parameters. 
    CmpStartDateObj = TF.createTime(cmpDataDate)
    CmpEndDateObj = EndDateObj_Qual - TF.createTimeInterval('1DAY')
    CmpTW = TF.createTimeWindow(CmpStartDateObj, CmpEndDateObj)
    # DSM2 directories and files
    DSM2Mod = 'HIST-CLB2K'
    DSM2Run = 'BASE-v812'
    WorkspaceDir = r'D:\workspace\vista\scripts\General Calibration'
    RootDir = 'D:/delta/models/'
    CommonDir = RootDir + 'dsm2_v8/common_input/'
    CalibDir = RootDir + '201X-Calibration/'
    TimeSeriesDir = RootDir + 'dsm2_v8/timeseries/'
    longBaseRunDir = 'd:/delta/models/201X-Calibration/BaseRun_1990-2009/'
    if UniformInitialManning:
        BaseRunDir = CalibDir + 'BaseRun-PEST-UniformManningN/'
    else:
        BaseRunDir = CalibDir + 'BaseRun-PEST/'
    BaseRunOutput = BaseRunDir + 'Output/'
    BaseRunBatch = BaseRunDir + 'baserun.bat'
    if priorCalibDir == '':
        HydroEchoFile = 'hydro_echo_' + DSM2Mod + '-' + DSM2Run + '.inp'
        QualEchoFile = 'qual_ec_echo_' + DSM2Mod + '-' + DSM2Run + '.inp'
    else:
        HydroEchoFile = 'hydro_echo_' + DSM2Mod + '.inp'
        QualEchoFile = 'qual_ec_echo_' + DSM2Mod + '.inp'
    DivRtnQFile = 'dicu_201203.dss'
    RtnECFile = 'dicuwq_3vals_extended.dss'
    ChanInpFile = 'channel_std_delta_grid_NAVD_20121214-calib.inp'
    GateInpFile = 'gate_std_delta_grid_20110418_NAVD.inp'
    ResInpFile = 'reservoir_std_delta_grid_NAVD_20121214.inp'
    ChanCalibFile = 'Calib-channels.inp'
    GateCalibFile = 'Calib-gates.inp'
    ResCalibFile = 'Calib-reservoirs.inp'
    XSectsCalibFile = 'Calib-xsects.inp'
    # 'Dummy' input files for cross-section & Ag div/drainage/EC
    # calibration (multiplier) factors; stores floating-point
    # numbers used to multiply the time-series values
    PESTInpXCFile = 'XCCalibCoeffs.inp'
    PESTInpAgFile = 'AgCalibCoeffs.inp'
    #
    fileSub = 'dsm2-base.sub'
    fileRun4 = 'dsm2-run4' + typePEST.upper() + '.bat'
    # observed data
    obsDataDir = CalibDir + 'Observed Data/'
    obsDataFile = obsDataDir + 'CalibObsData.dss'
    obsDataPaths = obsDataDir + 'PEST-Use-CalibObsData.txt'
    obsDSM2ChanLocs = CalibDir + 'DSM2_Locs.txt'
    # PEST outputs for Hydro and Qual runs, these contain output paths
    # matching observed data paths
    DSM2DSSOutHydroFile = 'PEST_Hydro_Out.inp'
    DSM2DSSOutQualFile = 'PEST_Qual_Out.inp'
    # The DSS file containing combined Hydro and Qual output
    CalibDSSOutFile = 'PESTCalib.dss'
    # The text equivalent of the DSS output, necessary for PEST
    DSM2OutFile = 'PESTCalib.out'
    # Pest directories and files
    PESTDir = CalibDir + 'PEST/'
    # Directory to perform Condorized PEST calibration
    if condorDir == None:
        if condorBaseDir == None:
            condorBaseDir = 'z:/delta/calib/'
        condorDir = condorBaseDir + 'Condor-'
        if priorCalibDir != '':
            condorDir += re.sub('/$', '', priorCalibDir.replace(condorDir, '')) + '#'
        condorDir += CVStr + str(NOPTMAX) + '-' + \
                     '_'.join(paramGroups) + '-' + '_'.join(useObsDataTypes)
        if useParamGroups:
            condorDir += '-TIED'
    condorDir = re.sub(fs + '$', '', condorDir.replace(bs, fs))
    # PEST control file name
    PESTFile = 'DSM2.pst'
    PESTChanTplFile = ChanCalibFile.split('.')[0] + '.tpl'
    PESTGateTplFile = GateCalibFile.split('.')[0] + '.tpl'
    PESTResTplFile = ResCalibFile.split('.')[0] + '.tpl'
    PESTTplXCFile = PESTInpXCFile.split('.')[0] + '.tpl'
    PESTTplAgFile = PESTInpAgFile.split('.')[0] + '.tpl'
    PESTInsFile = DSM2OutFile.split('.')[0] + '.ins'
    # tied, fixed, and none (if used) for channel groups
    # the text file was produced using ArcMap (export to text file)
    # create a dictionary for tied/fixed/none channels
    if useParamGroups:
        GroupChans = setGroups(PESTDir + 'GroupChans.txt')
        GroupNodes = setGroups(PESTDir + 'GroupNodes.txt')
    # remove old files
    for f in glob.glob(PESTDir + '*.tpl'):
        os.remove(f)
    ##
    # copy base run input file to calibration input file
    try:
        shutil.copyfile(CommonDir + ChanInpFile, PESTDir + ChanCalibFile)
    except:
        print 'Cannot find base grid input file', CommonDir + ChanInpFile
        raise
    # update calibration input file with a previous calibration's parameters
    if priorCalibDir != '':
        parFile = priorCalibDir + 'dsm2.par'
        coeffDict = readPESTParFile(parFile)
        # fixme: this only works for channel and xsect_layer sections
        if coeffDict:
            updateDSM2GridFile(coeffDict, PESTDir + ChanCalibFile)
        else:
            print 'Could not find prior calibration parameter file', parFile
            sys.exit(1)
            # read DSM2 base run info
    if priorCalibDir == '':
        ChanDict = parseInpSects(BaseRunOutput + HydroEchoFile, sectionName='CHANNEL')['CHANNEL']
        XSDict = parseInpSects(BaseRunOutput + HydroEchoFile, sectionName='XSECT_LAYER')['XSECT_LAYER']
        HydroAgDict = parseInpSects(BaseRunOutput + HydroEchoFile, sectionName='SOURCE_FLOW')['SOURCE_FLOW']
        QualAgDict = parseInpSects(BaseRunOutput + QualEchoFile, sectionName='NODE_CONCENTRATION')['NODE_CONCENTRATION']
    else:
        temp = parseInpSects(priorCalibDir + HydroEchoFile, sectionName='CHANNEL')
        ChanDict = parseInpSects(PESTDir + ChanCalibFile, sectionName='CHANNEL', sectionData=temp)['CHANNEL']
        temp.clear()
        temp = parseInpSects(priorCalibDir + HydroEchoFile, sectionName='XSECT_LAYER')
        XSDict = parseInpSects(PESTDir + ChanCalibFile, sectionName='XSECT_LAYER', sectionData=temp)['XSECT_LAYER']
        HydroAgDict = parseInpSects(priorCalibDir + HydroEchoFile, sectionName='SOURCE_FLOW')['SOURCE_FLOW']
        QualAgDict = parseInpSects(priorCalibDir + QualEchoFile, sectionName='NODE_CONCENTRATION')['NODE_CONCENTRATION']
    # read the gate input file for gate data
    gatePipeDict = parseInpSects(CommonDir + GateInpFile, 'GATE_PIPE_DEVICE')['GATE_PIPE_DEVICE']
    gateWeirDict = parseInpSects(CommonDir + GateInpFile, 'GATE_WEIR_DEVICE')['GATE_WEIR_DEVICE']
    # read the reservoir input file for reservoir connection data
    resCFDict = parseInpSects(CommonDir + ResInpFile, 'RESERVOIR_CONNECTION')['RESERVOIR_CONNECTION']
    ##
    # Observed data files, etc.
    # Observed data paths; the DSM2 output paths are determined from these.
    ##
    ## All observed data should already be in RTS 1HOUR time interval,
    ## checked for errors, units reconciled, etc.
    ##
    obsPaths = []
    fid = open(obsDataPaths, 'r')
    for line in fid.readlines():
        if line[0] == '#':
            continue
        for dt in useObsDataTypes:
            if line.find(fs + dt + fs) > 0:
                obsPaths.append(line.rstrip())
                continue
        #
    #
    fid.close()
    # DSM2 output locations
    # Each observed B part (location) must have a corresponding
    # DSM2 channel/length in this list of tuples
    DSM2ObsLoc = []
    fid = open(obsDSM2ChanLocs, 'r')
    for line in fid.readlines():
        if line[0] == '#':
            continue
        tmp = line.rstrip().split()
        sta = tmp[0]
        chanNo = int(tmp[1])
        chanDist = tmp[2]
        if chanDist != 'END':
            chanDist = int(chanDist)
        else:
            chanDist = int(ChanDict['LENGTH'][ChanDict['CHAN_NO'].index(str(chanNo))])
        DSM2ObsLoc.append(tuple([sta, chanNo, chanDist]))
    fid.close()
    # sort the paths to reproduce them in the same order
    # from the DSM2 output paths; sorting will be alphabetically
    # by C part (data type), then B part (location)
    obsPaths = sorted(obsPaths, key=obsDataBParts)
    obsPaths = sorted(obsPaths, key=obsDataCParts)
    #
    if 'DIV-FLOW' in paramGroups or 'DRAIN-FLOW' in paramGroups or 'DRAIN-EC' in paramGroups:
        PIAFId = open(PESTDir + PESTInpAgFile, 'w')
        PTAFId = open(PESTDir + PESTTplAgFile, 'w')
        PTAFId.write('%s\n' % ('ptf @'))
    if 'ELEV' in paramGroups or 'WIDTH' in paramGroups:
        PIXFId = open(PESTDir + PESTInpXCFile, 'w')
        PTXFId = open(PESTDir + PESTTplXCFile, 'w')
        PTXFId.write('ptf @\n')
    #
    PCFId = open(PESTDir + PESTFile, 'w')
    #
    ObsTempFile1 = PESTDir + 'temp1.txt'
    OTF1Id = open(ObsTempFile1, 'w')
    #
    # read observed data DSS file at desired locations and date range
    # write observed data to a temporary file for later inclusion in
    # the .pst file.
    #
    # Also produce the DSM2 Hydro and Qual output files for PEST calibration
    dss_group = opendss(obsDataFile)
    nObs = 0
    obsGroups = []
    dataTypes = []
    #
    DSM2HydroId = open(PESTDir + DSM2DSSOutHydroFile, 'w')
    DSM2QualId = open(PESTDir + DSM2DSSOutQualFile, 'w')
    DSM2HydroId.write('# PEST Calibration output for HYDRO.\n' \
                      'OUTPUT_CHANNEL\n' \
                      'NAME CHAN_NO DISTANCE VARIABLE INTERVAL PERIOD_OP FILE\n')
    DSM2QualId.write('# PEST Calibration output for QUAL.\n' \
                     'OUTPUT_CHANNEL\n' \
                     'NAME CHAN_NO DISTANCE VARIABLE INTERVAL PERIOD_OP FILE\n')
    totalDT = {"EC": 0., "STAGE": 0., "FLOW": 0.}
    countDT = {"EC": 0, "STAGE": 0, "FLOW": 0}
    for obsPath in obsPaths[:]:
        g = find(dss_group, obsPath.replace('+', '\+'))
        dataref = g.getAllDataReferences()
        if len(dataref) > 1:
            print 'Error, too many observed DSS paths for', obsPath
            sys.exit()
        if len(dataref) < 1:
            print 'Dropping path', obsPath, 'observed DSS path not found'
            del obsPaths[obsPaths.index(obsPath)]
            continue
        dataref = dataref[0]
        staName = dataref.getPathname().getPart(Pathname.B_PART)
        dataType = dataref.getPathname().getPart(Pathname.C_PART)
        dataTW = dataref.getTimeWindow()
        dataset = dataref.getData()
        perType = dataset.getAttributes().getYType().upper()
        # print staName, dataType
        dataCmpTW = dataTW.intersection(CmpTW)
        if dataCmpTW == None:
            print 'Dropping path', obsPath, 'no observed data in time window'
            del obsPaths[obsPaths.index(obsPath)]
            continue
        dataCmpDays = (dataCmpTW.getEndTime() - dataCmpTW.getStartTime()).getIntervalInMinutes( \
            dataCmpTW.getStartTime()) / 24. / 60.
        if dataCmpDays < 25.:
            print 'Dropping path', obsPath, 'Not enough data in time window (' + \
                                            str(dataCmpDays) + ' days)'
            del obsPaths[obsPaths.index(obsPath)]
            continue
        # check for a corresponding DSM2 channel output for the observed data
        try:
            tup = [t for t in DSM2ObsLoc if t[0] == staName][0]
        except:
            print 'Dropping path', obsPath, 'no channel location for station', staName
            del obsPaths[obsPaths.index(obsPath)]
            continue
        # get data and accumulate totals for later weighting calcs
        dataset = dataset.createSlice(CmpTW)
        countPath = Stats.countAcceptable(dataset, filter)
        if countPath <= 0:
            print 'Dropping path', obsPath, 'no valid data in time window'
            del obsPaths[obsPaths.index(obsPath)]
            continue
        if len(staName) > 7:
            print 'Dropping path', obsPath, 'station name', staName, 'too long'
            del obsPaths[obsPaths.index(obsPath)]
            continue
        obsGroup = getObsGroup(dataref.getPathname().toString())
        if obsGroup not in obsGroups:
            obsGroups.append(obsGroup)
        if dataType not in dataTypes:
            dataTypes.append(dataType)
        # use micro mhos/cm (micro siemens/cm) for all ECs
        if dataset.getAttributes().getYUnits().upper() == 'MS/CM' or \
                dataset.getAttributes().getYUnits().upper() == 'MMHOS/CM':
            dataset *= 1000.0
        # accumulate the total absolute value of each data type
        # for PEST weight of observations later
        apply(dataset, abs)
        totalPath = total(dataset)
        totalDT[dataType] += totalPath
        countDT[dataType] += countPath
    # End obsPaths test loop
    if adjStage:
        if not os.path.exists(longBaseRunDir + 'Output/' + CalibDSSOutFile):
            print 'Error: Stage adjustment requested but cannot find DSM2 output file', longBaseRunDir + 'Output/'
            sys.exit(1)
        for obsPath in obsPaths:
            if obsPath.find('/STAGE/') != -1:
                g = find(dss_group, obsPath.replace('+', '\+'))
                dataref = g.getAllDataReferences()[0]
                dfCmp = extendDataRef(dataref, CmpTW)
                staName = dataref.getPathname().getPart(Pathname.B_PART)
                dataType = dataref.getPathname().getPart(Pathname.C_PART)
                dataIntvl = dataref.getPathname().getPart(Pathname.E_PART)
                perType = dataset.getAttributes().getYType().upper()
                obsGroup = getObsGroup(dataref.getPathname().toString())
                dataset = dfCmp.getData()
                # if stage adjustment is desired and a DSM2 output file exists,
                # adjust observed stage so the averages of obs/dsm2 stage
                # are the same...cheap fix to perennial obs stage datum problem
                dsm2_group = opendss(longBaseRunDir + 'Output/' + CalibDSSOutFile)
                try:
                    g = find(dsm2_group, fs + staName + fs + dataType + fs)
                    if g == None:
                        print 'No computed data for', staName, dataType
                        continue
                    dsDSM2 = g.getAllDataReferences()[0].getData().createSlice(CmpTW)
                except:
                    dsDSM2 = None
                if dsDSM2 == None:
                    print 'No computed data for', staName, dataType
                    continue
                aveDSM2 = avg(dsDSM2)
                aveObs = avg(dataset)
                dataset = dataset + (aveDSM2 - aveObs)
                totalDT[dataType] += (aveDSM2 - aveObs) * Stats.countAcceptable(dataset, filter)
                print 'Adjusted: ', str(round((aveObs - aveDSM2), 3)), obsPath
    for obsPath in obsPaths:
        g = find(dss_group, obsPath.replace('+', '\+'))
        dataref = g.getAllDataReferences()[0]
        dfCmp = extendDataRef(dataref, CmpTW)
        staName = dataref.getPathname().getPart(Pathname.B_PART)
        dataType = dataref.getPathname().getPart(Pathname.C_PART)
        dataIntvl = dataref.getPathname().getPart(Pathname.E_PART)
        perType = dataset.getAttributes().getYType().upper()
        obsGroup = getObsGroup(dataref.getPathname().toString())
        dataset = dfCmp.getData()
        # get data, assign 0 weight if missing, correct EC units,
        # write to temp file
        # use micro mhos/cm (micro siemens/cm) for all ECs
        if dataset.getAttributes().getYUnits().upper() == 'MS/CM' or \
                dataset.getAttributes().getYUnits().upper() == 'MMHOS/CM':
            dataset *= 1000.0
        pathWeight = 1. / (totalDT[dataType] / countDT[dataType])
        tsi = dataset.getIterator()
        while not tsi.atEnd():
            el = tsi.getElement()
            nObs += 1
            el = tsi.getElement()
            timeObj = TF.createTime(long(el.getX()))
            dateStr = timeObj.format(DefaultTimeFormat('yyyyMMdd'))
            timeStr = timeObj.format(DefaultTimeFormat('HHmm'))
            val = el.getY()
            valStr = '%15.3f' % (val)
            if filter.isAcceptable(el):
                weight = pathWeight
            else:
                weight = 0.0
            dT = dataType.replace('STAGE', 'STG').replace('FLOW', 'Q')
            OTF1Id.write("%-30s %s %12.5E %s\n" % \
                         (staName + dT + dateStr + timeStr, valStr, weight, obsGroup))
            tsi.advance()
        # write the corresponding DSM2 output line for the observed data path
        tup = [t for t in DSM2ObsLoc if t[0] == staName][0]
        chanNo = tup[1]
        chanDist = tup[2]
        fmtStr = '%s    %3d %8d   %s     %s      %s  %s\n'
        #         if perType == "INST-VAL":
        #             pT = "INST"
        #         elif perType == "PER-AVER":
        #             pT = "AVE"
        #         else:
        #             pT = perType
        pT = "AVE"  # almost all observed data should be 1HOUR averaged
        # print obsPath, perType, chanNo, chanDist
        if dataType.lower() == 'stage' or \
                dataType.lower() == 'flow':
            DSM2HydroId.write(fmtStr % (staName, chanNo, chanDist, \
                                        dataType, dataIntvl, pT, CalibDSSOutFile))
        else:
            DSM2QualId.write(fmtStr % (staName, chanNo, chanDist, \
                                       dataType, dataIntvl, pT, CalibDSSOutFile))
    # End obsPaths write loop
    DSM2HydroId.write('END')
    DSM2QualId.write('END')
    OTF1Id.close()
    DSM2HydroId.close()
    DSM2QualId.close()
    print 'Wrote', DSM2HydroId.name
    print 'Wrote', DSM2QualId.name
    obsGroups.sort()  # ensure the data groups are alphabetical order
    # PEST inputs here
    # number of calibration parameters
    NPAR = 0
    if 'MANN' in paramGroups:
        NPAR += len(ChanDict['CHAN_NO'])
    if 'DISP' in paramGroups:
        NPAR += len(ChanDict['CHAN_NO'])
    if 'LENGTH' in paramGroups:
        NPAR += len(ChanDict['CHAN_NO'])
    if 'GATECF' in paramGroups:
        # count only gates with non-zero flow coefficients
        NPAR += countDevParams(gateWeirDict, ['CF_FROM_NODE', 'CF_TO_NODE'])
        NPAR += countDevParams(gatePipeDict, ['CF_FROM_NODE', 'CF_TO_NODE'])
    if 'RESERCF' in paramGroups:
        NPAR += countDevParams(resCFDict, ['COEF_IN', 'COEF_OUT'])
    if 'ELEV' in paramGroups or \
            'WIDTH' in paramGroups:
        NPAR += len(ChanDict['CHAN_NO'])
    param = 'DIV-FLOW'
    if param in paramGroups:
        for rowNo in range(len(HydroAgDict['NAME'])):
            try:
                CPart = HydroAgDict['PATH'][rowNo].upper().split(fs)[3]
            except:
                continue
            if CPart == param:
                NPAR += 1
    param = 'DRAIN-FLOW'
    if param in paramGroups:
        for rowNo in range(len(HydroAgDict['NAME'])):
            try:
                CPart = HydroAgDict['PATH'][rowNo].upper().split(fs)[3]
            except:
                continue
            if CPart == param:
                NPAR += 1
    param = 'DRAIN-EC'
    if param in paramGroups:
        for rowNo in range(len(QualAgDict['NAME'])):
            try:
                CPart = QualAgDict['PATH'][rowNo].upper().split(fs)[3]
            except:
                continue
            if CPart == param:
                NPAR += 1
    NOBS = nObs
    NPARGP = len(paramGroups)
    NPRIOR = NPAR  # every parameter has prior information
    # but don't use those FIXED or TIED
    if useParamGroups:
        NPRIOR = 0
        for key, value in sorted(GroupChans.iteritems(), key=lambda (k, v): (v, k)):
            if value[0].upper() == 'NONE':
                param, obj = key.split()
                if param in paramGroups:
                    NPRIOR += 1
    #
    NOBSGP = len(obsGroups) + NPARGP
    NTPLFLE = 0
    if 'MANN' in paramGroups or \
            'DISP' in paramGroups or \
            'LENGTH' in paramGroups:
        NTPLFLE += 1
    if 'GATECF' in paramGroups:
        NTPLFLE += 1
    if 'RESERCF' in paramGroups:
        NTPLFLE += 1
    if 'DIV-FLOW' in paramGroups or 'DRAIN-FLOW' in paramGroups or 'DRAIN-EC' in paramGroups:
        NTPLFLE += 1
    if 'ELEV' in paramGroups or 'WIDTH' in paramGroups:
        NTPLFLE += 1
    PESTMODE = 'estimation'
    RSTFLE = 'norestart'
    NINSFLE = 1
    PRECIS = 'single'
    DPOINT = 'point'
    NUMCOM = 1
    JACFILE = 0
    MESSFILE = 0
    PHIRATSUF = 0.3
    PHIREDLAM = 0.01
    RELPARMAX = 1.25
    FACPARMAX = 1.25
    FACORIG = 0.001
    PHIREDSWH = 0.1
    PHIREDSTP = 0.005
    NPHISTP = 4
    NPHINORED = 3
    RELPARSTP = 0.01
    NRELPAR = 3
    ICOV = 0
    ICOR = 0
    IEIG = 0
    # SVD values
    SVDMODE = 1
    MAXSING = 60
    EIGTHRESH = 1.0E-4
    EIGWRITE = 1
    # Least Squares values
    LSQRMODE = 0
    LSQR_ATOL = 1.e-10
    LSQR_BTOL = 1.e-10
    LSQR_CONLIM = 1.e3
    LSQR_ITNLIM = 10000
    LSQRWRITE = 1
    # Marquadt Lambda
    # ignore PEST manual about NUMLAM=0 when using SVD
    RLAMBDA1 = 5.0
    RLAMFAC = 2.0
    NUMLAM = 15
    # print the header and PEST control info to the PEST control (.pst) file
    PCFId.write('pcf\n')
    PCFId.write('* control data\n')
    PCFId.write('%s %s\n' % (RSTFLE, PESTMODE))
    PCFId.write('%d %d %d %d %d\n' % (NPAR, NOBS, NPARGP, NPRIOR, NOBSGP))
    PCFId.write('%d %d %s %s %d %d %d\n' % (NTPLFLE, NINSFLE, PRECIS, DPOINT, NUMCOM, JACFILE, MESSFILE))
    PCFId.write('%f %f %f %f %d\n' % (RLAMBDA1, RLAMFAC, PHIRATSUF, PHIREDLAM, NUMLAM))
    PCFId.write('%f %f %f\n' % (RELPARMAX, FACPARMAX, FACORIG))
    PCFId.write('%f\n' % (PHIREDSWH))
    PCFId.write('%d %f %d %d %f %d\n' % (NOPTMAX, PHIREDSTP, NPHISTP, NPHINORED, RELPARSTP, NRELPAR))
    PCFId.write('%d %d %d\n' % (ICOV, ICOR, IEIG))
    #
    if SVDMODE == 1:
        PCFId.write('* singular value decomposition\n')
        PCFId.write('%d\n' % SVDMODE)
        PCFId.write('%d %10.5E\n' % (MAXSING, EIGTHRESH))
        PCFId.write('%d\n' % (EIGWRITE))
    #
    if LSQRMODE == 1:
        PCFId.write('* lsqr\n')
        PCFId.write('%d\n' % LSQRMODE)
        PCFId.write('%10.5E %10.5E %10.5E %d\n' % (LSQR_ATOL, LSQR_BTOL, LSQR_CONLIM, LSQR_ITNLIM))
        PCFId.write('%d\n' % (LSQRWRITE))
    # parameter groups
    PCFId.write('* parameter groups\n')
    INCTYP = 'relative'
    DERINC = 0.10
    FORCEN = 'switch'
    DERINCMUL = 1.2
    #     DERMTHD = 'best_fit'
    DERMTHD = 'parabolic'
    #
    for param in paramGroups:
        DERINCLB = paramDERINCLB[paramGroups.index(param)]
        paramGrp = param.upper()
        if paramGrp == 'DISP':
            DERINC = 0.40
        if paramGrp == 'ELEV' or paramGrp == 'WIDTH':
            DERINC = 0.15
        if paramGrp == 'DRAIN-FLOW':
            paramGrp = 'DRN-Q'
            DERINC = 0.20
        if paramGrp == 'DIV-FLOW':
            paramGrp = 'DIV-Q'
            DERINC = 0.20
        if paramGrp == 'DRAIN-EC':
            paramGrp = 'DRN-EC'
            DERINC = 0.20
        PCFId.write('%s %s %4.3f %5.4f %s %3.1f %s\n' % \
                    (paramGrp, INCTYP, DERINC, DERINCLB, FORCEN, DERINCMUL, DERMTHD))
    #
    # parameter data
    PCFId.write('* parameter data\n')
    PARCHGLIM = 'RELATIVE'
    SCALE = 1.0
    OFFSET = 0.0
    DERCOM = 1
    paramInfo = dict([])
    tiedParams = dict([])
    for param in paramGroups:
        PARTRANS = 'LOG'
        param = param.upper()
        PARGP = param
        if param == 'MANN' or \
                param == 'DISP' or \
                param == 'LENGTH':
            # adjust channel parameters directly
            for rowNo in range(len(ChanDict['CHAN_NO'])):
                chan3 = "%03d" % int(ChanDict['CHAN_NO'][rowNo])
                PARNME = param + chan3
                if useParamGroups:
                    try:
                        key = param + ' ' + chan3
                        PARTRANS = GroupChans[key][0].upper()
                        # save Tied channels for later section
                        if PARTRANS == 'TIED':
                            tiedParams[PARNME] = param + "%03d" % int(GroupChans[key][1])
                        # use group "NONE" for fixed parameters
                        if PARTRANS == 'FIXED':
                            PARGP = 'NONE'
                        else:
                            PARGP = param
                    except:
                        pass
                if param == 'MANN':
                    PARVAL1 = float(ChanDict['MANNING'][rowNo])
                    PARLBND = PARVAL1 / 2.0
                    PARUBND = PARVAL1 * 3.0
                if param == 'DISP':
                    PARVAL1 = float(ChanDict['DISPERSION'][rowNo])
                    PARLBND = PARVAL1 / 10.0
                    PARUBND = PARVAL1 * 10.0
                if param == 'LENGTH':
                    PARVAL1 = float(ChanDict['LENGTH'][rowNo])
                    PARLBND = PARVAL1 / 1.5
                    PARUBND = PARVAL1 * 1.5
                PCFId.write('%8s %5s %s %12.4f %12.4f %12.4f %s %5.2f %5.2f %1d\n' % \
                            (PARNME, PARTRANS, PARCHGLIM, PARVAL1, PARLBND, PARUBND, PARGP, SCALE, OFFSET, DERCOM))
                # save parameter name, initial value, and upper/lower bounds for Prior Information calcs later,
                # if not Tied or Fixed
                if PARTRANS == 'NONE':
                    paramInfo[PARNME] = [PARVAL1, PARLBND, PARUBND, PARGP]
        #
        if param == 'GATECF':
            # Adjust gate coefficients directly, similar to channel parameters.
            # DSM2 gates are split between pipes and weirs, we'll follow the same pattern
            gateList = []
            for rowNo in range(0, len(gateWeirDict['GATE_NAME'])):
                PARVAL1 = float(gateWeirDict['CF_FROM_NODE'][rowNo])
                # skip zero-value gate coeffs
                if PARVAL1 != 0.0:
                    PARLBND = PARVAL1 * 0.5
                    PARUBND = PARVAL1 * 1.5
                    fullName = 'WEIRCFFR:' + gateWeirDict['GATE_NAME'][rowNo] + \
                               ':' + gateWeirDict['DEVICE'][rowNo]
                    # PEST requires the PARNME be less than or equal to 12 chars
                    PARNME = shortenName(gateList, fullName, 12, 'WEIRCFFR:')
                    PCFId.write('%8s %5s %s %10.3f %10.3f %10.3f %s %5.2f %5.2f %1d\n' % \
                                (PARNME, PARTRANS, PARCHGLIM, PARVAL1, PARLBND, PARUBND, PARGP, SCALE, OFFSET, DERCOM))
                    # save parameter name, initial value, and upper/lower bounds for Prior Information calcs later
                    paramInfo[PARNME] = [PARVAL1, PARLBND, PARUBND, PARGP]
                PARVAL1 = float(gateWeirDict['CF_TO_NODE'][rowNo])
                if PARVAL1 != 0.0:
                    PARLBND = PARVAL1 * 0.5
                    PARUBND = PARVAL1 * 1.5
                    fullName = 'WEIRCFTO:' + gateWeirDict['GATE_NAME'][rowNo] + \
                               ':' + gateWeirDict['DEVICE'][rowNo]
                    PARNME = shortenName(gateList, fullName, 12, 'WEIRCFTO:')
                    PCFId.write('%8s %5s %s %10.3f %10.3f %10.3f %s %5.2f %5.2f %1d\n' % \
                                (PARNME, PARTRANS, PARCHGLIM, PARVAL1, PARLBND, PARUBND, PARGP, SCALE, OFFSET, DERCOM))
                    # save parameter name, initial value, and upper/lower bounds for Prior Information calcs later
                    paramInfo[PARNME] = [PARVAL1, PARLBND, PARUBND, PARGP]
            for rowNo in range(0, len(gatePipeDict['GATE_NAME'])):
                PARVAL1 = float(gatePipeDict['CF_FROM_NODE'][rowNo])
                if PARVAL1 != 0.0:
                    PARLBND = PARVAL1 * 0.5
                    PARUBND = PARVAL1 * 1.5
                    fullName = 'PIPECFFR:' + gatePipeDict['GATE_NAME'][rowNo] + \
                               ':' + gatePipeDict['DEVICE'][rowNo]
                    PARNME = shortenName(gateList, fullName, 12, 'PIPECFFR:')
                    PCFId.write('%8s %5s %s %10.3f %10.3f %10.3f %s %5.2f %5.2f %1d\n' % \
                                (PARNME, PARTRANS, PARCHGLIM, PARVAL1, PARLBND, PARUBND, PARGP, SCALE, OFFSET, DERCOM))
                    # save parameter name, initial value, and upper/lower bounds for Prior Information calcs later
                    paramInfo[PARNME] = [PARVAL1, PARLBND, PARUBND, PARGP]
                PARVAL1 = float(gatePipeDict['CF_TO_NODE'][rowNo])
                if PARVAL1 != 0.0:
                    PARLBND = PARVAL1 * 0.5
                    PARUBND = PARVAL1 * 1.5
                    fullName = 'PIPECFTO:' + gatePipeDict['GATE_NAME'][rowNo] + \
                               ':' + gatePipeDict['DEVICE'][rowNo]
                    PARNME = shortenName(gateList, fullName, 12, 'PIPECFTO:')
                    PCFId.write('%8s %5s %s %10.3f %10.3f %10.3f %s %5.2f %5.2f %1d\n' % \
                                (PARNME, PARTRANS, PARCHGLIM, PARVAL1, PARLBND, PARUBND, PARGP, SCALE, OFFSET, DERCOM))
                    # save parameter name, initial value, and upper/lower bounds for Prior Information calcs later
                    paramInfo[PARNME] = [PARVAL1, PARLBND, PARUBND, PARGP]
        #
        if param == 'RESERCF':
            # adjust reservoir flow coefficients directly, similar to gate flow coeffs
            resList = []
            for rowNo in range(0, len(resCFDict['RES_NAME'])):
                PARVAL1 = float(resCFDict['COEF_IN'][rowNo])
                if PARVAL1 != 0.0:
                    PARLBND = PARVAL1 * 0.5
                    PARUBND = PARVAL1 * 1.5
                    fullName = 'RESCFIN:' + resCFDict['RES_NAME'][rowNo] + \
                               ':' + resCFDict['NODE'][rowNo]
                    PARNME = shortenName(resList, fullName, 12, 'RESCFIN:')
                    PCFId.write('%-12s %5s %s %10.3f %10.3f %10.3f %s %5.2f %5.2f %1d\n' % \
                                (PARNME, PARTRANS, PARCHGLIM, PARVAL1, PARLBND, PARUBND, PARGP, SCALE, OFFSET, DERCOM))
                    # save parameter name, initial value, and upper/lower bounds for Prior Information calcs later
                    paramInfo[PARNME] = [PARVAL1, PARLBND, PARUBND, PARGP]
                PARVAL1 = float(resCFDict['COEF_OUT'][rowNo])
                if PARVAL1 != 0.0:
                    PARLBND = PARVAL1 * 0.5
                    PARUBND = PARVAL1 * 1.5
                    fullName = 'RESCFOUT:' + resCFDict['RES_NAME'][rowNo] + \
                               ':' + resCFDict['NODE'][rowNo]
                    PARNME = shortenName(resList, fullName, 12, 'RESCFOUT:')
                    PCFId.write('%-12s %5s %s %10.3f %10.3f %10.3f %s %5.2f %5.2f %1d\n' % \
                                (PARNME, PARTRANS, PARCHGLIM, PARVAL1, PARLBND, PARUBND, PARGP, SCALE, OFFSET, DERCOM))
                    # save parameter name, initial value, and upper/lower bounds for Prior Information calcs later
                    paramInfo[PARNME] = [PARVAL1, PARLBND, PARUBND, PARGP]
        #
        if param == 'ELEV' or param == 'WIDTH':
            # cross-sections in channels;
            # instead of PEST adjusting the elevations or widths directly,
            # PEST will change coefficients for each cross-section which
            # will multiply the elevations or widths for each layer in a
            # pre-processor vscript before each DSM2 run.
            # Cross-section coeffs are per-channel, as are parameter groups.
            PARVAL1 = 1.0
            PARLBND = 0.6
            PARUBND = 1.5
            for rowNo in ChanDict['CHAN_NO']:
                chan3 = "%03d" % int(ChanDict['CHAN_NO'][rowNo])
                PARNME = param + chan3
                if useParamGroups:
                    try:
                        key = param + ' ' + chan3
                        PARTRANS = GroupChans[key][0].upper()
                        # save Tied channels for later section
                        if PARTRANS == 'TIED':
                            tiedParams[PARNME] = param + "%03d" % int(GroupChans[key][1])
                        # use group "NONE" for fixed parameters
                        if PARTRANS == 'FIXED':
                            PARGP = 'NONE'
                        else:
                            PARGP = param
                    except:
                        pass
                PCFId.write('%8s %5s %s %10.3f %10.3f %10.3f %s %5.2f %5.2f %1d\n' % \
                            (PARNME, PARTRANS, PARCHGLIM, PARVAL1, PARLBND, PARUBND, PARGP, SCALE, OFFSET, DERCOM))
                # create 'dummy' input/template files for x-sect calibration factors
                PARVAL1 = 1.0
                # PARVAL1 = random.uniform(PARLBND, PARUBND)
                PIXFId.write('%s %10.4f\n' % (PARNME, PARVAL1))
                PTXFId.write('%s %s\n' % (PARNME, '@' + PARNME + '  @'))
                # save parameter name, initial value, and upper/lower bounds for Prior Information calcs later,
                # if not Tied or Fixed
                if PARTRANS == 'NONE':
                    paramInfo[PARNME] = [PARVAL1, PARLBND, PARUBND, PARGP]
        if param == 'DIV-FLOW' or \
                param == 'DRAIN-FLOW':
            # these calibration parameters, being timeseries, will be updated by a pre-processor
            # vscript before each DSM2 run. 
            if param == 'DIV-FLOW':
                stdev3Up = 2.0
                stdev3Lo = 3.0
            if param == 'DRAIN-FLOW':
                stdev3Up = 4.0
                stdev3Lo = 4.0
            PARVAL1 = 1.0
            PARLBND = PARVAL1 / stdev3Lo
            PARUBND = PARVAL1 * stdev3Up
            for rowNo in range(len(HydroAgDict['NAME'])):
                try:
                    CPart = HydroAgDict['PATH'][rowNo].upper().split(fs)[3]
                except:
                    continue
                if CPart == param:
                    node3 = "%03d" % int(HydroAgDict['NODE'][rowNo])
                    if param == 'DRAIN-FLOW':
                        shortName = 'DRN-Q'
                    else:
                        shortName = 'DIV-Q'
                    PARGP = shortName  # shorten param group name to be less than 12 chars
                    PARNME = shortName + node3  # shorten param name to be less than 12 chars
                    if useParamGroups:
                        try:
                            key = shortName + ' ' + node3
                            PARTRANS = GroupNodes[key][0].upper()
                            # save Tied channels for later section
                            if PARTRANS == 'TIED':
                                tiedParams[PARNME] = shortName + "%03d" % int(GroupNodes[key][1])
                            # use group "NONE" for fixed parameters
                            if PARTRANS == 'FIXED':
                                PARGP = 'NONE'
                            else:
                                PARGP = shortName
                        except:
                            pass
                    PCFId.write('%8s %5s %s %10.3f %10.3f %10.3f %s %5.2f %5.2f %1d\n' % \
                                (PARNME, PARTRANS, PARCHGLIM, PARVAL1, PARLBND, PARUBND, PARGP, SCALE, OFFSET, DERCOM))
                    # create 'dummy' input/template files for Ag calibration factors
                    # PIAFId.write('%s %10.4f\n' % (PARNME,random.uniform(PARLBND, PARUBND)))
                    PIAFId.write('%s %10.4f\n' % (PARNME, 1.0))
                    PTAFId.write('%s %s\n' % (PARNME, '@' + PARNME + '     @'))
                    # save parameter name, initial value, and upper/lower bounds for Prior Information calcs later,
                    # if not Tied or Fixed
                    if PARTRANS == 'NONE':
                        paramInfo[PARNME] = [PARVAL1, PARLBND, PARUBND, PARGP]
        if param == 'DRAIN-EC':
            # similar to Div/Drain flows, but have to use rows from the input table
            stdev3 = 5.0
            PARVAL1 = 1.0
            PARLBND = PARVAL1 / stdev3
            PARUBND = PARVAL1 * stdev3
            for rowNo in range(len(QualAgDict['NAME'])):
                try:
                    CPart = QualAgDict['PATH'][rowNo].upper().split(fs)[3]
                except:
                    continue
                node3 = "%03d" % int(QualAgDict['NODE_NO'][rowNo])
                if CPart == param:
                    shortName = 'DRN-EC'
                    PARGP = shortName  # shorten param group name to be less than 12 chars
                    PARNME = shortName + node3
                    if useParamGroups:
                        try:
                            key = shortName + ' ' + node3
                            PARTRANS = GroupNodes[key][0].upper()
                            # save Tied channels for later section
                            if PARTRANS == 'TIED':
                                tiedParams[PARNME] = shortName + "%03d" % int(GroupNodes[key][1])
                            # use group "NONE" for fixed parameters
                            if PARTRANS == 'FIXED':
                                PARGP = 'NONE'
                            else:
                                PARGP = shortName
                        except:
                            pass
                    PCFId.write('%8s %5s %s %10.3f %10.3f %10.3f %s %5.2f %5.2f %1d\n' % \
                                (PARNME, PARTRANS, PARCHGLIM, PARVAL1, PARLBND, PARUBND, PARGP, SCALE, OFFSET, DERCOM))
                    # create 'dummy' input/template files for Ag calibration factors
                    #                        PIAFId.write('%s %10.4f\n' % (PARNME,random.uniform(PARLBND, PARUBND)))
                    PIAFId.write('%s %10.4f\n' % (PARNME, 1.0))
                    PTAFId.write('%s %s\n' % (PARNME, '@' + PARNME + '  @'))
                    # save parameter name, initial value, and upper/lower bounds for Prior Information calcs later,
                    # if not Tied or Fixed
                    if PARTRANS == 'NONE':
                        paramInfo[PARNME] = [PARVAL1, PARLBND, PARUBND, PARGP]
        # end of DRAIN-EC section
    # end of all parameter loop
    # write Tied section if needed
    if useParamGroups:
        for key, value in sorted(tiedParams.iteritems(), key=lambda (k, v): (v, k)):
            PCFId.write('%s %s\n' % (key, value))
    #
    if 'DIV-FLOW' in paramGroups or 'DRAIN-FLOW' in paramGroups or 'DRAIN-EC' in paramGroups:
        PIAFId.close()
        print 'Wrote', PIAFId.name
        PTAFId.close()
        print 'Wrote', PTAFId.name
    else:
        try:
            os.remove(PESTDir + PESTInpAgFile)
        except:
            pass
    if 'ELEV' in paramGroups or 'WIDTH' in paramGroups:
        PIXFId.close()
        print 'Wrote', PIXFId.name
        PTXFId.close()
        print 'Wrote', PTXFId.name
    else:
        try:
            os.remove(PESTDir + PESTInpXCFile)
        except:
            pass
    # Observed data groups
    PCFId.write('* observation groups\n')
    # Observed groups must also list prior information groups
    for obsGroup in obsGroups:
        PCFId.write('%s\n' % (obsGroup))
    for paramGroup in paramGroups:
        paramGrp = paramGroup.upper()
        if paramGrp == 'DRAIN-FLOW':
            paramGrp = 'DRN-Q'
        if paramGrp == 'DIV-FLOW':
            paramGrp = 'DIV-Q'
        if paramGrp == 'DRAIN-EC':
            paramGrp = 'DRN-EC'
        PCFId.write('%s\n' % (paramGrp + '_PI'))
    #
    # append the temp observed data file previously created
    OTF1Id = open(ObsTempFile1, 'r')
    PCFId.write('* observation data\n')
    PCFId.write(OTF1Id.read())
    OTF1Id.close()
    os.remove(ObsTempFile1)
    #
    # Model command line and I/O files
    PCFId.write('%s\n%s\n%s\n' % \
                ('* model command line', 'dsm2run.bat', '* model input/output'))
    if 'MANN' in paramGroups or \
            'DISP' in paramGroups or \
            'LENGTH' in paramGroups:
        PCFId.write('%s %s\n' % (PESTChanTplFile, ChanCalibFile))
    if 'GATECF' in paramGroups:
        PCFId.write('%s %s\n' % (PESTGateTplFile, GateCalibFile))
    if 'RESERCF' in paramGroups:
        PCFId.write('%s %s\n' % (PESTResTplFile, ResCalibFile))
    if 'DIV-FLOW' in paramGroups or 'DRAIN-FLOW' in paramGroups or 'DRAIN-EC' in paramGroups:
        PCFId.write('%s %s\n' % (PESTTplAgFile, PESTInpAgFile))
    if 'ELEV' in paramGroups or 'WIDTH' in paramGroups:
        PCFId.write('%s %s\n' % (PESTTplXCFile, PESTInpXCFile))
    PCFId.write('%s %s\n* prior information\n' % (PESTInsFile, DSM2OutFile))
    # write prior information
    # paramInfo[PARNME] = [value, lower, upper, group name]
    # make a sorted list of the parameter names so the .pst file is more readable
    paramList = []
    for p in paramInfo:
        paramList.append(p)
    paramList.sort()
    PIFAC = 1.0
    for PARNME in paramList:
        PILBL = PARNME + '_PI'
        PIVAL = paramInfo[PARNME][0]
        # for the Prior weight, assume the given lower/upper bounds of the parameters
        # are 3 Std Devs each from the mean.
        stDev = (paramInfo[PARNME][2] - paramInfo[PARNME][1]) / 6.
        WEIGHT = 1. / stDev
        OBGNME = paramInfo[PARNME][3] + '_PI'
        PCFId.write('%s %5.2f * %s = %15.5E %15.5E %s\n' % \
                    (PILBL, PIFAC, PARNME, PIVAL, WEIGHT, OBGNME))
    # write genie-only info to end of .pst file
    if typePEST == 'genie':
        PCFId.write('%s\n%s\n%s\n%s\n%s\n' % ( \
            '++ svd_pack(propack)', \
            '++ max_n_super(60) super_eigthres(1.e-10)', \
            '++ mat_inv(Q1/2J)', \
            '++# n_iter_base(1) n_iter_super(1)', \
            '++ lambdas(.01,0.05,0.1,0.5,1.,5.,10.)' \
            ))
    PCFId.close()
    print 'Wrote', PCFId.name
    ##
    ## Create PEST Template Files (.tpl)
    if 'MANN' in paramGroups or \
            'DISP' in paramGroups or \
            'LENGTH' in paramGroups:
        # DSM2 channel input template
        PTFId = open(PESTDir + PESTChanTplFile, 'w')
        DSM2InpId = open(PESTDir + ChanCalibFile, 'r')
        PTFId.write('ptf @\n')
        # read each line from the DSM2 grid input file;
        # for channel lines, replace Length, Manning, and Dispersion
        # with PEST placeholder names
        channelLines = False
        for line in DSM2InpId:
            if re.search('^ *END', line, re.I):
                # end of section
                channelLines = False
            if channelLines:
                if re.search('^ *#', line):  # comment line, continue
                    continue
                lineParts = line.split()
                # CHAN_NO  LENGTH  MANNING  DISPERSION  UPNODE  DOWNNODE
                chanNo = int(lineParts[0])
                chanLen = int(lineParts[1])
                chanMann = float(lineParts[2])
                chanDisp = float(lineParts[3])
                upNode = int(lineParts[4])
                downNode = int(lineParts[5])
                PTFId.write('%3d ' % (chanNo))
                if 'LENGTH' in paramGroups:
                    PTFId.write('@LENGTH%03d@  ' % (chanNo))
                else:
                    PTFId.write('%10d  ' % (chanLen))
                if 'MANN' in paramGroups:
                    PTFId.write('@MANN%03d     @  ' % (chanNo))
                else:
                    PTFId.write('%10.6f ' % (chanMann))
                if 'DISP' in paramGroups:
                    PTFId.write('@DISP%03d     @  ' % (chanNo))
                else:
                    PTFId.write('%10.3f ' % (chanDisp))
                PTFId.write('%5d %5d\n' % (upNode, downNode))
            else:
                PTFId.write(line)
            if re.search('CHAN_NO +LENGTH +MANNING +DISPERSION', line, re.I):
                # channel block header line, channel lines follow
                channelLines = True
        #            PTFId.write(line)
        PTFId.close()
        DSM2InpId.close()
        print 'Wrote', PTFId.name
    # DSM2 Gate input template
    if 'GATECF' in paramGroups:
        PTFId = open(PESTDir + PESTGateTplFile, 'w')
        DSM2InpId = open(CommonDir + GateInpFile, 'r')
        PTFId.write('ptf |\n')
        # read each line from the DSM2 gate input file;
        # for gate weir & pipe device lines, replace To and From flow coefficients 
        # with PEST placeholder names
        gateLines = False
        for line in DSM2InpId:
            lineParts = line.strip().upper().split()
            if re.search('^ *GATE_[A-Z]+_DEVICE *$', line, re.I):
                PTFId.write(line)
                # Pipe or Weir section?   
                if re.search('GATE_PIPE_DEVICE', line, re.I):
                    name = 'PIPE'
                if re.search('GATE_WEIR_DEVICE', line, re.I):
                    name = 'WEIR'
            if re.search('^ *END', line, re.I):
                # end of section
                if gateLines:
                    PTFId.write(line)
                    gateLines = False
            if gateLines:
                # GATE_NAME DEVICE NDUPLICATE (RADIUS|HEIGHT) ELEV CF_FROM_NODE CF_TO_NODE DEFAULT_OP
                gateName = lineParts[headersList.index('GATE_NAME')]
                devName = lineParts[headersList.index('DEVICE')]
                # accept only non-zero coeffs
                if float(lineParts[CF_FromLoc]) != 0.0:
                    # recreate shortened gate name
                    fullName = name + 'CFFR:' + gateName + ':' + devName
                    shortName = shortenName(gateList, fullName, 12, name + 'CFFR:')
                    lineParts[CF_FromLoc] = '|' + shortName + '      |'
                    # accept only non-zero coeffs
                if float(lineParts[CF_ToLoc]) != 0.0:
                    fullName = name + 'CFTO:' + gateName + ':' + devName
                    shortName = shortenName(gateList, fullName, 12, name + 'CFTO:')
                    lineParts[CF_ToLoc] = '|' + shortName + '      |'
                #
                for i in range(len(lineParts)):
                    PTFId.write('%s ' % lineParts[i])
                PTFId.write('\n')
            if re.search('GATE_NAME +.*CF_(FROM|TO)_NODE +.*CF_(FROM|TO)_NODE', line, re.I):
                # gate block header line, gate lines follow
                gateLines = True
                PTFId.write(line)
                headersList = lineParts
                # find which fields have the gate flow coeffs (CF_FROM_NODE and CF_TO_NODE)
                CF_FromLoc = headersList.index('CF_FROM_NODE')
                CF_ToLoc = headersList.index('CF_TO_NODE')
        PTFId.close()
        DSM2InpId.close()
        print 'Wrote', PTFId.name
    else:
        try:
            os.remove(PESTDir + PESTGateInpFile)
        except:
            pass
    if 'RESERCF' in paramGroups:
        # DSM2 Reservoir input template
        PTFId = open(PESTDir + PESTResTplFile, 'w')
        DSM2InpId = open(CommonDir + ResInpFile, 'r')
        PTFId.write('ptf |\n')
        # read each line from the DSM2 reservoir input file;
        # for reservoir coefficient lines, replace In and Out flow coefficients 
        # with PEST placeholder names
        resCFLines = False
        for line in DSM2InpId:
            lineParts = line.strip().upper().split()
            if re.search('^ *RESERVOIR_CONNECTION *$', line, re.I):
                PTFId.write(line)
            if re.search('^ *END', line, re.I):
                if resCFLines:
                    # end of reservoir coefficient lines
                    PTFId.write(line)
                    resCFLines = False
            if resCFLines:
                # RES_NAME NODE COEF_IN COEF_OUT
                resName = lineParts[headersList.index('RES_NAME')]
                resNode = lineParts[headersList.index('NODE')]
                # accept only non-zero coeffs
                if float(lineParts[CF_InLoc]) != 0.0:
                    shortName = shortenName(resList, 'RESCFIN:' + resName + ':' + resNode, 12, 'RESCFIN:')
                    lineParts[CF_InLoc] = '|' + shortName + '     |'
                if float(lineParts[CF_OutLoc]) != 0.0:
                    shortName = shortenName(resList, 'RESCFOUT:' + resName + ':' + resNode, 12, 'RESCFOUT:')
                    lineParts[CF_OutLoc] = '|' + shortName + '     |'
                for i in range(len(lineParts)):
                    PTFId.write('%s ' % lineParts[i])
                PTFId.write('\n')
            if re.search('RES_NAME +.*COEF_IN', line, re.I):
                # reservoir coefficient block header line, reservoir lines follow
                resCFLines = True
                PTFId.write(line)
                headersList = lineParts
                # find which fields have the reservoir flow coeffs (COEF_IN and COEF_OUT)
                CF_InLoc = headersList.index('COEF_IN')
                CF_OutLoc = headersList.index('COEF_OUT')
        PTFId.close()
        DSM2InpId.close()
        print 'Wrote', PTFId.name
    else:
        try:
            os.remove(PESTDir + PESTResInpFile)
        except:
            pass
    ##
    # Create the PEST_post_DSM2Run.py file for post-processing DSM2 calibration runs.
    # The post-processing generates text output of the DSS calibration stations,
    # and the PEST instruction (.ins) file. 
    preProcFile = 'PEST_pre_DSM2Run.py'
    postProcFile = 'PEST_post_DSM2Run.py'
    sq = "'"
    dq = '"'
    dataTypesStr = dq + '", "'.join(dataTypes) + dq
    WDSM2Id = open(PESTDir + postProcFile, 'w')
    WDSM2Id.write('import sys, os\n')
    WDSM2Id.write('from vtimeseries import *\n')
    WDSM2Id.write('from vdss import *\n')
    WDSM2Id.write('from vista.set import *\n')
    WDSM2Id.write('#from vista.db.dss import *\n')
    WDSM2Id.write('from vutils import *\n')
    WDSM2Id.write('from vista.time import TimeFactory\n')
    WDSM2Id.write('def writeText(filename,ds,outputFlags=False):\n')
    WDSM2Id.write('    """\n')
    WDSM2Id.write('    writeText(filename,ds,outputFlags=False)\n')
    WDSM2Id.write('    Writes the given data set to the given filename,\n')
    WDSM2Id.write('    using full precision available in the DSS file.\n')
    WDSM2Id.write('    """\n')
    WDSM2Id.write('    fid = open(filename,"w")\n')
    WDSM2Id.write('    fid.write(SetUtils.getHeader(ds).toString().replace("\\r",""))\n')
    WDSM2Id.write('    for i in range(len(ds)):\n')
    WDSM2Id.write('        dse = ds.getElementAt(i)\n')
    WDSM2Id.write('        fid.write("%s %15.10E\\n" % (dse.getXString(), dse.getY()))\n')
    WDSM2Id.write('    fid.close()\n')
    WDSM2Id.write('    return\n')
    WDSM2Id.write('#\n')
    WDSM2Id.write('if __name__ == "__main__":\n')
    WDSM2Id.write("    # This post-processor was generated by PEST_Create_Files.py\n" + \
                  "    # It translates DSM2 DSS output for calibration to a text file,\n" + \
                  "    # then generates the matching PEST instruction file for the output.\n" + \
                  "    # On initial PEST start, run this using the base dss output.\n")
    WDSM2Id.write("    TF = TimeFactory.getInstance()\n")
    WDSM2Id.write("    CalibDSSOutFile = sys.argv[1]\n")
    WDSM2Id.write("    TW = sys.argv[2]\n")
    WDSM2Id.write("    twList = []\n")
    WDSM2Id.write("    twList.append(TF.createTimeWindow(TW))\n")
    WDSM2Id.write("    tempfile = 'temp.out'\n")
    WDSM2Id.write("    try: fid = open(" + sq + DSM2OutFile + sq + ", 'w')\n")
    WDSM2Id.write("    except:\n")
    WDSM2Id.write("        print 'Error opening " + DSM2OutFile + "'\n")
    WDSM2Id.write("        raise\n")
    WDSM2Id.write("    for tw in twList:\n")
    WDSM2Id.write("        for dataType in [" + dataTypesStr + "]:\n")
    WDSM2Id.write("            dssgrp = opendss(CalibDSSOutFile)\n")
    WDSM2Id.write("            dssgrp.filterBy('/'+dataType+'/')\n")
    WDSM2Id.write("            for dssdr in dssgrp.getAllDataReferences():\n")
    WDSM2Id.write("                try:\n")
    WDSM2Id.write("                    dssdr = DataReference.create(dssdr,tw)\n")
    WDSM2Id.write("                    writeText(tempfile, dssdr.getData())\n")
    WDSM2Id.write("                except:\n")
    WDSM2Id.write("                    print 'Error with ',dssdr\n")
    WDSM2Id.write("                    raise\n")
    WDSM2Id.write("                tid = open(tempfile, 'r')\n")
    WDSM2Id.write("                fid.write(tid.read().replace('\t','    '))\n")
    WDSM2Id.write("                tid.close()\n")
    WDSM2Id.write("    #\n")
    WDSM2Id.write("    fid.close()\n")
    WDSM2Id.write("    print 'Wrote', fid.name\n")
    WDSM2Id.write("    if os.path.exists(tempfile):\n")
    WDSM2Id.write("       os.remove(tempfile)\n")
    WDSM2Id.write("    # generate PEST instruction (.ins) file\n")
    WDSM2Id.write("    fid = open('" + PESTInsFile + "', 'w')\n")
    WDSM2Id.write("    tid = open('" + DSM2OutFile + "', 'r')\n")
    WDSM2Id.write("    fid.write('pif @\\n')\n")
    WDSM2Id.write("    for line in tid:\n")
    WDSM2Id.write("        if re.search('^$', line):\n")
    WDSM2Id.write("            fid.write('@Units :@\\n')\n")
    WDSM2Id.write("            continue\n")
    WDSM2Id.write("        lineSplit = line.split()\n")
    WDSM2Id.write("        if line.find('Location: ') > -1:\n")
    WDSM2Id.write("            locStr = lineSplit[1].upper()\n")
    WDSM2Id.write("            continue\n")
    WDSM2Id.write("        if line.find('Type: ') > -1:\n")
    WDSM2Id.write("            typeStr = lineSplit[1].upper()\n")
    WDSM2Id.write("            typeStr = typeStr.replace('STAGE','STG').replace('FLOW','Q')\n")
    WDSM2Id.write("            continue\n")
    WDSM2Id.write(
        "        if re.search('^[0-9][0-9][A-Z][A-Z][A-Z][12][90][78901][0-9] [0-2][0-9][0-9][0-9][ \t]+[0-9eE.+-]+$',line) > -1:\n")
    WDSM2Id.write("            timeObj = TF.createTime(lineSplit[0]+' '+lineSplit[1])\n")
    WDSM2Id.write("            dateStr = timeObj.format(DefaultTimeFormat('yyyyMMdd'))\n")
    WDSM2Id.write("            timeStr = timeObj.format(DefaultTimeFormat('HHmm'))\n")
    WDSM2Id.write("            dataID = 'L1 (' + locStr + typeStr + dateStr + timeStr + ')16:80'\n")
    WDSM2Id.write("            fid.write(dataID + '\\n')\n")
    WDSM2Id.write("    fid.close()\n")
    WDSM2Id.write("    tid.close()\n")
    WDSM2Id.write("    print 'Wrote', fid.name\n")
    WDSM2Id.write("    sys.exit(0)\n")
    print 'Wrote', WDSM2Id.name
    WDSM2Id.close()
    #
    ## Create the run-time *.bat files for the Condor runs of PEST;
    ## this varies somewhat depending on type of PEST parallelization
    # Create batch file for Hydro and Qual runs under PEST
    WCONId = open(PESTDir + 'dsm2run.bat', 'w')
    WCONId.write("@echo off\n")
    WCONId.write("set VISTABINDIR=c:\\condor\\vista\\bin\\\n")
    WCONId.write("set PESTBINDIR=c:\\condor\\PEST\\bin\\\n")
    WCONId.write("setlocal\n")
    WCONId.write("for /f \"tokens=2 delims=[]\" %%f in ('ping -4 -n 1 %COMPUTERNAME% " +
                 "^| c:\Windows\system32\\find.exe /i \"pinging\"') do set IP=%%f\n")
    WCONId.write("echo Running on %COMPUTERNAME% (%IP%)\n")
    WCONId.write("echo  in directory %CD%\n")
    WCONId.write("echo  HTCondor cluster %HTCC% and process %HTCP%\n")
    WCONId.write("date /t\n")
    WCONId.write("time /t\n")
    WCONId.write("@copy /b/y " + DivRtnQFile + " " + DivRtnQFile.replace(".dss", "") + "-calib.dss\n")
    WCONId.write("@copy /b/y " + RtnECFile + " " + RtnECFile.replace(".dss", "") + "-calib.dss\n")
    WCONId.write("echo Running Pre-Processor\n")
    WCONId.write("call %VISTABINDIR%vscript.bat " + preProcFile + "\n")
    WCONId.write("if %ERRORLEVEL% NEQ 0 (\n")
    WCONId.write("echo Fatal Pre-Process ERRORLEVEL %ERRORLEVEL% 1>&2\n")
    WCONId.write("exit /b 1\n")
    WCONId.write(")\n")
    WCONId.write("if exist " + CalibDSSOutFile + " del /f/q " + CalibDSSOutFile + "\n")
    WCONId.write("set START_DATE=" + StartDateObj_Hydro.format().split()[0] + "\n")
    WCONId.write("set START_TIME=" + StartDateObj_Hydro.format().split()[1] + "\n")
    WCONId.write("set END_DATE=" + EndDateObj_Hydro.format().split()[0] + "\n")
    WCONId.write("set END_TIME=" + EndDateObj_Hydro.format().split()[1] + "\n")
    WCONId.write("set QUAL_START_DATE=" + StartDateObj_Qual.format().split()[0] + "\n")
    WCONId.write("set QUAL_END_DATE=" + EndDateObj_Qual.format().split()[0] + "\n")
    WCONId.write("set HYDRORSTFILE_IN=" + DSM2Mod + "-" + DSM2Run + ".hrf\n")
    WCONId.write("set QUALRSTFILE_IN=" + DSM2Mod + "-" + DSM2Run + ".qrf\n")
    WCONId.write("call :RUNDSM2\n")
    WCONId.write("set ERRLVL=%ERRORLEVEL%\n")
    WCONId.write("if %ERRLVL% NEQ 0 (\n")
    WCONId.write("echo DSM2 ERRORLEVEL %ERRLVL% 1>&2\n")
    WCONId.write("if exist " + ChanCalibFile + " xcopy /y/c/q " + ChanCalibFile + \
                 " " + bs + bs + "bdomo-002\\condor" + bs + "returnedFiles\n")
    WCONId.write("if exist " + XSectsCalibFile + " xcopy /y/c/q " + XSectsCalibFile + \
                 " " + bs + bs + "bdomo-002\\condor" + bs + "returnedFiles\n")
    WCONId.write("if exist " + PESTInpAgFile + " xcopy /y/c/q " + PESTInpAgFile + \
                 " " + bs + bs + "bdomo-002\\condor" + bs + "returnedFiles\n")
    WCONId.write("if %ERRLVL% EQU 97 GOTO FIN\n")
    WCONId.write("echo Fatal Error, exiting 1>&2\n")
    WCONId.write("exit /b 1\n")
    WCONId.write(")\n")
    WCONId.write("rem post-process to prepare output for PEST\n\n")
    WCONId.write("echo Running Post-Processor\n")
    WCONId.write("call %VISTABINDIR%vscript.bat " + postProcFile + \
                 " " + CalibDSSOutFile + " " + \
                 dq + CmpStartDateObj.format() + " - " + CmpEndDateObj.format() + dq + \
                 "\n")
    WCONId.write("if %ERRORLEVEL% NEQ 0 (\n")
    WCONId.write("ping -n 3 127.0.0.1 > nul\n")
    WCONId.write("echo Post-Process ERRORLEVEL %ERRORLEVEL%, trying once more 1>&2\n")
    WCONId.write("call %VISTABINDIR%vscript.bat " + postProcFile + \
                 " " + CalibDSSOutFile + " " + \
                 dq + CmpStartDateObj.format() + " - " + CmpEndDateObj.format() + dq + \
                 "\n")
    WCONId.write("if %ERRORLEVEL% NEQ 0 (\n")
    WCONId.write("echo Post-Process ERRORLEVEL %ERRORLEVEL%, 2nd & last try failed 1>&2\n")
    WCONId.write("date /t\n")
    WCONId.write("time /t\n")
    WCONId.write("exit /b 1\n")
    WCONId.write(")\n")
    WCONId.write(")\n")
    WCONId.write(":FIN\n")
    WCONId.write("rem Idiotic MS equivalent of touch\n")
    WCONId.write("copy /b dummy.txt +,,\n")
    #    WCONId.write("call %PESTBINDIR%pestchek.exe DSM2\n")
    #    WCONId.write("if ERRORLEVEL 1 exit /b 1\n")
    WCONId.write("date /t\n")
    WCONId.write("time /t\n")
    WCONId.write("exit /b 0\n")
    WCONId.write("\n")
    WCONId.write(":RUNDSM2\n")
    WCONId.write("echo Starting DSM2 on model date %START_DATE%\n")
    WCONId.write("ping -n 2 127.0.0.1 > nul\n")
    WCONId.write("date /t\n")
    WCONId.write("time /t\n")
    WCONId.write("del /f/q *.h5\n")
    WCONId.write("hydro.exe hydro.inp\n")
    WCONId.write("if %errorlevel% NEQ 0 (\n")
    WCONId.write("echo Hydro ERRORLEVEL %ERRORLEVEL% 1>&2\n")
    WCONId.write("echo Error in Hydro run. 1>&2\n")
    WCONId.write("xcopy /y/c/q hydro_echo_*.inp " + bs + bs + \
                 "bdomo-002\\condor" + bs + "returnedFiles\n")
    if contDSM2:
        WCONId.write("copy /y " + DSM2OutFile + ".base " + DSM2OutFile + "\n")
        WCONId.write("echo Returning base output file to PEST.\n")
        WCONId.write("exit /b 97\n")
    else:
        WCONId.write("echo Fatal error, exiting 1>&2\n")
        WCONId.write("exit /b 1\n")
    WCONId.write(")\n")
    WCONId.write("echo Hydro run OK\n")
    WCONId.write("ping -n 2 127.0.0.1 > nul\n")
    WCONId.write("date /t\n")
    WCONId.write("time /t\n")
    WCONId.write("qual.exe qual_ec.inp\n")
    WCONId.write("if %errorlevel% NEQ 0 (\n")
    WCONId.write("echo Qual ERRORLEVEL %ERRORLEVEL% 1>&2\n")
    WCONId.write("echo Error in Qual run. 1>&2\n")
    WCONId.write("xcopy /y/c/q qual_ec_echo_*.inp \\bdomo-002\condor" + \
                 bs + "returnedFiles\n")
    if contDSM2:
        WCONId.write("copy /y " + DSM2OutFile + ".base " + DSM2OutFile + "\n")
        WCONId.write("echo Returning base output file to PEST.\n")
        WCONId.write("exit /b 97\n")
    else:
        WCONId.write("echo Fatal error, exiting 1>&2\n")
        WCONId.write("exit /b 1\n")
    WCONId.write(")\n")
    WCONId.write("echo Qual run OK\n")
    WCONId.write("date /t\n")
    WCONId.write("time /t\n")
    WCONId.write("ping -n 5 127.0.0.1 > nul\n")
    WCONId.write("exit /b 0\n")
    WCONId.close()
    print 'Wrote', WCONId.name
    # Create the main .bat file to start a parallel PEST calibration.
    # This file is run once by the user for each calibration run; it copies necessary files,
    # does a warmup run (if needed), and starts the HTCondor submit jobs and the PEST console
    WDSM2Id = open(PESTDir + fileRun4, 'w')
    WDSM2Id.write("@echo off\n")
    WDSM2Id.write("Rem This file created by PEST_Create_Files.py\n")
    WDSM2Id.write("Rem Calibrate DSM2 using " + typePEST.upper() + " & HTCondor.\n")
    WDSM2Id.write("setlocal\n")
    WDSM2Id.write("set PESTDIR=" + PESTDir.replace("/", "\\") + "\n")
    WDSM2Id.write("set PESTDRV=" + re.sub(r'([a-zA-Z]:).*$', r'\1', PESTDir) + "\n")
    WDSM2Id.write("set BASERUNDIR=" + BaseRunDir.replace("/", "\\") + "\n")
    WDSM2Id.write("set HYDROEXE=" + RootDir.replace("/", "\\") + "hydro.exe\n")
    WDSM2Id.write("set QUALEXE=" + RootDir.replace("/", "\\") + "qual.exe\n")
    WDSM2Id.write("set CONDORBINDIR=c:\\condor\\bin\\\n")
    WDSM2Id.write("set VISTABINDIR=c:\\condor\\vista\\bin\\\n")
    WDSM2Id.write("set PESTBINDIR=c:\\condor\\PEST\\bin\\\n")
    WDSM2Id.write("set STUDYNAME=" + PESTFile.replace(".pst", "") + "\n")
    WDSM2Id.write("set RUNDIR=" + dq + condorDir.replace(fs, bs) + dq + "\n")
    WDSM2Id.write("set RUNDRV=" + re.sub(r'([a-zA-Z]:).*$', r'\1', condorDir) + "\n")
    if typePEST == 'genie':
        WDSM2Id.write("rem get this machine's IP number\n")
        WDSM2Id.write("for /f " + dq + "delims=[] tokens=2" + dq + \
                      " %%a in ('ping -4 %computername% -n 1 ^| findstr " + dq + "[" + dq + "') do (set MYIP=%%a)\n")
    WDSM2Id.write("\n")
    cmnFiles = glob.glob(CommonDir + '*.inp')
    WDSM2Id.write("set CMNFILES=")
    for f in cmnFiles:
        fn = os.path.basename(f)
        WDSM2Id.write(fn + ", ")
    WDSM2Id.write("\n")
    WDSM2Id.write("set TSFILES=" + DivRtnQFile + ", " + RtnECFile + ", " + \
                  "events.dss, gates-v8-06212012.dss, hist_19902012.dss\n")
    writeStr = "set PESTFILES=" + PESTFile + ", " + PESTInsFile
    if 'MANN' in paramGroups or \
            'DISP' in paramGroups or \
            'LENGTH' in paramGroups:
        writeStr += ", " + PESTChanTplFile
    if 'GATECF' in paramGroups:
        writeStr += ", " + PESTGateTplFile
    if 'RESERCF' in paramGroups:
        writeStr += ", " + PESTResTplFile
    if 'DIV-FLOW' in paramGroups or 'DRAIN-FLOW' in paramGroups or 'DRAIN-EC' in paramGroups:
        writeStr += ", " + PESTInpAgFile + ", " + PESTTplAgFile
    if 'ELEV' in paramGroups or 'WIDTH' in paramGroups:
        writeStr += ", " + PESTInpXCFile + ", " + PESTTplXCFile
    WDSM2Id.write(writeStr + "\n")
    WDSM2Id.write("\n")
    WDSM2Id.write("rem create a scratch directory for condor runs\n")
    WDSM2Id.write("\n")
    WDSM2Id.write("if not exist %RUNDIR% mkdir %RUNDIR%\n")
    WDSM2Id.write("del /q %RUNDIR%\n")
    WDSM2Id.write("if %ERRORLEVEL% NEQ 0 (\n")
    WDSM2Id.write("echo Cannot delete all files in Condor dir.\n")
    WDSM2Id.write("exit /b 1\n")
    WDSM2Id.write(")\n")
    WDSM2Id.write("\n")
    WDSM2Id.write("@copy /a " + dq + \
                  r"D:\workspace\vista\scripts\General Calibration\PEST_Calib_Functions.py" + dq + " %RUNDIR%\\\n")
    WDSM2Id.write("@copy /b %HYDROEXE% %RUNDIR%\\\n")
    WDSM2Id.write("@copy /b %QUALEXE% %RUNDIR%\\\n")
    WDSM2Id.write("@copy /a dsm2run.bat %RUNDIR%\\\n")
    WDSM2Id.write("@xcopy /q " + re.sub("/$", "", TimeSeriesDir).replace("/", bs) + " %RUNDIR%\\\n")
    WDSM2Id.write("@xcopy /q " + re.sub("/$", "", CommonDir).replace("/", bs) + " %RUNDIR%\\\n")
    WDSM2Id.write("\n")
    WDSM2Id.write("@copy /y " + preProcFile + " %RUNDIR%\\\n")
    WDSM2Id.write("@copy /y " + postProcFile + " %RUNDIR%\\\n")
    WDSM2Id.write("@copy /y *.rmf %RUNDIR%\\\n")
    WDSM2Id.write("@copy /y *.pst %RUNDIR%\\\n")
    WDSM2Id.write("@copy /y *.tpl %RUNDIR%\\\n")
    WDSM2Id.write("@copy /y *.inp %RUNDIR%\\\n")
    WDSM2Id.write("@copy /y dummy.txt %RUNDIR%\\\n")
    WDSM2Id.write("@copy /y " + fileSub + " %RUNDIR%\\dsm2.sub\n")
    WDSM2Id.write("\n")
    if useRestart:
        WDSM2Id.write("rem do the warmup run to create the restart file\n")
        WDSM2Id.write("cd %BASERUNDIR%\n")
        WDSM2Id.write("call " + BaseRunBatch + " " + warmupStartDateStr[:9] + " " + warmupEndDateStr[:9] + "\n")
        WDSM2Id.write("if ERRORLEVEL 1 exit /b 1\n")
    WDSM2Id.write("\n")
    WDSM2Id.write("%RUNDRV%\n")
    WDSM2Id.write("cd %RUNDIR%\n")
    WDSM2Id.write("\n")
    WDSM2Id.write("@copy /a/y " + BaseRunOutput.replace("/", "\\") + "*" + DSM2Run + ".?rf \n")
    WDSM2Id.write("\n")
    WDSM2Id.write("@ren config_calib.inp config.inp\n")
    if useRestart:
        WDSM2Id.write("@ren hydro_calib_restart.inp hydro.inp\n")
        WDSM2Id.write("@ren qual_ec_calib_restart.inp qual_ec.inp\n")
    else:
        WDSM2Id.write("@ren hydro_calib_cold.inp hydro.inp\n")
        WDSM2Id.write("@ren qual_ec_calib_cold.inp qual_ec.inp\n")
    WDSM2Id.write("Rem add calibration-specific input files\n")
    WDSM2Id.write("echo GRID >> hydro.inp\n")
    WDSM2Id.write("echo " + ChanCalibFile + " >> hydro.inp\n")
    # fixme: xs, gate, reser, and dicu params in 1st calib don't get loaded
    if 'ELEV' in paramGroups or 'WIDTH' in paramGroups:
        WDSM2Id.write("echo " + XSectsCalibFile + " >> hydro.inp\n")
    if 'GATECF' in paramGroups:
        WDSM2Id.write("echo " + GateCalibFile + " >> hydro.inp\n")
        WDSM2Id.write("@copy " + GateInpFile + " " + GateCalibFile + "\n")
    if 'RESERCF' in paramGroups:
        WDSM2Id.write("echo " + ResCalibFile + " >> hydro.inp\n")
        WDSM2Id.write("@copy " + ResInpFile + " " + ResCalibFile + "\n")
    WDSM2Id.write("echo END >> hydro.inp\n")
    WDSM2Id.write("rem Add calibration output to Hydro and Qual .inp files\n")
    WDSM2Id.write("echo OUTPUT_TIME_SERIES >> hydro.inp\n")
    WDSM2Id.write("echo " + DSM2DSSOutHydroFile + " >> hydro.inp\n")
    WDSM2Id.write("echo END >> hydro.inp\n")
    WDSM2Id.write("echo OUTPUT_TIME_SERIES >> qual_ec.inp\n")
    WDSM2Id.write("echo " + DSM2DSSOutQualFile + " >> qual_ec.inp\n")
    WDSM2Id.write("echo END >> qual_ec.inp\n")
    WDSM2Id.write("\n")
    WDSM2Id.write("rem finish Condor submit file for PEST\n")
    # minimum disk size is about 50MB + 500MB per year for DSM2 + PEST
    # Note: 476 MB is the highest usage I've seen for 1 year.
    # fixme: nYearsRun = runTSWin ??
    if useRestart:
        nYearsRun = 1.
    else:
        nYearsRun = 2.
    diskSize = 50 + 500 * nYearsRun
    WDSM2Id.write("echo request_disk = " + str(int(diskSize + 0.5)) + " MB >> %RUNDIR%\\dsm2.sub\n")
    if contDSM2:
        writeStr = "echo transfer_input_files = " + DSM2OutFile + ".base, "
    else:
        writeStr = "echo transfer_input_files = "
    writeStr += "dsm2run.bat, PEST_Calib_Functions.py, " \
                + preProcFile + ", " + postProcFile + ", " + DSM2OutFile + ", " \
                + DSM2DSSOutHydroFile + ", " + DSM2DSSOutQualFile + ", %PESTFILES%, " \
                + "hydro.exe, qual.exe, hydro.inp, qual_ec.inp, " + ChanCalibFile + ", " \
                + DSM2Mod + "-" + DSM2Run + ".hrf, " + DSM2Mod + "-" + DSM2Run + ".qrf, " \
                + " config.inp, dummy.txt"
    # fixme: account for prior calib run
    if 'GATECF' in paramGroups:
        writeStr += ", " + GateCalibFile
    if 'RESERCF' in paramGroups:
        writeStr += ", " + ResCalibFile
    writeStr += ", %CMNFILES% %TSFILES% >> %RUNDIR%\\dsm2.sub\n"
    WDSM2Id.write(writeStr)
    WDSM2Id.write("\n")
    # command and arguments depend on which PEST is to be used
    if typePEST == 'genie':
        WDSM2Id.write("echo executable = %PESTBINDIR%genie.exe >> %RUNDIR%\\dsm2.sub\n")
        WDSM2Id.write("echo arguments = /host %MYIP%:4004 /interval 1.0 /console pipe " + \
                      "/name slave-$(Cluster)-$(Process) >> %RUNDIR%\dsm2.sub\n")
    elif typePEST == 'beo':
        WDSM2Id.write("echo executable = %PESTBINDIR%BEOPEST64.exe >> %RUNDIR%\\dsm2.sub\n")
        WDSM2Id.write("echo arguments = %STUDYNAME% /H %COMPUTERNAME%:4004 >> %RUNDIR%\\dsm2.sub\n")
    WDSM2Id.write("echo queue " + str(NCondor) + " >> %RUNDIR%\\dsm2.sub\n")
    WDSM2Id.write("\n")
    WDSM2Id.write("rem do a single base run\n")
    WDSM2Id.write("call dsm2run.bat\n")
    WDSM2Id.write("if ERRORLEVEL 1 exit /b 1\n")
    WDSM2Id.write("ping -n 2 127.0.0.1 > nul\n")
    WDSM2Id.write("rem use the base run output as a substitute for a dsm2 run failure\n")
    WDSM2Id.write("@copy /b/y " + DSM2OutFile + " " + DSM2OutFile + ".base\n")
    WDSM2Id.write("\n")
    WDSM2Id.write("ping -n 2 127.0.0.1 > nul\n")
    WDSM2Id.write("%CONDORBINDIR%condor_submit dsm2.sub\n")
    WDSM2Id.write("if ERRORLEVEL 1 exit /b 1\n")
    WDSM2Id.write("echo Submitted " + typePEST + " slave jobs to condor.\n")
    WDSM2Id.write("REM delay a few seconds before running the main PEST programs\n")
    WDSM2Id.write("ping -n 5 127.0.0.1 > nul\n")
    if typePEST == 'genie':
        WDSM2Id.write("start /min cmd /c %PESTBINDIR%pest++.exe %STUDYNAME% /G %MYIP%:4004\n")
        #             WDSM2Id.write("start /min cmd /c %PESTBINDIR%pest++.exe %STUDYNAME% /G %MYIP%:4004 ^> pest++.out\n")
        WDSM2Id.write("%PESTBINDIR%genie.exe /port 4004 /ip %MYIP% /NFAIL 2 /RUNTIME 10\n")
        WDSM2Id.write("%PESTDRV%\n")
    elif typePEST == 'beo':
        # WDSM2Id.write("start %PESTBINDIR%BEOPEST64.exe %STUDYNAME% /H :4004 ^> beo.out\n")
        WDSM2Id.write("call %PESTBINDIR%BEOPEST64.exe %STUDYNAME% /H :4004\n")
    WDSM2Id.write("time /t\n")
    #
    WDSM2Id.close()
    print 'Wrote', WDSM2Id.name
    print
    print 'Summary', datetime.today().strftime("%Y-%b-%d %H:%M:%S")
    print 'NOPTMAX:', NOPTMAX, 'useParamGroups:', useParamGroups, 'adjStage:', adjStage
    print 'priorCalibDir:', priorCalibDir, 'useRestart:', useRestart, 'contDSM2:', contDSM2
    print 'UniformInitialManning:', UniformInitialManning
    print 'paramGroups:', paramGroups
    print 'useObsDataTypes:', useObsDataTypes
    print 'typePEST:', typePEST, 'cmpDataDate:', cmpDataDate, 'CVStr:', CVStr
    print 'Processed observed data over time period', CmpTW.toString()
    print 'HTCondor directory', condorDir
    sys.exit(0)
#
