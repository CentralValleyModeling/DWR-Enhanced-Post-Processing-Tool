/*
 * Enhanced Post Processing Tool (EPPT) Copyright (c) 2019.
 *
 * EPPT is copyrighted by the State of California, Department of Water Resources. It is licensed
 * under the GNU General Public License, version 2. This means it can be
 * copied, distributed, and modified freely, but you may not restrict others
 * in their ability to copy, distribute, and modify it. See the license below
 * for more details.
 *
 * GNU General Public License
 */

package executivereport;

import gov.ca.water.reportdata.executivereport.ExecutiveReport;
import gov.ca.water.reportdata.executivereport.ModelInputs;
import hec.heclib.dss.CondensedReference;
import hec.heclib.dss.DSSPathname;
import hec.heclib.dss.HecDss;
import hec.io.TimeSeriesContainer;

import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Vector;

public class ExecutiveReportProcessor
{

    private final List<Module> _modules = new ArrayList<>();

    public void processFile(List<Module> modules, Path dssFile) throws Exception
    {
        //ExecutiveReportsLinks erLinks = new ExecutiveReportsLinks();
        //erLinks.readCSVFile(csvFile);
        _modules.addAll(modules);
        HecDss hD = HecDss.open(dssFile.toString());

        for(Module mod : _modules)
        {

            List<SubModule> subModules = mod.getSubModules();
            for(SubModule sm : subModules)
            {
                List<FlagViolation> violations = new ArrayList<>();
                List<String> linkedRecords = sm.getLinkedRecords();
                SubModule.FlagType flagValue = sm.getFlagValue();
                for(String lr : linkedRecords)
                {

                    DSSPathname pathName = new DSSPathname();
                    pathName.setAPart("*");
                    pathName.setBPart(lr);
                    pathName.setCPart("*");
                    pathName.setDPart("*");
                    pathName.setEPart("*");
                    pathName.setFPart("*");


                    try
                    {
                        String dssPath = pathName.toString();

                        Vector catalogedPathnames = hD.getCatalogedPathnames(dssPath);
                        if(!catalogedPathnames.isEmpty())
                        {
                           dssPath = catalogedPathnames.get(0).toString();
                        }
                        TimeSeriesContainer result = (TimeSeriesContainer) hD.get(dssPath, true);

                        double[] values = result.values;
                        int[] times = result.times;

                        violations = getRowsThatHaveTheFlagValue(values, times, flagValue);

                    }
                    catch (IllegalArgumentException e)
                    {
                        e.printStackTrace();
                    }
                    catch (Exception e)
                    {
                        e.printStackTrace();
                    }

                }
                sm.addAllViolations(violations);
            }
        }
        createExucutiveReport();
    }


 //are the values and the time arrays always going to be the same length?
    private List<FlagViolation> getRowsThatHaveTheFlagValue(double[] vals, int[] times, SubModule.FlagType flagType)
    {
        List<FlagViolation> violationRows = new ArrayList<>();
        for(int i = 0;i<vals.length;i++)
        {
            int value = (int) vals[i];
            switch (value)
            {
                case 0:
                {
                    if (flagType == SubModule.FlagType.ZERO)
                    {
                        violationRows.add(new FlagViolation(times[i]));
                    }
                    break;
                }
                case 1:
                {
                    if (flagType == SubModule.FlagType.ONE)
                    {
                        violationRows.add(new FlagViolation(times[i]));
                    }
                    break;
                }
                case 2:
                {
                    if (flagType == SubModule.FlagType.TWO)
                    {
                        violationRows.add(new FlagViolation(times[i]));
                    }
                    break;
                }
            }
        }

        return violationRows;
    }


    public void createExucutiveReport()
    {
        ExecutiveReportScenario scenario = new ExecutiveReportScenario(_modules);
//        ExecutiveReport er = new ExecutiveReport(null, mis, resOps, mifs, nods, ngp,
//                deltaOps, new ArrayList<>(), new ArrayList<>(), new ArrayList<>());
    }

//    private List<ModelInputs> createModelInputs(Module module)
//    {
////        module.getViolations();
////        List<ModelInputs> mis = new ArrayList<>();
////        ModelInputs mi = new ModelInputs()
//
//    }


}

