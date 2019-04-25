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

package gov.ca.water.reportengine.executivereport;

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

    /**
     * Gets the violations from the dssFile and puts them into the correct subModule
     *
     * @param modules
     * @param dssFile
     * @throws Exception
     */
    public void readViolationsFromDssFile(List<Module> modules, Path dssFile, int scenarioNumber) throws Exception
    {
        _modules.addAll(modules);
        HecDss hD = HecDss.open(dssFile.toString());

        for (Module mod : _modules)
        {

            if("CoordinatedOperationsAgreement".equalsIgnoreCase(mod.getName()))
            {
                setMaxValueForCOAModuleFromDssFile(mod,dssFile,scenarioNumber);
                continue;
            }

            List<SubModule> subModules = mod.getSubModules();
            for (SubModule sm : subModules)
            {
                List<FlagViolation> violations = new ArrayList<>();
                List<String> linkedRecords = sm.getLinkedRecords();
                SubModule.FlagType flagValue = sm.getFlagValue();
                for (String lr : linkedRecords)
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
                        if (!catalogedPathnames.isEmpty())
                        {
                            dssPath = catalogedPathnames.get(0).toString();
                        }
                        TimeSeriesContainer result = (TimeSeriesContainer) hD.get(dssPath, true);

                        double[] values = result.values;
                        int[] times = result.times;

                        violations = getRowsThatHaveTheFlagValue(values, times, flagValue);

                    }
                    catch (Exception e)
                    {
                        throw new ExecutiveReportException("Error reading dssFile during executive report generation: " + dssFile.toString(), e);
                    }

                }
                if (scenarioNumber == 1)
                {
                    sm.addBaseViolations(violations);
                }
                else
                {
                    sm.addAlternativeViolations("alt" + scenarioNumber, violations);
                }
            }
        }
    }


    public void setMaxValueForCOAModuleFromDssFile(Module mod, Path dssFile, int scenarioNumber) throws Exception
    {
        HecDss hD = HecDss.open(dssFile.toString());


        List<SubModule> subModules = mod.getSubModules();
        FlagViolation violation = null;
        for (SubModule sm : subModules)
        {
            List<String> linkedRecords = sm.getLinkedRecords();
            for (String lr : linkedRecords)
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
                    if (!catalogedPathnames.isEmpty())
                    {
                        dssPath = catalogedPathnames.get(0).toString();
                    }
                    else
                    {
                        throw new ExecutiveReportException("Error reading dssFile during executive report generation: " + dssFile.toString()
                        + ". Could not find any records with name: " + dssPath);
                    }
                    TimeSeriesContainer result = (TimeSeriesContainer) hD.get(dssPath, true);

                    double[] values = result.values;

                    double maxValue = getMaxValue(values);

                    violation = new FlagViolation(maxValue);

                }
                catch (Exception e)
                {
                    throw new ExecutiveReportException("Error reading dssFile during executive report generation: " + dssFile.toString(), e);
                }

            }
            if (scenarioNumber == 1)
            {
                List<FlagViolation> violations = new ArrayList<>();
                violations.add(violation);
                sm.addBaseViolations(violations);
            }
            else
            {
                List<FlagViolation> violations = new ArrayList<>();
                violations.add(violation);
                sm.addAlternativeViolations("alt" + scenarioNumber, violations);
            }
        }

    }

    private double getMaxValue(double[] numbers)
    {
        double maxValue = numbers[0];
        for (int i = 1; i < numbers.length; i++)
        {
            if (numbers[i] > maxValue)
            {
                maxValue = numbers[i];
            }
        }
        return maxValue;
    }

    //are the values and the time arrays always going to be the same length?
    private List<FlagViolation> getRowsThatHaveTheFlagValue(double[] vals, int[] times, SubModule.FlagType flagType)
    {
        List<FlagViolation> violationRows = new ArrayList<>();
        for (int i = 0; i < vals.length; i++)
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

}

