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

import java.nio.file.Path;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Vector;
import java.util.logging.Level;

import com.google.common.flogger.FluentLogger;
import gov.ca.water.calgui.project.EpptScenarioRun;
import gov.ca.water.reportengine.EpptReportException;

import hec.heclib.dss.DSSPathname;
import hec.heclib.dss.HecDss;
import hec.heclib.util.HecTime;
import hec.heclib.util.HecTimeArray;
import hec.io.TimeSeriesContainer;
import hec.lang.Const;

public class DTSProcessor
{
	private static final int ID_COLUMN = 0;
	private static final int MODULE_COLUMN = 1;
	private static final int SUB_MODULE_COLUMN = 2;
	//private static final int LINKED_VARS_COLUMN = 99;
	private static final int FLAG_COLUMN = 4;
	private final List<Module> _modules = new ArrayList<>();


	public DTSProcessor(List<Module> modules)
	{
		_modules.addAll(modules);

	}

	public Map<EpptScenarioRun, Map<SubModule, List<FlagViolation>>> processDSSFiles(List<EpptScenarioRun> runs, List<Path> dssFiles)
			throws EpptReportException
	{
		//once we know how to get the post process dss files from the run then we can get rid of the dss files parameter.

		if(runs.size() != dssFiles.size())
		{
			throw new EpptReportException(
					"Different number of DSS files as runs. Number of runs: " + runs.size() + " Number of DSS files: " + dssFiles.size() + ".");
		}
		Map<EpptScenarioRun, Map<SubModule, List<FlagViolation>>> runToViolations = new HashMap<>();


		for(int i = 0; i < runs.size(); i++)
		{
			Map<SubModule, List<FlagViolation>> subModuleToViolations = processDSSFile(dssFiles.get(i));
			runToViolations.put(runs.get(i), subModuleToViolations);
		}
		return runToViolations;
	}

	/**
	 * Gets the violations from the dssFile and puts them into the correct subModule
	 *
	 * @param dssFile
	 * @throws Exception
	 */
	private Map<SubModule, List<FlagViolation>> processDSSFile(Path dssFile) throws EpptReportException
	{
		Map<SubModule, List<FlagViolation>> subModToViolations = new HashMap<>();
		HecDss hD = null;
		try
		{
			hD = HecDss.open(dssFile.toString());

			for(Module mod : _modules)
			{

				if("COA".equalsIgnoreCase(mod.getName()))
				{
					Map<SubModule, List<FlagViolation>> subModuleListMap = setMaxValueForCOAModuleFromDssFile(mod, hD);
					for(Map.Entry<SubModule, List<FlagViolation>> entry : subModuleListMap.entrySet())
					{
						subModToViolations.put(entry.getKey(), entry.getValue());
					}
					continue;
				}

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


						String dssPath = pathName.toString();
						try
						{

							Vector catalogedPathnames = hD.getCatalogedPathnames(dssPath);
							if(!catalogedPathnames.isEmpty())
							{
								dssPath = catalogedPathnames.get(0).toString();
							}
							TimeSeriesContainer result = (TimeSeriesContainer) hD.get(dssPath, true);

							FlagViolation flagViolationFromRecord = createFlagViolationFromRecord(result, flagValue, lr);
							if(flagViolationFromRecord != null)
							{
								violations.add(flagViolationFromRecord);
							}

						}
						catch(Exception e)
						{
							if(e.getMessage() != null && e.getMessage().contains("Unable to recognize record"))
							{
								FluentLogger.forEnclosingClass().at(Level.INFO).withCause(e).log("Skipping record: %s from file: %s", dssPath,
										dssFile);
							}
							else
							{
								throw new ExecutiveReportException("Error reading dssFile during executive report generation: " + dssFile.toString(),
										e);
							}
						}

					}
					subModToViolations.put(sm, violations);
				}
			}
		}
		catch(Exception e)
		{
			throw new EpptReportException("Unable to open dssFile: " + dssFile, e);
		}
		finally
		{
			if(hD != null)
			{
				hD.close();
			}
		}
		return subModToViolations;
	}


	private Map<SubModule, List<FlagViolation>> setMaxValueForCOAModuleFromDssFile(Module mod, HecDss hD) throws EpptReportException
	{
		Map<SubModule, List<FlagViolation>> subModToViolations = new HashMap<>();
		try
		{
			List<SubModule> subModules = mod.getSubModules();
			FlagViolation violation = null;
			for(SubModule sm : subModules)
			{
				List<String> linkedRecords = sm.getLinkedRecords();
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
							TimeSeriesContainer result = (TimeSeriesContainer) hD.get(dssPath, true);

							double[] values = result.values;

							double maxValue = getMaxValue(values);

							violation = new FlagViolation(maxValue, lr);

							List<FlagViolation> violations = new ArrayList<>();
							violations.add(violation);
							subModToViolations.put(sm, violations);
						}
						else
						{
							throw new ExecutiveReportException("Error reading dssFile during executive report generation: " + hD.getFilename()
									+ " Could not find record with name: " + dssPath);
						}

					}
					catch(ExecutiveReportException ex)
					{
						throw ex;
					}
					catch(Exception e)
					{
						throw new ExecutiveReportException("Error reading dssFile during executive report generation: " + hD.getFilename(), e);
					}

				}


			}
		}
		catch(Exception e)
		{
			throw new EpptReportException("Unable to open DSS file: " + hD.getFilename(), e);
		}
		finally
		{
			if(hD != null)
			{
				hD.close();
			}
		}
		return subModToViolations;
	}

	private double getMaxValue(double[] numbers)
	{
		double maxValue = numbers[0];
		for(int i = 1; i < numbers.length; i++)
		{
			if(numbers[i] > maxValue)
			{
				maxValue = numbers[i];
			}
		}
		return maxValue;
	}

	//are the values and the time arrays always going to be the same length?
	private FlagViolation createFlagViolationFromRecord(TimeSeriesContainer tsc, SubModule.FlagType flagType, String linkedVar)
	{
		List<HecTime> violationTimes = new ArrayList<>();
		double[] vals = tsc.values;
		HecTimeArray times = tsc.getTimes();
		for(int i = 0; i < times.numberElements(); i++)
		{
			HecTime hecTime = times.elementAt(i);
			int value = (int) Math.round(tsc.getValue(hecTime));
			if(Const.isValid(value))
			{
				switch(value)
				{
					case 0:
					{
						if(flagType == SubModule.FlagType.ZERO)
						{
							violationTimes.add(hecTime);
						}
						break;
					}
					case 1:
					{
						if(flagType == SubModule.FlagType.ONE)
						{
							violationTimes.add(hecTime);
						}
						break;
					}
					case 2:
					{
						if(flagType == SubModule.FlagType.TWO)
						{
							violationTimes.add(hecTime);
						}
						break;
					}
				}

			}
		}


		if(violationTimes.isEmpty())
		{
			return null;
		}
		else
		{
			return new FlagViolation(violationTimes, linkedVar);
		}
	}


	//read the csv file and create modules and sub modules
	//    private void readCSVFile(Path moduleCSVPath, Path moduleLinkingCSVPath) throws ExecutiveReportException
	//    {
	//        String line = "";
	//        String csvSplitBy = ",";
	//
	//        try (BufferedReader br = Files.newBufferedReader(moduleCSVPath))
	//        {
	//
	//            //skip first line
	//            int i = 0;
	//            while ((line = br.readLine()) != null)
	//            {
	//                if (i == 0)
	//                {
	//                    i++;
	//                    continue;
	//                }
	//                // use comma as separator
	//                String[] row = line.split(csvSplitBy);
	//                String modName = row[MODULE_COLUMN];
	//                Module mod = findModuleByName(modName);
	//                if (mod == null)
	//                {
	//                    //create a new module object
	//                    mod = new Module(modName);
	//                    _modules.add(mod);
	//                }
	//               // int modID = Integer.parseInt(row[ID_COLUMN]);
	//                //List<String> linkedFiles = getLinkedFilesFromID(modID, moduleLinkingCSVPath);
	//                updateSubModules(mod, row);
	//            }
	//
	//            //we have finished creating all the mods and sub mods. Now add the linked variables to the submods
	//            addLinkedVariablesToSubModules(moduleLinkingCSVPath);
	//        }
	//        catch (IOException e)
	//        {
	//            throw new ExecutiveReportException("Error reading the csv file: " + moduleCSVPath, e);
	//        }
	//    }
	//
	//    private void addLinkedVariablesToSubModules(Path moduleLinkingCSVPath) throws ExecutiveReportException
	//    {
	//        for(Module mod: _modules)
	//        {
	//            for(SubModule sub: mod.getSubModules())
	//            {
	//                int id = sub.getId();
	//                List<String> linkedFiles = getLinkedFilesFromID(id, moduleLinkingCSVPath);
	//                sub.addLinkedRecords(linkedFiles);
	//            }
	//        }
	//    }
	//
	//    private void updateSubModules(Module mod, String[] rowValues)
	//    {
	//        if(mod != null)
	//        {
	//            List<SubModule> subModules = mod.getSubModules();
	//            for (SubModule subMod : subModules)
	//            {
	//                if (Objects.equals(subMod.getName(), rowValues[SUB_MODULE_COLUMN]))
	//                {
	//                    //the submodule already exists
	//                    //subMod.addLinkedRecord(linkedValues);
	//                    return;
	//                }
	//            }
	//            //if we get here, then this is a new module
	//            int modID = Integer.parseInt(rowValues[ID_COLUMN]);
	//            String name = rowValues[SUB_MODULE_COLUMN];
	//
	//            SubModule subMod = new SubModule(modID,name , convertStringToFlagType(rowValues[FLAG_COLUMN]));
	//            //subMod.addLinkedRecord(rowValues[LINKED_VARS_COLUMN]);
	//            subModules.add(subMod);
	//        }
	//    }
	//
	//    private List<String> getLinkedFilesFromID(int id, Path moduleLinkingCSVPath) throws ExecutiveReportException
	//    {
	//        String line = "";
	//        String csvSplitBy = ",";
	//        List<String> retval = new ArrayList<>();
	//        try (BufferedReader br = Files.newBufferedReader(moduleLinkingCSVPath))
	//        {
	//            //skip first line
	//            int i = 0;
	//            while ((line = br.readLine()) != null)
	//            {
	//                if (i == 0)
	//                {
	//                    i++;
	//                    continue;
	//                }
	//                String[] row = line.split(csvSplitBy);
	//                int rowID = Integer.parseInt(row[0]);
	//                if(rowID == id)
	//                {
	//                    retval.add(row[1]);
	//                }
	//            }
	//        }
	//        catch (IOException e)
	//        {
	//            throw new ExecutiveReportException("Error reading the csv file: " + moduleLinkingCSVPath, e);
	//        }
	//        return retval;
	//    }
	//
	//    private Module findModuleByName(String modName)
	//    {
	//        for (Module mod : _modules)
	//        {
	//            if (mod.getName().equals(modName))
	//            {
	//                return mod;
	//            }
	//        }
	//        return null;
	//    }
	//
	//
	//
	//    private SubModule.FlagType convertStringToFlagType(String flag)
	//    {
	//        SubModule.FlagType retVal = SubModule.FlagType.NEGATIVE_INFINITY;
	//        if ("0".equalsIgnoreCase(flag))
	//        {
	//            retVal = SubModule.FlagType.ZERO;
	//        }
	//        else if ("1".equalsIgnoreCase(flag))
	//        {
	//            retVal = SubModule.FlagType.ONE;
	//        }
	//        else if ("2".equalsIgnoreCase(flag))
	//        {
	//            retVal = SubModule.FlagType.TWO;
	//        }
	//        return retVal;
	//    }
	//
	//    List<Module> getModules()
	//    {
	//        return new ArrayList<>( _modules);
	//    }


}

