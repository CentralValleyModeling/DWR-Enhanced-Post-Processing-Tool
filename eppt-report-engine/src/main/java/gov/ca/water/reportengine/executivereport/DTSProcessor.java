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
	private static final FluentLogger LOGGER = FluentLogger.forEnclosingClass();
	private final List<Module> _modules = new ArrayList<>();


	public DTSProcessor(List<Module> modules)
	{
		_modules.addAll(modules);
	}

	public Map<EpptScenarioRun, Map<SubModule, List<FlagViolation>>> processDSSFiles(List<EpptScenarioRun> runs)
			throws EpptReportException
	{
		List<Path> dssFiles = new ArrayList<>();
		for(EpptScenarioRun run : runs)
		{
			dssFiles.add(run.getPostProcessDss());
		}
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
	 * @param dssFile Path to dss file with dts records
	 * @throws Exception
	 */
	private Map<SubModule, List<FlagViolation>> processDSSFile(Path dssFile) throws EpptReportException
	{
		LOGGER.at(Level.INFO).log("Processing DTS Records in file %s", dssFile);
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
					List<FlagViolation> violations = getViolations(dssFile, hD, sm);
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

	private List<FlagViolation> getViolations(Path dssFile, HecDss hD, SubModule sm) throws ExecutiveReportException
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
		return violations;
	}


	private Map<SubModule, List<FlagViolation>> setMaxValueForCOAModuleFromDssFile(Module mod, HecDss hD) throws EpptReportException
	{
		Map<SubModule, List<FlagViolation>> subModToViolations = new HashMap<>();
		try
		{
			List<SubModule> subModules = mod.getSubModules();
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

					createViolationWithMaxValue(hD, subModToViolations, sm, lr, pathName);
				}
			}
		}
		catch (ExecutiveReportException e)
		{
			throw new EpptReportException("Unable to open DSS file: " + hD.getFilename(), e);
		}
		finally
		{
			if (hD != null)
			{
				hD.close();
			}
		}
		return subModToViolations;
	}

	private void createViolationWithMaxValue(HecDss hD, Map<SubModule, List<FlagViolation>> subModToViolations, SubModule sm, String lr, DSSPathname pathName) throws ExecutiveReportException
	{
		FlagViolation violation;
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

	private FlagViolation createFlagViolationFromRecord(TimeSeriesContainer tsc, SubModule.FlagType flagType, String linkedVar)
	{
		List<HecTime> violationTimes = new ArrayList<>();
		HecTimeArray times = tsc.getTimes();
		for(int i = 0; i < times.numberElements(); i++)
		{
			addTimeIfInViolation(tsc, flagType, violationTimes, times, i);
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

	private void addTimeIfInViolation(TimeSeriesContainer tsc, SubModule.FlagType flagType, List<HecTime> violationTimes, HecTimeArray times, int i)
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

}

