/*
 * Enhanced Post Processing Tool (EPPT) Copyright (c) 2020.
 *
 *  EPPT is copyrighted by the State of California, Department of Water Resources. It is licensed
 *  under the GNU General Public License, version 2. This means it can be
 *  copied, distributed, and modified freely, but you may not restrict others
 *  in their ability to copy, distribute, and modify it. See the license below
 *  for more details.
 *
 *  GNU General Public License
 */

package gov.ca.water.reportengine.detailedissues;

import java.time.LocalDateTime;
import java.time.Month;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.NavigableMap;
import java.util.Objects;
import java.util.TreeMap;
import java.util.logging.Level;

import com.google.common.flogger.FluentLogger;
import gov.ca.water.calgui.EpptInitializationException;
import gov.ca.water.calgui.bo.DetailedIssue;
import gov.ca.water.calgui.bo.GUILinksAllModelsBO;
import gov.ca.water.calgui.bo.ThresholdLinksBO;
import gov.ca.water.calgui.bo.WaterYearDefinition;
import gov.ca.water.calgui.bo.WaterYearIndexModel;
import gov.ca.water.calgui.bo.WaterYearType;
import gov.ca.water.calgui.busservice.impl.GuiLinksSeedDataSvcImpl;
import gov.ca.water.calgui.busservice.impl.ThresholdLinksSeedDataSvc;
import gov.ca.water.calgui.busservice.impl.WaterYearTableReader;
import gov.ca.water.calgui.project.EpptScenarioRun;
import gov.ca.water.calgui.scripts.DssCache;
import gov.ca.water.calgui.scripts.DssMissingRecordException;
import gov.ca.water.calgui.scripts.DssReader;
import gov.ca.water.reportengine.EpptReportException;
import gov.ca.water.reportengine.executivereport.FlagViolation;
import gov.ca.water.reportengine.executivereport.Module;
import gov.ca.water.reportengine.executivereport.SubModule;
import gov.ca.water.reportengine.standardsummary.StandardSummaryErrors;

import hec.lang.Const;

import static java.util.stream.Collectors.toList;

public class DetailedIssueProcessor
{

	private static final FluentLogger LOGGER = FluentLogger.forEnclosingClass();
	private final StandardSummaryErrors _standardSummaryErrors;
	private final Map<EpptScenarioRun, Map<SubModule, List<FlagViolation>>> _runsToViolations;
	private final List<Module> _modules;
	private final List<DetailedIssue> _allDetailedIssues;
	private final List<EpptScenarioRun> _runs;
	private final boolean _isCFS;

	public DetailedIssueProcessor(StandardSummaryErrors standardSummaryErrors,
								  Map<EpptScenarioRun, Map<SubModule, List<FlagViolation>>> runsToViolations, List<Module> modules,
								  List<DetailedIssue> allDetailedIssues, List<EpptScenarioRun> runs, boolean isCFS)
	{
		_standardSummaryErrors = standardSummaryErrors;
		_runsToViolations = runsToViolations;
		_modules = modules;
		_allDetailedIssues = allDetailedIssues;
		_runs = runs;
		_isCFS = isCFS;
	}

	public Map<EpptScenarioRun, Map<Module, List<DetailedIssueViolation>>> process() throws EpptReportException
	{
		Map<EpptScenarioRun, Map<Module, List<DetailedIssueViolation>>> moduleToDIV = new HashMap<>();

		for(EpptScenarioRun run : _runs)
		{
			if(_runsToViolations.containsKey(run))
			{
				LOGGER.at(Level.INFO).log("Processing Detailed Issues for Scenario Run: %s", run.getName());
				Map<Module, List<DetailedIssueViolation>> modToDIVs = new HashMap<>();
				Map<SubModule, List<FlagViolation>> subModToViolations = _runsToViolations.get(run);
				try
				{
					WaterYearTableReader wyTypeReader = new WaterYearTableReader(run);
					List<WaterYearIndexModel> waterYearTypes = wyTypeReader.read();
					for(Module mod : _modules)
					{
						if(!Objects.equals("Model Inputs", mod.getName()))
						{

							LOGGER.at(Level.INFO).log("Processing Detailed Issues for Module: %s", mod.getName());
							List<DetailedIssueViolation> detailedIssueViolations = processModule(run, subModToViolations, mod,
									waterYearTypes.get(0).getWaterYearTypes());
							modToDIVs.put(mod, detailedIssueViolations);
						}
					}
					moduleToDIV.put(run, modToDIVs);
				}
				catch(EpptInitializationException e)
				{
					throw new EpptReportException("Unable to process water year types from: " + run.getLookupDirectory(), e);
				}
			}
			else
			{
				LOGGER.at(Level.INFO).log("No Detailed Issues for: %s", run.getName());
			}
		}

		return moduleToDIV;
	}

	private List<DetailedIssueViolation> processModule(EpptScenarioRun run,
													   Map<SubModule, List<FlagViolation>> subModToViolations, Module mod,
													   List<WaterYearType> waterYearTypes)
	{
		List<DetailedIssueViolation> dIVsForMod = new ArrayList<>();

		for(SubModule subMod : mod.getSubModules())
		{
			if(subModToViolations.containsKey(subMod))
			{
				LOGGER.at(Level.INFO).log("Processing Detailed Issues for Sub-Module: %s", subMod.getName());
				dIVsForMod.addAll(subModToViolations.get(subMod)
													.stream()
													.map(violation -> processFlagViolation(run, violation, subMod, waterYearTypes))
													.filter(Objects::nonNull)
													.collect(toList()));
			}
			else
			{
				LOGGER.at(Level.INFO).log("No Detailed Issues for: %s", subMod.getName());
			}
		}
		return dIVsForMod;
	}

	private DetailedIssueViolation processFlagViolation(EpptScenarioRun run, FlagViolation violation, SubModule subMod,
														List<WaterYearType> waterYearTypes)
	{
		DetailedIssueViolation div = null;
		DetailedIssue di = getDetailedIssueThatMatchViolation(violation);
		if(di != null)
		{
			div = createDetailedIssueViolation(violation, di.getGuiLink(), di.getThresholdLink(), run, subMod, waterYearTypes);
		}
		return div;
	}


	private DetailedIssueViolation createDetailedIssueViolation(FlagViolation violation, int guiID, int thresholdID, EpptScenarioRun run,
																SubModule subMod,
																List<WaterYearType> waterYearTypesFromFile)
	{
		//create title
		GUILinksAllModelsBO guiLink = null;
		if(Const.isValid(guiID))
		{
			guiLink = GuiLinksSeedDataSvcImpl.getSeedDataSvcImplInstance().getGuiLink(Integer.toString(guiID));
		}
		ThresholdLinksBO thresholdLink = null;
		if(Const.isValid(thresholdID))
		{
			thresholdLink = ThresholdLinksSeedDataSvc.getSeedDataSvcImplInstance().getObjById(thresholdID);
		}
		String title = subMod.getTitle();
		if(guiLink != null)
		{
			String guiLinkTitle = guiLink.getPlotTitle();
			if(guiLinkTitle.trim().isEmpty())
			{
				guiLinkTitle = violation.getDtsFileName();
			}
			title = title.replace("%title%", guiLinkTitle);
		}
		else
		{
			title = title.replace("%title%", violation.getDtsFileName());
		}
		if(thresholdLink != null)
		{
			title = title.replace("%Threshold Label%", thresholdLink.getLabel());
		}

		if(Objects.equals("", title))
		{
			title = "Undefined";
		}
		DssReader dssReader = new DssReader(run, new WaterYearDefinition("", Month.OCTOBER, Month.SEPTEMBER, 1, 1), new DssCache());
		NavigableMap<LocalDateTime, Double> guiLinkData = new TreeMap<>();
		String valueUnits = "";
		try
		{
			//get the specific container for the threshold values
			guiLinkData = dssReader.getGuiLinkData(guiID, !_isCFS);
			valueUnits = dssReader.getUnits();
		}
		catch(DssMissingRecordException e)
		{
			_standardSummaryErrors.addError(LOGGER,"Unable to process value for flagged violation", e);
		}
		NavigableMap<LocalDateTime, Double> thresholdData = new TreeMap<>();
		String standardUnits = "";
		try
		{
			//get the specific container for the values
			thresholdData = dssReader.getThresholdData(thresholdID, !_isCFS);
			standardUnits = dssReader.getUnits();
		}
		catch(DssMissingRecordException e)
		{
			_standardSummaryErrors.addError(LOGGER,"Unable to process threshold value for flagged violation", e);
		}
		LOGGER.at(Level.INFO).log("Processing violations from: %s", violation.getDtsFileName());
		List<LocalDateTime> violationTimes = violation.getTimes();
		if(violationTimes.size() > 10)
		{
			LOGGER.at(Level.INFO).log("Showing first 10 violations. Total violation count: %s", violationTimes.size());
		}
		List<LocalDateTime> times = violationTimes
				.stream()
				.limit(10)
				.collect(toList());
		Map<LocalDateTime, Double> actualValues = getActualValues(times, guiLinkData);
		Map<LocalDateTime, Double> thresholdValues = getThresholdValues(times, thresholdData);
		Map<LocalDateTime, String> waterYearTypes = getWaterYearTypes(times, waterYearTypesFromFile);
		return new DetailedIssueViolation(times, title, actualValues, thresholdValues, waterYearTypes, valueUnits, standardUnits,
				violation.getTimes().size());
	}

	private Map<LocalDateTime, Double> getActualValues(List<LocalDateTime> times, NavigableMap<LocalDateTime, Double> data)
	{
		Map<LocalDateTime, Double> values = new HashMap<>();
		for(LocalDateTime time : times)
		{
			Double value = data.getOrDefault(time, Double.NaN);
			if(value != null)
			{
				values.put(time, value);
			}
		}
		return values;
	}

	private Map<LocalDateTime, Double> getThresholdValues(List<LocalDateTime> times, NavigableMap<LocalDateTime, Double> data)
	{
		Map<LocalDateTime, Double> thresholds = new HashMap<>();
		for(LocalDateTime time : times)
		{
			Double threshold = data.getOrDefault(time, Double.NaN);
			if(threshold != null)
			{
				thresholds.put(time, threshold);
			}
		}
		return thresholds;
	}

	private Map<LocalDateTime, String> getWaterYearTypes(List<LocalDateTime> times,
														 List<WaterYearType> waterYearTypesFromFile)
	{
		Map<LocalDateTime, String> types = new HashMap<>();
		for(LocalDateTime time : times)
		{
			String waterYearTypeString = "Undefined";
			for(WaterYearType wyt : waterYearTypesFromFile)
			{
				if(wyt.getYear() == time.minusMonths(1).getYear())
				{
					waterYearTypeString = wyt.getWaterYearPeriod().toString();
					break;
				}
			}
			types.put(time, waterYearTypeString);
		}

		return types;
	}

	private DetailedIssue getDetailedIssueThatMatchViolation(FlagViolation violation)
	{
		for(DetailedIssue di : _allDetailedIssues)
		{
			if(Objects.equals(di.getLinkedVar(), violation.getDtsFileName()))
			{
				return di;
			}
		}
		return null;
	}

}
