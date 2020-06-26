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

package gov.ca.water.calgui.busservice.impl;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.LocalDate;
import java.time.Month;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.logging.Level;
import java.util.regex.Pattern;
import java.util.stream.Stream;

import com.google.common.flogger.FluentLogger;
import gov.ca.water.calgui.EpptInitializationException;
import gov.ca.water.calgui.bo.WaterYearDefinition;
import gov.ca.water.calgui.bo.WaterYearIndexModel;
import gov.ca.water.calgui.bo.WaterYearPeriod;
import gov.ca.water.calgui.bo.WaterYearType;
import gov.ca.water.calgui.constant.Constant;
import gov.ca.water.calgui.project.EpptDssContainer;
import gov.ca.water.calgui.project.EpptScenarioRun;
import gov.ca.water.calgui.project.NamedDssPath;
import gov.ca.water.calgui.scripts.DssCache;
import gov.ca.water.calgui.scripts.DssReader;
import gov.ca.water.calgui.techservice.impl.FilePredicates;

import hec.heclib.dss.DSSPathname;
import hec.io.TimeSeriesContainer;

import static java.util.stream.Collectors.toList;

public class WaterYearTableReader
{
	private static final FluentLogger LOGGER = FluentLogger.forEnclosingClass();
	private static final Pattern WYTPES_TABLE_SPLIT = Pattern.compile("\\s+");
	private static final Map<EpptScenarioRun, List<WaterYearIndexModel>> MODELS = new HashMap<>();

	private final Path _waterYearTypeTable;
	private final EpptScenarioRun _epptScenarioRun;
	private final WaterYearNameLookup _waterYearNameLookup;
	private final WaterYearIndexModelReader _waterYearIndexModelReader;


	public WaterYearTableReader(EpptScenarioRun epptScenarioRun) throws EpptInitializationException
	{
		_epptScenarioRun = epptScenarioRun;
		_waterYearTypeTable = _epptScenarioRun.getLookupDirectory().resolve(Constant.WY_TYPES_TABLE);
		_waterYearNameLookup = new WaterYearNameLookup();
		_waterYearIndexModelReader = new WaterYearIndexModelReader(_epptScenarioRun);
	}

	public List<WaterYearIndexModel> forceRead() throws EpptInitializationException
	{
		MODELS.remove(_epptScenarioRun);
		return read();
	}

	public List<WaterYearIndexModel> read() throws EpptInitializationException
	{
		if(MODELS.get(_epptScenarioRun) == null)
		{
			LOGGER.at(Level.FINE).log("Reading Water Year Type Table: %s", _waterYearTypeTable);
			Map<Integer, WaterYearIndexModel> dssWaterYearIndexModels = readDssFile();
			List<WaterYearIndexModel> tableWaterYearIndexModels = readTableFile();
			tableWaterYearIndexModels.forEach(m -> dssWaterYearIndexModels.merge(m.getWaterYearIndexId(), m, (o1, o2) -> o1));
			List<WaterYearIndexModel> models = dssWaterYearIndexModels.values()
																			.stream()
																			.sorted(Comparator.comparing(WaterYearIndexModel::getWaterYearIndexId))
																			.collect(toList());
			MODELS.put(_epptScenarioRun, models);
		}
		return MODELS.get(_epptScenarioRun);
	}

	private Map<Integer, WaterYearIndexModel> readDssFile()
	{
		Map<Integer, WaterYearIndexModel> retval = new HashMap<>();
		Map<Integer, DSSPathname> waterYearIndexIds = _waterYearIndexModelReader.getWaterYearIndexIds();
		for(Map.Entry<Integer, DSSPathname> entry : waterYearIndexIds.entrySet())
		{
			DSSPathname value = entry.getValue();
			Integer waterYearIndexId = entry.getKey();
			DSSGrabber1SvcImpl dssGrabber1Svc = new DSSGrabber1SvcImpl();
			dssGrabber1Svc.setDssPathname(value);
			EpptDssContainer original = _epptScenarioRun.getDssContainer();
			NamedDssPath copyDvFile = cloneWithNewAPart(original.getDvDssFile(), value.getAPart());
			NamedDssPath copySvFile = cloneWithNewAPart(original.getSvDssFile(), value.getAPart());
			NamedDssPath copyIvFile = cloneWithNewAPart(original.getIvDssFile(), value.getAPart());
			NamedDssPath copyDtsFile = cloneWithNewAPart(original.getDtsDssFile(), value.getAPart());
			List<NamedDssPath> copiedExtraDssFiles = original.getExtraDssFiles().stream().map(f -> cloneWithNewAPart(f, value.getAPart())).collect(toList());
			EpptDssContainer dssContainer = new EpptDssContainer(copyDvFile, copySvFile, copyIvFile, copyDtsFile, copiedExtraDssFiles);
			EpptScenarioRun copyEpptScenarioRun = new EpptScenarioRun(_epptScenarioRun.getName(), _epptScenarioRun.getDescription(), _epptScenarioRun.getModel(),
					_epptScenarioRun.getOutputPath(), _epptScenarioRun.getWreslDirectory(), _epptScenarioRun.getLookupDirectory(), dssContainer, _epptScenarioRun.getColor());
			dssGrabber1Svc.setScenarioRuns(copyEpptScenarioRun, Collections.emptyList());
			dssGrabber1Svc.setDateRange(LocalDate.of(1850, java.time.Month.JANUARY, 1),
					LocalDate.of(2150, Month.JANUARY, 1));
			TimeSeriesContainer[] primarySeries = dssGrabber1Svc.getPrimarySeries();
			if(primarySeries != null && primarySeries[0] != null)
			{
				Month month = _waterYearIndexModelReader.getMonth(waterYearIndexId);
				DssReader dssReader = new DssReader(_epptScenarioRun, new WaterYearDefinition("", month, month, 1, 1), new DssCache());
				List<WaterYearType> waterYearTypes = dssReader.timeSeriesContainerToMap(primarySeries, false)
															  .entrySet()
															  .stream()
															  .filter(s -> s.getKey().minusMonths(1).getMonth() == month)
															  .map(e -> new WaterYearType(e.getKey().minusMonths(1).getYear(),
																	  getWaterYearPeriod(waterYearIndexId, e.getValue().intValue())))
															  .collect(toList());
				getWaterYearIndexName(waterYearIndexId).ifPresent(s -> retval.put(waterYearIndexId, new WaterYearIndexModel(waterYearIndexId, s, waterYearTypes)));
			}
		}
		return retval;
	}

	private NamedDssPath cloneWithNewAPart(NamedDssPath dssFile, String aPart)
	{
		if(dssFile != null)
		{
			if(aPart == null || aPart.isEmpty())
			{
				aPart = dssFile.getAPart();
			}
			return new NamedDssPath(dssFile.getDssPath(), dssFile.getAliasName(), aPart, dssFile.getEPart(), dssFile.getFPart());
		}
		else
		{
			return null;
		}
	}

	private List<WaterYearIndexModel> readTableFile() throws EpptInitializationException
	{
		List<WaterYearIndexModel> retval = new ArrayList<>();
		try(Stream<String> lines = Files.lines(_waterYearTypeTable))
		{
			List<String[]> collect = lines.filter(l -> !l.isEmpty())
										  .filter(FilePredicates.commentFilter())
										  .map(WYTPES_TABLE_SPLIT::split)
										  .filter(l -> l.length > 1)
										  .collect(toList());
			if(!collect.isEmpty())
			{
				String[] headers = collect.get(0);
				for(int i = 1; i < headers.length; i++)
				{
					List<WaterYearType> waterYearTypes = new ArrayList<>();
					String header = headers[i];
					Optional<Integer> waterYearIndexIdOpt = _waterYearIndexModelReader.getIndexIdForTableHeader(header);
					if(waterYearIndexIdOpt.isPresent())
					{
						int waterYearIndexId = waterYearIndexIdOpt.get();
						Optional<String> waterYearIndexNameOpt = getWaterYearIndexName(waterYearIndexId);
						if(waterYearIndexNameOpt.isPresent())
						{
							String waterYearIndexName = waterYearIndexNameOpt.get();
							WaterYearIndexModel waterYearIndexModel = createModelForRow(collect, i, waterYearTypes, waterYearIndexId, waterYearIndexName);
							retval.add(waterYearIndexModel);
						}
					}
				}
			}
		}
		catch(IOException e)
		{
			throw new EpptInitializationException("Error reading the table file: " + _waterYearTypeTable.toAbsolutePath(), e);
		}
		return retval;
	}

	private Optional<String> getWaterYearIndexName(int waterYearIndexId)
	{
		return WaterYearIndexReader.getInstance()
								   .getWaterYearIndexDefinitions()
								   .stream()
								   .filter(s -> s.matchesModel(waterYearIndexId))
								   .findAny()
								   .map(WaterYearIndexReader.WaterYearIndexDefinition::getDisplayName);
	}

	private WaterYearIndexModel createModelForRow(List<String[]> collect, int columnIndex, List<WaterYearType> waterYearTypes, int waterYearIndexId, String waterYearIndexName)
	{
		for(int j = 1; j < collect.size(); j++)
		{
			String[] row = collect.get(j);
			if(columnIndex < row.length)
			{
				int waterYearCode = Integer.parseInt(row[columnIndex]);
				WaterYearPeriod waterYearPeriod = getWaterYearPeriod(waterYearIndexId, waterYearCode);
				int year = Integer.parseInt(row[0]);
				waterYearTypes.add(new WaterYearType(year, waterYearPeriod));
			}
		}
		List<WaterYearPeriod> waterYearPeriods = _waterYearNameLookup.getSortedWaterYearPeriods(waterYearIndexId);
		waterYearTypes.sort(Comparator.comparingInt(t -> waterYearPeriods.indexOf(t.getWaterYearPeriod())));
		return new WaterYearIndexModel(waterYearIndexId, waterYearIndexName, waterYearTypes);
	}

	private WaterYearPeriod getWaterYearPeriod(int waterYearIndexId, int waterYearCode)
	{
		return new WaterYearPeriod(_waterYearNameLookup.getWaterYearType(waterYearCode, waterYearIndexId));
	}
}
