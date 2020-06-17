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

package gov.ca.water.plotly;

import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.time.Month;
import java.util.ArrayList;
import java.util.EnumMap;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.NavigableMap;
import java.util.TreeMap;

import gov.ca.water.calgui.EpptInitializationException;
import gov.ca.water.calgui.busservice.impl.WaterYearDefinitionSvc;
import gov.ca.water.calgui.constant.Constant;
import gov.ca.water.calgui.project.EpptScenarioRun;
import gov.ca.water.plotly.qaqc.PlotlyBubble;
import gov.ca.water.plotly.qaqc.PlotlyExceedance;
import gov.ca.water.plotly.qaqc.PlotlyExceedancePage;
import gov.ca.water.plotly.qaqc.PlotlyMonthly;

import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 08-19-2019
 */
public class TestPlotlyChartsToSvg
{
	static
	{
		String original = System.getProperty("user.dir");
		System.setProperty("user.dir", original + "\\target\\test-classes");
		try
		{
			WaterYearDefinitionSvc.createSeedDataSvcImplInstance();
		}
		catch(EpptInitializationException e)
		{
			e.printStackTrace();
		}
	}

	//	@Test
	public void testPlotlyBubbleToSvgFromTemplate() throws Exception
	{
		Path path = Paths.get("TestBubbleFromTemplate.svg");
		Files.deleteIfExists(path);
		EpptScenarioRun base = new EpptScenarioRun(null, null, null, null, null, null, null, Constant.getPlotlyDefaultColor(0));
		EpptScenarioRun alt = new EpptScenarioRun(null, null, null, null, null, null, null, Constant.getPlotlyDefaultColor(1));
		Map<EpptScenarioRun, PlotlyBubble.BubbleData> alternativeData = new HashMap<>();
		alternativeData.put(alt, buildBubbleData());
		PlotlyBubble bubble = new PlotlyBubble("CVP San Luis vs CVP NOD EOS Carryover Storage", "CVP North of Delta", "CVP San Luis", base, buildBubbleData(),
				alternativeData);
		PlotlySvgPrinter.printJsonToPath(path, bubble);
		PlotlySvgPrinter.printSvg(path.toAbsolutePath().getParent());
		assertTrue(Files.exists(path));
		Files.deleteIfExists(path);
		//		Files.deleteIfExists(Paths.get(path.toString().replace("svg", "json")));
	}

	//	@Test
	public void testPlotlyExceedanceToSvgFromTemplate() throws Exception
	{
		Path path = Paths.get("TestExceedanceFromTemplate.svg");
		Files.deleteIfExists(path);
		EpptScenarioRun base = new EpptScenarioRun(null, null, null, null, null, null, null, Constant.getPlotlyDefaultColor(0));
		EpptScenarioRun alt = new EpptScenarioRun(null, null, null, null, null, null, null, Constant.getPlotlyDefaultColor(1));
		Map<EpptScenarioRun, ExceedanceData> alternativeData = new HashMap<>();
		alternativeData.put(alt, buildAltExceedanceData());
		PlotlyExceedance bubble = new PlotlyExceedance("CVP San Luis vs CVP NOD EOS Carryover Storage", "CVP North of Delta", "CVP San Luis", base, buildBaseExceedanceData(),
				alternativeData);
		PlotlySvgPrinter.printJsonToPath(path, bubble);
		PlotlySvgPrinter.printSvg(path.toAbsolutePath().getParent());
		assertTrue(Files.exists(path));
		Files.deleteIfExists(path);
		//		Files.deleteIfExists(Paths.get(path.toString().replace("svg", "json")));
	}

	//	@Test
	public void testPlotlyMonthlyToSvgFromTemplate() throws Exception
	{
		Path path = Paths.get("TestMonthlyFromTemplate.svg");
		Files.deleteIfExists(path);
		EpptScenarioRun base = new EpptScenarioRun("base", null, null, null, null, null, null, Constant.getPlotlyDefaultColor(0));
		EpptScenarioRun alt = new EpptScenarioRun("alt", null, null, null, null, null, null, Constant.getPlotlyDefaultColor(1));
		Map<EpptScenarioRun, List<PlotlyMonthly.MonthlyData>> primaryData = buildPrimaryMonthlyData(base, alt);
		Map<EpptScenarioRun, Map<String, List<PlotlyMonthly.MonthlyData>>> thresholdData = buildThresholdMonthlyData(base, alt);
		PlotlyMonthly bubble = new PlotlyMonthly("Trinity Imports", "", "Storage", primaryData, thresholdData);
		PlotlySvgPrinter.printJsonToPath(path, bubble);
		PlotlySvgPrinter.printSvg(path.toAbsolutePath().getParent());
		assertTrue(Files.exists(path));
		Files.deleteIfExists(path);
		//		Files.deleteIfExists(Paths.get(path.toString().replace("svg", "json")));
	}

	private Map<EpptScenarioRun, List<PlotlyMonthly.MonthlyData>> buildPrimaryMonthlyData(EpptScenarioRun base, EpptScenarioRun alt)
	{
		Map<EpptScenarioRun, List<PlotlyMonthly.MonthlyData>> retval = new HashMap<>();
		for(Month month : Month.values())
		{
			retval.computeIfAbsent(base, (k) -> new ArrayList<>()).add(new PlotlyMonthly.MonthlyData(month, Math.random() * 100));
			retval.computeIfAbsent(alt, (k) -> new ArrayList<>()).add(new PlotlyMonthly.MonthlyData(month, Math.random() * 100));
		}
		return retval;
	}

	private Map<EpptScenarioRun, Map<String, List<PlotlyMonthly.MonthlyData>>> buildThresholdMonthlyData(EpptScenarioRun base, EpptScenarioRun alt)
	{
		Map<EpptScenarioRun, Map<String, List<PlotlyMonthly.MonthlyData>>> retval = new HashMap<>();
		Map<String, List<PlotlyMonthly.MonthlyData>> baseThresholds = retval.computeIfAbsent(base, (k) -> new HashMap<>());
		List<PlotlyMonthly.MonthlyData> baseThreshold = new ArrayList<>();
		baseThresholds.put("BaseThresh", baseThreshold);
		Map<String, List<PlotlyMonthly.MonthlyData>> altThresholds = retval.computeIfAbsent(alt, (k) -> new HashMap<>());
		List<PlotlyMonthly.MonthlyData> altThreshold = new ArrayList<>();
		altThresholds.put("AltThresh", altThreshold);
		for(Month month : Month.values())
		{
			baseThreshold.add(new PlotlyMonthly.MonthlyData(month, Math.random() * 100));
			altThreshold.add(new PlotlyMonthly.MonthlyData(month, Math.random() * 100));
		}
		return retval;
	}

	//	@Test
	public void testPlotlyExceedancePageToSvgFromTemplate() throws Exception
	{
		Path path = Paths.get("TestExceedancePageFromTemplate.svg");
		Files.deleteIfExists(path);
		EpptScenarioRun base = new EpptScenarioRun("Base", null, null, null, null, null, null, Constant.getPlotlyDefaultColor(0));
		EpptScenarioRun alt = new EpptScenarioRun("Alt", null, null, null, null, null, null, Constant.getPlotlyDefaultColor(1));

		Map<EpptScenarioRun, PlotlyExceedancePage.ExceedanceMonthData> exceedanceData = new HashMap<>();
		exceedanceData.put(base, buildBaseExceedanceMonthData());
		exceedanceData.put(alt, buildAltExceedanceMonthData());
		PlotlyExceedancePage exceedancePage = new PlotlyExceedancePage("CVP San Luis vs CVP NOD EOS Carryover Storage", "CVP North of Delta", exceedanceData);
		PlotlySvgPrinter.printJsonToPath(path, exceedancePage);
		PlotlySvgPrinter.printSvg(path.toAbsolutePath().getParent());
		assertTrue(Files.exists(path));
		//		Files.deleteIfExists(path);
		//		Files.deleteIfExists(Paths.get(path.toString().replace("svg", "json")));
	}

	private PlotlyExceedancePage.ExceedanceMonthData buildBaseExceedanceMonthData()
	{
		EnumMap<Month, ExceedanceData> data = new EnumMap<>(Month.class);
		for(Month month : Month.values())
		{
			data.put(month, buildBaseExceedanceData());
		}
		return new PlotlyExceedancePage.ExceedanceMonthData(data);
	}

	private PlotlyExceedancePage.ExceedanceMonthData buildAltExceedanceMonthData()
	{
		EnumMap<Month, ExceedanceData> data = new EnumMap<>(Month.class);
		for(Month month : Month.values())
		{
			data.put(month, buildAltExceedanceData());
		}
		return new PlotlyExceedancePage.ExceedanceMonthData(data);
	}

	private ExceedanceData buildBaseExceedanceData()
	{
		NavigableMap<Double, Double> primaryData = new TreeMap<>();
		primaryData.put(.99, 1000.0);
		primaryData.put(.88, 1111.0);
		primaryData.put(.78, 1200.0);
		primaryData.put(.23, 1900.0);
		primaryData.put(.11, 2500.0);
		primaryData.put(.06, 3000.0);
		primaryData.put(.02, 5000.0);
		Map<String, NavigableMap<Double, Double>> thresholdData = new HashMap<>();
		NavigableMap<Double, Double> threshold = new TreeMap<>();
		threshold.put(.99, 1200.0);
		threshold.put(.88, 2000.0);
		threshold.put(.78, 3000.0);
		threshold.put(.23, 4000.0);
		threshold.put(.11, 5000.0);
		threshold.put(.06, 5100.0);
		threshold.put(.01, 5200.0);
		thresholdData.put("BaseThresh", threshold);
		return new ExceedanceData("Base", primaryData, thresholdData);
	}

	private ExceedanceData buildAltExceedanceData()
	{
		NavigableMap<Double, Double> primaryData = new TreeMap<>();
		primaryData.put(.95, 1000.0);
		primaryData.put(.82, 1233.0);
		primaryData.put(.71, 1455.0);
		primaryData.put(.62, 3600.0);
		primaryData.put(.29, 4200.0);
		primaryData.put(.19, 5100.0);
		primaryData.put(.15, 6010.0);
		Map<String, NavigableMap<Double, Double>> thresholdData = new HashMap<>();
		NavigableMap<Double, Double> threshold = new TreeMap<>();
		threshold.put(.99, 4000.0);
		threshold.put(.88, 4000.0);
		threshold.put(.78, 4000.0);
		threshold.put(.23, 4000.0);
		threshold.put(.11, 4000.0);
		threshold.put(.56, 4000.0);
		threshold.put(.1, 4000.0);
		thresholdData.put("AltThresh", threshold);
		return new ExceedanceData("Alt", primaryData, thresholdData);
	}

	private PlotlyBubble.BubbleData buildBubbleData()
	{
		List<Double> xArray = new ArrayList<>();
		xArray.add(1001.0);
		xArray.add(1003.0);
		xArray.add(2005.0);
		xArray.add(3005.0);
		xArray.add(4050.0);
		xArray.add(5505.0);
		xArray.add(6050.0);
		List<Double> yArray = new ArrayList<>();
		yArray.add(1001.0);
		yArray.add(1050.0);
		yArray.add(2005.0);
		yArray.add(3005.0);
		yArray.add(4050.0);
		yArray.add(5505.0);
		yArray.add(6050.0);
		List<String> markerArray = new ArrayList<>();
		markerArray.add("90");
		markerArray.add("55");
		markerArray.add("79");
		markerArray.add("16");
		markerArray.add("25");
		markerArray.add("84");
		markerArray.add("02");
		return new PlotlyBubble.BubbleData(xArray, yArray, markerArray);
	}

}
