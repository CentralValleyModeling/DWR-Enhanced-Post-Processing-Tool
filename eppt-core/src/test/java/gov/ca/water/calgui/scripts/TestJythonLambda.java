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

package gov.ca.water.calgui.scripts;

import java.io.File;
import java.math.BigInteger;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.time.LocalDateTime;
import java.time.Month;
import java.time.YearMonth;
import java.time.format.TextStyle;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.NavigableMap;
import java.util.Optional;
import java.util.TreeMap;
import java.util.function.Function;
import java.util.stream.DoubleStream;

import javax.script.ScriptException;

import gov.ca.water.calgui.bo.CommonPeriodFilter;
import gov.ca.water.calgui.bo.GUILinksAllModelsBO;
import gov.ca.water.calgui.bo.MonthPeriodFilter;
import gov.ca.water.calgui.bo.WaterYearDefinition;
import gov.ca.water.calgui.bo.WaterYearIndex;
import gov.ca.water.calgui.bo.WaterYearPeriod;
import gov.ca.water.calgui.bo.WaterYearPeriodRange;
import gov.ca.water.calgui.bo.WaterYearPeriodRangesFilter;
import gov.ca.water.calgui.bo.WaterYearType;
import gov.ca.water.calgui.busservice.impl.DetailedIssuesReader;
import gov.ca.water.calgui.busservice.impl.GuiLinksSeedDataSvcImpl;
import gov.ca.water.calgui.busservice.impl.ThresholdLinksSeedDataSvc;
import gov.ca.water.calgui.busservice.impl.WaterYearDefinitionSvc;
import gov.ca.water.calgui.busservice.impl.WaterYearTableReader;
import gov.ca.water.calgui.project.EpptDssContainer;
import gov.ca.water.calgui.project.EpptScenarioRun;
import gov.ca.water.calgui.project.NamedDssPath;
import javafx.scene.paint.Color;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import rma.stats.EmpiricalDist;

import static java.util.stream.Collectors.averagingDouble;
import static java.util.stream.Collectors.collectingAndThen;
import static java.util.stream.Collectors.groupingBy;
import static java.util.stream.Collectors.toList;
import static java.util.stream.Collectors.toMap;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 08-12-2019
 */
class TestJythonLambda
{

	private static WaterYearDefinition waterYearDefinition;

	@BeforeAll
	static void setup() throws Exception
	{
		Path target = Paths.get(System.getProperty("user.dir")).resolve("target").resolve("test-classes");
		System.setProperty("user.dir", target.toString());
		GuiLinksSeedDataSvcImpl.createSeedDataSvcImplInstance();
		WaterYearDefinitionSvc.createSeedDataSvcImplInstance();
		ThresholdLinksSeedDataSvc.createSeedDataSvcImplInstance();
		DetailedIssuesReader.createDetailedIssues();
		waterYearDefinition = new WaterYearDefinition("Test", Month.OCTOBER, Month.SEPTEMBER);
	}

	@Test
	void testStaticImport() throws Exception
	{
		EpptScenarioRun scenarioRun = buildRun();
		JythonScriptRunner runner = new JythonScriptRunner(scenarioRun, new CommonPeriodFilter(LocalDateTime.now(), LocalDateTime.now()), waterYearDefinition);
		List<Double> collect = DoubleStream.of(1.0).boxed().collect(toList());
		runner.runScript("from java.util.stream import DoubleStream");
		Object o = runner.runScript("DoubleStream.of(1.0).boxed().collect(toList())");
		assertEquals(collect, o);
	}

	@Test
	void testAnnualExceedanceWYD() throws Exception
	{
		EpptScenarioRun scenarioRun = buildRun();

		Month startOfPeriodMonth = Month.MARCH;
		JythonScriptRunner runner = new JythonScriptRunner(scenarioRun, new CommonPeriodFilter(LocalDateTime.now(), LocalDateTime.now()), waterYearDefinition);
		DssReader dssReader = new DssReader(scenarioRun, waterYearDefinition, LocalDateTime.of(1850, Month.JANUARY, 1, 0, 0),
				LocalDateTime.of(2150, Month.JANUARY, 1, 0, 0));
		NavigableMap<Double, Double> collect = dssReader.getGuiLinkData(102).entrySet().stream().collect(
				collectingAndThen(groupingBy(mapToPeriodStartYear(startOfPeriodMonth), averagingDouble(v -> v.getValue())),
						generateExceedanceValues()));
		assertFalse(collect.isEmpty());
		Object o = runner.runScript(
				"dssReader.getGuiLinkData(102).entrySet().stream().collect(collectingAndThen(groupingBy(mapToPeriodStartYear(Month.MARCH), averagingDouble(jdf(lambda e : e.getValue()))),generateExceedanceValues()))");
		assertEquals(collect, o);
	}

	@Test
	void testDtsTitleFunction() throws Exception
	{
		EpptScenarioRun scenarioRun = buildRun();

		TitleReader titleReader = new TitleReader(scenarioRun);
		String dtsTitle = titleReader.getDtsTitle(170);
		Assertions.assertEquals("Negative Carriage Water", dtsTitle);
		JythonScriptRunner runner = new JythonScriptRunner(scenarioRun, new CommonPeriodFilter(LocalDateTime.now(), LocalDateTime.now()), waterYearDefinition);
		Object o = runner.runScript("titleReader.getDtsTitle(170)");
		assertEquals(dtsTitle, o);
	}

	@Test
	void testGuiLinkTitleFunction() throws Exception
	{
		EpptScenarioRun scenarioRun = buildRun();

		TitleReader titleReader = new TitleReader(scenarioRun);
		String dtsTitle = titleReader.getGuiLinkId(102);
		Assertions.assertEquals("Trinity Reservoir Storage", dtsTitle);
		JythonScriptRunner runner = new JythonScriptRunner(scenarioRun, new CommonPeriodFilter(LocalDateTime.now(), LocalDateTime.now()), waterYearDefinition);
		Object o = runner.runScript("titleReader.getGuiLinkId(102)");
		assertEquals(dtsTitle, o);
	}

	@Test
	void testEntirePeriodExceedanceAllMonths() throws Exception
	{
		EpptScenarioRun scenarioRun = buildRun();
		JythonScriptRunner runner = new JythonScriptRunner(scenarioRun, new CommonPeriodFilter(LocalDateTime.now(), LocalDateTime.now()), waterYearDefinition);
		DssReader dssReader = new DssReader(scenarioRun, waterYearDefinition, LocalDateTime.of(1850, Month.JANUARY, 1, 0, 0),
				LocalDateTime.of(2150, Month.JANUARY, 1, 0, 0));
		NavigableMap<Double, Double> collect = calculateExceedance(dssReader.getGuiLinkData(102).values());
		assertFalse(collect.isEmpty());
		Object o = runner.runScript("calculateExceedance(ArrayList(dssReader.getGuiLinkData(102).values()))");
		assertEquals(collect, o);
	}

	@Test
	void testEntirePeriodExceedanceDtsAllMonths() throws Exception
	{
		EpptScenarioRun scenarioRun = buildRun();
		JythonScriptRunner runner = new JythonScriptRunner(scenarioRun, new CommonPeriodFilter(LocalDateTime.now(), LocalDateTime.now()), waterYearDefinition);
		DssReader dssReader = new DssReader(scenarioRun, waterYearDefinition, LocalDateTime.of(1850, Month.JANUARY, 1, 0, 0),
				LocalDateTime.of(2150, Month.JANUARY, 1, 0, 0));
		NavigableMap<Double, Double> collect = calculateExceedance(dssReader.getDtsData(170).values());
		assertFalse(collect.isEmpty());
		Object o = runner.runScript("calculateExceedance(ArrayList(dssReader.getDtsData(170).values()))");
		assertEquals(collect, o);
	}

	@Test
	void testEntirePeriodDtsPerMonth() throws Exception
	{
		EpptScenarioRun scenarioRun = buildRun();
		DssReader dssReader = new DssReader(scenarioRun, waterYearDefinition, LocalDateTime.of(1850, Month.JANUARY, 1, 0, 0),
				LocalDateTime.of(2150, Month.JANUARY, 1, 0, 0));
		MonthPeriodFilter periodFilter = new MonthPeriodFilter(Month.JULY);
		Map<Double, Double> collect = calculateExceedance(
				dssReader.getDtsData(170).entrySet().stream().filter(periodFilter).map(Map.Entry::getValue).collect(toList()));
		assertFalse(collect.isEmpty());
		JythonScriptRunner runner = new JythonScriptRunner(scenarioRun, new CommonPeriodFilter(LocalDateTime.now(), LocalDateTime.now()), waterYearDefinition);
		runner.setPeriodFilter(periodFilter);
		Object o = runner.runScript(
				"calculateExceedance(dssReader.getDtsData(170).entrySet().stream().filter(periodFilter).map(jf(lambda x : x.getValue())).collect(toList()))");
		assertEquals(collect, o);
	}

	@Test
	void testExceedanceSingleMonth() throws Exception
	{
		EpptScenarioRun scenarioRun = buildRun();
		DssReader dssReader = new DssReader(scenarioRun, waterYearDefinition, LocalDateTime.of(1850, Month.JANUARY, 1, 0, 0),
				LocalDateTime.of(2150, Month.JANUARY, 1, 0, 0));
		MonthPeriodFilter periodFilter = new MonthPeriodFilter(Month.JULY);
		Map<Double, Double> collect = calculateExceedance(
				dssReader.getGuiLinkData(102).entrySet().stream().filter(new MonthPeriodFilter(Month.JULY)).map(Map.Entry::getValue).collect(
						toList()));
		assertFalse(collect.isEmpty());
		JythonScriptRunner runner = new JythonScriptRunner(scenarioRun, new CommonPeriodFilter(LocalDateTime.now(), LocalDateTime.now()), waterYearDefinition);
		runner.setPeriodFilter(periodFilter);
		Object o = runner.runScript(
				"calculateExceedance(dssReader.getGuiLinkData(102).entrySet().stream().filter(MonthPeriodFilter(Month.JULY)).map(jf(lambda x : x.getValue())).collect(toList()))");
		assertEquals(collect, o);
	}

	@Test
	void testEntirePeriodExceedancePerMonth() throws Exception
	{
		EpptScenarioRun scenarioRun = buildRun();
		DssReader dssReader = new DssReader(scenarioRun, waterYearDefinition, LocalDateTime.of(1850, Month.JANUARY, 1, 0, 0),
				LocalDateTime.of(2150, Month.JANUARY, 1, 0, 0));
		MonthPeriodFilter periodFilter = new MonthPeriodFilter(Month.JULY);
		Map<Double, Double> collect = calculateExceedance(
				dssReader.getGuiLinkData(102).entrySet().stream().filter(periodFilter).map(Map.Entry::getValue).collect(toList()));
		assertFalse(collect.isEmpty());
		JythonScriptRunner runner = new JythonScriptRunner(scenarioRun, new CommonPeriodFilter(LocalDateTime.now(), LocalDateTime.now()), waterYearDefinition);
		runner.setPeriodFilter(periodFilter);
		Object o = runner.runScript(
				"calculateExceedance(dssReader.getGuiLinkData(102).entrySet().stream().filter(periodFilter).map(jf(lambda x : x.getValue())).collect(toList()))");
		assertEquals(collect, o);
	}

	@Test
	void testEntirePeriodMonthlyAverage() throws Exception
	{
		EpptScenarioRun scenarioRun = buildRun();
		DssReader dssReader = new DssReader(scenarioRun, waterYearDefinition, LocalDateTime.of(1850, Month.JANUARY, 1, 0, 0),
				LocalDateTime.of(2150, Month.JANUARY, 1, 0, 0));
		CommonPeriodFilter commonPeriodFilter = new CommonPeriodFilter(LocalDateTime.of(1950, Month.JULY, 1, 0, 0),
				LocalDateTime.of(1999, Month.JULY, 1, 0, 0));
		Map<Month, Double> collect = dssReader.getGuiLinkData(102).entrySet().stream().filter(commonPeriodFilter).collect(
				groupingBy(k -> k.getKey().getMonth(), averagingDouble(
						Map.Entry::getValue)));
		assertFalse(collect.isEmpty());
		JythonScriptRunner runner = new JythonScriptRunner(scenarioRun, commonPeriodFilter, waterYearDefinition);
		Object o = runner.runScript(
				"dssReader.getGuiLinkData(102).entrySet().stream().filter(commonPeriodFilter).collect(groupingBy(jf(lambda k : k.getKey().getMonth()), averagingDouble(jdf(lambda e : e.getValue()))))");
		assertEquals(collect, o);
	}

	@Test
	void testPeriodAverageDtsFunction() throws Exception
	{
		EpptScenarioRun scenarioRun = buildRun();
		DssReader dssReader = new DssReader(scenarioRun, waterYearDefinition, LocalDateTime.of(1850, Month.JANUARY, 1, 0, 0),
				LocalDateTime.of(2150, Month.JANUARY, 1, 0, 0));
		CommonPeriodFilter commonPeriodFilter = new CommonPeriodFilter(LocalDateTime.of(1950, Month.JULY, 1, 0, 0),
				LocalDateTime.of(1999, Month.JULY, 1, 0, 0));
		Double collect = dssReader.getDtsData(170).entrySet().stream().filter(commonPeriodFilter).mapToDouble(Map.Entry::getValue).average().orElse(
				Double.NaN);
		assertNotNull(collect);
		assertFalse(collect.isNaN());
		JythonScriptRunner runner = new JythonScriptRunner(scenarioRun, commonPeriodFilter, waterYearDefinition);
		runner.setPeriodFilter(commonPeriodFilter);
		Object o = runner.runScript(
				"dssReader.getDtsData(170).entrySet().stream().filter(periodFilter).mapToDouble(jdf(lambda e : e.getValue())).average().orElse(0)");
		assertEquals(collect, o);
	}

	@Test
	void testPeriodAverageDtsFunctionOneMonth() throws Exception
	{
		EpptScenarioRun scenarioRun = buildRun();
		DssReader dssReader = new DssReader(scenarioRun, waterYearDefinition, LocalDateTime.of(1850, Month.JANUARY, 1, 0, 0),
				LocalDateTime.of(2150, Month.JANUARY, 1, 0, 0));
		CommonPeriodFilter periodFilter = new CommonPeriodFilter(LocalDateTime.of(1950, Month.JULY, 1, 0, 0),
				LocalDateTime.of(1999, Month.JULY, 1, 0, 0));
		Double collect = dssReader.getDtsData(170).entrySet().stream().filter(periodFilter).filter(new MonthPeriodFilter(Month.JULY)).mapToDouble(
				Map.Entry::getValue).average().orElse(Double.NaN);
		assertNotNull(collect);
		assertFalse(collect.isNaN());
		JythonScriptRunner runner = new JythonScriptRunner(scenarioRun, periodFilter, waterYearDefinition);
		runner.setPeriodFilter(periodFilter);
		Object o = runner.runScript(
				"dssReader.getDtsData(170).entrySet().stream().filter(periodFilter).filter(MonthPeriodFilter(Month.JULY)).mapToDouble(jdf(lambda e : e.getValue())).average().orElse(0)");
		assertEquals(collect, o);
	}

	@Test
	void testPeriodGreaterThan0AverageFunction() throws Exception
	{
		EpptScenarioRun scenarioRun = buildRun();
		DssReader dssReader = new DssReader(scenarioRun, waterYearDefinition, LocalDateTime.of(1850, Month.JANUARY, 1, 0, 0),
				LocalDateTime.of(2150, Month.JANUARY, 1, 0, 0));
		CommonPeriodFilter periodFilter = new CommonPeriodFilter(LocalDateTime.of(1950, Month.JULY, 1, 0, 0),
				LocalDateTime.of(1999, Month.JULY, 1, 0, 0));
		long collect = dssReader.getDtsData(170).entrySet().stream().filter(periodFilter).mapToDouble(Map.Entry::getValue).filter(v -> v > 0).count();
		JythonScriptRunner runner = new JythonScriptRunner(scenarioRun, periodFilter, waterYearDefinition);
		runner.setPeriodFilter(periodFilter);
		Object o = runner.runScript(
				"dssReader.getDtsData(170).entrySet().stream().filter(commonPeriodFilter).mapToDouble(jdf(lambda e : e.getValue())).filter(jpDouble(lambda v : v > 0)).count()");
		assertEquals(collect, ((BigInteger) o).longValue());
	}

	@Test
	void testPeriodLessThan0AverageFunction() throws Exception
	{
		EpptScenarioRun scenarioRun = buildRun();
		DssReader dssReader = new DssReader(scenarioRun, waterYearDefinition, LocalDateTime.of(1850, Month.JANUARY, 1, 0, 0),
				LocalDateTime.of(2150, Month.JANUARY, 1, 0, 0));
		CommonPeriodFilter periodFilter = new CommonPeriodFilter(LocalDateTime.of(1950, Month.JULY, 1, 0, 0),
				LocalDateTime.of(1999, Month.JULY, 1, 0, 0));
		long collect = dssReader.getDtsData(170).entrySet().stream().filter(periodFilter).mapToDouble(Map.Entry::getValue).filter(v -> v > 0).count();
		JythonScriptRunner runner = new JythonScriptRunner(scenarioRun, periodFilter, waterYearDefinition);
		runner.setPeriodFilter(periodFilter);
		Object o = runner.runScript(
				"dssReader.getDtsData(170).entrySet().stream().filter(commonPeriodFilter).mapToDouble(jdf(lambda e : e.getValue())).filter(jpDouble(lambda v : v < 0)).count()");
		assertEquals(collect, ((BigInteger) o).longValue());
	}

	@Test
	void testPeriodValuesDtsCount() throws Exception
	{
		EpptScenarioRun scenarioRun = buildRun();
		DssReader dssReader = new DssReader(scenarioRun, waterYearDefinition, LocalDateTime.of(1850, Month.JANUARY, 1, 0, 0),
				LocalDateTime.of(2150, Month.JANUARY, 1, 0, 0));
		CommonPeriodFilter periodFilter = new CommonPeriodFilter(LocalDateTime.of(1950, Month.JULY, 1, 0, 0),
				LocalDateTime.of(1999, Month.JULY, 1, 0, 0));
		double comparisonValue = 100;
		long collect = dssReader.getDtsData(170).entrySet().stream().filter(periodFilter).mapToDouble(Map.Entry::getValue).filter(
				v -> v == comparisonValue).count();
		JythonScriptRunner runner = new JythonScriptRunner(scenarioRun, periodFilter, waterYearDefinition);
		runner.setPeriodFilter(periodFilter);
		runner.setComparisonValue(comparisonValue);
		Object o = runner.runScript(
				"dssReader.getDtsData(170).entrySet().stream().filter(commonPeriodFilter).mapToDouble(jdf(lambda e : e.getValue())).filter(jpDouble(lambda v : v == comparisonValue)).count()");
		assertEquals(collect, ((BigInteger) o).longValue());
	}

	@Test
	void testReportDvWhenDtsAbove() throws Exception
	{
		EpptScenarioRun scenarioRun = buildRun();
		DssReader dssReader = new DssReader(scenarioRun, waterYearDefinition, LocalDateTime.of(1850, Month.JANUARY, 1, 0, 0),
				LocalDateTime.of(2150, Month.JANUARY, 1, 0, 0));
		CommonPeriodFilter periodFilter = new CommonPeriodFilter(LocalDateTime.of(1950, Month.JULY, 1, 0, 0),
				LocalDateTime.of(1999, Month.JULY, 1, 0, 0));
		WaterYearIndex waterYearIndex = new WaterYearIndex("Sac", new ArrayList<>(), new ArrayList<>());
		WaterYearPeriod waterYearType = new WaterYearPeriod("Wet");
		List<String> collect = dssReader.getDtsData(170).entrySet().stream().filter(periodFilter).filter(v -> v.getValue() == 300).map(
				v -> getMatchingGuiLinkEntry(dssReader, 102, v)).filter(Optional::isPresent).map(Optional::get).map(
				e -> buildListPrefix(e, waterYearIndex, waterYearType) + e.getValue()).collect(toList());
		JythonScriptRunner runner = new JythonScriptRunner(scenarioRun, periodFilter, waterYearDefinition);
		runner.setPeriodFilter(periodFilter);
		runner.setWaterYearIndex(waterYearIndex);
		runner.setWaterYearType(waterYearType);
		Object o = runner.runScript(
				"dssReader.getDtsData(170).entrySet().stream().filter(periodFilter).filter(jp(lambda v: v.getValue() == 300)).map(jf(lambda v:  getMatchingGuiLinkEntry(102, v))).filter(jp(lambda o : o.isPresent())).map(jf(lambda o : o.get())).map(jf(lambda e : buildListPrefix(e) + String.valueOf(e.getValue()))).collect(toList())");
		assertEquals(collect, o);
	}

	@Test
	void testReportDvWhenDtsEquals() throws Exception
	{
		EpptScenarioRun scenarioRun = buildRun();
		DssReader dssReader = new DssReader(scenarioRun, waterYearDefinition, LocalDateTime.of(1850, Month.JANUARY, 1, 0, 0),
				LocalDateTime.of(2150, Month.JANUARY, 1, 0, 0));
		CommonPeriodFilter periodFilter = new CommonPeriodFilter(LocalDateTime.of(1950, Month.JULY, 1, 0, 0),
				LocalDateTime.of(1999, Month.JULY, 1, 0, 0));
		WaterYearIndex waterYearIndex = new WaterYearIndex("Sac", new ArrayList<>(), new ArrayList<>());
		WaterYearPeriod waterYearType = new WaterYearPeriod("Wet");
		List<String> collect = dssReader.getDtsData(170).entrySet().stream().filter(periodFilter).filter(v -> v.getValue() == 200).map(
				v -> getMatchingGuiLinkEntry(dssReader, 102, v)).filter(Optional::isPresent).map(Optional::get).map(
				e -> buildListPrefix(e, waterYearIndex, waterYearType) + e.getValue()).collect(toList());
		JythonScriptRunner runner = new JythonScriptRunner(scenarioRun, periodFilter, waterYearDefinition);
		runner.setPeriodFilter(periodFilter);
		runner.setWaterYearIndex(waterYearIndex);
		runner.setWaterYearType(waterYearType);
		Object o = runner.runScript(
				"dssReader.getDtsData(170).entrySet().stream().filter(periodFilter).filter(jp(lambda v: v.getValue() == 200)).map(jf(lambda v:  getMatchingGuiLinkEntry(102, v))).filter(jp(lambda o : o.isPresent())).map(jf(lambda o : o.get())).map(jf(lambda e : buildListPrefix(e) + String.valueOf(e.getValue()))).collect(toList())");
		assertEquals(collect, o);
	}

	@Test
	void testReportDvWhenDtsBelow() throws Exception
	{
		EpptScenarioRun scenarioRun = buildRun();
		DssReader dssReader = new DssReader(scenarioRun, waterYearDefinition, LocalDateTime.of(1850, Month.JANUARY, 1, 0, 0),
				LocalDateTime.of(2150, Month.JANUARY, 1, 0, 0));
		CommonPeriodFilter periodFilter = new CommonPeriodFilter(LocalDateTime.of(1950, Month.JULY, 1, 0, 0),
				LocalDateTime.of(1999, Month.JULY, 1, 0, 0));
		WaterYearIndex waterYearIndex = new WaterYearIndex("Sac", new ArrayList<>(), new ArrayList<>());
		WaterYearPeriod waterYearType = new WaterYearPeriod("Wet");
		List<String> collect = dssReader.getDtsData(170).entrySet().stream().filter(periodFilter).filter(v -> v.getValue() == 200).map(
				v -> getMatchingGuiLinkEntry(dssReader, 102, v)).filter(Optional::isPresent).map(Optional::get).map(
				e -> buildListPrefix(e, waterYearIndex, waterYearType) + e.getValue()).collect(toList());
		JythonScriptRunner runner = new JythonScriptRunner(scenarioRun, periodFilter, waterYearDefinition);
		runner.setPeriodFilter(periodFilter);
		runner.setWaterYearIndex(waterYearIndex);
		runner.setWaterYearType(waterYearType);
		Object o = runner.runScript(
				"dssReader.getDtsData(170).entrySet().stream().filter(periodFilter).filter(jp(lambda v: v.getValue() == 100)).map(jf(lambda v:  getMatchingGuiLinkEntry(102, v))).filter(jp(lambda o : o.isPresent())).map(jf(lambda o : o.get())).map(jf(lambda e : buildListPrefix(e) + String.valueOf(e.getValue()))).collect(toList())");
		assertEquals(collect, o);
	}

	@Test
	void testReportDvWhenGreaterThan0() throws Exception
	{
		EpptScenarioRun scenarioRun = buildRun();
		DssReader dssReader = new DssReader(scenarioRun, waterYearDefinition, LocalDateTime.of(1850, Month.JANUARY, 1, 0, 0),
				LocalDateTime.of(2150, Month.JANUARY, 1, 0, 0));
		CommonPeriodFilter periodFilter = new CommonPeriodFilter(LocalDateTime.of(1950, Month.JULY, 1, 0, 0),
				LocalDateTime.of(1999, Month.JULY, 1, 0, 0));
		WaterYearIndex waterYearIndex = new WaterYearIndex("Sac", new ArrayList<>(), new ArrayList<>());
		WaterYearPeriod waterYearType = new WaterYearPeriod("Wet");
		List<String> collect = dssReader.getGuiLinkData(102).entrySet().stream().filter(periodFilter).filter(v -> v.getValue() > 0).map(
				e -> buildListPrefix(e, waterYearIndex, waterYearType) + e.getValue()).collect(toList());
		JythonScriptRunner runner = new JythonScriptRunner(scenarioRun, periodFilter, waterYearDefinition);
		runner.setPeriodFilter(periodFilter);
		runner.setWaterYearIndex(waterYearIndex);
		runner.setWaterYearType(waterYearType);
		Object o = runner.runScript(
				"dssReader.getGuiLinkData(102).entrySet().stream().filter(periodFilter).filter(jp(lambda v: v.getValue() > 0)).map(jf(lambda e : buildListPrefix(e) + String.valueOf(e.getValue()))).collect(toList())");
		assertEquals(collect, o);
	}

	@Test
	void testReportThresholdWhenDtsGreaterThan() throws Exception
	{
		EpptScenarioRun scenarioRun = buildRun();
		DssReader dssReader = new DssReader(scenarioRun, waterYearDefinition, LocalDateTime.of(1850, Month.JANUARY, 1, 0, 0),
				LocalDateTime.of(2150, Month.JANUARY, 1, 0, 0));
		CommonPeriodFilter periodFilter = new CommonPeriodFilter(LocalDateTime.of(1950, Month.JULY, 1, 0, 0),
				LocalDateTime.of(1999, Month.JULY, 1, 0, 0));
		WaterYearIndex waterYearIndex = new WaterYearIndex("Sac", new ArrayList<>(), new ArrayList<>());
		WaterYearPeriod waterYearType = new WaterYearPeriod("Wet");
		List<String> collect = dssReader.getDtsData(170).entrySet().stream().filter(periodFilter).filter(
				e -> {
					try
					{
						return e.getValue() > dssReader.getThresholdData(2).get(e.getKey());
					}
					catch(DssMissingRecordException ex)
					{
						ex.printStackTrace();
						return false;
					}
				}).map(
				e -> {
					try
					{
						return buildListPrefix(e, waterYearIndex, waterYearType) + String.join(",", String.valueOf(e.getValue()),
								String.valueOf(dssReader.getThresholdData(2).get(e.getKey())));
					}
					catch(DssMissingRecordException ex)
					{
						ex.printStackTrace();
						return "";
					}
				}).collect(toList());
		JythonScriptRunner runner = new JythonScriptRunner(scenarioRun, periodFilter, waterYearDefinition);
		runner.setPeriodFilter(periodFilter);
		runner.setWaterYearIndex(waterYearIndex);
		runner.setWaterYearType(waterYearType);
		Object o = runner.runScript(
				"dssReader.getDtsData(170).entrySet().stream().filter(periodFilter).filter(jp(lambda e: e.getValue() > dssReader.getThresholdData(2).get(e.getKey()))).map(jf(lambda e : buildListPrefix(e) + String.join(\",\", String.valueOf(e.getValue()), String.valueOf(dssReader.getThresholdData(2).get(e.getKey()))))).collect(toList())");
		assertEquals(collect, o);
	}

	@Test
	void testReportThresholdWhenDtsLessThan() throws Exception
	{
		EpptScenarioRun scenarioRun = buildRun();
		DssReader dssReader = new DssReader(scenarioRun, waterYearDefinition, LocalDateTime.of(1850, Month.JANUARY, 1, 0, 0),
				LocalDateTime.of(2150, Month.JANUARY, 1, 0, 0));
		CommonPeriodFilter periodFilter = new CommonPeriodFilter(LocalDateTime.of(1950, Month.JULY, 1, 0, 0),
				LocalDateTime.of(1999, Month.JULY, 1, 0, 0));
		WaterYearIndex waterYearIndex = new WaterYearIndex("Sac", new ArrayList<>(), new ArrayList<>());
		WaterYearPeriod waterYearType = new WaterYearPeriod("Wet");
		List<String> collect = dssReader.getDtsData(170).entrySet().stream().filter(periodFilter).filter(
				e -> {
					try
					{
						return e.getValue() < dssReader.getThresholdData(2).get(e.getKey());
					}
					catch(DssMissingRecordException ex)
					{
						ex.printStackTrace();
						return false;
					}
				}).map(
				e -> {
					try
					{
						return buildListPrefix(e, waterYearIndex, waterYearType) + String.join(",", String.valueOf(e.getValue()),
								String.valueOf(dssReader.getThresholdData(2).get(e.getKey())));
					}
					catch(DssMissingRecordException ex)
					{
						ex.printStackTrace();
						return "";
					}
				}).collect(toList());
		JythonScriptRunner runner = new JythonScriptRunner(scenarioRun, periodFilter, waterYearDefinition);
		runner.setPeriodFilter(periodFilter);
		runner.setWaterYearIndex(waterYearIndex);
		runner.setWaterYearType(waterYearType);
		Object o = runner.runScript(
				"dssReader.getDtsData(170).entrySet().stream().filter(periodFilter).filter(jp(lambda e: e.getValue() < dssReader.getThresholdData(2).get(e.getKey()))).map(jf(lambda e : buildListPrefix(e) + String.join(\",\", String.valueOf(e.getValue()), String.valueOf(dssReader.getThresholdData(2).get(e.getKey()))))).collect(toList())");
		assertEquals(collect, o);
	}

	@Test
	void testReportValueWhenDtsNotEqual() throws Exception
	{
		EpptScenarioRun scenarioRun = buildRun();
		DssReader dssReader = new DssReader(scenarioRun, waterYearDefinition, LocalDateTime.of(1850, Month.JANUARY, 1, 0, 0),
				LocalDateTime.of(2150, Month.JANUARY, 1, 0, 0));
		CommonPeriodFilter periodFilter = new CommonPeriodFilter(LocalDateTime.of(1950, Month.JULY, 1, 0, 0),
				LocalDateTime.of(1999, Month.JULY, 1, 0, 0));
		WaterYearIndex waterYearIndex = new WaterYearIndex("Sac", new ArrayList<>(), new ArrayList<>());
		WaterYearPeriod waterYearType = new WaterYearPeriod("Wet");
		List<String> collect = dssReader.getDtsData(170).entrySet().stream().filter(periodFilter).filter(
				e -> {
					try
					{
						return e.getValue() < dssReader.getThresholdData(2).get(e.getKey());
					}
					catch(DssMissingRecordException ex)
					{
						ex.printStackTrace();
						return false;
					}
				}).map(
				e -> {
					try
					{
						return buildListPrefix(e, waterYearIndex, waterYearType) + String.join(",", String.valueOf(e.getValue()),
								String.valueOf(dssReader.getThresholdData(2).get(e.getKey())));
					}
					catch(DssMissingRecordException ex)
					{
						ex.printStackTrace();
						return "";
					}
				}).collect(toList());
		JythonScriptRunner runner = new JythonScriptRunner(scenarioRun, periodFilter, waterYearDefinition);
		runner.setPeriodFilter(periodFilter);
		runner.setWaterYearIndex(waterYearIndex);
		runner.setWaterYearType(waterYearType);
		Object o = runner.runScript(
				"dssReader.getDtsData(170).entrySet().stream().filter(periodFilter).filter(jp(lambda e: e.getValue() != dssReader.getThresholdData(2).get(e.getKey()))).map(jf(lambda e : buildListPrefix(e) + String.join(\",\", String.valueOf(e.getValue()), String.valueOf(dssReader.getThresholdData(2).get(e.getKey()))))).collect(toList())");
		assertEquals(collect, o);
	}

	@Test
	void testRetrieveDv() throws Exception
	{
		EpptScenarioRun scenarioRun = buildRun();
		DssReader dssReader = new DssReader(scenarioRun, waterYearDefinition, LocalDateTime.of(1850, Month.JANUARY, 1, 0, 0),
				LocalDateTime.of(2150, Month.JANUARY, 1, 0, 0));
		CommonPeriodFilter periodFilter = new CommonPeriodFilter(LocalDateTime.of(1950, Month.JULY, 1, 0, 0),
				LocalDateTime.of(1999, Month.JULY, 1, 0, 0));
		//		WaterYearIndex waterYearIndex = new WaterYearIndex("Sac", new ArrayList<>());
		//		WaterYearPeriod waterYearType = new WaterYearPeriod("Wet");
		Month month = Month.MARCH;
		//		WaterYearDefinition waterYearDefinition = new WaterYearDefinition("", month, month.minus(1));
		//		LocalDateTime date = LocalDateTime.of(1950, 1, 1, 0, 0, 0);
		//		int year = waterYearDefinition.getYear(date);
		Map<Integer, Double> collect = dssReader.getGuiLinkData(102).entrySet().stream().filter(e -> e.getKey().getMonth().equals(month)).collect(
				toMap(e -> e.getKey().getYear(), e -> e.getValue()));
		JythonScriptRunner runner = new JythonScriptRunner(scenarioRun, periodFilter, waterYearDefinition);
		Object o = runner.runScript(
				"dssReader.getGuiLinkData(102).entrySet().stream().filter(jp(lambda e : e.getKey().getMonth().equals(Month.MARCH))).collect(toMap(jf(lambda e : e.getKey().getYear()), jf(lambda e: e.getValue())))");
		assertEquals(collect, o);
	}

	@Test
	void testRetrieveDts() throws Exception
	{
		EpptScenarioRun scenarioRun = buildRun();
		DssReader dssReader = new DssReader(scenarioRun, waterYearDefinition, LocalDateTime.of(1850, Month.JANUARY, 1, 0, 0),
				LocalDateTime.of(2150, Month.JANUARY, 1, 0, 0));
		CommonPeriodFilter periodFilter = new CommonPeriodFilter(LocalDateTime.of(1950, Month.JULY, 1, 0, 0),
				LocalDateTime.of(1999, Month.JULY, 1, 0, 0));
		Month month = Month.MARCH;
		Map<Integer, Double> collect = dssReader.getDtsData(296).entrySet().stream().filter(e -> e.getKey().getMonth().equals(month)).collect(
				toMap(e -> e.getKey().getYear(), e -> e.getValue()));
		JythonScriptRunner runner = new JythonScriptRunner(scenarioRun, periodFilter, waterYearDefinition);
		Object o = runner.runScript(
				"dssReader.getDtsData(296).entrySet().stream().filter(jp(lambda e : e.getKey().getMonth().equals(Month.MARCH))).collect(toMap(jf(lambda e : e.getKey().getYear()), jf(lambda e: e.getValue())))");
		assertEquals(collect, o);
	}

	@Test
	void testReportMonthWhenUnbalanced() throws Exception
	{
		EpptScenarioRun scenarioRun = buildRun();
		DssReader dssReader = new DssReader(scenarioRun, waterYearDefinition, LocalDateTime.of(1850, Month.JANUARY, 1, 0, 0),
				LocalDateTime.of(2150, Month.JANUARY, 1, 0, 0));
		CommonPeriodFilter periodFilter = new CommonPeriodFilter(LocalDateTime.of(1950, Month.JULY, 1, 0, 0),
				LocalDateTime.of(1999, Month.JULY, 1, 0, 0));
		WaterYearIndex waterYearIndex = new WaterYearIndex("Sac", new ArrayList<>(), new ArrayList<>());
		WaterYearPeriod waterYearType = new WaterYearPeriod("Wet");
		List<String> collect = dssReader.getDtsData(170).entrySet().stream().filter(periodFilter).filter(e -> e.getValue() != 0).map(
				e -> buildListPrefix(e, waterYearIndex, waterYearType)).collect(toList());
		JythonScriptRunner runner = new JythonScriptRunner(scenarioRun, periodFilter, waterYearDefinition);
		runner.setPeriodFilter(periodFilter);
		runner.setWaterYearIndex(waterYearIndex);
		runner.setWaterYearType(waterYearType);
		Object o = runner.runScript(
				"dssReader.getDtsData(170).entrySet().stream().filter(periodFilter).filter(jp(lambda e: e.getValue() != 0)).map(jf(lambda e : buildListPrefix(e))).collect(toList())");
		assertEquals(collect, o);
	}

	private String buildListPrefix(Map.Entry<LocalDateTime, Double> e, WaterYearIndex index, WaterYearPeriod waterYearType)
	{
		return e.getKey().getMonth().getDisplayName(TextStyle.FULL,
				Locale.getDefault()) + " " + e.getKey().getYear() + "(" + index + " " + waterYearType.getPeriodName() + "): ";
	}

	private Optional<Map.Entry<LocalDateTime, Double>> getMatchingGuiLinkEntry(DssReader dssReader, int guiLinkId, Map.Entry<LocalDateTime, Double> v)
	{
		try
		{
			return dssReader.getGuiLinkData(guiLinkId).entrySet().stream().filter(e -> e.getKey().equals(v.getKey())).findAny();
		}
		catch(DssMissingRecordException e)
		{
			e.printStackTrace();
			return Optional.empty();
		}
	}

	@Test
	void testPeriodAverageFunction() throws Exception
	{
		EpptScenarioRun scenarioRun = buildRun();
		DssReader dssReader = new DssReader(scenarioRun, waterYearDefinition, LocalDateTime.of(1850, Month.JANUARY, 1, 0, 0),
				LocalDateTime.of(2150, Month.JANUARY, 1, 0, 0));
		CommonPeriodFilter periodFilter = new CommonPeriodFilter(LocalDateTime.of(1950, Month.JULY, 1, 0, 0),
				LocalDateTime.of(1999, Month.JULY, 1, 0, 0));
		Double collect = dssReader.getGuiLinkData(102).entrySet().stream().filter(periodFilter).filter(new MonthPeriodFilter(Month.JULY)).mapToDouble(
				Map.Entry::getValue).average().orElse(Double.NaN);
		assertNotNull(collect);
		assertFalse(collect.isNaN());
		JythonScriptRunner runner = new JythonScriptRunner(scenarioRun, periodFilter, waterYearDefinition);
		runner.setPeriodFilter(periodFilter);
		Object o = runner.runScript(
				"dssReader.getGuiLinkData(102).entrySet().stream().filter(periodFilter).filter(MonthPeriodFilter(Month.JULY)).mapToDouble(jdf(lambda e : e.getValue())).average().orElse(0)");
		assertEquals(collect, o);
	}

	@Test
	void testPeriodAverageFunctionOneMonth() throws Exception
	{
		EpptScenarioRun scenarioRun = buildRun();
		DssReader dssReader = new DssReader(scenarioRun, waterYearDefinition, LocalDateTime.of(1850, Month.JANUARY, 1, 0, 0),
				LocalDateTime.of(2150, Month.JANUARY, 1, 0, 0));
		CommonPeriodFilter periodFilter = new CommonPeriodFilter(LocalDateTime.of(1950, Month.JULY, 1, 0, 0),
				LocalDateTime.of(1999, Month.JULY, 1, 0, 0));
		Double collect = dssReader.getGuiLinkData(102).entrySet().stream().filter(periodFilter).filter(new MonthPeriodFilter(Month.JULY)).mapToDouble(
				Map.Entry::getValue).average().orElse(Double.NaN);
		assertNotNull(collect);
		assertFalse(collect.isNaN());
		JythonScriptRunner runner = new JythonScriptRunner(scenarioRun, periodFilter, waterYearDefinition);
		runner.setPeriodFilter(periodFilter);
		Object o = runner.runScript(
				"dssReader.getGuiLinkData(102).entrySet().stream().filter(periodFilter).filter(MonthPeriodFilter(Month.JULY)).mapToDouble(jdf(lambda e : e.getValue())).average().orElse(Double.NaN)");
		assertEquals(collect, o);
	}

	@Test
	void testPeriodAnnualAverageFunction() throws Exception
	{
		EpptScenarioRun scenarioRun = buildRun();
		DssReader dssReader = new DssReader(scenarioRun, waterYearDefinition, LocalDateTime.of(1850, Month.JANUARY, 1, 0, 0),
				LocalDateTime.of(2150, Month.JANUARY, 1, 0, 0));
		CommonPeriodFilter periodFilter = new CommonPeriodFilter(LocalDateTime.of(1950, Month.JULY, 1, 0, 0),
				LocalDateTime.of(1999, Month.JULY, 1, 0, 0));
		Month endOfPeriodMonth = Month.SEPTEMBER;
		WaterYearPeriod annual = new WaterYearPeriod("Annual");
		WaterYearPeriodRange waterYearPeriodRange = new WaterYearPeriodRange(annual, new WaterYearType(1925, annual),
				new WaterYearType(1928, annual));
		Double collect = dssReader.getGuiLinkData(102)
								  .entrySet()
								  .stream()
								  .filter(buildPeriodFilterForEndMonth(endOfPeriodMonth, Collections.singletonList(waterYearPeriodRange)))
								  .mapToDouble(Map.Entry::getValue)
								  .average()
								  .orElse(Double.NaN);
		assertNotNull(collect);
		assertFalse(collect.isNaN());
		JythonScriptRunner runner = new JythonScriptRunner(scenarioRun, periodFilter, waterYearDefinition);
		runner.setPeriodFilter(periodFilter);
		runner.setWaterYearPeriodRanges(Collections.singletonList(waterYearPeriodRange));
		Object o = runner.runScript(
				"dssReader.getGuiLinkData(102).entrySet().stream().filter(buildPeriodFilterForEndMonth(waterYearPeriodRanges, Month.SEPTEMBER)).mapToDouble(jdf(lambda e : e.getValue())).average().orElse(Double.NaN)");
		assertEquals(collect, o);
	}

	private WaterYearPeriodRangesFilter buildPeriodFilterForEndMonth(Month endOfPeriodMonth, List<WaterYearPeriodRange> waterYearPeriodRange)
	{
		return new WaterYearPeriodRangesFilter(waterYearPeriodRange, new WaterYearDefinition("", endOfPeriodMonth.plus(1), endOfPeriodMonth));
	}

	@Test
	void testEntirePeriodMonthlyAverageDts() throws Exception
	{
		EpptScenarioRun scenarioRun = buildRun();
		DssReader dssReader = new DssReader(scenarioRun, waterYearDefinition, LocalDateTime.of(1850, Month.JANUARY, 1, 0, 0),
				LocalDateTime.of(2150, Month.JANUARY, 1, 0, 0));
		CommonPeriodFilter commonPeriodFilter = new CommonPeriodFilter(LocalDateTime.of(1950, Month.JULY, 1, 0, 0),
				LocalDateTime.of(1999, Month.JULY, 1, 0, 0));
		Map<Month, Double> collect = dssReader.getDtsData(170).entrySet().stream().filter(commonPeriodFilter).collect(
				groupingBy(k -> k.getKey().getMonth(), averagingDouble(v -> v.getValue())));
		assertFalse(collect.isEmpty());
		JythonScriptRunner runner = new JythonScriptRunner(scenarioRun, commonPeriodFilter, waterYearDefinition);
		Object o = runner.runScript(
				"dssReader.getDtsData(170).entrySet().stream().filter(commonPeriodFilter).collect(groupingBy(jf(lambda e : e.getKey().getMonth()), averagingDouble(jdf(lambda e : e.getValue()))))");
		assertEquals(collect, o);
	}

	private EpptScenarioRun buildRun()
	{
		Path outputPath = Paths.get("");
		Path wreslMain = new File(
				TestJythonLambda.class.getClassLoader().getResource("mainControl.wresl").getFile()).toPath();
		Path dvPath = new File(
				TestJythonLambda.class.getClassLoader().getResource("SampleDV_Base.dss").getFile()).toPath();
		NamedDssPath dvDssFile = new NamedDssPath(dvPath, "DV", "CALSIM", "1MON", "2020D09E");
		Path ivPath = new File(
				TestJythonLambda.class.getClassLoader().getResource("SampleINIT_Base.dss").getFile()).toPath();
		NamedDssPath ivDssFile = new NamedDssPath(ivPath, "INIT", "CALSIM", "1MON", "2020D09E");
		Path dtsPath = Paths.get("C:\\Users\\adam\\Documents\\EPPT\\Project1\\ScenarioRuns\\CalSim2_Base\\SampleDTS.dss");
		NamedDssPath dtsDssFile = new NamedDssPath(dtsPath, "DTS", "CALSIM", "1MON", "2020D09E");
		Path svPath = new File(
				TestJythonLambda.class.getClassLoader().getResource("SampleSV_Base.dss").getFile()).toPath();
		NamedDssPath svDssFile = new NamedDssPath(svPath, "SV", "CALSIM", "1MON", "2020D09E");
		List<NamedDssPath> extraDssFiles = Collections.emptyList();
		EpptDssContainer dssContainer = new EpptDssContainer(dvDssFile, svDssFile, ivDssFile, dtsDssFile, extraDssFiles);
		return new EpptScenarioRun("Test", "", GUILinksAllModelsBO.Model.findModel("CalSim2"), outputPath,
				wreslMain, Paths.get("C:\\Git\\DWR\\EPPT\\DWR-Enhanced-Post-Processing-Tool\\eppt-core\\src\\test\\resources\\lookup\\wytypes.table"),
				dssContainer, Color.BLUE);
	}

	private static int getPeriodStartYear(LocalDateTime date, Month startOfPeriod)
	{
		WaterYearPeriod period = new WaterYearPeriod("Term");
		WaterYearPeriodRange range = new WaterYearPeriodRange(period, new WaterYearType(date.getYear(), period),
				new WaterYearType(date.getYear(), period));
		WaterYearDefinition term = new WaterYearDefinition("Term", startOfPeriod, startOfPeriod.minus(1));
		YearMonth start = range.getStart(term);
		return start.getYear();
	}

	private static NavigableMap<Double, Double> calculateExceedance(Collection<Double> values)
	{
		NavigableMap<Double, Double> retval = new TreeMap<>();
		double[] doubles = values.stream().mapToDouble(v -> v).sorted().toArray();
		EmpiricalDist empiricalDist = new EmpiricalDist(EmpiricalDist.InterpType.LINEAR, doubles);
		for(int i = 0; i < doubles.length; i++)
		{
			retval.put(empiricalDist.getExceed(i), doubles[i]);
		}
		return retval;
	}

	private static Function<Map<Object, Double>, NavigableMap<Double, Double>> generateExceedanceValues()
	{
		return v -> calculateExceedance(v.values());
	}

	private static Function<Map.Entry<LocalDateTime, Double>, Integer> mapToPeriodStartYear(Month startOfPeriodMonth)
	{
		return v -> getPeriodStartYear(v.getKey(), startOfPeriodMonth);
	}

	@Test
	public void testFunction() throws ScriptException
	{
		EpptScenarioRun scenarioRun = buildRun();
		DssReader dssReader = new DssReader(scenarioRun, waterYearDefinition, LocalDateTime.of(1850, Month.JANUARY, 1, 0, 0),
				LocalDateTime.of(2150, Month.JANUARY, 1, 0, 0));
		CommonPeriodFilter periodFilter = new CommonPeriodFilter(LocalDateTime.of(1950, Month.JULY, 1, 0, 0),
				LocalDateTime.of(1999, Month.JULY, 1, 0, 0));
		Month endOfPeriodMonth = Month.SEPTEMBER;
		WaterYearPeriod annual = new WaterYearPeriod("Annual");
		WaterYearPeriodRange waterYearPeriodRange = new WaterYearPeriodRange(annual, new WaterYearType(1925, annual),
				new WaterYearType(1928, annual));
		String function = "dssReader.getDtsData(237).entrySet().stream().filter(periodFilter).filter(jp(lambda e: e.getValue() != dssReader.getThresholdData(237).get(e.getKey()))).map(jf(lambda e : buildListPrefix(e) + String.join(\",\", String.valueOf(e.getValue()), String.valueOf(dssReader.getThresholdData(237).get(e.getKey()))))).collect(toList())";

		JythonScriptRunner runner = new JythonScriptRunner(scenarioRun, periodFilter, waterYearDefinition);
		runner.setPeriodFilter(periodFilter);
		System.out.println(runner.runScript(function));
	}

	@Test
	public void testTrendFunction() throws Exception
	{
		EpptScenarioRun scenarioRun = buildRun();
		DssReader dssReader = new DssReader(scenarioRun, waterYearDefinition, LocalDateTime.of(1850, Month.JANUARY, 1, 0, 0),
				LocalDateTime.of(2150, Month.JANUARY, 1, 0, 0));
		WaterYearDefinition waterYearDefinition = WaterYearDefinitionSvc.getWaterYearDefinitionSvc().getDefinitions().get(0);
		NavigableMap<LocalDateTime, Double> dtsData = dssReader.getDtsData(237);
		WaterYearTableReader waterYearTableReader = new WaterYearTableReader(scenarioRun.getWaterYearTable());
		List<WaterYearIndex> waterYearIndices = waterYearTableReader.read();
		WaterYearIndex waterYearIndex = waterYearIndices.stream()
											.filter(p -> p.toString().equalsIgnoreCase("SAC Index"))
											.findAny()
											.orElseThrow(() -> new Exception("No SAC index"));
		List<WaterYearPeriodRange> criticallyDry = waterYearIndex.getLongWaterYearPeriodRanges().getOrDefault(new WaterYearPeriod("Critical"),
				new ArrayList<>());
		WaterYearPeriodRangesFilter waterYearPeriodRangesFilter = new WaterYearPeriodRangesFilter(criticallyDry, waterYearDefinition);
		TreeMap<LocalDateTime, Double> filteredData = new TreeMap<>(dtsData.entrySet().stream()
																		   .filter(waterYearPeriodRangesFilter)
																		   .collect(toMap(e -> e.getKey(), e -> e.getValue())));

		//		String function = "dssReader.getDtsData(237).entrySet().stream().filter(periodFilter).filter(jp(lambda e: e.getValue() != dssReader.getThresholdData(237).get(e.getKey()))).map(jf(lambda e : buildListPrefix(e) + String.join(\",\", String.valueOf(e.getValue()), String.valueOf(dssReader.getThresholdData(237).get(e.getKey()))))).collect(toList())";
		//
		////		JythonScriptRunner runner = new JythonScriptRunner(scenarioRun, periodFilter, waterYearDefinition);
		////		runner.setPeriodFilter(periodFilter);
		//		System.out.println(runner.runScript(function));
	}
}
