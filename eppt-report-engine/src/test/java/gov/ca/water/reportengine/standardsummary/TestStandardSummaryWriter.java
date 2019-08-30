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

package gov.ca.water.reportengine.standardsummary;

import java.io.BufferedWriter;
import java.net.URI;
import java.net.URL;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.regex.Pattern;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import gov.ca.water.calgui.bo.GUILinksAllModelsBO;
import gov.ca.water.calgui.bo.WaterYearDefinition;
import gov.ca.water.calgui.bo.WaterYearIndex;
import gov.ca.water.calgui.bo.WaterYearPeriod;
import gov.ca.water.calgui.bo.WaterYearPeriodRange;
import gov.ca.water.calgui.bo.WaterYearType;
import gov.ca.water.calgui.busservice.impl.WaterYearDefinitionSvc;
import gov.ca.water.calgui.busservice.impl.WaterYearTableReader;
import gov.ca.water.calgui.constant.Constant;
import gov.ca.water.calgui.project.EpptScenarioRun;
import org.junit.jupiter.api.Test;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import static java.util.stream.Collectors.toList;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 08-26-2019
 */
public class TestStandardSummaryWriter
{
	private static final Logger LOGGER = Logger.getLogger(TestStandardSummaryWriter.class.getName());

	@Test
	public void testWriter() throws Exception
	{
		String original = System.getProperty("user.dir");
		System.setProperty("user.dir", original + "\\target\\test-classes");
		WaterYearDefinitionSvc.createSeedDataSvcImplInstance();
		System.setProperty("user.dir", original);
		URL resource = TestStandardSummaryReader.class.getClassLoader().getResource("dwr_eppt/config/SummaryComponents_Types-Draft_8-26.csv");
		assertNotNull(resource,"Standard Summary Statistics configuration file must exist");
		URI uri = resource.toURI();
		Path path = Paths.get(uri);
		StandardSummaryReader standardSummaryReader = new StandardSummaryReader(path);
		List<String> orderedChartIds = standardSummaryReader.getOrderedChartIds();
		Map<String, EpptChart> stringEpptChartMap = standardSummaryReader.readLines();
		List<EpptChart> collect = orderedChartIds.stream()
												 .map(stringEpptChartMap::get)
												 .filter(Objects::nonNull)
												 .collect(toList());

		DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
		Document document = factory.newDocumentBuilder().newDocument();
		EpptScenarioRun baseRun = new EpptScenarioRun("Base", "desc", GUILinksAllModelsBO.Model.findModel("CalSim2"),
				Paths.get("Test.pdf"), Paths.get("mainWresl.wresl"), Paths.get("dwr_eppt\\wresl\\lookup\\wytypes.table"),
				null, javafx.scene.paint.Color.PINK);
		EpptScenarioRun altRun = new EpptScenarioRun("Alt", "desc", GUILinksAllModelsBO.Model.findModel("CalSim2"),
				Paths.get("Test.pdf"), Paths.get("mainWresl.wresl"), Paths.get(""), null, javafx.scene.paint.Color.PINK);

		WaterYearDefinition waterYearDefinition = WaterYearDefinitionSvc.getWaterYearDefinitionSvc().getDefinitions().get(0);
		WaterYearIndex waterYearIndex = new WaterYearTableReader(baseRun.getWaterYearTable()).read().get(0);
		WaterYearPeriod longTermPeriod = new WaterYearPeriod("Long Term");
		WaterYearPeriodRange longTermRange = new WaterYearPeriodRange(longTermPeriod, new WaterYearType(1930, longTermPeriod),
				new WaterYearType(1999, longTermPeriod));
		Map<WaterYearPeriod, List<WaterYearPeriodRange>> waterYearPeriodRanges = new HashMap<>();

		WaterYearPeriod dryPeriod = new WaterYearPeriod("Dry");
		List<WaterYearPeriodRange> dryRanges = new ArrayList<>();
		dryRanges.add(new WaterYearPeriodRange(dryPeriod, new WaterYearType(1930, dryPeriod),
				new WaterYearType(1940, dryPeriod)));
		dryRanges.add(new WaterYearPeriodRange(dryPeriod, new WaterYearType(1950, dryPeriod),
				new WaterYearType(1960, dryPeriod)));
		WaterYearPeriod wetPeriod = new WaterYearPeriod("Wet");
		List<WaterYearPeriodRange> wetRanges = new ArrayList<>();
		wetRanges.add(new WaterYearPeriodRange(wetPeriod, new WaterYearType(1935, wetPeriod),
				new WaterYearType(1945, wetPeriod)));
		wetRanges.add(new WaterYearPeriodRange(wetPeriod, new WaterYearType(1955, wetPeriod),
				new WaterYearType(1965, wetPeriod)));
		waterYearPeriodRanges.put(dryPeriod, dryRanges);
		waterYearPeriodRanges.put(wetPeriod, wetRanges);
		List<String> disabledStandardSummaryModules = new ArrayList<>();
		SummaryReportParameters reportParameters = new SummaryReportParameters(waterYearDefinition, waterYearIndex, longTermRange, waterYearPeriodRanges, PercentDiffStyle.FULL, disabledStandardSummaryModules);
		Path imagePath = Constant.QAQC_IMAGE_PATH;
		imagePath.toFile().delete();
		StandardSummaryWriter standardSummaryWriter = new StandardSummaryWriter(document, baseRun, Collections.singletonList(altRun),
				reportParameters, imagePath);
		Element write = standardSummaryWriter.write(collect);
		document.appendChild(write);

		Path outputXml = Paths.get("StandardSummary.xml");
		try(BufferedWriter writer = Files.newBufferedWriter(outputXml))
		{
			printPretty(document, writer);
			LOGGER.log(Level.INFO, "Output XML to: " + outputXml.toAbsolutePath());
		}
		Path ugly = Paths.get("StandardSummary-Ugly.xml");
		try(BufferedWriter writer = Files.newBufferedWriter(ugly))
		{
			printUgly(document, writer);
			LOGGER.log(Level.INFO, "Output XML to: " + ugly.toAbsolutePath());
		}
	}

	private void printUgly(Document document, BufferedWriter writer) throws TransformerException
	{
		Transformer transformer = TransformerFactory.newInstance().newTransformer();
		DOMSource source = new DOMSource(document);
		StreamResult result = new StreamResult(writer);
		transformer.transform(source, result);
	}

	private void printPretty(Document document, BufferedWriter writer) throws TransformerException
	{
		Transformer transformer = TransformerFactory.newInstance().newTransformer();
		transformer.setOutputProperty(OutputKeys.INDENT, "yes");
		transformer.setOutputProperty("{http://xml.apache.org/xslt}indent-amount", "2");
		DOMSource source = new DOMSource(document);
		StreamResult result = new StreamResult(writer);
		transformer.transform(source, result);
	}

	@Test
	public void testDoublePipeSplit()
	{
		String test = "One||Two";
		String[] split = Pattern.compile("\\|\\|").split(test);
		assertEquals(2, split.length);
		assertEquals("One", split[0]);
		assertEquals("Two", split[1]);
	}
}
