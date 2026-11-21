/*
 * Enhanced Post Processing Tool (EPPT) Copyright (c) 2026.
 *
 * EPPT is copyrighted by the State of California, Department of Water Resources. It is licensed
 * under the GNU General Public License, version 2. This means it can be
 * copied, distributed, and modified freely, but you may not restrict others
 * in their ability to copy, distribute, and modify it. See the license below
 * for more details.
 *
 * GNU General Public License
 */

package gov.ca.water.plotly;

import java.awt.Color;
import java.awt.Graphics2D;
import java.awt.geom.Dimension2D;
import java.awt.image.BufferedImage;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.time.Month;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.NavigableMap;
import java.util.TreeMap;

import javax.xml.parsers.DocumentBuilderFactory;

import gov.ca.water.calgui.project.EpptScenarioRun;
import gov.ca.water.plotly.qaqc.PlotlyExceedance;
import gov.ca.water.plotly.qaqc.PlotlyMonthly;
import org.apache.batik.anim.dom.SAXSVGDocumentFactory;
import org.apache.batik.bridge.BridgeContext;
import org.apache.batik.bridge.DocumentLoader;
import org.apache.batik.bridge.GVTBuilder;
import org.apache.batik.bridge.UserAgentAdapter;
import org.apache.batik.gvt.GraphicsNode;
import org.apache.batik.util.XMLResourceDescriptor;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.w3c.dom.Element;
import org.w3c.dom.svg.SVGDocument;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Renders report charts with the templates and plotly.js the app ships, so a plotly upgrade that breaks report images fails here.
 */
class PlotlySvgPrinterTest
{
	private static final Path EPPT_HOME = Paths.get("../eppt-nbui/src/main").toAbsolutePath().normalize();
	private static final Path TEMPLATES = EPPT_HOME.resolve("config/qaqc/templates");
	private static final Path PLOTLY_JS = EPPT_HOME.resolve("lib/plotly/dist/plotly.min.js");
	private static final String SVG_NAMESPACE = "http://www.w3.org/2000/svg";

	private final EpptScenarioRun _base = new EpptScenarioRun("Base Scenario", null, null, null, null, null, null,
			javafx.scene.paint.Color.BLUE);
	private final EpptScenarioRun _alt = new EpptScenarioRun("Alt Scenario", null, null, null, null, null, null,
			javafx.scene.paint.Color.RED);

	@Test
	void rendersReportChartsToSvg(@TempDir Path dir) throws Exception
	{
		PlotlySvgPrinter.printJsonToPath(dir.resolve("monthly.svg"), buildMonthly());
		PlotlySvgPrinter.printJsonToPath(dir.resolve("exceedance.svg"), buildExceedance());

		PlotlySvgPrinter.printSvg(dir, PLOTLY_JS);

		assertSvg(dir.resolve("monthly.svg"), "Shasta Monthly Storage");
		assertSvg(dir.resolve("exceedance.svg"), "Shasta Storage Exceedance");
	}

	private static void assertSvg(Path svg, String title) throws Exception
	{
		assertTrue(Files.isRegularFile(svg), "SVG not written: " + svg);
		String text = new String(Files.readAllBytes(svg), StandardCharsets.UTF_8);
		DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
		factory.setNamespaceAware(true);
		Element root = factory.newDocumentBuilder().parse(svg.toFile()).getDocumentElement();
		assertEquals("svg", root.getLocalName());
		assertEquals(SVG_NAMESPACE, root.getNamespaceURI());
		assertEquals("700", root.getAttribute("width"));
		assertEquals("500", root.getAttribute("height"));
		assertTrue(text.contains("class=\"main-svg\""), "missing plotly main-svg");
		assertTrue(text.contains("class=\"scatterlayer"), "missing scatter traces");
		assertTrue(text.contains(">" + title + "<"), "missing chart title " + title);
		assertTrue(text.contains(">Base Scenario<"), "missing legend entry for base");
		assertTrue(text.contains(">Alt Scenario<"), "missing legend entry for alternative");
		assertPaintsWithBatik(svg);
	}

	/**
	 * Jasper draws report SVGs with Batik, which is stricter than a browser.
	 */
	private static void assertPaintsWithBatik(Path svg) throws Exception
	{
		SAXSVGDocumentFactory factory = new SAXSVGDocumentFactory(XMLResourceDescriptor.getXMLParserClassName());
		SVGDocument document = factory.createSVGDocument(svg.toUri().toString());
		UserAgentAdapter userAgent = new UserAgentAdapter();
		BridgeContext context = new BridgeContext(userAgent, new DocumentLoader(userAgent));
		GraphicsNode node = new GVTBuilder().build(context, document);
		Dimension2D size = context.getDocumentSize();
		BufferedImage image = new BufferedImage((int) size.getWidth(), (int) size.getHeight(), BufferedImage.TYPE_INT_RGB);
		Graphics2D graphics = image.createGraphics();
		graphics.setColor(Color.WHITE);
		graphics.fillRect(0, 0, image.getWidth(), image.getHeight());
		node.paint(graphics);
		graphics.dispose();
		long drawn = 0;
		for(int x = 0; x < image.getWidth(); x++)
		{
			for(int y = 0; y < image.getHeight(); y++)
			{
				if(image.getRGB(x, y) != Color.WHITE.getRGB())
				{
					drawn++;
				}
			}
		}
		assertTrue(drawn > 1000, "Batik drew almost nothing: " + drawn + " pixels");
	}

	private PlotlyMonthly buildMonthly()
	{
		Map<EpptScenarioRun, List<PlotlyMonthly.MonthlyData>> data = new LinkedHashMap<>();
		data.put(_base, new ArrayList<>());
		data.put(_alt, new ArrayList<>());
		for(Month month : Month.values())
		{
			data.get(_base).add(new PlotlyMonthly.MonthlyData(month, 3000.0 + month.getValue() * 50));
			data.get(_alt).add(new PlotlyMonthly.MonthlyData(month, 2800.0 + month.getValue() * 60));
		}
		return new PlotlyMonthly("Shasta Monthly Storage", "Month", "Storage (TAF)", data, Collections.emptyMap())
		{
			@Override
			protected Path getTemplatePath()
			{
				return TEMPLATES.resolve("monthly_chart.json");
			}
		};
	}

	private PlotlyExceedance buildExceedance()
	{
		Map<EpptScenarioRun, ExceedanceData> alternatives = new HashMap<>();
		alternatives.put(_alt, exceedanceData("Alt Scenario", 2500.0));
		return new PlotlyExceedance("Shasta Storage Exceedance", "Storage (TAF)", "Exceedance Probability (%)", _base,
				exceedanceData("Base Scenario", 3000.0), alternatives)
		{
			@Override
			protected Path getTemplatePath()
			{
				return TEMPLATES.resolve("exceedance_chart.json");
			}
		};
	}

	private static ExceedanceData exceedanceData(String scenario, double start)
	{
		NavigableMap<Double, Double> primary = new TreeMap<>();
		for(int i = 1; i <= 9; i++)
		{
			primary.put(i / 10.0, start + (10 - i) * 200);
		}
		return new ExceedanceData(scenario, primary, Collections.emptyMap());
	}
}
