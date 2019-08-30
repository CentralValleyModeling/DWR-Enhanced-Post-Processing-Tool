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

import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;
import java.util.logging.Level;
import java.util.logging.Logger;

import gov.ca.water.calgui.project.EpptScenarioRun;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 08-26-2019
 */
class ListBuilder extends TableBuilder
{
	private static final Logger LOGGER = Logger.getLogger(ListBuilder.class.getName());

	ListBuilder(Document document, EpptScenarioRun base, List<EpptScenarioRun> alternatives,
				SummaryReportParameters reportParameters)
	{
		super(document, base, alternatives, reportParameters);
	}


	void buildList(Element retval, EpptChart epptChart)
	{
		appendScenarios(retval, epptChart);
	}

	private void appendScenarios(Element retval, EpptChart epptChart)
	{
		List<Element> elements = buildScenarios(epptChart);
		for(int i = 0; i < elements.size(); i++)
		{
			Element element = elements.get(i);
			element.setAttribute(SCENARIO_ORDER_ATTRIBUTE, String.valueOf(i));
			retval.appendChild(element);
		}
	}

	private List<Element> buildScenarios(EpptChart epptChart)
	{
		List<Element> retval = new ArrayList<>();
		EpptScenarioRun base = getBase();
		Element baseElement = buildScenarioElement(BASE_NAME, base, epptChart);
		retval.add(baseElement);
		for(int i = 0; i < getAlternatives().size(); i++)
		{
			EpptScenarioRun alternative = getAlternatives().get(i);
			Element altElement = buildScenarioElement(ALT_NAME, alternative, epptChart);
			altElement.setAttribute(SCENARIO_ORDER_ATTRIBUTE, String.valueOf(i + 1));
			retval.add(altElement);
		}
		return retval;
	}

	private Element buildScenarioElement(String name, EpptScenarioRun scenarioRun, EpptChart epptChart)
	{
		Element retval = getDocument().createElement(SCENARIO_ELEMENT);
		retval.setAttribute(SCENARIO_NAME_ATTRIBUTE, name);
		Function<ChartComponent, Element> valueFunction = v -> buildValueForChart(scenarioRun, v);
		appendTitles(retval, epptChart, valueFunction);
		return retval;
	}

	private Element buildValueForChart(EpptScenarioRun scenarioRun, ChartComponent v)
	{
		Element retval = getDocument().createElement(VALUE_ELEMENT);
		Object value = createJythonValueGenerator(scenarioRun, v.getFunction()).generateObjectValue();
		if(value == null)
		{
			LOGGER.log(Level.WARNING, "Unable to generate scenario value for: {0} value is null for scenario: {1}",
					new Object[]{v, scenarioRun.getName()});
		}
		else
		{
			String textRaw = String.valueOf(value);
			retval.setTextContent(textRaw);
		}
		return retval;
	}
}
