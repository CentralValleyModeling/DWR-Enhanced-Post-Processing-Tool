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

import java.util.List;
import java.util.function.Function;
import java.util.logging.Level;
import java.util.logging.Logger;

import gov.ca.water.calgui.bo.WaterYearPeriod;
import gov.ca.water.calgui.project.EpptScenarioRun;
import gov.ca.water.reportengine.EpptReportException;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

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
		appendTitles(retval, epptChart);
	}

	void appendTitles(Element retval, EpptChart epptChart)
	{
		int i = 0;
		List<ChartComponent> componentsForTitle = epptChart.getChartComponents();
		Element titleElement = buildTitle(BASE_NAME, componentsForTitle, v -> buildValueForChart(getBase(), v));
		titleElement.setAttribute(TITLE_ORDER_ATTRIBUTE, String.valueOf(i));
		if(titleElement.getChildNodes().getLength() > 0)
		{
			retval.appendChild(titleElement);
		}
		i++;
		for(EpptScenarioRun scenarioRun : getAlternatives())
		{
			Element altTitle = buildTitle(ALT_NAME, componentsForTitle, v -> buildValueForChart(scenarioRun, v));
			altTitle.setAttribute(TITLE_ORDER_ATTRIBUTE, String.valueOf(i));
			if(altTitle.getChildNodes().getLength() > 0)
			{
				retval.appendChild(altTitle);
			}
			i++;
		}
	}

	private Element buildValueForChart(EpptScenarioRun scenarioRun, ChartComponent v)
	{
		Element retval = getDocument().createElement("placeholder");
		try
		{
			Object value = createJythonValueGenerator(scenarioRun, v.getFunction(), getReportParameters().getWaterYearIndex()).generateObjectValue();

			if(value == null)
			{
				LOGGER.log(Level.WARNING, "Unable to generate scenario value for: " + v + " value is null for scenario: " + scenarioRun.getName());
			}
			else if(value instanceof List)
			{
				List rows = (List) value;
				for(Object row : rows)
				{
					Element valueElem = getDocument().createElement(VALUE_ELEMENT);
					String textRaw = String.valueOf(row);
					valueElem.setTextContent(textRaw);
					retval.appendChild(valueElem);
				}
			}
		}
		catch(EpptReportException e)
		{
			LOGGER.log(Level.SEVERE, "Error running jython script", e);
		}
		return retval;
	}

	@Override
	Element buildComponent(ChartComponent component, Function<ChartComponent, Element> valueFunction)
	{
		Element retval = getDocument().createElement(COMPONENT_ELEMENT);
		retval.setAttribute(COMPONENT_NAME_ATTRIBUTE, component.getComponent());
		buildRowLabel(component.getComponent(), retval);
		buildValue(retval, component, valueFunction);
		return retval;
	}

	@Override
	void buildValue(Element componentElement, ChartComponent e, Function<ChartComponent, Element> valueFunction)
	{
		Element apply = valueFunction.apply(e);
		if(apply != null)
		{
			NodeList childNodes = apply.getChildNodes();
			for(int i = 0; i < childNodes.getLength(); i++)
			{
				Node item = childNodes.item(i);
				if(item instanceof Element)
				{
					((Element) item).setAttribute(VALUE_ORDER_ATTRIBUTE, String.valueOf(i));
					componentElement.appendChild(item);
				}
			}
		}
	}
}
