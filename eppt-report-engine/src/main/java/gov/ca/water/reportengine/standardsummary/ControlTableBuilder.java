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
import java.util.Objects;
import java.util.function.Function;
import java.util.function.IntFunction;
import java.util.logging.Level;
import java.util.logging.Logger;

import gov.ca.water.calgui.project.EpptScenarioRun;
import gov.ca.water.reportengine.EpptReportException;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import static gov.ca.water.reportengine.EPPTReport.EQUAL_TO_CONSTANT;
import static gov.ca.water.reportengine.EPPTReport.GREATER_THAN_CONSTANT;
import static gov.ca.water.reportengine.EPPTReport.LESS_THAN_CONSTANT;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 08-26-2019
 */
class ControlTableBuilder extends TableBuilder
{
	private static final Logger LOGGER = Logger.getLogger(ControlTableBuilder.class.getName());

	ControlTableBuilder(Document document, EpptScenarioRun base, List<EpptScenarioRun> alternatives,
						SummaryReportParameters reportParameters)
	{
		super(document, base, alternatives, reportParameters);
	}

	void buildTable(Element retval, EpptChart epptChart)
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
		int index = 0;
		Element baseElement = buildScenario(base, BASE_NAME, epptChart);
		baseElement.setAttribute(SCENARIO_ORDER_ATTRIBUTE, String.valueOf(index));
		retval.add(baseElement);
		index++;
		for(EpptScenarioRun alternative : getAlternatives())
		{
			Element altElement = buildScenario(alternative, ALT_NAME, epptChart);
			altElement.setAttribute(SCENARIO_ORDER_ATTRIBUTE, String.valueOf(index));
			index++;
			retval.add(altElement);
			Element element = buildScenarioDiff(alternative, epptChart);
			element.setAttribute(SCENARIO_ORDER_ATTRIBUTE, String.valueOf(index));
			index++;
			retval.add(element);
		}
		return retval;
	}

	private Element buildScenarioDiff(EpptScenarioRun alternative, EpptChart epptChart)
	{
		IntFunction<Function<ChartComponent, Element>> valueFunction = comparisonValue ->
				v -> buildDiffValueForChart(alternative, v, comparisonValue);
		return buildScenarioElement(DIFF_NAME, epptChart, valueFunction);
	}

	private Element buildScenario(EpptScenarioRun scenarioRun, String name, EpptChart epptChart)
	{
		IntFunction<Function<ChartComponent, Element>> valueFunction = comparisonValue ->
				v -> buildValueForChart(scenarioRun, v, comparisonValue);
		return buildScenarioElement(name, epptChart, valueFunction);
	}

	private Element buildScenarioElement(String name, EpptChart epptChart, IntFunction<Function<ChartComponent, Element>> valueFunction)
	{
		Element retval = getDocument().createElement(SCENARIO_ELEMENT);
		retval.setAttribute(SCENARIO_NAME_ATTRIBUTE, name);

		Element lessThan = buildComparisonElement(LESS_THAN_NAME, epptChart, valueFunction.apply(LESS_THAN_CONSTANT));
		Element equalTo = buildComparisonElement(EQUAL_TO_NAME, epptChart, valueFunction.apply(EQUAL_TO_CONSTANT));
		Element greaterThan = buildComparisonElement(GREATER_THAN_NAME, epptChart, valueFunction.apply(GREATER_THAN_CONSTANT));
		lessThan.setAttribute(COMPARISON_ORDER_ATTRIBUTE, String.valueOf(0));
		equalTo.setAttribute(COMPARISON_ORDER_ATTRIBUTE, String.valueOf(1));
		greaterThan.setAttribute(COMPARISON_ORDER_ATTRIBUTE, String.valueOf(2));
		retval.appendChild(lessThan);
		retval.appendChild(equalTo);
		retval.appendChild(greaterThan);
		return retval;
	}

	private Element buildComparisonElement(String name, EpptChart epptChart, Function<ChartComponent, Element> valueFunction)
	{
		Element retval = getDocument().createElement(COMPARISON_ELEMENT);
		retval.setAttribute(COMPARISON_NAME_ATTRIBUTE, name);
		appendTitles(retval, epptChart, valueFunction);
		return retval;
	}

	private Element buildDiffValueForChart(EpptScenarioRun alternative, ChartComponent v, int comparisonValue)
	{
		Element retval = getDocument().createElement(VALUE_ELEMENT);
		EpptScenarioRun base = getBase();
		try
		{
			long baseValue = createJythonValueGenerator(base, v.getFunction(), comparisonValue).generateCount();

			long altValue = createJythonValueGenerator(alternative, v.getFunction(), comparisonValue).generateCount();

			long diff = Math.round(baseValue - altValue);
			String absoluteText = String.valueOf(diff);
			retval.setTextContent(absoluteText);
			if(getReportParameters().getPercentDiffStyle() == PercentDiffStyle.PERCENT)
			{

				if(baseValue != 0)
				{
					int percent = (int) ((altValue - baseValue) / baseValue) * 100;
					retval.setAttribute(VALUE_FULL_TEXT_ATTRIBUTE, percent + "%");
				}
				else
				{
					retval.setAttribute(VALUE_FULL_TEXT_ATTRIBUTE, "N/A");
				}
			}
			else if(getReportParameters().getPercentDiffStyle() == PercentDiffStyle.FULL)
			{
				if(baseValue != 0)
				{
					int percent = (int) ((altValue - baseValue) / baseValue) * 100;
					retval.setAttribute(VALUE_FULL_TEXT_ATTRIBUTE, absoluteText + "\n(" + percent + "%)");
				}
				else
				{
					retval.setAttribute(VALUE_FULL_TEXT_ATTRIBUTE, absoluteText + "\n(N/A)");
				}
			}
		}
		catch(EpptReportException e)
		{
			LOGGER.log(Level.SEVERE, "Error running jython script", e);
		}
		return retval;
	}

	private Element buildValueForChart(EpptScenarioRun scenarioRun, ChartComponent v, int comparisonValue)
	{
		Element retval = getDocument().createElement(VALUE_ELEMENT);
		try
		{
			long count = createJythonValueGenerator(scenarioRun, v.getFunction(), comparisonValue).generateCount();
			String textRaw = String.valueOf(count);
			retval.setTextContent(textRaw);
		}
		catch(EpptReportException e)
		{
			LOGGER.log(Level.SEVERE, "Error running jython script", e);
		}
		return retval;
	}

}
