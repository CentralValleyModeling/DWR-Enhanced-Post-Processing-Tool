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
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.regex.Pattern;

import gov.ca.water.calgui.constant.Constant;
import gov.ca.water.calgui.project.EpptScenarioRun;
import gov.ca.water.calgui.scripts.DssMissingRecordException;
import gov.ca.water.reportengine.EpptReportException;
import gov.ca.water.reportengine.jython.JythonValueGenerator;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import rma.util.RMAConst;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 08-26-2019
 */
class CoaTableBuilder extends TableBuilder
{
	private static final Logger LOGGER = Logger.getLogger(CoaTableBuilder.class.getName());
	private static final Pattern DOUBLE_PIPE_PATTERN = Pattern.compile("\\|\\|");

	CoaTableBuilder(Document document, EpptScenarioRun base, List<EpptScenarioRun> alternatives,
					SummaryReportParameters reportParameters, StandardSummaryErrors standardSummaryErrors)
	{
		super(document, base, alternatives, reportParameters, standardSummaryErrors);
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

		String cvpName = "CVP";
		getCvpName(epptChart);
		String swpName = "SWP";
		getSwpName(epptChart);


		epptChart.getChartComponents();
		List<ChartComponent> cvpChartComponents = new ArrayList<>();
		List<ChartComponent> swpChartComponents = new ArrayList<>();
		for(int i = 0; i < epptChart.getChartComponents().size(); i++)
		{
			if(i % 2 == 0)
			{
				cvpChartComponents.add(epptChart.getChartComponents().get(i));
			}
			else
			{
				swpChartComponents.add(epptChart.getChartComponents().get(i));
			}
		}
		EpptChart cvpChart = new EpptChart(epptChart.getModule(), epptChart.getSection(), epptChart.getSubModule(), epptChart.getChartType(),
				epptChart.getChartId(), cvpChartComponents);
		EpptChart swpChart = new EpptChart(epptChart.getModule(), epptChart.getSection(), epptChart.getSubModule(), epptChart.getChartType(),
				epptChart.getChartId(), swpChartComponents);
		Element cvpElement = buildRegionElement(cvpName, scenarioRun, cvpChart);
		Element swpElement = buildRegionElement(swpName, scenarioRun, swpChart);
		Element totalElement = buildRegionTotalElement(scenarioRun, cvpChart, swpChart);
		cvpElement.setAttribute(REGION_ORDER_ATTRIBUTE, String.valueOf(0));
		swpElement.setAttribute(REGION_ORDER_ATTRIBUTE, String.valueOf(1));
		totalElement.setAttribute(REGION_ORDER_ATTRIBUTE, String.valueOf(2));
		retval.appendChild(cvpElement);
		retval.appendChild(swpElement);
		retval.appendChild(totalElement);
		return retval;
	}

	private String getCvpName(EpptChart epptChart)
	{
		return epptChart.getChartComponents().stream()
						.map(chart -> chart.getSubHeader())
						.map(DOUBLE_PIPE_PATTERN::split)
						.map(s -> s[0])
						.findAny()
						.orElse("");
	}

	private String getSwpName(EpptChart epptChart)
	{
		return epptChart.getChartComponents().stream()
						.map(chart -> chart.getSubHeader())
						.map(DOUBLE_PIPE_PATTERN::split)
						.filter(s -> s.length > 1)
						.map(s -> s[1])
						.findAny()
						.orElse("");
	}

	private Element buildRegionElement(String name, EpptScenarioRun scenarioRun, EpptChart epptChart)
	{
		Element retval = getDocument().createElement(REGION_ELEMENT);
		retval.setAttribute(REGION_NAME_ATTRIBUTE, name);
		Function<ChartComponent, Element> valueFunction = v -> buildValueForChart(scenarioRun, v);
		appendTitles(retval, epptChart, valueFunction);
		return retval;
	}

	private Element buildRegionTotalElement(EpptScenarioRun scenarioRun, EpptChart cvpChart, EpptChart swpChart)
	{
		Element retval = getDocument().createElement(REGION_ELEMENT);
		retval.setAttribute(REGION_NAME_ATTRIBUTE, TOTAL_NAME);
		Function<ChartComponent, Element> valueFunction = v -> buildTotalValue(scenarioRun, swpChart, v);
		appendTitles(retval, cvpChart, valueFunction);
		return retval;
	}

	private Element buildTotalValue(EpptScenarioRun scenarioRun, EpptChart swpChart, ChartComponent v)
	{
		List<ChartComponent> chartComponents = swpChart.getChartComponents();
		for(ChartComponent component : chartComponents)
		{
			if(Objects.equals(component.getComponent(), v.getComponent()))
			{
				Element totalElem = getDocument().createElement(VALUE_ELEMENT);
				try
				{
					Object vObj = createJythonValueGenerator(scenarioRun, v.getFunction()).generateObjectValue();
					Object cObj = createJythonValueGenerator(scenarioRun, component.getFunction()).generateObjectValue();
					if(vObj instanceof Number && cObj instanceof Number)
					{
						totalElem.setTextContent(String.valueOf(Math.round(((Number) vObj).doubleValue() + ((Number) cObj).doubleValue())));
						return totalElem;
					}
				}
				catch(DssMissingRecordException e)
				{
					LOGGER.log(Level.FINE, "Missing record, displaying as NR", e);
					totalElem.setTextContent(NO_RECORD_TEXT);
				}
				catch(EpptReportException e)
				{
					getStandardSummaryErrors().addError(LOGGER, "Error running Script for total", e);
				}
				return totalElem;
			}
		}
		return buildTotalValueForChart(scenarioRun, v);
	}

	private Element buildTotalValueForChart(EpptScenarioRun alternative, ChartComponent v)
	{
		Element retval = getDocument().createElement(VALUE_ELEMENT);
		EpptScenarioRun base = getBase();
		try
		{
			JythonValueGenerator baseJythonValueGenerator = createJythonValueGenerator(base, v.getFunction());
			Object baseValue = baseJythonValueGenerator.generateObjectValue();

			Object altValue = createJythonValueGenerator(alternative, v.getFunction()).generateObjectValue();
			if(baseValue == null)
			{
				getStandardSummaryErrors().addError(LOGGER,
						"Unable to generate diff value for: " + v + " value is null for scenario: " + base.getName());
			}
			else if(altValue == null)
			{
				getStandardSummaryErrors().addError(LOGGER,
						"Unable to generate diff value for: " + v + " value is null for scenario: " + alternative.getName());
			}
			else if(baseValue instanceof Double && !RMAConst.isValidValue((Double) baseValue) && (Double)baseValue != -3.402823466E38)
			{
				getStandardSummaryErrors().addError(LOGGER,
						"Unable to generate diff value for: " + v + " value is invalid (" + baseValue + ") for scenario: " + base.getName());
			}
			else if(altValue instanceof Double && !RMAConst.isValidValue((Double) altValue) && (Double)altValue != -3.402823466E38)
			{
				getStandardSummaryErrors().addError(LOGGER,
						"Unable to generate diff value for: " + v + " value is invalid (" + baseValue + ") for scenario: " + alternative.getName());
			}
			else if(baseValue instanceof Double && altValue instanceof Double)
			{
				double value = (double) baseValue + (double) altValue;
				String textValue;
				textValue = Constant.getStringForDouble(value).getValue();
				retval.setTextContent(textValue);
			}
		}
		catch(DssMissingRecordException e)
		{
			LOGGER.log(Level.FINE, "Missing record, displaying as NR", e);
			retval.setTextContent(NO_RECORD_TEXT);
		}
		catch(EpptReportException e)
		{
			logScriptException(LOGGER, v, e);
		}
		return retval;
	}

	private Element buildValueForChart(EpptScenarioRun scenarioRun, ChartComponent v)
	{
		Element retval = getDocument().createElement(VALUE_ELEMENT);
		try
		{
			JythonValueGenerator jythonValueGenerator = createJythonValueGenerator(scenarioRun, v.getFunction());
			Object value = jythonValueGenerator.generateObjectValue();

			if(value == null)
			{
				getStandardSummaryErrors().addError(LOGGER,
						"Unable to generate scenario value for: " + v + " value is null for scenario: " + scenarioRun.getName());
			}
			else if(value instanceof Double && !RMAConst.isValidValue((Double) value))
			{
				getStandardSummaryErrors().addError(LOGGER,
						"Unable to generate scenario value for: " + v + " value is invalid (" + value + ") for scenario: " + scenarioRun.getName());
			}
			else
			{
				if(value instanceof Double)
				{
					String textValue;
					textValue = Constant.getStringForDouble((Double) value).getValue();
					retval.setTextContent(textValue);
				}
				else
				{
					String textRaw = String.valueOf(value);
					retval.setTextContent(textRaw);
				}
			}
		}
		catch(DssMissingRecordException e)
		{
			LOGGER.log(Level.FINE, "Missing record, displaying as NR", e);
			retval.setTextContent(NO_RECORD_TEXT);
		}
		catch(EpptReportException e)
		{
			logScriptException(LOGGER, v, e);
		}
		return retval;
	}
}
