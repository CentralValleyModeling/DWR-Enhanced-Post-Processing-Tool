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

import java.time.format.TextStyle;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Locale;
import java.util.function.Function;
import java.util.logging.Level;
import java.util.logging.Logger;

import gov.ca.water.calgui.bo.PeriodFilter;
import gov.ca.water.calgui.bo.WaterYearPeriod;
import gov.ca.water.calgui.bo.WaterYearPeriodFilter;
import gov.ca.water.calgui.bo.WaterYearPeriodRange;
import gov.ca.water.calgui.bo.WaterYearPeriodRangeFilter;
import gov.ca.water.calgui.bo.WaterYearType;
import gov.ca.water.calgui.project.EpptScenarioRun;
import gov.ca.water.reportengine.EpptReportException;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import rma.util.RMAConst;

import static java.util.stream.Collectors.toList;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 08-26-2019
 */
class BaseAltDiffTableBuilder extends TableBuilder
{
	private static final Logger LOGGER = Logger.getLogger(BaseAltDiffTableBuilder.class.getName());

	BaseAltDiffTableBuilder(Document document, EpptScenarioRun base, List<EpptScenarioRun> alternatives,
							SummaryReportParameters reportParameters)
	{
		super(document, base, alternatives, reportParameters);
	}


	void buildTable(Element retval, EpptChart epptChart)
	{
		String units = "TAF";
		SummaryReportParameters reportParameters = getReportParameters();
		retval.setAttribute(UNITS_ATTRIBUTE, units);
		String startMonth = reportParameters.getWaterYearDefinition().getStartMonth().getDisplayName(TextStyle.SHORT, Locale.getDefault());
		String endMonth = reportParameters.getWaterYearDefinition().getEndMonth().getDisplayName(TextStyle.SHORT, Locale.getDefault());
		retval.setAttribute(WATER_YEAR_DEF_ATTRIBUTE, startMonth + "-" + endMonth);
		List<Element> periodElements = new ArrayList<>();
		periodElements.add(buildPeriod("Long Term", Collections.singletonList(reportParameters.getLongTermRange()), epptChart));
		if(reportParameters.getWaterYearIndex() != null)
		{
			periodElements.add(buildWaterYearIndex(epptChart));
		}
		periodElements.addAll(reportParameters.getWaterYearPeriodRanges().entrySet().stream()
											  .map(e -> buildPeriod(e.getKey().toString(), e.getValue(), epptChart))
											  .collect(toList()));
		for(int i = 0; i < periodElements.size(); i++)
		{
			Element element = periodElements.get(i);
			element.setAttribute(PERIOD_TYPE_ORDER_ATTRIBUTE, String.valueOf(i));
			retval.appendChild(element);
		}
	}

	private Element buildWaterYearIndex(EpptChart epptChart)
	{
		Element retval = getDocument().createElement(PERIOD_TYPE_ELEMENT);
		retval.setAttribute(PERIOD_TYPE_NAME_ATTRIBUTE, getReportParameters().getWaterYearIndex().toString());
		List<Element> collect = getReportParameters().getWaterYearIndex().getWaterYearTypes().stream().map(
				WaterYearType::getWaterYearPeriod).distinct()
													 .map(e -> buildSeasonalType(e, epptChart))
													 .collect(toList());
		for(int i = 0; i < collect.size(); i++)
		{
			Element element = collect.get(i);
			element.setAttribute(SEASONAL_TYPE_ORDER_ATTRIBUTE, String.valueOf(i));
			retval.appendChild(element);
		}
		return retval;
	}

	private Element buildPeriod(String periodName, List<WaterYearPeriodRange> ranges, EpptChart epptChart)
	{
		Element retval = getDocument().createElement(PERIOD_TYPE_ELEMENT);
		retval.setAttribute(PERIOD_TYPE_NAME_ATTRIBUTE, periodName);
		for(int i = 0; i < ranges.size(); i++)
		{
			Element element = buildSeasonalType(ranges.get(i), epptChart);
			element.setAttribute(SEASONAL_TYPE_ORDER_ATTRIBUTE, String.valueOf(i));
			retval.appendChild(element);
		}
		return retval;
	}

	private Element buildSeasonalType(WaterYearPeriod period, EpptChart epptChart)
	{
		Element retval = getDocument().createElement(SEASONAL_TYPE_ELEMENT);
		retval.setAttribute(SEASONAL_TYPE_NAME_ATTRIBUTE, period.getPeriodName());
		SummaryReportParameters reportParameters = getReportParameters();
		PeriodFilter filter = new WaterYearPeriodFilter(period, reportParameters.getWaterYearIndex());
		appendScenarios(retval, filter, epptChart);
		return retval;
	}

	private void appendScenarios(Element retval, PeriodFilter filter, EpptChart epptChart)
	{
		List<Element> elements = buildScenarios(filter, epptChart);
		for(int i = 0; i < elements.size(); i++)
		{
			Element element = elements.get(i);
			element.setAttribute(SCENARIO_ORDER_ATTRIBUTE, String.valueOf(i));
			retval.appendChild(element);
		}
	}

	private Element buildSeasonalType(WaterYearPeriodRange range, EpptChart epptChart)
	{
		Element retval = getDocument().createElement(SEASONAL_TYPE_ELEMENT);
		retval.setAttribute(SEASONAL_TYPE_NAME_ATTRIBUTE, range.toString(getReportParameters().getWaterYearDefinition(), getMonthYearFormatter()));
		SummaryReportParameters reportParameters = getReportParameters();
		PeriodFilter filter = new WaterYearPeriodRangeFilter(range, reportParameters.getWaterYearDefinition());
		appendScenarios(retval, filter, epptChart);
		return retval;
	}

	List<Element> buildScenarios(PeriodFilter filter, EpptChart epptChart)
	{
		List<Element> retval = new ArrayList<>();
		EpptScenarioRun base = getBase();
		int index = 0;
		Element baseElement = buildScenario(base, BASE_NAME, filter, epptChart);
		baseElement.setAttribute(SCENARIO_ORDER_ATTRIBUTE, String.valueOf(index));
		index++;
		retval.add(baseElement);
		for(EpptScenarioRun alternative : getAlternatives())
		{
			Element altElement = buildScenario(alternative, ALT_NAME, filter, epptChart);
			altElement.setAttribute(SCENARIO_ORDER_ATTRIBUTE, String.valueOf(index));
			index++;
			retval.add(altElement);
			Element element = buildScenarioDiff(alternative, filter, epptChart);
			altElement.setAttribute(SCENARIO_ORDER_ATTRIBUTE, String.valueOf(index));
			retval.add(element);
		}
		return retval;
	}

	private Element buildScenarioDiff(EpptScenarioRun alternative, PeriodFilter filter, EpptChart epptChart)
	{
		Function<ChartComponent, Element> valueFunction = v -> buildDiffValueForChart(alternative, v, filter);
		return buildScenarioElement(DIFF_NAME, epptChart, valueFunction);
	}

	private Element buildScenario(EpptScenarioRun scenarioRun, String name, PeriodFilter filter, EpptChart epptChart)
	{
		Function<ChartComponent, Element> valueFunction = v -> buildValueForChart(scenarioRun, v, filter);
		return buildScenarioElement(name, epptChart, valueFunction);
	}

	private Element buildScenarioElement(String name, EpptChart epptChart, Function<ChartComponent, Element> valueFunction)
	{
		Element retval = getDocument().createElement(SCENARIO_ELEMENT);
		retval.setAttribute(SCENARIO_NAME_ATTRIBUTE, name);
		appendTitles(retval, epptChart, valueFunction);
		return retval;
	}

	private Element buildDiffValueForChart(EpptScenarioRun alternative, ChartComponent v, PeriodFilter filter)
	{
		Element retval = getDocument().createElement(VALUE_ELEMENT);
		EpptScenarioRun base = getBase();
		try
		{
			Double baseValue = createJythonValueGenerator(filter, base, v.getFunction()).generateValue();

			Double altValue = createJythonValueGenerator(filter, alternative, v.getFunction()).generateValue();
			if(baseValue == null)
			{
				LOGGER.log(Level.WARNING,
						"Unable to generate diff value for: " + v + " value is null for scenario: " + base.getName());
			}
			else if(altValue == null)
			{
				LOGGER.log(Level.WARNING,
						"Unable to generate diff value for: " + v + " value is null for scenario: " + alternative.getName());
			}
			else if(!RMAConst.isValidValue(baseValue))
			{
				LOGGER.log(Level.WARNING,
						"Unable to generate diff value for: " + v + " value is invalid (" + baseValue + ") for scenario: " + base.getName());
			}
			else if(!RMAConst.isValidValue(altValue))
			{
				LOGGER.log(Level.WARNING,
						"Unable to generate diff value for: " + v + " value is invalid (" + altValue + ") for scenario: " + alternative.getName());
			}
			else
			{
				long baseValueRounded = Math.round(baseValue);
				long altValueRounded = Math.round(altValue);
				long absoluteDiff = altValueRounded - baseValueRounded;
				String absoluteText = String.valueOf(absoluteDiff);
				retval.setTextContent(absoluteText);
				if(getReportParameters().getPercentDiffStyle() == PercentDiffStyle.PERCENT)
				{

					if(baseValueRounded != 0)
					{
						long percent = Math.round(((altValueRounded - baseValueRounded) / baseValueRounded) * 100);
						retval.setAttribute(VALUE_FULL_TEXT_ATTRIBUTE, percent + "%");
					}
					else
					{
						retval.setAttribute(VALUE_FULL_TEXT_ATTRIBUTE, "N/A");
					}
				}
				else if(getReportParameters().getPercentDiffStyle() == PercentDiffStyle.FULL)
				{
					if(baseValueRounded != 0)
					{
						long percent = Math.round(((altValueRounded - baseValueRounded) / baseValueRounded) * 100);
						retval.setAttribute(VALUE_FULL_TEXT_ATTRIBUTE, absoluteText + "\n(" + percent + "%)");
					}
					else
					{
						retval.setAttribute(VALUE_FULL_TEXT_ATTRIBUTE, absoluteText + "\n(N/A)");
					}
				}
			}
		}
		catch(EpptReportException e)
		{
			LOGGER.log(Level.SEVERE, "Error running jython script", e);
		}
		return retval;
	}

	private Element buildValueForChart(EpptScenarioRun scenarioRun, ChartComponent v, PeriodFilter filter)
	{
		Element retval = getDocument().createElement(VALUE_ELEMENT);
		try
		{
			Double value = createJythonValueGenerator(filter, scenarioRun, v.getFunction()).generateValue();

			if(value == null)
			{
				LOGGER.log(Level.WARNING, "Unable to generate scenario value for: " + v + " value is null for scenario: " + scenarioRun.getName());
			}
			else if(!RMAConst.isValidValue(value))
			{
				LOGGER.log(Level.WARNING, "Unable to generate scenario value for: " + v + " value is invalid (" + value + ") for scenario: " + scenarioRun.getName());
			}
			else
			{
				String textRaw = String.valueOf(value);
				retval.setTextContent(textRaw);
			}
		}
		catch(EpptReportException e)
		{
			LOGGER.log(Level.SEVERE, "Error running jython script", e);
		}
		return retval;
	}


}
