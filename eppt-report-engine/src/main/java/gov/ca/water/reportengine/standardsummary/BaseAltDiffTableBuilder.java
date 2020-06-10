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
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.function.Function;
import java.util.logging.Level;
import java.util.logging.Logger;

import gov.ca.water.calgui.bo.AnnualPeriodFilter;
import gov.ca.water.calgui.bo.PeriodFilter;
import gov.ca.water.calgui.bo.WaterYearPeriodRange;
import gov.ca.water.calgui.bo.WaterYearPeriodRangeFilter;
import gov.ca.water.calgui.bo.WaterYearPeriodRangesFilter;
import gov.ca.water.calgui.bo.YearlyWaterYearPeriodRangeFilter;
import gov.ca.water.calgui.constant.Constant;
import gov.ca.water.calgui.project.EpptScenarioRun;
import gov.ca.water.calgui.scripts.DssMissingRecordException;
import gov.ca.water.reportengine.EpptReportException;
import gov.ca.water.reportengine.jython.JythonValueGenerator;
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
	private String _units;

	BaseAltDiffTableBuilder(Document document, EpptScenarioRun base, List<EpptScenarioRun> alternatives,
							SummaryReportParameters reportParameters,
							StandardSummaryErrors standardSummaryErrors)
	{
		super(document, base, alternatives, reportParameters, standardSummaryErrors);
	}


	void buildTable(Element retval, EpptChart epptChart)
	{
		_units = null;
		SummaryReportParameters reportParameters = getReportParameters();
		String startMonth = reportParameters.getWaterYearDefinition().getStartMonth().getDisplayName(TextStyle.SHORT, Locale.getDefault());
		String endMonth = reportParameters.getWaterYearDefinition().getEndMonth().getDisplayName(TextStyle.SHORT, Locale.getDefault());
		if("resops-summary-may".equalsIgnoreCase(epptChart.getChartId()))
		{
			retval.setAttribute(WATER_YEAR_DEF_ATTRIBUTE, "End of May");
		}
		else if("resops-summary-sept".equalsIgnoreCase(epptChart.getChartId()))
		{
			retval.setAttribute(WATER_YEAR_DEF_ATTRIBUTE, "End of September");
		}
		else
		{
			retval.setAttribute(WATER_YEAR_DEF_ATTRIBUTE, startMonth + "-" + endMonth);
		}
		List<Element> periodElements = new ArrayList<>();
		for(SummaryReportParameters.WaterYearPeriodRangeGroup group : reportParameters.getGroupedWaterYearPeriodRanges())
		{
			periodElements.add(buildAnnualPeriods(epptChart, group));
		}
		for(int i = 0; i < periodElements.size(); i++)
		{
			Element element = periodElements.get(i);
			element.setAttribute(PERIOD_TYPE_ORDER_ATTRIBUTE, String.valueOf(i));
			retval.appendChild(element);
		}
		if(_units != null)
		{
			retval.setAttribute(UNITS_ATTRIBUTE, _units);
		}
		else
		{
			retval.setAttribute(UNITS_ATTRIBUTE, "");
		}
	}

	private Element buildAnnualPeriods(EpptChart epptChart,
									   SummaryReportParameters.WaterYearPeriodRangeGroup range)
	{
		Element retval = getDocument().createElement(PERIOD_TYPE_ELEMENT);
		retval.setAttribute(PERIOD_TYPE_NAME_ATTRIBUTE, range.getGroupName());
		List<Element> collect = range.getFilters()
									 .stream()
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

	private Element buildSeasonalType(Map<EpptScenarioRun, WaterYearPeriodRangesFilter> period, EpptChart epptChart)
	{
		Element retval = getDocument().createElement(SEASONAL_TYPE_ELEMENT);
		retval.setAttribute(SEASONAL_TYPE_NAME_ATTRIBUTE, period.values().iterator().next().getName());
		appendScenarios(retval, period, epptChart);
		return retval;
	}

	private void appendScenarios(Element retval, Map<EpptScenarioRun, WaterYearPeriodRangesFilter> filters, EpptChart epptChart)
	{
		List<Element> elements = buildScenarios(filters, epptChart);
		for(int i = 0; i < elements.size(); i++)
		{
			Element element = elements.get(i);
			element.setAttribute(SCENARIO_ORDER_ATTRIBUTE, String.valueOf(i));
			retval.appendChild(element);
		}
	}

	private void appendScenarios(Element retval, PeriodFilter filter, AnnualPeriodFilter annualPeriodFilter, EpptChart epptChart)
	{
		List<Element> elements = buildScenarios(filter, annualPeriodFilter, epptChart);
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
		AnnualPeriodFilter annualPeriodFilter = new YearlyWaterYearPeriodRangeFilter(range);
		appendScenarios(retval, filter, annualPeriodFilter, epptChart);
		return retval;
	}

	List<Element> buildScenarios(Map<EpptScenarioRun, WaterYearPeriodRangesFilter> filters, EpptChart epptChart)
	{
		WaterYearPeriodRangesFilter baseFilter = filters.get(getBase());
		AnnualPeriodFilter baseAnnualPeriodFilter = baseFilter::testAnnual;
		List<Element> retval = new ArrayList<>();
		EpptScenarioRun base = getBase();
		int index = 0;
		Element baseElement = buildScenario(base, BASE_NAME, baseFilter, baseAnnualPeriodFilter, epptChart);
		baseElement.setAttribute(SCENARIO_ORDER_ATTRIBUTE, String.valueOf(index));
		index++;
		retval.add(baseElement);
		for(EpptScenarioRun alternative : getAlternatives())
		{
			WaterYearPeriodRangesFilter altFilter = filters.get(alternative);
			AnnualPeriodFilter altAnnualPeriodFilter = altFilter::testAnnual;
			Element altElement = buildScenario(alternative, ALT_NAME, altFilter, altAnnualPeriodFilter, epptChart);
			altElement.setAttribute(SCENARIO_ORDER_ATTRIBUTE, String.valueOf(index));
			index++;
			retval.add(altElement);
			Element element = buildScenarioDiff(alternative, filters, epptChart);
			altElement.setAttribute(SCENARIO_ORDER_ATTRIBUTE, String.valueOf(index));
			retval.add(element);
		}
		return retval;
	}

	List<Element> buildScenarios(PeriodFilter filter, AnnualPeriodFilter annualPeriodFilter, EpptChart epptChart)
	{
		List<Element> retval = new ArrayList<>();
		EpptScenarioRun base = getBase();
		int index = 0;
		Element baseElement = buildScenario(base, BASE_NAME, filter, annualPeriodFilter, epptChart);
		baseElement.setAttribute(SCENARIO_ORDER_ATTRIBUTE, String.valueOf(index));
		index++;
		retval.add(baseElement);
		for(EpptScenarioRun alternative : getAlternatives())
		{
			Element altElement = buildScenario(alternative, ALT_NAME, filter, annualPeriodFilter, epptChart);
			altElement.setAttribute(SCENARIO_ORDER_ATTRIBUTE, String.valueOf(index));
			index++;
			retval.add(altElement);
			Element element = buildScenarioDiff(alternative, filter, annualPeriodFilter, epptChart);
			altElement.setAttribute(SCENARIO_ORDER_ATTRIBUTE, String.valueOf(index));
			retval.add(element);
		}
		return retval;
	}

	private Element buildScenarioDiff(EpptScenarioRun alternative, Map<EpptScenarioRun, WaterYearPeriodRangesFilter> filters, EpptChart epptChart)
	{
		Function<ChartComponent, Element> valueFunction = v -> buildDiffValueForChart(alternative, v, filters);
		return buildScenarioElement(DIFF_NAME, epptChart, valueFunction);
	}

	private Element buildScenarioDiff(EpptScenarioRun alternative, PeriodFilter filter, AnnualPeriodFilter annualPeriodFilter, EpptChart epptChart)
	{
		Function<ChartComponent, Element> valueFunction = v -> buildDiffValueForChart(alternative, v, filter, annualPeriodFilter);
		return buildScenarioElement(DIFF_NAME, epptChart, valueFunction);
	}

	private Element buildScenario(EpptScenarioRun scenarioRun, String name, PeriodFilter filter, AnnualPeriodFilter annualPeriodFilter,
								  EpptChart epptChart)
	{
		Function<ChartComponent, Element> valueFunction = v -> buildValueForChart(scenarioRun, v, filter, annualPeriodFilter);
		return buildScenarioElement(name, epptChart, valueFunction);
	}

	private Element buildScenarioElement(String name, EpptChart epptChart, Function<ChartComponent, Element> valueFunction)
	{
		Element retval = getDocument().createElement(SCENARIO_ELEMENT);
		retval.setAttribute(SCENARIO_NAME_ATTRIBUTE, name);
		appendTitles(retval, epptChart, valueFunction);
		return retval;
	}

	private Element buildDiffValueForChart(EpptScenarioRun alternative, ChartComponent v, Map<EpptScenarioRun, WaterYearPeriodRangesFilter> filters)
	{
		Element retval = getDocument().createElement(VALUE_ELEMENT);
		EpptScenarioRun base = getBase();
		try
		{
			WaterYearPeriodRangesFilter baseFilter = filters.get(base);
			AnnualPeriodFilter baseAnnualPeriodFilter = baseFilter::testAnnual;
			WaterYearPeriodRangesFilter altFilter = filters.get(alternative);
			AnnualPeriodFilter altAnnualPeriodFilter = altFilter::testAnnual;
			JythonValueGenerator baseValueGenerator = createJythonValueGenerator(baseFilter, baseAnnualPeriodFilter, base, v.getFunction());
			Double baseValue = baseValueGenerator.generateValue();
			JythonValueGenerator altValueGenerator = createJythonValueGenerator(altFilter, altAnnualPeriodFilter, alternative, v.getFunction());
			Double altValue = altValueGenerator.generateValue();
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
			else if(!RMAConst.isValidValue(baseValue) && baseValue != -3.402823466E38)
			{
				getStandardSummaryErrors().addError(LOGGER,
						"Unable to generate diff value for: " + v + " value is invalid (" + baseValue + ") for scenario: " + base.getName());
			}
			else if(!RMAConst.isValidValue(altValue) && altValue != -3.402823466E38)
			{
				getStandardSummaryErrors().addError(LOGGER,
						"Unable to generate diff value for: " + v + " value is invalid (" + altValue + ") for scenario: " + alternative.getName());
			}
			else
			{
				applyPercentDiffStyles(retval, baseValue, altValue);
				String units = baseValueGenerator.getUnits();
				if(units == null)
				{
					units = altValueGenerator.getUnits();
				}
				if(units != null && _units == null)
				{
					_units = units;
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

	private Element buildDiffValueForChart(EpptScenarioRun alternative, ChartComponent v, PeriodFilter filter,
										   AnnualPeriodFilter annualPeriodFilter)
	{
		Element retval = getDocument().createElement(VALUE_ELEMENT);
		EpptScenarioRun base = getBase();
		try
		{
			JythonValueGenerator baseValueGenerator = createJythonValueGenerator(filter, annualPeriodFilter, base, v.getFunction());
			Double baseValue = baseValueGenerator.generateValue();
			JythonValueGenerator altValueGenerator = createJythonValueGenerator(filter, annualPeriodFilter, alternative, v.getFunction());
			Double altValue = altValueGenerator.generateValue();
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
			else if(!RMAConst.isValidValue(baseValue) && baseValue != -3.402823466E38)
			{
				getStandardSummaryErrors().addError(LOGGER,
						"Unable to generate diff value for: " + v + " value is invalid (" + baseValue + ") for scenario: " + base.getName());
			}
			else if(!RMAConst.isValidValue(altValue) && altValue != -3.402823466E38)
			{
				getStandardSummaryErrors().addError(LOGGER,
						"Unable to generate diff value for: " + v + " value is invalid (" + altValue + ") for scenario: " + alternative.getName());
			}
			else
			{
				applyPercentDiffStyles(retval, baseValue, altValue);
				String units = baseValueGenerator.getUnits();
				if(units == null)
				{
					units = altValueGenerator.getUnits();
				}
				if(units != null && _units == null)
				{
					_units = units;
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

	private void applyPercentDiffStyles(Element retval, double baseValue, double altValue)
	{
		long absoluteDiff = Math.round(altValue) - Math.round(baseValue);
		String absoluteText = String.valueOf(absoluteDiff);
		retval.setTextContent(absoluteText);
		if(getReportParameters().getPercentDiffStyle() == PercentDiffStyle.PERCENT)
		{

			if(baseValue != 0)
			{
				double percent = ((altValue - baseValue) / baseValue) * 100;
				String textValue = Constant.getStringForDouble(percent).getValue();
				retval.setAttribute(VALUE_FULL_TEXT_ATTRIBUTE, textValue + "%");
			}
			else if(altValue == 0)
			{
				retval.setAttribute(VALUE_FULL_TEXT_ATTRIBUTE, "0%");
			}
			else
			{
				retval.setAttribute(VALUE_FULL_TEXT_ATTRIBUTE, "A=" + altValue);
			}
		}
		else if(getReportParameters().getPercentDiffStyle() == PercentDiffStyle.FULL)
		{
			if(baseValue != 0)
			{
				double percent = ((altValue - baseValue) / baseValue) * 100;
				String textValue = Constant.getStringForDouble(percent).getValue();
				retval.setAttribute(VALUE_FULL_TEXT_ATTRIBUTE, absoluteText + "\n(" + textValue + "%)");
			}
			else if(altValue == 0)
			{
				retval.setAttribute(VALUE_FULL_TEXT_ATTRIBUTE, absoluteText + "\n(0%)");
			}
			else
			{
				retval.setAttribute(VALUE_FULL_TEXT_ATTRIBUTE, absoluteText + "\n(A=" + Math.round(altValue) + ")");
			}
		}
	}

	Element buildValueForChart(EpptScenarioRun scenarioRun, ChartComponent v, PeriodFilter filter, AnnualPeriodFilter annualPeriodFilter)
	{
		Element retval = getDocument().createElement(VALUE_ELEMENT);
		try
		{
			JythonValueGenerator jythonValueGenerator = createJythonValueGenerator(filter, annualPeriodFilter, scenarioRun, v.getFunction());
			Double value = jythonValueGenerator.generateValue();

			if(value == null)
			{
				getStandardSummaryErrors().addError(LOGGER,
						"Unable to generate scenario value for: " + v + " value is null for scenario: " + scenarioRun.getName());
			}
			else if(!RMAConst.isValidValue(value) && value != -3.402823466E38)
			{
				getStandardSummaryErrors().addError(LOGGER,
						"Unable to generate scenario value for: " + v + " value is invalid (" + value + ") for scenario: " + scenarioRun.getName());
			}
			else
			{
				String units = jythonValueGenerator.getUnits();
				if(units != null && _units == null)
				{
					_units = units;
				}
				String textValue = Constant.getStringForDouble(value).getValue();
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


}
