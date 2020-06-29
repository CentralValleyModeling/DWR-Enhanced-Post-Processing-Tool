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

import java.awt.*;
import java.util.ArrayList;
import java.util.DoubleSummaryStatistics;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.logging.Level;
import java.util.logging.Logger;

import gov.ca.water.calgui.bo.AnnualPeriodFilter;
import gov.ca.water.calgui.bo.PeriodFilter;
import gov.ca.water.calgui.bo.WaterYearPeriodRangesFilter;
import gov.ca.water.calgui.constant.Constant;
import gov.ca.water.calgui.project.EpptScenarioRun;
import gov.ca.water.calgui.scripts.DssMissingRecordException;
import gov.ca.water.reportengine.EpptReportException;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import rma.util.RMAConst;
import rma.util.TwoColorColorContour;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 08-26-2019
 */
class PercentDiffTableBuilder extends BaseAltDiffTableBuilder
{
	private static final Logger LOGGER = Logger.getLogger(PercentDiffTableBuilder.class.getName());
	private final Map<Element, Double> _valueElements = new HashMap<>();

	PercentDiffTableBuilder(Document document, EpptScenarioRun base, List<EpptScenarioRun> alternatives,
							SummaryReportParameters reportParameters,
							StandardSummaryErrors standardSummaryErrors)
	{
		super(document, base, alternatives, reportParameters, standardSummaryErrors);
	}

	@Override
	void buildTable(Element retval, EpptChart epptChart)
	{
		super.buildTable(retval, epptChart);
		SummaryReportParameters reportParameters = getReportParameters();
		DoubleSummaryStatistics doubleSummaryStatistics = _valueElements.values().stream().mapToDouble(i -> i).summaryStatistics();
		TwoColorColorContour positiveContour = reportParameters.getPositiveContour();
		positiveContour.setMinValue(0);
		positiveContour.setMaxValue(Math.max(0, doubleSummaryStatistics.getMax()));
		TwoColorColorContour negativeContour = reportParameters.getNegativeContour();
		negativeContour.setMinValue(Math.min(0, doubleSummaryStatistics.getMin()));
		negativeContour.setMaxValue(0);
		for(Map.Entry<Element, Double> entry : _valueElements.entrySet())
		{
			Element key = entry.getKey();
			Double value = entry.getValue();
			Color color = Color.WHITE;
			if(value != null)
			{
				if(value >= 0)
				{
					color = positiveContour.getColor(value);
				}
				else
				{
					color = negativeContour.getColor(value);
				}
			}
			String hexColor = Constant.colorToHex(color);
			key.setAttribute(BACKGROUND_COLOR_ATTRIBUTE, hexColor);
		}
	}

	@Override
	List<Element> buildScenarios(PeriodFilter filter, AnnualPeriodFilter annualPeriodFilter, EpptChart epptChart)
	{
		List<Element> retval = new ArrayList<>();
		for(int i = 0; i < getAlternatives().size(); i++)
		{
			EpptScenarioRun alternative = getAlternatives().get(i);
			Element element = buildPercentDiffElement(alternative, filter, annualPeriodFilter, filter, annualPeriodFilter, epptChart);
			element.setAttribute(SCENARIO_ORDER_ATTRIBUTE, String.valueOf(i));
			retval.add(element);
		}
		return retval;
	}

	@Override
	List<Element> buildScenarios(Map<EpptScenarioRun, WaterYearPeriodRangesFilter> filters, EpptChart epptChart)
	{
		WaterYearPeriodRangesFilter baseFilter = filters.get(getBase());
		AnnualPeriodFilter baseAnnualPeriodFilter = baseFilter::testAnnual;
		List<Element> retval = new ArrayList<>();
		for(int i = 0; i < getAlternatives().size(); i++)
		{
			EpptScenarioRun alternative = getAlternatives().get(i);
			WaterYearPeriodRangesFilter altFilter = filters.get(alternative);
			AnnualPeriodFilter altAnnualPeriodFilter = altFilter::testAnnual;
			Element element = buildPercentDiffElement(alternative, baseFilter, baseAnnualPeriodFilter,
					altFilter, altAnnualPeriodFilter, epptChart);
			element.setAttribute(SCENARIO_ORDER_ATTRIBUTE, String.valueOf(i));
			retval.add(element);
		}
		return retval;
	}

	private Element buildPercentDiffElement(EpptScenarioRun alternative, PeriodFilter baseFilter, AnnualPeriodFilter baseAnnualPeriodFilter,
											PeriodFilter altFilter, AnnualPeriodFilter altAnnualPeriodFilter, EpptChart epptChart)
	{
		Element retval = getDocument().createElement(SCENARIO_ELEMENT);
		Function<ChartComponent, Element> valueFunction = v -> buildPercentDiffValueForChart(alternative, v, baseFilter, baseAnnualPeriodFilter,
				altFilter, altAnnualPeriodFilter);
		appendTitles(retval, epptChart, valueFunction);
		return retval;
	}

	private Element buildPercentDiffValueForChart(EpptScenarioRun alternative, ChartComponent v, PeriodFilter baseFilter,
												  AnnualPeriodFilter baseAnnualPeriodFilter, PeriodFilter altFilter,
												  AnnualPeriodFilter altAnnualPeriodFilter)
	{
		Element retval = getDocument().createElement(VALUE_ELEMENT);
		EpptScenarioRun base = getBase();
		try
		{
			Double baseValue = createJythonValueGenerator(baseFilter, baseAnnualPeriodFilter, base, v.getFunction()).generateValue();
			Double altValue = createJythonValueGenerator(altFilter, altAnnualPeriodFilter, alternative, v.getFunction()).generateValue();
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
						"Unable to generate diff value for: " + v + " value is invalid (" + baseValue + ") for scenario: " + alternative.getName());
			}
			else
			{
				if(baseValue != 0)
				{
					double percentage = ((altValue - baseValue) / baseValue) * 100;
					String textValue = Constant.getStringForDouble(percentage).getValue();
					retval.setTextContent(textValue + "%");
					_valueElements.put(retval, percentage);
				}
				else if(altValue == 0)
				{
					retval.setTextContent("0%");
					_valueElements.put(retval, 0.0);
				}
				else
				{
					retval.setTextContent("A=" + altValue);
				}
			}
		}
		catch(DssMissingRecordException e)
		{
			LOGGER.log(Level.FINE, "Missing record, displaying as NR", e);
			retval.setTextContent(NO_RECORD_TEXT);
		}
		catch(EpptReportException | RuntimeException e)
		{
			getStandardSummaryErrors().addError(LOGGER, "Error executing jython script: " + e, e);
		}
		return retval;
	}

}
