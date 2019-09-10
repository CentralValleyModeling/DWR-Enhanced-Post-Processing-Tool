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
import javax.script.ScriptException;

import gov.ca.water.calgui.bo.PeriodFilter;
import gov.ca.water.calgui.project.EpptScenarioRun;
import gov.ca.water.reportengine.EpptReportException;
import gov.ca.water.reportengine.jython.JythonValueGenerator;
import org.w3c.dom.Document;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 08-26-2019
 */
abstract class StandardSummaryChartBuilder
{
	static final String BASE_NAME = "Base";
	static final String ALT_NAME = "Alt";
	static final String DIFF_NAME = "Diff";
	static final String LESS_THAN_NAME = "Less Than";
	static final String GREATER_THAN_NAME = "Greater Than";
	static final String TOTAL_NAME = "Total";
	static final String UNITS_ATTRIBUTE = "units";
	static final String EQUAL_TO_NAME = "Equal To";
	static final String WATER_YEAR_DEF_ATTRIBUTE = "water-year-def";
	static final String PERIOD_TYPE_ELEMENT = "period-type";
	static final String PERIOD_TYPE_NAME_ATTRIBUTE = "period-type-name";
	static final String PERIOD_TYPE_ORDER_ATTRIBUTE = "period-type-order";
	static final String SEASONAL_TYPE_ELEMENT = "seasonal-type";
	static final String SEASONAL_TYPE_NAME_ATTRIBUTE = "seasonal-type-name";
	static final String SEASONAL_TYPE_ORDER_ATTRIBUTE = "seasonal-type-order";
	static final String SCENARIO_ELEMENT = "scenario";
	static final String SCENARIO_NAME_ATTRIBUTE = "scenario-name";
	static final String SCENARIO_ORDER_ATTRIBUTE = "scenario-order";
	static final String TITLE_ELEMENT = "title";
	static final String TITLE_ORDER_ATTRIBUTE = "title-order";
	static final String TITLE_NAME_ATTRIBUTE = "title-name";
	static final String HEADER_ELEMENT = "header";
	static final String HEADER_ORDER_ATTRIBUTE = "header-order";
	static final String HEADER_NAME_ATTRIBUTE = "header-name";
	static final String SUBHEADER_ELEMENT = "sub-header";
	static final String SUBHEADER_ORDER_ATTRIBUTE = "sub-header-order";
	static final String SUBHEADER_NAME_ATTRIBUTE = "sub-header-name";
	static final String COMPONENT_ELEMENT = "component";
	static final String COMPONENT_ORDER_ATTRIBUTE = "component-order";
	static final String COMPONENT_NAME_ATTRIBUTE = "component-name";
	static final String ROW_LABEL_ELEMENT = "row-label";
	static final String VALUE_ELEMENT = "value";
	static final String VALUE_FULL_TEXT_ATTRIBUTE = "value-full-text";
	static final String VALUE_PERCENT_TEXT_ATTRIBUTE = "value-percent-text";
	static final String COMPARISON_ELEMENT = "less-than-greater-than-equal-to";
	static final String COMPARISON_ORDER_ATTRIBUTE = "less-greater-equal-order";
	static final String COMPARISON_NAME_ATTRIBUTE = "less-greater-equal-label";
	static final String REGION_ELEMENT = "region";
	static final String REGION_ORDER_ATTRIBUTE = "region-order";
	static final String REGION_NAME_ATTRIBUTE = "region-label";
	static final String BACKGROUND_COLOR_ATTRIBUTE = "background-color";
	static final String SVG_FILE_LOCATION_ATTRIBUTE = "svg-file";
	private final Document _document;
	private final EpptScenarioRun _base;
	private final List<EpptScenarioRun> _alternatives;
	private final SummaryReportParameters _reportParameters;

	StandardSummaryChartBuilder(Document document, EpptScenarioRun base, List<EpptScenarioRun> alternatives, SummaryReportParameters reportParameters)
	{
		_document = document;
		_base = base;
		_alternatives = alternatives;
		_reportParameters = reportParameters;
	}

	Document getDocument()
	{
		return _document;
	}

	EpptScenarioRun getBase()
	{
		return _base;
	}

	List<EpptScenarioRun> getAlternatives()
	{
		return _alternatives;
	}

	SummaryReportParameters getReportParameters()
	{
		return _reportParameters;
	}

	JythonValueGenerator createJythonValueGenerator(PeriodFilter filter, EpptScenarioRun epptScenarioRun, String function) throws EpptReportException
	{
		try
		{
			return new JythonValueGenerator(filter, epptScenarioRun, function, _reportParameters.getCommonPeriodFilter());
		}
		catch(ScriptException e)
		{
			throw new EpptReportException("Error initializing Jython script runner", e);
		}
	}

	JythonValueGenerator createJythonValueGenerator(EpptScenarioRun epptScenarioRun, String function) throws EpptReportException
	{
		try
		{
			return new JythonValueGenerator(epptScenarioRun, function, _reportParameters.getCommonPeriodFilter());
		}
		catch(ScriptException e)
		{
			throw new EpptReportException("Error initializing Jython script runner", e);
		}
	}

	JythonValueGenerator createJythonValueGenerator(EpptScenarioRun epptScenarioRun, String function, int comparisonValue) throws EpptReportException
	{
		try
		{
			return new JythonValueGenerator(epptScenarioRun, function, _reportParameters.getCommonPeriodFilter(), comparisonValue);
		}
		catch(ScriptException e)
		{
			throw new EpptReportException("Error initializing Jython script runner", e);
		}
	}
}
