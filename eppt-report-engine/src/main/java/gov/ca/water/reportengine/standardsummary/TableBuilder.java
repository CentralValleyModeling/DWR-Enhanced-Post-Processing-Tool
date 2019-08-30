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

import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.function.Function;

import gov.ca.water.calgui.project.EpptScenarioRun;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import static java.util.stream.Collectors.groupingBy;
import static java.util.stream.Collectors.toList;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 08-28-2019
 */
abstract class TableBuilder extends StandardSummaryChartBuilder
{
	private final DateTimeFormatter _monthYearFormatter = DateTimeFormatter.ofPattern("MMM yy");

	TableBuilder(Document document, EpptScenarioRun base,
				 List<EpptScenarioRun> alternatives,
				 SummaryReportParameters reportParameters)
	{
		super(document, base, alternatives, reportParameters);
	}

	void appendTitles(Element retval, EpptChart epptChart, Function<ChartComponent, Element> valueFunction)
	{
		Map<String, List<ChartComponent>> groupedByTitle = epptChart.getChartComponents().stream().collect(groupingBy(ChartComponent::getTitle));
		int i = 0;
		for(String title : epptChart.getChartComponents().stream().map(ChartComponent::getTitle).distinct().collect(toList()))
		{
			List<ChartComponent> componentsForTitle = groupedByTitle.getOrDefault(title, new ArrayList<>());
			Element titleElement = buildTitle(title, componentsForTitle, valueFunction);
			titleElement.setAttribute(TITLE_ORDER_ATTRIBUTE, String.valueOf(i));
			retval.appendChild(titleElement);
			i++;
		}
	}

	private Element buildTitle(String title, List<ChartComponent> componentsForTitle, Function<ChartComponent, Element> valueFunction)
	{
		Element retval = getDocument().createElement(TITLE_ELEMENT);
		if(!title.isEmpty())
		{
			appendTitlePlaceholder(title, componentsForTitle, valueFunction, retval);
		}
		Map<String, List<ChartComponent>> groupedByHeaders = componentsForTitle.stream().collect(groupingBy(ChartComponent::getHeader));
		int i = 0;
		for(String header : componentsForTitle.stream().map(ChartComponent::getHeader).distinct().collect(toList()))
		{
			List<ChartComponent> componentsForHeader = groupedByHeaders.getOrDefault(header, new ArrayList<>());
			Element headerElement = buildHeader(header, componentsForHeader, valueFunction);
			headerElement.setAttribute(HEADER_ORDER_ATTRIBUTE, String.valueOf(i));
			retval.appendChild(headerElement);
			i++;
		}
		return retval;
	}

	private void appendTitlePlaceholder(String title, List<ChartComponent> componentsForTitle, Function<ChartComponent, Element> valueFunction,
										Element retval)
	{
		Element headerPlaceholder = getDocument().createElement(HEADER_ELEMENT);
		Element subHeaderPlaceholder = getDocument().createElement(SUBHEADER_ELEMENT);
		Element componentPlaceholder = getDocument().createElement(COMPONENT_ELEMENT);
		buildRowLabel(title, componentPlaceholder);
		subHeaderPlaceholder.appendChild(componentPlaceholder);
		headerPlaceholder.appendChild(subHeaderPlaceholder);
		retval.appendChild(headerPlaceholder);
		componentsForTitle.stream()
						  .filter(c -> c.getHeader().isEmpty())
						  .filter(c -> c.getSubHeader().isEmpty())
						  .filter(c -> c.getComponent().isEmpty())
						  .findAny()
						  .ifPresent(e -> buildValue(componentPlaceholder, e, valueFunction));
	}

	private Element buildHeader(String header, List<ChartComponent> componentsForHeader, Function<ChartComponent, Element> valueFunction)
	{
		Element retval = getDocument().createElement(HEADER_ELEMENT);
		if(!header.isEmpty())
		{
			appendHeaderPlaceholder(header, componentsForHeader, valueFunction, retval);
		}
		Map<String, List<ChartComponent>> groupedBySubheaders = componentsForHeader.stream().distinct().collect(
				groupingBy(ChartComponent::getSubHeader));
		int i = 0;
		for(String subHeader : componentsForHeader.stream().map(ChartComponent::getSubHeader).collect(toList()))
		{
			List<ChartComponent> componentsForSubHeaders = groupedBySubheaders.getOrDefault(subHeader, new ArrayList<>());
			Element subHeaderElement = buildSubHeader(subHeader, componentsForSubHeaders, valueFunction);
			subHeaderElement.setAttribute(SUBHEADER_ORDER_ATTRIBUTE, String.valueOf(i));
			retval.appendChild(subHeaderElement);
			i++;
		}
		return retval;
	}

	private void appendHeaderPlaceholder(String header, List<ChartComponent> componentsForHeader, Function<ChartComponent, Element> valueFunction,
										 Element retval)
	{
		Element subHeaderPlaceholder = getDocument().createElement(SUBHEADER_ELEMENT);
		Element componentPlaceholder = getDocument().createElement(COMPONENT_ELEMENT);
		buildRowLabel(header, componentPlaceholder);
		subHeaderPlaceholder.appendChild(componentPlaceholder);
		retval.appendChild(subHeaderPlaceholder);
		componentsForHeader.stream()
						   .filter(c -> c.getSubHeader().isEmpty())
						   .filter(c -> c.getComponent().isEmpty())
						   .findAny()
						   .ifPresent(e -> buildValue(componentPlaceholder, e, valueFunction));
	}

	private Element buildSubHeader(String subHeader, List<ChartComponent> componentsForSubHeaders, Function<ChartComponent, Element> valueFunction)
	{
		Element retval = getDocument().createElement(SUBHEADER_ELEMENT);
		if(!subHeader.isEmpty())
		{
			appendSubHeaderPlaceholder(subHeader, componentsForSubHeaders, valueFunction, retval);
		}
		int i = 0;
		for(ChartComponent component : componentsForSubHeaders)
		{
			Element componentElement = buildComponent(component, valueFunction);
			componentElement.setAttribute(COMPONENT_ORDER_ATTRIBUTE, String.valueOf(i));
			retval.appendChild(componentElement);
			i++;
		}
		return retval;
	}

	private void appendSubHeaderPlaceholder(String subHeader, List<ChartComponent> componentsForSubHeaders,
											Function<ChartComponent, Element> valueFunction, Element retval)
	{
		Element componentPlaceholder = getDocument().createElement(COMPONENT_ELEMENT);
		buildRowLabel(subHeader, componentPlaceholder);
		retval.appendChild(componentPlaceholder);
		componentsForSubHeaders.stream()
							   .filter(c -> c.getComponent().isEmpty())
							   .findAny()
							   .ifPresent(e -> buildValue(componentPlaceholder, e, valueFunction));
	}

	private void buildValue(Element componentElement, ChartComponent e, Function<ChartComponent, Element> valueFunction)
	{
		componentElement.appendChild(valueFunction.apply(e));
	}

	private void buildRowLabel(String text, Element retval)
	{
		Element rowLabel = getDocument().createElement(ROW_LABEL_ELEMENT);
		retval.appendChild(rowLabel);
		rowLabel.setTextContent(text);
	}

	private Element buildComponent(ChartComponent component, Function<ChartComponent, Element> valueFunction)
	{
		Element retval = getDocument().createElement(COMPONENT_ELEMENT);
		buildRowLabel(component.getComponent(), retval);
		buildValue(retval, component, valueFunction);
		return retval;
	}

	DateTimeFormatter getMonthYearFormatter()
	{
		return _monthYearFormatter;
	}
}
