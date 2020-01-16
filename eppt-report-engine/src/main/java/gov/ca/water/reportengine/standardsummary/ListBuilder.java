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
import java.util.Arrays;
import java.util.List;
import java.util.Objects;
import java.util.function.Function;
import java.util.logging.Logger;

import gov.ca.water.calgui.project.EpptScenarioRun;
import gov.ca.water.calgui.scripts.DssMissingRecordException;
import gov.ca.water.reportengine.EpptReportException;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import static java.util.stream.Collectors.toList;

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
				SummaryReportParameters reportParameters, StandardSummaryErrors standardSummaryErrors)
	{
		super(document, base, alternatives, reportParameters, standardSummaryErrors);
	}


	void buildList(Element retval, EpptChart epptChart)
	{
		appendTitles(retval, epptChart);
	}

	private void appendTitles(Element retval, EpptChart epptChart)
	{
		int i = 0;
		List<ChartComponent> componentsForTitle = epptChart.getChartComponents();
		Element titleElement = buildTitle(BASE_NAME, componentsForTitle, v -> buildValueForChart(getBase(), v));
		if(titleElement != null)
		{
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
	}

	@Override
	void appendHeaderPlaceholder(String header, List<ChartComponent> componentsForHeader, Function<ChartComponent, Element> valueFunction,
								 Element retval)
	{
		//No-op
	}

	@Override
	Element buildSubHeader(String subHeader, List<ChartComponent> componentsForSubHeaders, Function<ChartComponent, Element> valueFunction)
	{
		Element retval = getDocument().createElement(SUBHEADER_ELEMENT);
		buildComponents(componentsForSubHeaders, valueFunction, retval);
		return retval;
	}

	private Element buildValueForChart(EpptScenarioRun scenarioRun, ChartComponent v)
	{
		Element retval = getDocument().createElement("placeholder");
		try
		{
			Object value = createJythonValueGenerator(scenarioRun, v.getFunction(), getReportParameters().getWaterYearIndex(scenarioRun))
					.generateObjectValue();

			if(value == null)
			{
				getStandardSummaryErrors().addError(LOGGER,
						"Unable to generate scenario value for: " + v + " value is null for scenario: " + scenarioRun.getName());
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
		catch(DssMissingRecordException e)
		{
			getStandardSummaryErrors().addError(LOGGER, "Missing record for list, displaying as NR", e);
			Element valueElem = getDocument().createElement(VALUE_ELEMENT);
			valueElem.setTextContent(NO_RECORD_TEXT);
			retval.appendChild(valueElem);
		}
		catch(EpptReportException e)
		{
			logScriptException(LOGGER, v, e);
		}
		return retval;
	}

	@Override
	Element buildComponent(ChartComponent component, Function<ChartComponent, Element> valueFunction)
	{
		Element retval = getDocument().createElement(COMPONENT_ELEMENT);
		retval.setAttribute(COMPONENT_NAME_ATTRIBUTE, component.getComponent());
		buildValue(retval, component, valueFunction);
		return retval;
	}

	@Override
	void appendSubHeaderPlaceholder(String subHeader, List<ChartComponent> componentsForSubHeaders,
									Function<ChartComponent, Element> valueFunction, Element retval)
	{
		Element componentPlaceholder = getDocument().createElement(COMPONENT_ELEMENT);
		retval.appendChild(componentPlaceholder);
		componentsForSubHeaders.stream()
							   .filter(c -> c.getComponent().isEmpty())
							   .findAny()
							   .ifPresent(e -> buildValue(componentPlaceholder, e, valueFunction));
	}

	@Override
	void buildValue(Element componentElement, ChartComponent e, Function<ChartComponent, Element> valueFunction)
	{
		Element apply = valueFunction.apply(e);
		if(apply != null)
		{
			List<String> headers = Arrays.asList(e.getSubHeader().split(","));
			List<List<String>> rows = new ArrayList<>();
			NodeList childNodes = apply.getChildNodes();
			for(int i = 0; i < childNodes.getLength(); i++)
			{
				Node item = childNodes.item(i);
				String textContent = item.getTextContent();
				if(Objects.equals(textContent, NO_RECORD_TEXT))
				{
					//Fill everything in the row with NO_RECORD_TEXT
					List<String> nrRow = headers.stream().map(f -> NO_RECORD_TEXT).collect(toList());
					nrRow.add(NO_RECORD_TEXT);
					rows.add(nrRow);
				}
				else
				{
					String[] split = textContent.split(":");
					if(split.length > 1)
					{
						List<String> row = new ArrayList<>();
						String date = split[0];
						row.add(date);
						String text = split[1];
						String[] columns = text.split(",");
						row.addAll(Arrays.asList(columns));
						rows.add(row);
					}
				}
			}
			if(!rows.isEmpty() && !rows.get(0).isEmpty())
			{
				for(int i = 0; i < rows.get(0).size() - 1; i++)
				{
					String header = "";
					if(i < headers.size())
					{
						header = headers.get(i);
					}
					Element element = getDocument().createElement(LIST_HEADER_ELEMENT);
					element.setAttribute(NAME_ORDER_ATTRIBUTE, Integer.toString(i));
					element.setAttribute(LIST_NAME_ATTRIBUTE, header);
					componentElement.appendChild(element);
					for(int j = 0; j < rows.size(); j++)
					{
						List<String> row = rows.get(j);
						if(i + 1 < row.size())
						{
							Element value = getDocument().createElement(VALUE_ELEMENT);
							value.setAttribute(DATE_ORDER_ATTRIBUTE, Integer.toString(j));
							value.setAttribute(DATE_ATTRIBUTE, row.get(0));
							value.setTextContent(row.get(i + 1));
							element.appendChild(value);
						}
					}
				}
			}
		}
	}
}
