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

package gov.ca.water.trendreporting;

import java.io.BufferedReader;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import javafx.scene.control.ToggleButton;
import org.w3c.dom.Document;
import org.w3c.dom.NodeList;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;


/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 09-24-2019
 */
public class TrendReportTabConfig
{
	private final String _text;
	private final boolean _statsDisabled;
	private final boolean _seasonalPeriodDisabled;
	private final Path _path;
	private final boolean _waterYearIndexDisabled;

	public TrendReportTabConfig(Path path) throws IOException, ParserConfigurationException, SAXException
	{
		_path = path;
		try(BufferedReader reader = Files.newBufferedReader(path))
		{
			DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
			dbf.setValidating(false);
			dbf.setNamespaceAware(true);
			dbf.setIgnoringComments(false);
			dbf.setIgnoringElementContentWhitespace(false);
			dbf.setExpandEntityReferences(false);
			DocumentBuilder db = dbf.newDocumentBuilder();
			Document parse = db.parse(new InputSource(reader));
			_text = getHtmlTitle(parse, path.getFileName().toString()).trim();
			_statsDisabled = attributeDisabled(parse, "support-statistic");
			_seasonalPeriodDisabled = attributeDisabled(parse, "support-seasonal-period");
			_waterYearIndexDisabled = attributeDisabled(parse, "support-water-year-index");
		}
	}

	public Path getPath()
	{
		return _path;
	}

	public boolean isWaterYearIndexDisabled()
	{
		return _waterYearIndexDisabled;
	}

	public boolean isSeasonalPeriodDisabled()
	{
		return _seasonalPeriodDisabled;
	}

	public boolean isStatsDisabled()
	{
		return _statsDisabled;
	}

	public String getText()
	{
		return _text;
	}

	private String getHtmlTitle(Document parse, String filename)
	{
		String text;
		NodeList head = parse.getElementsByTagName("title");
		if(head != null && head.getLength() > 0)
		{
			text = head.item(0).getTextContent();
		}
		else
		{
			text = filename;
		}
		return text;
	}

	private boolean attributeDisabled(Document parse, String attribute)
	{
		boolean attributeDisabled = true;
		NodeList meta = parse.getElementsByTagName("meta");
		for(int i = 0; i < meta.getLength(); i++)
		{
			org.w3c.dom.Node item = meta.item(i);
			org.w3c.dom.Node name = item.getAttributes().getNamedItem("name");
			if(name != null)
			{
				String metaName = name.getTextContent();
				org.w3c.dom.Node content = item.getAttributes().getNamedItem("content");
				if(content != null && attribute.equals(metaName))
				{
					attributeDisabled = !Boolean.parseBoolean(content.getTextContent());
					break;
				}
			}
		}
		return attributeDisabled;
	}
}
