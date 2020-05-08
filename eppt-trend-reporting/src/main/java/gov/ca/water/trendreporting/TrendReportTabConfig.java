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

import org.w3c.dom.Document;
import org.w3c.dom.Node;
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
	private final Path _path;
	private final boolean _aggregateSupported;

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
			_text = extractHtmlTitle(parse, path.getFileName().toString()).trim();
			_aggregateSupported = extractAggregateSupported(parse);
		}
	}

	private boolean extractAggregateSupported(Document parse)
	{
		boolean retval = false;
		NodeList metaNodes = parse.getElementsByTagName("meta");
		if(metaNodes != null)
		{
			for(int i = 0; i < metaNodes.getLength(); i++)
			{
				Node item = metaNodes.item(i);
				Node namedItem = item.getAttributes().getNamedItem("name");
				Node contentItem = item.getAttributes().getNamedItem("content");
				if(contentItem != null && namedItem != null && "supports-aggregate".equals(namedItem.getNodeValue()))
				{
					retval = Boolean.parseBoolean(contentItem.getNodeValue());
				}
			}
		}
		return retval;
	}

	public boolean isAggregateSupported()
	{
		return _aggregateSupported;
	}

	public Path getPath()
	{
		return _path;
	}

	public String getText()
	{
		return _text;
	}

	private String extractHtmlTitle(Document parse, String filename)
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

	@Override
	public String toString()
	{
		return getText();
	}
}
