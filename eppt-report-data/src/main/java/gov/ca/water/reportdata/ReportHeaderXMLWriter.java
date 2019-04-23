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

package gov.ca.water.reportdata;

import org.w3c.dom.Document;
import org.w3c.dom.Element;

import javax.xml.XMLConstants;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerConfigurationException;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.List;

import org.apache.log4j.Logger;

public class ReportHeaderXMLWriter
{
    private static Logger LOGGER = Logger.getLogger(ReportHeaderXMLWriter.class.getName());
    private static final String REPORT_HEADER = "report-header";
    private static final String AUTHOR = "author";
    private static final String SUBTITLES = "subtitle";
    private static final String BASE_FILE = "base-file";
    private static final String ALTERNATIVES = "alternatives";
    private static final String ALTERNATIVE = "alternative";



    public ReportHeaderXMLWriter()
    {

    }


    public void Write(String filePath, ReportHeader reportHeader)
    {
        DocumentBuilderFactory documentFactory = DocumentBuilderFactory.newInstance();
        try
        {
            DocumentBuilder documentBuilder = documentFactory.newDocumentBuilder();
            Document document = documentBuilder.newDocument();

            Element root = document.createElement(REPORT_HEADER);
            document.appendChild(root);

            Element authorElem = document.createElement(AUTHOR);
            authorElem.appendChild(document.createTextNode(reportHeader.getAuthor()));
            root.appendChild(authorElem);

            Element subtitleElem = document.createElement(SUBTITLES);
            subtitleElem.appendChild(document.createTextNode(reportHeader.getSubTitle()));
            root.appendChild(subtitleElem);

            Element baseFileElem = document.createElement(BASE_FILE);
            baseFileElem.appendChild(document.createTextNode(reportHeader.getBaseFile()));
            root.appendChild(baseFileElem);


            List<String> alternativeNames = reportHeader.getAlternativeNames();
            if(!alternativeNames.isEmpty())
            {
                Element alternativesParentElem = document.createElement(ALTERNATIVES);
                for(String altName : alternativeNames)
                {
                    Element altElem = document.createElement(ALTERNATIVE);
                    altElem.appendChild(document.createTextNode(altName));
                    alternativesParentElem.appendChild(altElem);
                }
                root.appendChild(alternativesParentElem);
            }


            TransformerFactory transformerFactory = TransformerFactory.newInstance();
            transformerFactory.setAttribute(XMLConstants.ACCESS_EXTERNAL_DTD, "");
            transformerFactory.setAttribute(XMLConstants.ACCESS_EXTERNAL_STYLESHEET, "");
            Transformer transformer = transformerFactory.newTransformer();
            DOMSource domSource = new DOMSource(document);
            StreamResult streamResult = new StreamResult(new File(filePath));

            transformer.transform(domSource, streamResult);
        }
        catch (ParserConfigurationException e)
        {
            LOGGER.fatal(e.getMessage(), e);
        }
        catch (TransformerConfigurationException e)
        {
            e.printStackTrace();
        }
        catch (TransformerException e)
        {
            e.printStackTrace();
        }

    }

}
