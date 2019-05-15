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

package gov.ca.water.reportengine.reportheader;

import org.w3c.dom.Attr;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import java.util.List;

public class ReportHeaderXMLCreator
{
    private static final String REPORT_HEADER = "report-header";
    private static final String AUTHOR = "author";
    private static final String SUBTITLES = "subtitle";
    private static final String BASE_FILE = "base";
    private static final String NAME_ATTR = "name";
    private static final String ALTERNATIVES = "alternatives";
    private static final String ALTERNATIVE = "alternative";


    public ReportHeaderXMLCreator()
    {

    }


    public Element createReportHeaderElement(ReportHeader reportHeader, Document document)
    {
        Element root = null;

        root = document.createElement(REPORT_HEADER);

        Element authorElem = document.createElement(AUTHOR);
        authorElem.appendChild(document.createTextNode(reportHeader.getAuthor()));
        root.appendChild(authorElem);

        Element subtitleElem = document.createElement(SUBTITLES);
        subtitleElem.appendChild(document.createTextNode(reportHeader.getSubTitle()));
        root.appendChild(subtitleElem);

        Element baseFileElem = document.createElement(BASE_FILE);
        Attr baseFileAttr = document.createAttribute(NAME_ATTR);
        baseFileAttr.setValue(reportHeader.getBaseFile());
        baseFileElem.setAttributeNode(baseFileAttr);
        root.appendChild(baseFileElem);


        List<String> alternativeNames = reportHeader.getAlternativeNames();
        if (!alternativeNames.isEmpty())
        {
            Element alternativesParentElem = document.createElement(ALTERNATIVES);
            for (String altName : alternativeNames)
            {
                Element altElem = document.createElement(ALTERNATIVE);
                Attr altAttr = document.createAttribute(NAME_ATTR);
                altAttr.setValue(altName);
                altElem.setAttributeNode(altAttr);
                alternativesParentElem.appendChild(altElem);
            }
            root.appendChild(alternativesParentElem);
        }

        return root;
    }

}
