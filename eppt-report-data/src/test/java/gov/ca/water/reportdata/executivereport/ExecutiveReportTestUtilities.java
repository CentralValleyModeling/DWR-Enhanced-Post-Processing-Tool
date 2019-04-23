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

package gov.ca.water.reportdata.executivereport;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import java.util.ArrayList;
import java.util.List;

public class ExecutiveReportTestUtilities
{


    static String getStudyOrderForModule(Element moduleElement)
    {
        NodeList elementsByTagName = moduleElement.getElementsByTagName(ExecutiveReportXMLWriter.STUDY);
        return ((Element) elementsByTagName.item(0)).getAttribute(ExecutiveReportXMLWriter.STUDY_ORDER);

    }

    static String getModelEntriesOrderForModule(Element moduleElement)
    {
        NodeList elementsByTagName = moduleElement.getElementsByTagName(ExecutiveReportXMLWriter.MODEL_ENTRIES);
        return ((Element) elementsByTagName.item(0)).getAttribute(ExecutiveReportXMLWriter.MODEL_ORDER);
    }

    static String getStudyValue(Element moduleElement)
    {
        NodeList elementsByTagName = moduleElement.getElementsByTagName(ExecutiveReportXMLWriter.STUDY);
        String value = elementsByTagName.item(0).getTextContent();
        value = value.replace(ExecutiveReportXMLWriter.NEW_LINE,"\r");
        return value;
    }

    static String getModelEntriesValue(Element moduleElement)
    {
        NodeList elementsByTagName = moduleElement.getElementsByTagName(ExecutiveReportXMLWriter.MODEL_ENTRIES);
        String value = elementsByTagName.item(0).getTextContent();
        value = value.replace(ExecutiveReportXMLWriter.NEW_LINE,"\r");
        return  value;
    }

    static List<Element> getModuleElementsWithName(Document doc, String modelEntriesName)
    {
        List<Element> moduleElements = new ArrayList<>();
        NodeList elementsByTagName = doc.getElementsByTagName(ExecutiveReportXMLWriter.MODULE);
        for (int i = 0; i < elementsByTagName.getLength(); i++)
        {

            Node nNode = elementsByTagName.item(i);
            Element modelEntriesElem = (Element)((Element) nNode).getElementsByTagName(ExecutiveReportXMLWriter.MODEL_ENTRIES).item(0);

            String name = modelEntriesElem.getAttribute(ExecutiveReportXMLWriter.NAME);
            if (name.equals(modelEntriesName))
            {
                moduleElements.add((Element)elementsByTagName.item(i));
            }
        }
        return moduleElements;
    }

}
