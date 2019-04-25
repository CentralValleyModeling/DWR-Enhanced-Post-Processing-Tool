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

import gov.ca.water.reportengine.TestQAQCReportBase;
import gov.ca.water.reportengine.executivereport.ExecutiveReportXMLCreator;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import java.util.ArrayList;
import java.util.List;

public class TestReportHeaderBase extends TestQAQCReportBase
{

    public String getAuthor(Element elem)
    {
        NodeList elementsByTagName = elem.getElementsByTagName("author");
        return ((Element) elementsByTagName.item(0)).getTextContent();
    }

    public String getSubtitle(Element elem)
    {
        NodeList elementsByTagName = elem.getElementsByTagName("subtitle");
        return ((Element) elementsByTagName.item(0)).getTextContent();
    }

    public String getBase(Element elem)
    {
        NodeList elementsByTagName = elem.getElementsByTagName("base");
        return ((Element) elementsByTagName.item(0)).getAttribute("name");
    }

    public List<String> getAlternatives(Element elem)
    {
        List<String> retVal = new ArrayList<>();
        NodeList elementsByTagName = elem.getElementsByTagName("alternatives");
        //there will only be one "alternatives" element
        Node nNode = elementsByTagName.item(0);
        if(nNode == null)
        {
            return retVal;
        }
        elementsByTagName = ((Element) nNode).getElementsByTagName("alternative");
        for (int i = 0; i < elementsByTagName.getLength(); i++)
        {
            retVal.add(((Element) elementsByTagName.item(i)).getAttribute("name"));

        }
        return retVal;
    }


}
