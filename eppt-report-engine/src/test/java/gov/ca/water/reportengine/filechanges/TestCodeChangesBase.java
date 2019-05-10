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

package gov.ca.water.reportengine.filechanges;

import gov.ca.water.reportengine.TestQAQCReportBase;
import org.junit.jupiter.api.Assertions;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import java.io.File;
import java.net.URL;
import java.nio.file.Path;
import java.util.List;

public class TestCodeChangesBase extends TestQAQCReportBase
{




    Element getHeaderElement(Element codeChangeElem)
    {
        return getChildElementWithName(codeChangeElem, "header");
        //Element alternativesElem = getChildElementWithName(headerElem, "alternatives");
        //return getChildElementWithName(alternativesElem, "alternative");
    }

    String getFilesUpdated(Element headerElem)
    {
        Element filesUpdated = getChildElementWithName(headerElem, "files-updated");
        return filesUpdated.getTextContent();
    }

    String getFilesAddedFromAltElem(Element headerElem)
    {
        Element filesAdded = getChildElementWithName(headerElem, "files-added-to");
        return filesAdded.getTextContent();
    }

    String getFilesDeletedFromCodeChangeElem(Element headerElem)
    {
        //Element headerElem = getChildElementWithName(headerElem, "header");
        Element deletedFiles = getChildElementWithName(headerElem, "files-deleted-from-base");
        return deletedFiles.getTextContent();
    }


    Element getSectionElement(Element codeChangesElem, String sectionName)
    {
        NodeList sections = codeChangesElem.getElementsByTagName("section");
        for (int i = 0; i < sections.getLength(); i++)
        {
            Element sectionElem = (Element) sections.item(i);
            if (sectionElem.getAttribute("section-name").equals(sectionName))
            {
                return sectionElem;
            }
        }

        return null;
    }


    NodeList getTypeElementsInSection(Element sectionElement)
    {
        return sectionElement.getElementsByTagName("type");
    }

    String getChangeType(Element typeToTest)
    {
        return typeToTest.getAttribute("change-type");
    }

    String getChangeSubType(Element subTypeToTest)
    {
        return subTypeToTest.getAttribute("change-subtype");
    }

    String getChangedFile(Element changeElem)
    {
        return changeElem.getTextContent();
    }

    NodeList getSubTypes(Element typeElem)
    {
        return typeElem.getElementsByTagName("subtype");
    }

    NodeList getChanges(Element subTypeElem)
    {
        return subTypeElem.getElementsByTagName("change");
    }


    void testSection(Element modifiedSectionElementFromMaster, Element modifiedSectionElementToTest)
    {
        NodeList typeElementsInSectionTest = getTypeElementsInSection(modifiedSectionElementToTest);
        NodeList typeElementsInSectionMaster = getTypeElementsInSection(modifiedSectionElementFromMaster);

        Assertions.assertEquals(typeElementsInSectionMaster.getLength(), typeElementsInSectionTest.getLength());

        for (int i = 0; i < typeElementsInSectionTest.getLength(); i++)
        {
            Element typeElemTest = (Element) typeElementsInSectionTest.item(i);
            Element typeElemMaster = (Element) typeElementsInSectionMaster.item(i);

            Assertions.assertEquals(getChangeType(typeElemTest), getChangeType(typeElemMaster));

            NodeList subTypesTest = getSubTypes(typeElemTest);
            NodeList subTypesMaster = getSubTypes(typeElemMaster);

            Assertions.assertEquals(subTypesMaster.getLength(), subTypesTest.getLength());

            for (int j = 0; j < subTypesMaster.getLength(); j++)
            {
                Element subTypeElemTest = (Element) subTypesTest.item(j);
                Element subTypeElemMaster = (Element) subTypesMaster.item(j);

                Assertions.assertEquals(getChangeSubType(subTypeElemMaster), getChangeSubType(subTypeElemTest));

                NodeList changesTest = getChanges(subTypeElemTest);
                NodeList changesMaster = getChanges(subTypeElemMaster);

                Assertions.assertEquals(changesMaster.getLength(), changesTest.getLength());

                for (int k = 0; k < changesMaster.getLength(); k++)
                {
                    Element changeElemTest = (Element) changesTest.item(k);
                    Element changeElemMaster = (Element) changesMaster.item(k);

                    Assertions.assertEquals(getChangedFile(changeElemMaster), getChangedFile(changeElemTest));
                }

            }
        }
    }


}
