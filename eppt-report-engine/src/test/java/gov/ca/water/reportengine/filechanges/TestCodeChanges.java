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

import java.nio.file.Path;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

public class TestCodeChanges extends TestCodeChangesBase
{
//    private static final String XML_PATH = System.getProperty("user.dir") + "\\codeChanges.xml";
    private static final String CODE_CHANGES = "code-changes";

    private Document _qAQCMaster;
    private Document _qAQCReportToTest;




    @Test
    void testAssumptionChangesWithAltSameModel() throws Exception
    {
        Document doc = getDoc();
        CodeChangesXMLCreator creator = new CodeChangesXMLCreator();
        creator.createCodeChangesElement(getCodeChangesCsvPath(),getBaseOutputPath(),getAltOutputPath(),doc);

        Path path = writeXmlFile(doc);

        _qAQCReportToTest = loadReportToTest(path);
        _qAQCMaster = loadComparisonSameModelReport();

        Element elemToTest = getElementsWithName(_qAQCReportToTest, CODE_CHANGES);
        Element elemFromMaster = getElementsWithName(_qAQCMaster, CODE_CHANGES);

        //test header values
        Element headerElemToTest = getHeaderElement(elemToTest);
        Element headerElemFromMaster = getHeaderElement(elemFromMaster);

        Assertions.assertEquals( getFilesUpdated(headerElemFromMaster), getFilesUpdated(headerElemToTest),"header: files updated");
        Assertions.assertEquals( getFilesAddedFromAltElem(headerElemFromMaster),getFilesAddedFromAltElem(headerElemToTest), "header: files added");
        Assertions.assertEquals( getFilesDeletedFromCodeChangeElem(elemFromMaster), getFilesDeletedFromCodeChangeElem(elemToTest), "header: files deleted");

        //test modified files section
        Element modifiedSectionElementToTest = getSectionElement(elemToTest, "Modified Files");
        Element modifiedSectionElementFromMaster = getSectionElement(elemFromMaster, "Modified Files");

        testSection(modifiedSectionElementFromMaster, modifiedSectionElementToTest);

        //test new files section
        Element newSectionTest = getSectionElement(elemToTest, "New");
        Element newSectionMaster = getSectionElement(elemFromMaster, "New");

        testSection(newSectionMaster, newSectionTest);

        //test deleted files section
        Element deleteSectionTest = getSectionElement(elemToTest, "Deleted");
        Element deleteSectionMaster = getSectionElement(elemFromMaster, "Deleted");

        testSection(deleteSectionMaster, deleteSectionTest);


    }



}
