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

import org.junit.jupiter.api.Test;
import org.w3c.dom.Document;

import java.awt.dnd.DropTargetDragEvent;

public class TestCodeChanges extends TestCodeChangesBase
{
    private static final String XML_PATH = System.getProperty("user.dir") + "\\codeChanges.xml";
    private static final String CODE_CHANGES = "code-changes";

    private Document _qAQCMaster;
    private Document _qAQCReportToTest;




    @Test
    void testAssumptionChangesWithAltSameModel() throws Exception
    {
        Document doc = getDoc();
        CodeChangesXMLCreator creator = new CodeChangesXMLCreator();
        creator.appendCodeChangesElement(getCodeChangesCsvPath(),getBaseOutputPath(),getAltOutputPath(),"Alternative 1",doc);

        writeXmlFile(XML_PATH, doc);

        _qAQCReportToTest = loadReportToTest(XML_PATH);
        _qAQCMaster = loadComparisonSameModelReport();

    }



}
