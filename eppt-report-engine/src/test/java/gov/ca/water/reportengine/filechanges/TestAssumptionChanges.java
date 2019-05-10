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

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

import java.nio.file.Path;
import java.util.List;

public class TestAssumptionChanges extends TestAssumptionChangesBase
{
    private static final String XML_PATH = System.getProperty("user.dir") + "\\assumptionChanges.xml";
    private static final String ASSUMPTION_CHANGES = "assumption-changes";
    private final double _tolerance = .001;

    private Document _qAQCMaster;
    private Document _qAQCReportToTest;




    @Test
    void testAssumptionChangesWithAltSameModel() throws Exception
    {
        Path initCondCSV = getInitialConditionsCSV();
        Path initBasePath = getInitialConditionsBaseDSSPath();
        Path initAltPath = getInitialConditionsAltDSSPath();
        Path stateVarCSV = getAssumpChangesStateVariablesCSV();
        Path svBasePath = getStateVariableBaseDSSPath();
        Path sVAltPath = getStateVariableAltDSSPath();

        AssumptionChangesDataProcessor initProcessor = new AssumptionChangesDataProcessor(initCondCSV, _tolerance);
        AssumptionChangesStatistics initCondStats = initProcessor.processAssumptionChanges(initBasePath, initAltPath);

        AssumptionChangesDataProcessor stateVarProcessor = new AssumptionChangesDataProcessor(stateVarCSV, _tolerance);
        AssumptionChangesStatistics stateVarStats = stateVarProcessor.processAssumptionChanges(svBasePath, sVAltPath);


        Document doc = getDoc();

        AssumptionChangesXMLCreator assumpCreater = new AssumptionChangesXMLCreator();

        FileChangesStatistics stats = new FileChangesStatistics(initCondStats, stateVarStats, null);

        assumpCreater.appendAssumptionChangesElement(doc, stats);

        writeXmlFile(XML_PATH, doc);

        _qAQCReportToTest = loadReportToTest(XML_PATH);
        _qAQCMaster = loadComparisonSameModelReport();

        //get the elements to compare
        Element elemToTest = getElementsWithName(_qAQCReportToTest, ASSUMPTION_CHANGES);
        Element elemFromMaster = getElementsWithName(_qAQCMaster, ASSUMPTION_CHANGES);

        Element initConditionsToTest = getInitialConditionsElem(elemToTest);
        Element stateVarToTest = getStateVariablesElem(elemToTest);

        Element initCondMaster = getInitialConditionsElem(elemFromMaster);
        Element stateVarMaster = getStateVariablesElem(elemFromMaster);

        //test init cond
        Assertions.assertEquals(getCommonRecDifData(initCondMaster), getCommonRecDifData(initConditionsToTest), "init cond CommonRecDifData");
        Assertions.assertEquals(getRecordsOnlyBase(initCondMaster), getRecordsOnlyBase(initConditionsToTest), "init cond OnlyInBase");
        Assertions.assertEquals(getRecordsOnlyAlt(initCondMaster), getRecordsOnlyAlt(initConditionsToTest), "init cond OnlyInAlt");

        //test state variables
        Assertions.assertEquals(getCommonRecDifData(stateVarMaster), getCommonRecDifData(stateVarToTest), "state var CommonRecDifData");
        Assertions.assertEquals(getRecordsOnlyBase(stateVarMaster), getRecordsOnlyBase(stateVarToTest), "state var OnlyInBase");
        Assertions.assertEquals(getRecordsOnlyAlt(stateVarMaster), getRecordsOnlyAlt(stateVarToTest), "state var OnlyInAlt");


        NodeList recordsTest = getCommonRecordsDifDataRecords(initConditionsToTest);
        NodeList recordsMaster = getCommonRecordsDifDataRecords(initCondMaster);

        Assertions.assertEquals(recordsMaster.getLength(), recordsTest.getLength(), "common records list");

        for(int i = 0;i<recordsMaster.getLength();i++)
        {
            String recordFromTest = recordsTest.item(0).getTextContent();
            String recordFromMaster = recordsMaster.item(0).getTextContent();

            Assertions.assertEquals(recordFromMaster, recordFromTest, "record");

        }


        recordsTest = getOnlyInBaseRecords(initConditionsToTest);
        recordsMaster = getOnlyInBaseRecords(initCondMaster);

        Assertions.assertEquals(recordsMaster.getLength(), recordsTest.getLength(), "only in base list");

        for(int i = 0;i<recordsMaster.getLength();i++)
        {
            String recordFromTest = recordsTest.item(0).getTextContent();
            String recordFromMaster = recordsMaster.item(0).getTextContent();

            Assertions.assertEquals(recordFromMaster, recordFromTest, "record");

        }


        recordsTest = getOnlyInAltRecords(initConditionsToTest);
        recordsMaster = getOnlyInAltRecords(initCondMaster);

        Assertions.assertEquals(recordsMaster.getLength(), recordsTest.getLength(), "only in alt list");

        for(int i = 0;i<recordsMaster.getLength();i++)
        {
            String recordFromTest = recordsTest.item(0).getTextContent();
            String recordFromMaster = recordsMaster.item(0).getTextContent();

            Assertions.assertEquals(recordFromMaster, recordFromTest, "record");

        }

    }


}
