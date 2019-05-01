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

package gov.ca.water.reportengine.assumptionchanges;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import java.nio.file.Path;
import java.util.List;

public class TestAssumptionChanges extends TestAssumptionChangesBase
{
    private static final String XML_PATH = System.getProperty("user.dir") + "\\assumptionChanges.xml";
    private static final String ASSUMPTION_CHANGES = "assumption-changes";

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

        AssumptionChangesDataProcessor initProcessor = new AssumptionChangesDataProcessor(initCondCSV);
        FileChangesStatistics initCondStats = initProcessor.processAssumptionChanges(initBasePath, initAltPath);

        AssumptionChangesDataProcessor stateVarProcessor = new AssumptionChangesDataProcessor(stateVarCSV);
        FileChangesStatistics stateVarStats = stateVarProcessor.processAssumptionChanges(svBasePath, sVAltPath);


        Document doc = getDoc();

        AssumptionChangesXMLCreator assumpCreater = new AssumptionChangesXMLCreator();
        assumpCreater.appendAssumptionChangesElement(doc, initCondStats, stateVarStats);

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

        List<String> changesListToTest = getChangesList(elemToTest);
        List<String> changesListFromMaster = getChangesList(elemFromMaster);

        Assertions.assertTrue(changesListFromMaster.size() == changesListToTest.size());

        for(int i = 0;i< changesListToTest.size();i++)
        {
            Assertions.assertEquals(changesListFromMaster.get(i), changesListToTest.get(i));
        }

    }



}
