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

package gov.ca.water.reportengine.executivereport;

import gov.ca.water.reportengine.TestQAQCReportBase;
import gov.ca.water.reportengine.filechanges.AssumptionChangesDataProcessor;
import gov.ca.water.reportengine.filechanges.FileChangesStatistics;
import org.junit.jupiter.api.Assertions;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import java.io.File;
import java.net.URL;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;

public class ExecutiveReportTestBase extends TestQAQCReportBase
{
    private static final String XML_PATH = System.getProperty("user.dir") + "\\executivereport.xml";
    private Document _qAQCMaster;
    private Document _qAQCReportToTest;


    void testBaseOnly(String moduleName, String masterModelEntriesName) throws Exception
    {

        //create executive report and write it out
        Document doc = getDoc();
        ExecutiveReportXMLCreator erWriter = new ExecutiveReportXMLCreator();

       // List<FileChangesStatistics> statsForAllAlternatives = getFileChangeStatsList();

        erWriter.appendExecutiveReportTableElement(getCSVPath(), getDssFilePathsForBaseOnly(), null, false, doc);
        writeXmlFile(XML_PATH, doc);

        //read the xml file to test
        _qAQCReportToTest = loadReportToTest(XML_PATH);
        _qAQCMaster = loadBaseOnlyReport();

        //get the elements to compare
        List<Element> elemsToTest = getModuleElementsWithName(_qAQCReportToTest, moduleName);
        List<Element> elemsFromMaster = getModuleElementsWithName(_qAQCMaster, masterModelEntriesName);

        //check that the number of modules (# of columns) is equal to the base + number of alternatives
        Assertions.assertTrue(elemsToTest.size() == 1);

        for (int i = 0; i < elemsToTest.size(); i++)
        {
            Assertions.assertEquals(getStudyOrderForModule(elemsFromMaster.get(i)), getStudyOrderForModule(elemsToTest.get(i)), "Study Order");
            Assertions.assertEquals(getModelEntriesOrderForModule(elemsFromMaster.get(i)), getModelEntriesOrderForModule(elemsToTest.get(i)), "Model Entries Order");

            Assertions.assertEquals(getStudyValue(elemsFromMaster.get(i)), getStudyValue(elemsToTest.get(i)), "Study Value");
            Assertions.assertEquals(getModelEntriesValue(elemsFromMaster.get(i)), getModelEntriesValue(elemsToTest.get(i)), "Model Entries Value");
        }
    }

    void testOneAlternativeSameModel(String moduleName, String masterModelEntriesName) throws Exception
    {

        //create executive report and write it out
        Document doc = getDoc();
        ExecutiveReportXMLCreator erWriter = new ExecutiveReportXMLCreator();
        List<FileChangesStatistics> statsForAllAlternatives = getFileChangeStatsList();
        erWriter.appendExecutiveReportTableElement(getCSVPath(), getDssFilePathsForSameModel(),statsForAllAlternatives, true, doc);
        writeXmlFile(XML_PATH, doc);

        //read the xml file to test
        _qAQCReportToTest = loadReportToTest(XML_PATH);
        _qAQCMaster = loadComparisonSameModelReport();

        //get the elements to compare
        List<Element> elemsToTest = getModuleElementsWithName(_qAQCReportToTest, moduleName);
        List<Element> elemsFromMaster = getModuleElementsWithName(_qAQCMaster, masterModelEntriesName);

        //check that the number of modules (# of columns) is equal to the base + number of alternatives
        Assertions.assertTrue(elemsToTest.size() == 2);

        for (int i = 0; i < elemsToTest.size(); i++)
        {
            Assertions.assertEquals(getStudyOrderForModule(elemsFromMaster.get(i)), getStudyOrderForModule(elemsToTest.get(i)), "Study Order");
            Assertions.assertEquals(getModelEntriesOrderForModule(elemsFromMaster.get(i)), getModelEntriesOrderForModule(elemsToTest.get(i)), "Model Entries Order");

            Assertions.assertEquals(getStudyValue(elemsFromMaster.get(i)), getStudyValue(elemsToTest.get(i)), "Study Value");
            Assertions.assertEquals(getModelEntriesValue(elemsFromMaster.get(i)), getModelEntriesValue(elemsToTest.get(i)), "Model Entries Value");
        }
    }

    private List<FileChangesStatistics> getFileChangeStatsList() throws Exception
    {
        Path stateVarCSV = getAssumpChangesStateVariablesCSV();

        Path svBasePath = getStateVariableBaseDSSPath();
        Path sVAltPath = getStateVariableAltDSSPath();

        AssumptionChangesDataProcessor stateVarProcessor = new AssumptionChangesDataProcessor(stateVarCSV);
        FileChangesStatistics stateVarStats = stateVarProcessor.processAssumptionChanges(svBasePath, sVAltPath);

        List<FileChangesStatistics> statsForAllAlternatives = new ArrayList<>();
        statsForAllAlternatives.add(stateVarStats);
        return statsForAllAlternatives;
    }

    public List<Element> getModuleElementsWithName(Document doc, String modelEntriesName)
    {
        List<Element> moduleElements = new ArrayList<>();
        NodeList elementsByTagName = doc.getElementsByTagName(ExecutiveReportXMLCreator.MODULE);
        for (int i = 0; i < elementsByTagName.getLength(); i++)
        {

            Node nNode = elementsByTagName.item(i);
            Element modelEntriesElem = (Element) ((Element) nNode).getElementsByTagName(ExecutiveReportXMLCreator.MODEL_ENTRIES).item(0);

            String name = modelEntriesElem.getAttribute(ExecutiveReportXMLCreator.NAME);
            if (name.equals(modelEntriesName))
            {
                moduleElements.add((Element) elementsByTagName.item(i));
            }
        }
        return moduleElements;
    }

    public List<Path> getDssFilePathsForBaseOnly()
    {
        URL resource = this.getClass().getClassLoader().getResource("SamplePstPrcss_Base_v1.01.dss");
        Path dssFilePath = new File(resource.getPath()).toPath();

        List<Path> dssFiles = new ArrayList<>();
        dssFiles.add(dssFilePath);

        return dssFiles;
    }

    public List<Path> getDssFilePathsForSameModel()
    {
        URL resource = this.getClass().getClassLoader().getResource("SamplePstPrcss_Base_v1.01.dss");
        Path dssFilePath = new File(resource.getPath()).toPath();

        URL resource2 = this.getClass().getClassLoader().getResource("SamplePstPrcss_Alt1_v1.01.dss");
        Path dssFilePath2 = new File(resource2.getPath()).toPath();

        List<Path> dssFiles = new ArrayList<>();
        dssFiles.add(dssFilePath);
        dssFiles.add(dssFilePath2);

        return dssFiles;
    }







    public String getStudyOrderForModule(Element moduleElement)
    {
        NodeList elementsByTagName = moduleElement.getElementsByTagName(ExecutiveReportXMLCreator.STUDY);
        return ((Element) elementsByTagName.item(0)).getAttribute(ExecutiveReportXMLCreator.STUDY_ORDER);

    }

    public String getModelEntriesOrderForModule(Element moduleElement)
    {
        NodeList elementsByTagName = moduleElement.getElementsByTagName(ExecutiveReportXMLCreator.MODEL_ENTRIES);
        return ((Element) elementsByTagName.item(0)).getAttribute(ExecutiveReportXMLCreator.MODEL_ORDER);
    }

    public String getStudyValue(Element moduleElement)
    {
        NodeList elementsByTagName = moduleElement.getElementsByTagName(ExecutiveReportXMLCreator.STUDY);
        String value = elementsByTagName.item(0).getTextContent();
        value = value.replace(ExecutiveReportXMLCreator.NEW_LINE, "\r");
        return value;
    }

    public String getModelEntriesValue(Element moduleElement)
    {
        NodeList elementsByTagName = moduleElement.getElementsByTagName(ExecutiveReportXMLCreator.MODEL_ENTRIES);
        String value = elementsByTagName.item(0).getTextContent();
        value = value.replace(ExecutiveReportXMLCreator.NEW_LINE, "\r");
        return value;
    }



}
