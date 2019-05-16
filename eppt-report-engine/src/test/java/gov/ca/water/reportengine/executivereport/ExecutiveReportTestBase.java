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

import gov.ca.water.calgui.project.EpptScenarioRun;
import gov.ca.water.reportengine.ModuleCreator;
import gov.ca.water.reportengine.TestQAQCReportBase;
import gov.ca.water.reportengine.filechanges.*;
import org.junit.jupiter.api.Assertions;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

public class ExecutiveReportTestBase extends TestQAQCReportBase
{
//    private static final String XML_PATH = System.getProperty("user.dir") +  "\\executivereport.xml";
    private Document _qAQCMaster;
    private Document _qAQCReportToTest;
    private final double _tolerance = .001;


    void testBaseOnly(String moduleName) throws Exception
    {

        //create executive report and write it out
        Document doc = getDoc();
        ExecutiveReportXMLCreator erWriter = new ExecutiveReportXMLCreator();

        EpptScenarioRun baseScenarioRun = getBaseScenarioRun();
        List<EpptScenarioRun> allRuns = new ArrayList<>();
        allRuns.add(baseScenarioRun);

        ModuleCreator mc = new ModuleCreator();
        List<Module> modules = mc.createModules(getCSVPath(), getModuleLinkingCSVPath());

        DTSProcessor dtsProcessor = new DTSProcessor(modules);
        Map<EpptScenarioRun, Map<SubModule, List<FlagViolation>>> runsToViolations = dtsProcessor.processDSSFiles(allRuns);



        erWriter.createExecutiveReportTableElement(allRuns, runsToViolations, modules,  null, false, doc);
        Path path = writeXmlFile(doc);

        //read the xml file to test
        _qAQCReportToTest = loadReportToTest(path);
        _qAQCMaster = loadBaseOnlyReport();

        //get the elements to compare
        List<Element> elemsToTest = getModuleElementsWithName(_qAQCReportToTest, moduleName);
        List<Element> elemsFromMaster = getModuleElementsWithName(_qAQCMaster, moduleName);

        //check that the number of modules (# of columns) is equal to the base + number of alternatives
        Assertions.assertEquals(1, elemsToTest.size());

        for (int i = 0; i < elemsToTest.size(); i++)
        {
            Assertions.assertEquals(getStudyOrderForModule(elemsFromMaster.get(i)), getStudyOrderForModule(elemsToTest.get(i)), "Study Order");
            Assertions.assertEquals(getModelEntriesOrderForModule(elemsFromMaster.get(i)), getModelEntriesOrderForModule(elemsToTest.get(i)), "Model Entries Order");

            Assertions.assertEquals(getStudyValue(elemsFromMaster.get(i)), getStudyValue(elemsToTest.get(i)), "Study Value");
            Assertions.assertEquals(getModelEntriesValue(elemsFromMaster.get(i)), getModelEntriesValue(elemsToTest.get(i)), "Model Entries Value");
        }
    }

    void testOneAlternativeSameModel(String moduleName) throws Exception
    {
        //create executive report and write it out
        Document doc = getDoc();
        ExecutiveReportXMLCreator erWriter = new ExecutiveReportXMLCreator();
        List<FileChangesStatistics> statsForAllAlternatives = getFileChangeStatsList();

        ModuleCreator mc = new ModuleCreator();
        List<Module> modules = mc.createModules(getCSVPath(), getModuleLinkingCSVPath());

        EpptScenarioRun baseScenarioRun = getBaseScenarioRun();
        List<EpptScenarioRun> altScenarioRuns = getAltScenarioRuns();
        List<EpptScenarioRun> allRuns = new ArrayList<>();
        allRuns.add(baseScenarioRun);
        allRuns.addAll(altScenarioRuns);

        DTSProcessor dtsProcessor = new DTSProcessor(modules);
        Map<EpptScenarioRun, Map<SubModule, List<FlagViolation>>> runsToViolations = dtsProcessor.processDSSFiles(allRuns);


        erWriter.createExecutiveReportTableElement(allRuns, runsToViolations, modules,statsForAllAlternatives, true, doc);
        Path path = writeXmlFile(doc);

        //read the xml file to test
        _qAQCReportToTest = loadReportToTest(path);
        _qAQCMaster = loadComparisonSameModelReport();

        //get the elements to compare
        List<Element> elemsToTest = getModuleElementsWithName(_qAQCReportToTest, moduleName);
        List<Element> elemsFromMaster = getModuleElementsWithName(_qAQCMaster, moduleName);

        //check that the number of modules (# of columns) is equal to the base + number of alternatives
        Assertions.assertEquals(2, elemsToTest.size());

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

        CodeChangesDataProcessor processor = new CodeChangesDataProcessor(getCodeChangesCsvPath());
        CodeChangesStatistics codeChangeStats = processor.processCodeChanges(getBaseOutputPath(), getAltOutputPath());

        FileChangesStatistics fileChangesStatistics = new FileChangesStatistics(initCondStats, stateVarStats, codeChangeStats);

        List<FileChangesStatistics> statsForAllAlternatives = new ArrayList<>();
        statsForAllAlternatives.add(fileChangesStatistics);
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
