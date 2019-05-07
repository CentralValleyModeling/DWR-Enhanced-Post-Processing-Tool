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

import gov.ca.water.reportengine.filechanges.FileChangesStatistics;
import org.w3c.dom.*;
import org.xml.sax.SAXException;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import java.io.IOException;
import java.net.URL;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

public class ExecutiveReportXMLCreator
{
    private static final String TABLE_HEADER = "executive-report-table";
    static final String NEW_LINE = "&#xD;";

    static final String MODULE = "module";
    static final String MODEL_ENTRIES = "model-entries";
    static final String NAME = "name";
    static final String STUDY = "study";
    static final String STUDY_ORDER = "study-order";
    static final String MODEL_ORDER = "model-order";
    private static final String MODULE_HEADER_ATTR = "header";
    private static final String MODULE_HEADER_VALUE = "Issues";

    private static final String BASE_STUDY = "Base Study";
    private static final String ALTERNATIVE_STUDY = "Alternative Study";


    //reservoir operations row
    static final String RES_OPS = "ReservoirOperations";

    //Upstream mif reqs
    static final String UPSTREAM_MIF_REQ = "UpstreamMinimumFlowRequirements";

    //nod weirs ops
    static final String NOD_WEIR = "DeltaWeirsOperations";

    //nod groundwater pumping
    static final String NOD_GROUND_PUMP = "DeltaGroundwaterPumping";

    //delta operations row
    static final String DELTA_OPS = "DeltaOperations";

    //COA row
    static final String COA = "CoordinatedOperationsAgreement";

    //Allocations and deliveries row
    static final String ALLOCATIONS_DEL = "AllocationsAndDeliveries";

    //mass balance row
    static final String MASS_BALANCE = "MassBalance";

    //values from the helper xml
    //private Document _helperDoc;


    public ExecutiveReportXMLCreator()
    {

    }

    /**
     * Creates the entire executive report element
     *
     * @param modulesCSVPath
     * @param dssFiles
     * @return
     * @throws Exception
     */
    void appendExecutiveReportTableElement(Path modulesCSVPath,Path moduleLinkingCSVPath, List<Path> dssFiles, List<FileChangesStatistics> modelInputStats, boolean sameModel, Document doc) throws Exception
    {
        if (dssFiles.size() == 1)
        {
            sameModel = false;
        }
        //loadHelperXMLFile();

        Element execReportTableElem = doc.createElement(TABLE_HEADER);

        int i = 1;
        for (Path path : dssFiles)
        {
            ExecutiveReportProcessor processor = new ExecutiveReportProcessor(modulesCSVPath, moduleLinkingCSVPath);
            List<Module> modules = processor.processDSSFile(path, i);
            //now the modules have the violation numbers

            List<Element> scenarioElements = createScenarioElements(modules, getAssumptionChangesForScenario(modelInputStats, i), i, sameModel, doc);
            scenarioElements.forEach(execReportTableElem::appendChild);

            i++;
        }

        doc.appendChild(execReportTableElem);

    }

    private FileChangesStatistics getAssumptionChangesForScenario(List<FileChangesStatistics> stats, int scenarioNumber)
    {
        FileChangesStatistics retval = null;
        if (stats == null || scenarioNumber < 2)
        {
            return null;
        }
        if (stats.size() > scenarioNumber - 2)
        {
            retval = stats.get(scenarioNumber - 2);
        }
        return retval;
    }

    /**
     * Creates all the elements in a scenario (goes down the column of the table)
     *
     * @param modules
     * @param scenarioNumber
     * @param doc
     * @return
     */
    private List<Element> createScenarioElements(List<Module> modules, FileChangesStatistics assumpStats, int scenarioNumber, boolean sameModel, Document doc)
    {

        List<Element> retVal = new ArrayList<>();
        int rowNumber = 1;
        for (Module mod : modules)
        {
            if ("Model Inputs".equalsIgnoreCase(mod.getName()))
            {
                if (sameModel)
                {
                    retVal.add(createModelInputsElementFromModule(mod, assumpStats, scenarioNumber, rowNumber, doc));
                    rowNumber++;
                }
                //else skip it, we don't want to write it out
            }
            else if ("COA".equalsIgnoreCase(mod.getName()))
            {
                //add this element using the max value from the list of values
                retVal.add(createCOAElementFromModule(mod, scenarioNumber, rowNumber, doc));
                rowNumber++;
            }
            else
            {
                //do the generic creator
                retVal.add(createElementFromModule(mod, scenarioNumber, rowNumber, doc));
                rowNumber++;
            }
        }
        return retVal;
    }


    /**
     * @param module
     * @param alternativeNumber base = 1, alt1 = 2, alt2 = 3
     * @param rowNumber
     * @param doc
     * @return
     */
    private Element createElementFromModule(Module module, int alternativeNumber, int rowNumber, Document doc)
    {
        List<String> subModuleStrings = new ArrayList<>();

        for (SubModule sm : module.getSubModules())
        {

           // NodeList moduleNodes = _helperDoc.getElementsByTagName(module.getName());
            //if (moduleNodes.getLength() > 0)
            {
                //Node moduleNode = moduleNodes.item(0);
                //if (moduleNode.getNodeType() == Node.ELEMENT_NODE)
                {
                    //Element elem = (Element) moduleNode;
                    int numViolations;
                    if (alternativeNumber == 1)
                    {
                        numViolations = sm.getBaseViolations().size();
                    }
                    else
                    {
                        numViolations = sm.getAlternativeViolations(alternativeNumber).size();
                    }
                    String subModuleText = sm.getName(); //elem.getElementsByTagName(sm.getName()).item(0).getTextContent();
                    String formattedText = String.format(subModuleText, numViolations);
                    subModuleStrings.add(formattedText);
                }
            }
        }

        String elementString = subModuleStrings.stream().map(String::valueOf).collect(Collectors.joining(NEW_LINE));

        if (alternativeNumber == 1)
        {
            return createBaseModuleElement(doc, rowNumber, module.getName(), elementString);
        }
        else
        {
            return createAlternativeModuleElement(doc, alternativeNumber, rowNumber, module.getName(), elementString);

        }

    }

    private Element createModelInputsElementFromModule(Module module, FileChangesStatistics assumpStats, int scenarioNumber, int rowNumber, Document doc)
    {
        List<String> subModuleStrings = new ArrayList<>();

        if (scenarioNumber == 1)
        {
            return createBaseModuleElement(doc, rowNumber, module.getName(), "Base");
        }

        List<SubModule> subModules = module.getSubModules();
        if (subModules.size() == 4)
        {
            int stateVariableChangesSize = assumpStats.getChangedFiles().size();
            //subModules.get(0).addAlternativeViolations(scenarioNumber, stateVariableChangesSize);
            //subModules.get(1)
            //subModules.get(2)
            //subModules.get(3)

        }

        String elementString = subModuleStrings.stream().map(String::valueOf).collect(Collectors.joining(NEW_LINE));
        return createAlternativeModuleElement(doc, scenarioNumber, rowNumber, module.getName(), elementString);

    }

    private Element createCOAElementFromModule(Module module, int alternativeNumber, int rowNumber, Document doc)
    {
        List<String> subModuleStrings = new ArrayList<>();

        for (SubModule sm : module.getSubModules())
        {

            //NodeList moduleNodes = _helperDoc.getElementsByTagName(module.getName());
            //if (moduleNodes.getLength() > 0)
            {
                //Node moduleNode = moduleNodes.item(0);
                //if (moduleNode.getNodeType() == Node.ELEMENT_NODE)
                {
                   // Element elem = (Element) moduleNode;
                    double maxValue;
                    if (alternativeNumber == 1)
                    {
                        maxValue = sm.getBaseViolations().get(0).getMaxValue();
                    }
                    else
                    {
                        maxValue = sm.getAlternativeViolations(alternativeNumber).get(0).getMaxValue();
                    }
                    String subModuleText = sm.getName(); //elem.getElementsByTagName(sm.getName()).item(0).getTextContent();
                    String formattedText = String.format(subModuleText, maxValue);
                    subModuleStrings.add(formattedText);
                }
            }
        }

        String elementString = subModuleStrings.stream().map(String::valueOf).collect(Collectors.joining(NEW_LINE));

        if (alternativeNumber == 1)
        {
            return createBaseModuleElement(doc, rowNumber, module.getName(), elementString);
        }
        else
        {
            return createAlternativeModuleElement(doc, alternativeNumber, rowNumber, module.getName(), elementString);
        }
    }


    /**
     * @param doc
     * @param altColumnNumber this needs to start with 2 since base is in column 1
     * @param row
     * @param rowName
     * @param value
     * @return
     */
    private Element createAlternativeModuleElement(Document doc, int altColumnNumber, int row, String rowName, String value)
    {
        Element moduleElement = createModuleElement(doc);
        moduleElement.appendChild(createAlternativeStudyElement(doc, altColumnNumber));
        moduleElement.appendChild(createModelEntriesElement(doc, row, rowName, value));
        return moduleElement;
    }

    private Element createBaseModuleElement(Document doc, int row, String rowName, String value)
    {
        Element moduleElement = createModuleElement(doc);
        moduleElement.appendChild(createBaseStudyElement(doc));
        moduleElement.appendChild(createModelEntriesElement(doc, row, rowName, value));
        return moduleElement;
    }

    private Element createModuleElement(Document doc)
    {
        Element moduleElement = doc.createElement(MODULE);
        Attr headerAttr = doc.createAttribute(MODULE_HEADER_ATTR);
        headerAttr.setValue(MODULE_HEADER_VALUE);
        moduleElement.setAttributeNode(headerAttr);
        return moduleElement;
    }

    private Element createModelEntriesElement(Document doc, int row, String rowName, String value)
    {
        Element modelEntry = doc.createElement(MODEL_ENTRIES);

        Attr modelOrderAttr = doc.createAttribute(MODEL_ORDER);
        String rowNumber = "";
        if (row < 10)
        {
            rowNumber = "0" + row;
        }
        else
        {
            rowNumber = Integer.toString(row);
        }
        modelOrderAttr.setValue(rowNumber);
        modelEntry.setAttributeNode(modelOrderAttr);

        Attr nameAttr = doc.createAttribute(NAME);
        nameAttr.setValue(rowName);
        modelEntry.setAttributeNode(nameAttr);

        modelEntry.appendChild(doc.createTextNode(value));

        return modelEntry;
    }

    private Element createBaseStudyElement(Document doc)
    {
        Element baseStudyElem = doc.createElement(STUDY);
        Attr baseStudyAttr = doc.createAttribute(STUDY_ORDER);
        baseStudyAttr.setValue(Integer.toString(1));
        baseStudyElem.setAttributeNode(baseStudyAttr);
        baseStudyElem.appendChild(doc.createTextNode(BASE_STUDY));

        return baseStudyElem;
    }

    private Element createAlternativeStudyElement(Document doc, int altNumber)
    {
        Element altStudyElem = doc.createElement(STUDY);
        Attr altStudyAttr = doc.createAttribute(STUDY_ORDER);
        altStudyAttr.setValue(Integer.toString(altNumber));
        altStudyElem.setAttributeNode(altStudyAttr);
        altStudyElem.appendChild(doc.createTextNode(ALTERNATIVE_STUDY));

        return altStudyElem;
    }

//    private void loadHelperXMLFile() throws ExecutiveReportException
//    {
//
//        URL helperXMLURL = this.getClass().getClassLoader().getResource("ExecutiveReportHelperXML.xml");
//        String path = helperXMLURL.getPath();
//        try
//        {
//            DocumentBuilderFactory dbFactory = DocumentBuilderFactory.newInstance();
//            DocumentBuilder dBuilder = dbFactory.newDocumentBuilder();
//            _helperDoc = dBuilder.parse(path);
//            _helperDoc.getDocumentElement().normalize();
//        }
//        catch (ParserConfigurationException | IOException | SAXException e)
//        {
//            throw new ExecutiveReportException("Error while trying to read the xml file: " + path, e);
//        }
//    }
}
