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

import java.util.*;
import java.util.logging.Level;
import java.util.stream.Collectors;

import com.google.common.flogger.FluentLogger;
import gov.ca.water.calgui.project.EpptScenarioRun;
import gov.ca.water.reportengine.filechanges.AssumptionChangesStatistics;
import gov.ca.water.reportengine.filechanges.CodeChangesStatistics;
import gov.ca.water.reportengine.filechanges.FileChangesStatistics;
import org.w3c.dom.Attr;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import hec.lang.Const;

public class ExecutiveReportXMLCreator
{
    private static final FluentLogger LOGGER = FluentLogger.forEnclosingClass();
    static final String NEW_LINE = "\n";
    static final String MODULE = "module";
    static final String MODEL_ENTRIES = "model-entries";
    static final String NAME = "name";
    static final String STUDY = "study";
    static final String STUDY_ORDER = "study-order";
    static final String MODEL_ORDER = "model-order";
    private static final String TABLE_HEADER = "executive-report-table";
    private static final String MODULE_HEADER_ATTR = "header";
    private static final String MODULE_HEADER_VALUE = "Issues";
    private static final String BASE_STUDY = "Base Study";
    private static final String ALTERNATIVE_STUDY = "Alternative Study";


    public Element createExecutiveReportTableElement(List<EpptScenarioRun> runs,
                                                     Map<EpptScenarioRun, Map<SubModule, List<FlagViolation>>> runsToViolations,
                                                     List<Module> modules, List<FileChangesStatistics> modelInputStats, boolean sameModel,
                                                     Document doc)
    {
        LOGGER.at(Level.INFO).log("Writing Executive Summary to XML");
        if (runs.size() == 1)
        {
            sameModel = false;
        }

        Element execReportTableElem = doc.createElement(TABLE_HEADER);

        for (int i = 0; i < runs.size(); i++)
        {
            Map<SubModule, List<FlagViolation>> subModToViolations = new HashMap<>();
            if (runsToViolations.containsKey(runs.get(i)))
            {
                subModToViolations = runsToViolations.get(runs.get(i));
            }
            FileChangesStatistics assumpChanges = getAssumptionChangesForScenario(modelInputStats, i);

            List<Element> scenarioElements = createScenarioElements(subModToViolations, modules, assumpChanges, i, sameModel, doc);
            scenarioElements.forEach(execReportTableElem::appendChild);
        }
        return execReportTableElem;

    }

    private FileChangesStatistics getAssumptionChangesForScenario(List<FileChangesStatistics> stats, int scenarioNumber)
    {
        FileChangesStatistics retval = null;
        if (stats == null || scenarioNumber == 0)
        {
            return null;
        }
        if (stats.size() >= scenarioNumber)
        {
            retval = stats.get(scenarioNumber - 1);
        }
        return retval;
    }

    /**
     * Creates all the elements in a scenario (goes down the column of the table)
     *
     * @param modules        The list of all the modules
     * @param scenarioNumber 0 is the base, 1 is first alternative, etc
     * @param doc            The document to create the elements
     * @return
     */
    private List<Element> createScenarioElements(Map<SubModule, List<FlagViolation>> subModToViolations, List<Module> modules,
                                                 FileChangesStatistics assumpStats, int scenarioNumber, boolean sameModel, Document doc)
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
                retVal.add(createCOAElementFromModule(subModToViolations, mod, scenarioNumber, rowNumber, doc));
                rowNumber++;
            }
            else
            {
                //do the generic creator
                retVal.add(createElementFromModule(subModToViolations, mod, scenarioNumber, rowNumber, doc));
                rowNumber++;
            }
        }
        return retVal;
    }


    /**
     * @param module
     * @param scenarioNumber base = 0, alt1 = 1, alt2 = 2
     * @param rowNumber      starts with 1 and actually gets put into the xml element
     * @param doc
     * @return
     */
    private Element createElementFromModule(Map<SubModule, List<FlagViolation>> subModToViolations, Module module, int scenarioNumber, int rowNumber,
                                            Document doc)
    {
        List<String> subModuleStrings = new ArrayList<>();

        for (SubModule sm : module.getSubModules())
        {
            int numViolations = 0;
            if (subModToViolations.containsKey(sm))
            {
                List<FlagViolation> violations = subModToViolations.get(sm);
                for (FlagViolation violation : violations)
                {
                    numViolations += violation.getTimes().size();
                }
            }

            String subModuleText = sm.getName();
            String formattedText = String.format(subModuleText, numViolations);
            subModuleStrings.add(formattedText);
        }

        String elementString = subModuleStrings.stream().map(String::valueOf).collect(Collectors.joining(NEW_LINE));

        if (scenarioNumber == 0)
        {
            return createBaseModuleElement(doc, rowNumber, module.getName(), elementString);
        }
        else
        {
            return createAlternativeModuleElement(doc, scenarioNumber, rowNumber, module.getName(), elementString);

        }

    }

    private Element createModelInputsElementFromModule(Module module, FileChangesStatistics assumpStats, int scenarioNumber, int rowNumber,
                                                       Document doc)
    {
        List<String> subModuleStrings = new ArrayList<>();

        if (scenarioNumber == 0)
        {
            return createBaseModuleElement(doc, rowNumber, module.getName(), "N/A");
        }

        List<SubModule> subModules = module.getSubModules();
        if (subModules.size() == 9 && assumpStats != null)
        {
            //initial conditions
            AssumptionChangesStatistics initAssumpStats = assumpStats.getInitAssumptionStats();
            int initOnlyInBase = initAssumpStats.getRecordsOnlyInBase().size();
            int initOnlyInAlt = initAssumpStats.getRecordsOnlyInAlt().size();
            int initChanged = initAssumpStats.getChangedFiles().size();

            //state variables
            AssumptionChangesStatistics svAssumpStats = assumpStats.getSVAssumptionStats();
            int svOnlyInBase = svAssumpStats.getRecordsOnlyInBase().size();
            int svOnlyInAlt = svAssumpStats.getRecordsOnlyInAlt().size();
            int svChanged = svAssumpStats.getChangedFiles().size();

            //code changes
            CodeChangesStatistics codeChangesStats = assumpStats.getCodeChangesStatistics();
            int filesAddedToAlt = codeChangesStats.getFilesAddedToAlt().size();
            int filesDeletedFromBase = codeChangesStats.getFilesDeletedFromBase().size();
            int codeChangesModifiedFiles = codeChangesStats.getCodeChangesModifiedFiles().size();

            for (SubModule sm : subModules)
            {
                addFormattedTextToSubModule(subModuleStrings, initOnlyInBase, initOnlyInAlt, initChanged, svOnlyInBase,
                        svOnlyInAlt, svChanged, filesAddedToAlt, filesDeletedFromBase, codeChangesModifiedFiles, sm);
            }


        }

        String elementString = subModuleStrings.stream().map(String::valueOf).collect(Collectors.joining(NEW_LINE));
        if (Objects.equals(elementString, ""))
        {
            elementString = "No Changes";
        }
        return createAlternativeModuleElement(doc, scenarioNumber, rowNumber, module.getName(), elementString);

    }

    private void addFormattedTextToSubModule(List<String> subModuleStrings, int initOnlyInBase, int initOnlyInAlt, int initChanged,
                                             int svOnlyInBase, int svOnlyInAlt, int svChanged, int filesAddedToAlt, int filesDeletedFromBase, int codeChangesModifiedFiles, SubModule sm)
    {
        String formattedText;
        switch (sm.getId())
        {
            case 1:
            {
                if (initChanged > 0)
                {
                    formattedText = String.format(sm.getName(), initChanged);
                    subModuleStrings.add(formattedText);
                }
                break;
            }
            case 2:
            {
                if (initOnlyInBase > 0)
                {
                    formattedText = String.format(sm.getName(), initOnlyInBase);
                    subModuleStrings.add(formattedText);
                }
                break;
            }
            case 3:
            {
                if (initOnlyInAlt > 0)
                {
                    formattedText = String.format(sm.getName(), initOnlyInAlt);
                    subModuleStrings.add(formattedText);
                }
                break;
            }
            case 4:
            {
                if (svChanged > 0)
                {
                    formattedText = String.format(sm.getName(), svChanged);
                    subModuleStrings.add(formattedText);
                }
                break;
            }
            case 15:
            {
                if (svOnlyInBase > 0)
                {
                    formattedText = String.format(sm.getName(), svOnlyInBase);
                    subModuleStrings.add(formattedText);
                }
                break;
            }
            case 16:
            {
                if (svOnlyInAlt > 0)
                {
                    formattedText = String.format(sm.getName(), svOnlyInAlt);
                    subModuleStrings.add(formattedText);
                }
                break;
            }
            case 17:
            {
                if (codeChangesModifiedFiles > 0)
                {
                    formattedText = String.format(sm.getName(), codeChangesModifiedFiles);
                    subModuleStrings.add(formattedText);
                }
                break;
            }
            case 18:
            {
                if (filesAddedToAlt > 0)
                {
                    formattedText = String.format(sm.getName(), filesAddedToAlt);
                    subModuleStrings.add(formattedText);
                }
                break;
            }
            case 19:
            {
                if (filesDeletedFromBase > 0)
                {
                    formattedText = String.format(sm.getName(), filesDeletedFromBase);
                    subModuleStrings.add(formattedText);
                }
                break;
            }

        }
    }

    private Element createCOAElementFromModule(Map<SubModule, List<FlagViolation>> subModToViolations, Module module, int alternativeNumber,
                                               int rowNumber, Document doc)
    {
        List<String> subModuleStrings = new ArrayList<>();

        for (SubModule sm : module.getSubModules())
        {

            String formattedText;
            if (subModToViolations.containsKey(sm))
            {
                double maxValue = subModToViolations.get(sm).get(0).getMaxValue();
                String subModuleText = sm.getName();
                formattedText = String.format(subModuleText, maxValue);
            }
            else
            {
                formattedText = "NR";
            }
            subModuleStrings.add(formattedText);
        }

        String elementString = subModuleStrings.stream().map(String::valueOf).collect(Collectors.joining(NEW_LINE));

        if (alternativeNumber == 0)
        {
            return createBaseModuleElement(doc, rowNumber, module.getName(), elementString);
        }
        else
        {
            return createAlternativeModuleElement(doc, alternativeNumber, rowNumber, module.getName(), elementString);
        }
    }


    private Element createAlternativeModuleElement(Document doc, int altColumnNumber, int row, String rowName, String value)
    {
        //scenario list starts with zero, but in the xml it will start with 1 so I increment by 1 here
        altColumnNumber++;
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
        String rowNumber;
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
}
