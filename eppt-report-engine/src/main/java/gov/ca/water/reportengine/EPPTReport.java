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

package gov.ca.water.reportengine;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.time.Duration;
import java.time.LocalDateTime;
import java.time.temporal.ChronoUnit;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.logging.Level;
import javax.xml.XMLConstants;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import com.google.common.flogger.FluentLogger;
import gov.ca.water.calgui.bo.GUILinksAllModelsBO;
import gov.ca.water.calgui.project.EpptScenarioRun;
import gov.ca.water.reportengine.detailedissues.DetailedIssue;
import gov.ca.water.reportengine.detailedissues.DetailedIssueProcessor;
import gov.ca.water.reportengine.detailedissues.DetailedIssueViolation;
import gov.ca.water.reportengine.detailedissues.DetailedIssuesXMLCreator;
import gov.ca.water.reportengine.executivereport.DTSProcessor;
import gov.ca.water.reportengine.executivereport.ExecutiveReportXMLCreator;
import gov.ca.water.reportengine.executivereport.FlagViolation;
import gov.ca.water.reportengine.executivereport.Module;
import gov.ca.water.reportengine.executivereport.SubModule;
import gov.ca.water.reportengine.filechanges.AssumptionChangesDataProcessor;
import gov.ca.water.reportengine.filechanges.AssumptionChangesStatistics;
import gov.ca.water.reportengine.filechanges.AssumptionChangesXMLCreator;
import gov.ca.water.reportengine.filechanges.CodeChangesDataProcessor;
import gov.ca.water.reportengine.filechanges.CodeChangesStatistics;
import gov.ca.water.reportengine.filechanges.CodeChangesXMLCreator;
import gov.ca.water.reportengine.filechanges.FileChangesStatistics;
import gov.ca.water.reportengine.reportheader.ReportHeader;
import gov.ca.water.reportengine.reportheader.ReportHeaderXMLCreator;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import static gov.ca.water.calgui.constant.Constant.CONFIG_DIR;
import static gov.ca.water.calgui.constant.Constant.CSV_EXT;

public class EPPTReport
{
    private static final FluentLogger LOGGER = FluentLogger.forEnclosingClass();

    private static final String INIT_COND_CSV = "AssumptionChangesIV" + CSV_EXT;
    private static final String STATE_VAR_CSV = "AssumptionChangesSV" + CSV_EXT;
    private static final String CODE_CHANGES_CSV = "CodeChanges" + CSV_EXT;

    private static final String MODULES_CSV = CONFIG_DIR + "/Modules" + CSV_EXT;
    private static final String DETAILS_CSV = CONFIG_DIR + "/Details" + CSV_EXT;

    private final Path _wyTypeTable;
    private final Path _wyNameLookup;
    private final Path _pathToWriteOut;
    private final EpptScenarioRun _baseRun;
    private final List<EpptScenarioRun> _altRuns = new ArrayList<>();
    private final double _tolerance;
    private final String _author;
    private final String _subtitle;
    private List<Module> _modules;


    private List<DetailedIssue> _allDetailedIssues;

    public EPPTReport(Path wyTypeTable, Path wyNameLookup, Path pathToWriteOut, EpptScenarioRun baseRun, List<EpptScenarioRun> altRuns,
                      double tolerance, String author, String subtitle)
    {
        _wyTypeTable = wyTypeTable;
        _wyNameLookup = wyNameLookup;
        _pathToWriteOut = pathToWriteOut;
        _baseRun = baseRun;
        _altRuns.addAll(altRuns);
        _tolerance = tolerance;
        _author = author;
        _subtitle = subtitle;
    }

    //what about base only vs alt vs alts
    public void writeReport() throws QAQCReportException
    {
        LocalDateTime start = LocalDateTime.now();
        try
        {
            LOGGER.at(Level.INFO).log("============= Starting Report Processor: %s =============", start);


            LOGGER.at(Level.INFO).log("Validating report inputs");
            validateReportInputs();

            Document doc = createDoc();

            //do some up front processing

            //create the modules
            ModuleCreator mc = new ModuleCreator();
            _modules = mc.createModules(Paths.get(MODULES_CSV), Paths.get(DETAILS_CSV));
            _allDetailedIssues = mc.getAllDetailedIssues();

            //create the file change stats. One for each alt to the base in order of alts
            List<FileChangesStatistics> fileChangeStats = getFileChangeStatsList();

            List<EpptScenarioRun> allRuns = new ArrayList<>();
            allRuns.add(_baseRun);
            allRuns.addAll(_altRuns);
            DTSProcessor dtsProcessor = new DTSProcessor(_modules);
            Map<EpptScenarioRun, Map<SubModule, List<FlagViolation>>> runsToFlagViolations =
                    dtsProcessor.processDSSFiles(allRuns);

            //create the rootnode
            Element rootElement = doc.createElement("qaqc-report");
            rootElement.setAttribute("alternative-count", Integer.toString(_altRuns.size()));
            rootElement.setAttribute("show-assumption-changes", Boolean.toString(canShowAssumptionChanges()));
            doc.appendChild(rootElement);

            //create and add the report title page
            LOGGER.at(Level.INFO).log("Generate QAQC Title page data");
            rootElement.appendChild(createTitlePageElem(doc));

            //code changes and assump have to be the same model

            //create and add the executive report
            LOGGER.at(Level.INFO).log("Generate QAQC Executive Summary data");
            rootElement.appendChild(createExecutiveReportElem(runsToFlagViolations, fileChangeStats, doc));

            //create and add the detailed issues
            LOGGER.at(Level.INFO).log("Generate QAQC Detailed Issues data");
            rootElement.appendChild(createDetailedIssueReportElem(runsToFlagViolations, doc));

            //create and add the assumption changes
            if (!fileChangeStats.isEmpty())
            {
                LOGGER.at(Level.INFO).log("Generate QAQC Assumption Changes data");
                rootElement.appendChild(createAssumptionChangesElem(fileChangeStats.get(0), doc));

                //create and add the code changes
                LOGGER.at(Level.INFO).log("Generate QAQC Code Changes data");
                rootElement.appendChild(createCodeChangesElem(_baseRun.getOutputPath(), _altRuns.get(0), doc));
            }
            LOGGER.at(Level.INFO).log("Writing data to XML: %s", _pathToWriteOut);
            writeXmlFile(doc);
        }
        catch (RuntimeException | IOException | EpptReportException | ParserConfigurationException | TransformerException e)
        {
            throw new QAQCReportException("Error in report generation", e);
        }
        finally
        {
            LocalDateTime end = LocalDateTime.now();
            LOGGER.at(Level.INFO).log("============= Report Processing Finished: %s =============", end);
            long minutes = ChronoUnit.MINUTES.between(start, end);
            long seconds = Duration.between(start, end).minus(minutes, ChronoUnit.MINUTES).getSeconds();
            LOGGER.at(Level.INFO).log("============= Report Processing Took: %smin %ssec =============", minutes, seconds);
        }
    }

    private void validateReportInputs() throws EpptReportException
    {
        if(_wyTypeTable == null)
        {
            String errorMsg = "Water year type table is null";
            LOGGER.at(Level.SEVERE).log(errorMsg);
            throw new EpptReportException(errorMsg);
        }
        if(!_wyTypeTable.toFile().exists())
        {
            String errorMsg = "Water year type table file could not be found. Path: " + _wyTypeTable;
            LOGGER.at(Level.SEVERE).log(errorMsg);
            throw new EpptReportException(errorMsg);
        }

        if(_wyNameLookup == null)
        {
            String errorMsg = "Water year name lookup csv is null";
            LOGGER.at(Level.SEVERE).log(errorMsg);
            throw new EpptReportException(errorMsg);
        }
        if(!_wyNameLookup.toFile().exists())
        {
            String errorMsg = "Water year name lookup file could not be found. Path: " + _wyNameLookup;
            LOGGER.at(Level.SEVERE).log(errorMsg);
            throw new EpptReportException(errorMsg);
        }


        if(_pathToWriteOut == null)
        {
            String errorMsg = "The selected path to write the report to is null";
            LOGGER.at(Level.SEVERE).log(errorMsg);
            throw new EpptReportException(errorMsg);
        }

        if(_baseRun == null)
        {
            String errorMsg = "The base scenario is null";
            LOGGER.at(Level.SEVERE).log(errorMsg);
            throw new EpptReportException(errorMsg);
        }

        List<EpptScenarioRun> allRuns = new ArrayList<>();
        allRuns.add(_baseRun);
        allRuns.addAll(_altRuns);

        for(EpptScenarioRun run : allRuns)
        {
            checkCorrectFilePaths(run);
        }

    }

    private void checkCorrectFilePaths(EpptScenarioRun run) throws EpptReportException
    {
        if(run.getPostProcessDss() == null)
        {
            String errorMsg = "Scenario " + run.getName() + " is  missing the post process dss file.";
            LOGGER.at(Level.SEVERE).log(errorMsg);
            throw new EpptReportException(errorMsg);
        }
        if (!run.getPostProcessDss().toFile().exists())
        {
            String errorMsg = "Scenario " + run.getName() + " is  missing the post process dss file. Invalid path: " + run.getPostProcessDss();
            LOGGER.at(Level.SEVERE).log(errorMsg);
            throw new EpptReportException(errorMsg);
        }


        if(run.getDssContainer().getIvDssFile() == null)
        {
            String errorMsg = "Scenario " + run.getName() + " is  missing the IV dss file.";
            LOGGER.at(Level.SEVERE).log(errorMsg);
            throw new EpptReportException(errorMsg);
        }
        if (!run.getDssContainer().getIvDssFile().getDssPath().toFile().exists())
        {
            String errorMsg = "Scenario " + run.getName() + " is  missing the IV dss file. Invalid path: " + run.getDssContainer().getIvDssFile().getDssPath();
            LOGGER.at(Level.SEVERE).log(errorMsg);
            throw new EpptReportException(errorMsg);
        }

        if(run.getDssContainer().getSvDssFile() == null)
        {
            String errorMsg = "Scenario " + run.getName() + " is  missing the SV dss file.";
            LOGGER.at(Level.SEVERE).log(errorMsg);
            throw new EpptReportException(errorMsg);
        }
        if (!run.getDssContainer().getSvDssFile().getDssPath().toFile().exists())
        {
            String errorMsg = "Scenario " + run.getName() + " is  missing the SV dss file. Invalid path: " + run.getDssContainer().getSvDssFile().getDssPath();
            LOGGER.at(Level.SEVERE).log(errorMsg);
            throw new EpptReportException(errorMsg);
        }

        if(run.getDssContainer().getDvDssFile() == null)
        {
            String errorMsg = "Scenario " + run.getName() + " is  missing the DV dss file.";
            LOGGER.at(Level.SEVERE).log(errorMsg);
            throw new EpptReportException(errorMsg);
        }
        if (!run.getDssContainer().getDvDssFile().getDssPath().toFile().exists())
        {
            String errorMsg = "Scenario " + run.getName() + " is  missing the DV dss file. Invalid path: " + run.getDssContainer().getDvDssFile().getDssPath();
            LOGGER.at(Level.SEVERE).log(errorMsg);
            throw new EpptReportException(errorMsg);
        }


        if(run.getOutputPath() == null)
        {
            String errorMsg = "Scenario " + run.getName() + " is  missing the output path.";
            LOGGER.at(Level.SEVERE).log(errorMsg);
            throw new EpptReportException(errorMsg);
        }
        if (!run.getOutputPath().toFile().exists())
        {
            String errorMsg = "Scenario " + run.getName() + " is  missing the output path. Invalid path: " + run.getOutputPath();
            LOGGER.at(Level.SEVERE).log(errorMsg);
            throw new EpptReportException(errorMsg);
        }


    }

    private boolean canShowAssumptionChanges()
    {
        boolean retval = false;
        if (!_altRuns.isEmpty())
        {
            GUILinksAllModelsBO.Model baseModel = _baseRun.getModel();
            GUILinksAllModelsBO.Model altModel = _altRuns.get(0).getModel();
            if (Objects.equals(baseModel, altModel))
            {
                retval = true;
            }
        }
        return retval;

    }

    private void writeXmlFile(Document doc) throws TransformerException
    {
        TransformerFactory transformerFactory = TransformerFactory.newInstance();
        transformerFactory.setFeature(XMLConstants.FEATURE_SECURE_PROCESSING, true);
        Transformer transformer = transformerFactory.newTransformer();
        DOMSource domSource = new DOMSource(doc);

        LOGGER.at(Level.INFO).log("Generating XML at path: %s", _pathToWriteOut);
        StreamResult streamResult = new StreamResult(_pathToWriteOut.toString());
        transformer.transform(domSource, streamResult);
        LOGGER.at(Level.INFO).log("Finished writing XML");
    }

    private Element createDetailedIssueReportElem(Map<EpptScenarioRun, Map<SubModule, List<FlagViolation>>> runsToFlagViolations, Document doc)
            throws EpptReportException
    {
        List<EpptScenarioRun> allRuns = new ArrayList<>();
        allRuns.add(_baseRun);
        allRuns.addAll(_altRuns);
        DetailedIssueProcessor processor =
                new DetailedIssueProcessor(_wyTypeTable, _wyNameLookup, runsToFlagViolations, _modules, _allDetailedIssues, allRuns, false);
        Map<EpptScenarioRun, Map<Module, List<DetailedIssueViolation>>> runsToDetailedViolations = processor.process();

        DetailedIssuesXMLCreator xmlCreator = new DetailedIssuesXMLCreator();
        return xmlCreator.createDetailedIssuesElement(runsToDetailedViolations, _baseRun, _altRuns, _modules, doc);
    }

    private Element createCodeChangesElem(Path baseOutputPath, EpptScenarioRun altRun, Document doc) throws IOException, EpptReportException
    {
        CodeChangesXMLCreator creator = new CodeChangesXMLCreator();
        return creator.createCodeChangesElement(getCodeChangesCsv(), baseOutputPath, altRun.getOutputPath(), altRun.getName(), doc);
    }

    private Element createAssumptionChangesElem(FileChangesStatistics fileChangesStatistics, Document doc)
    {
        AssumptionChangesXMLCreator assumpCreater = new AssumptionChangesXMLCreator();
        return assumpCreater.createAssumptionChangesElement(doc, fileChangesStatistics);
    }

    private Element createExecutiveReportElem(Map<EpptScenarioRun, Map<SubModule, List<FlagViolation>>> runsToFlagViolations,
                                              List<FileChangesStatistics> fileChangesStatistics, Document doc)
    {
        Element execRootElem = doc.createElement("executive-report");
        ExecutiveReportXMLCreator erWriter = new ExecutiveReportXMLCreator();

        List<EpptScenarioRun> allRuns = new ArrayList<>();
        allRuns.add(_baseRun);
        allRuns.addAll(_altRuns);

        Element executiveReportTableElement = erWriter.createExecutiveReportTableElement(allRuns, runsToFlagViolations, _modules,
                fileChangesStatistics, true, doc);
        execRootElem.appendChild(executiveReportTableElement);
        return execRootElem;
    }

    private Element createTitlePageElem(Document doc)
    {
        List<String> altNames = new ArrayList<>();
        for (EpptScenarioRun altRun : _altRuns)
        {
            altNames.add(altRun.getName());
        }
        ReportHeader rh = new ReportHeader(_author, _subtitle, _baseRun.getName(), altNames);
        ReportHeaderXMLCreator rhWriter = new ReportHeaderXMLCreator();
        return rhWriter.createReportHeaderElement(rh, doc);
    }


    private Document createDoc() throws ParserConfigurationException
    {
        DocumentBuilderFactory documentFactory = DocumentBuilderFactory.newInstance();
        DocumentBuilder documentBuilder = documentFactory.newDocumentBuilder();
        return documentBuilder.newDocument();
    }


    private List<FileChangesStatistics> getFileChangeStatsList() throws EpptReportException, IOException
    {
        List<FileChangesStatistics> statsForAllAlternatives = new ArrayList<>();

        Path baseInitDSSPath = _baseRun.getDssContainer().getIvDssFile().getDssPath();
        Path baseStateVarDSSPath = _baseRun.getDssContainer().getSvDssFile().getDssPath();
        Path baseOutputPath = _baseRun.getOutputPath();
        for (EpptScenarioRun altRun : _altRuns)
        {

            if (Objects.equals(altRun.getModel(), _baseRun.getModel()))
            {
                LOGGER.at(Level.INFO).log("Processing Assumption Changes Initial Conditions for: %s", altRun.getName());
                AssumptionChangesDataProcessor initProcessor = new AssumptionChangesDataProcessor(getInitialConditionsCsv(), _tolerance);
                AssumptionChangesStatistics initCondStats =
                        initProcessor.processAssumptionChanges(baseInitDSSPath, altRun.getDssContainer().getIvDssFile().getDssPath());

                LOGGER.at(Level.INFO).log("Processing Assumption State Variables for: %s", altRun.getName());
                AssumptionChangesDataProcessor stateVarProcessor = new AssumptionChangesDataProcessor(getStateVariableCsv(), _tolerance);
                AssumptionChangesStatistics stateVarStats =
                        stateVarProcessor.processAssumptionChanges(baseStateVarDSSPath, altRun.getDssContainer().getSvDssFile().getDssPath());

                LOGGER.at(Level.INFO).log("Processing Code Changes for: %s", altRun.getName());
                CodeChangesDataProcessor processor = new CodeChangesDataProcessor(getCodeChangesCsv());
                CodeChangesStatistics codeChangeStats = processor.processCodeChanges(baseOutputPath, altRun.getOutputPath());

                FileChangesStatistics fileChangesStatistics = new FileChangesStatistics(initCondStats, stateVarStats, codeChangeStats);
                statsForAllAlternatives.add(fileChangesStatistics);
            }
        }

        return statsForAllAlternatives;
    }

    private Path getInitialConditionsCsv()
    {
        GUILinksAllModelsBO.Model model = _baseRun.getModel();
        return Paths.get(CONFIG_DIR).resolve(model.toString()).resolve(INIT_COND_CSV);
    }

    private Path getStateVariableCsv()
    {
        GUILinksAllModelsBO.Model model = _baseRun.getModel();
        return Paths.get(CONFIG_DIR).resolve(model.toString()).resolve(STATE_VAR_CSV);
    }

    private Path getCodeChangesCsv()
    {
        GUILinksAllModelsBO.Model model = _baseRun.getModel();
        return Paths.get(CONFIG_DIR).resolve(model.toString()).resolve(CODE_CHANGES_CSV);
    }

}
