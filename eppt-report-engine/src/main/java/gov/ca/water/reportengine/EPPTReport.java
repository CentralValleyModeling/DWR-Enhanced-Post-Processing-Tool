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

import gov.ca.water.calgui.project.EpptScenarioRun;
import gov.ca.water.reportengine.detailedissues.DetailedIssue;
import gov.ca.water.reportengine.detailedissues.DetailedIssueProcessor;
import gov.ca.water.reportengine.detailedissues.DetailedIssueViolation;
import gov.ca.water.reportengine.detailedissues.DetailedIssuesXMLCreator;
import gov.ca.water.reportengine.executivereport.*;
import gov.ca.water.reportengine.filechanges.*;
import gov.ca.water.reportengine.reportheader.ReportHeader;
import gov.ca.water.reportengine.reportheader.ReportHeaderXMLCreator;
import org.w3c.dom.Document;

import javax.lang.model.type.ArrayType;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import static gov.ca.water.calgui.constant.Constant.CONFIG_DIR;
import static gov.ca.water.calgui.constant.Constant.CSV_EXT;

public class EPPTReport
{
    private static final String GUI_LINKS_ALL_MODELS_FILENAME = CONFIG_DIR + "/GUI_Links_All_Models" + CSV_EXT;

    private static final String INIT_COND_CSV = CONFIG_DIR + "/AssumptionChangesInitialConditionsDSSPaths" + CSV_EXT;

    private static final String STATE_VAR_CSV = CONFIG_DIR + "/AssumptionChangesStateVariablesDSSPaths" + CSV_EXT;

    private static final String MODULES_CSV = CONFIG_DIR + "/ExecutiveReportModulesCSV" + CSV_EXT;

    private static final String DETAILS_CSV = CONFIG_DIR + "/Details" + CSV_EXT;

    private static final String CODE_CHANGES_CSV = CONFIG_DIR + "/CodeChangesDSSPaths" + CSV_EXT;


    private final Path _pathToWriteOut;
    private final EpptScenarioRun _baseRun;
    private final List<EpptScenarioRun> _altRuns;
    private final double _tolerance;
    private final String _author;
    private final String _subtitle;
    private List<Module> _modules;


    private List<DetailedIssue> _allDetailedIssues;

    public EPPTReport(Path pathToWriteOut, EpptScenarioRun baseRun, List<EpptScenarioRun> altRuns, double tolerance, String author, String subtitle)
    {
        _pathToWriteOut = pathToWriteOut;
        _baseRun = baseRun;
        _altRuns = altRuns;
        _tolerance = tolerance;


        _author = author;
        _subtitle = subtitle;
    }

    //what about base only vs alt vs alts
    public void writeReport() throws Exception
    {
        Document doc = getDoc();

        //do some up front processing

        //create the modules
        ModuleCreator mc = new ModuleCreator();
        _modules = mc.createModules(Paths.get(MODULES_CSV), Paths.get(DETAILS_CSV));
        _allDetailedIssues = mc.getAllDetailedIssues();

        //create the file change stats. One for each alt to the base in order of alts
        List<FileChangesStatistics> fileChangeStats = getFileChangeStatsList();



        //create and add the report title page
        addTitlePageToDoc(doc);


        //code changes and assump have to be the same model


        //create and add the executive report
        addExecutiveReportToDoc(fileChangeStats, doc);

        //create and add the detailed issues
        addDetailedIssueReport(doc);

        //create and add the assumption changes
        addAssumptionChangesToDoc(fileChangeStats.get(0), doc);//i need to rethink how i am doing these stats. This one doesn't support multiple alts

        //create and add the code changes
        addCodeChangesToDoc(_baseRun.getOutputPath(), _altRuns.get(0), doc);

        writeXmlFile(doc);
    }

    private void writeXmlFile(Document doc) throws TransformerException
    {
        TransformerFactory transformerFactory = TransformerFactory.newInstance();
        Transformer transformer = transformerFactory.newTransformer();
        DOMSource domSource = new DOMSource(doc);

        StreamResult streamResult = new StreamResult(_pathToWriteOut.toString());
        transformer.transform(domSource, streamResult);
    }

    private void addDetailedIssueReport(Document doc) throws Exception
    {
        DTSProcessor dtsProcessor = new DTSProcessor(_modules);
        List<EpptScenarioRun> allRuns = new ArrayList<>();
        allRuns.add(_baseRun);
        allRuns.addAll(_altRuns);

        Map<EpptScenarioRun, Map<SubModule, List<FlagViolation>>> runsToFlagViolations =
                dtsProcessor.processDSSFiles(allRuns, getPostProcessDSSPathsForAllRuns());

        DetailedIssueProcessor processor = new DetailedIssueProcessor(runsToFlagViolations, _modules, _allDetailedIssues, allRuns,true);
        Map<EpptScenarioRun, Map<Module, List<DetailedIssueViolation>>> runsToDetailedViolations = processor.process();

        DetailedIssuesXMLCreator xmlCreator = new DetailedIssuesXMLCreator();
        xmlCreator.appendDetailedIssuesElement(runsToDetailedViolations, _baseRun, _altRuns, _modules,doc);
    }

    private List<Path> getPostProcessDSSPathsForAllRuns()
    {
        List<Path> retval = new ArrayList<>();
        retval.add(_baseRun.getDssContainer().getDvDssFile().getDssPath());//what dss file is this? shouldn't it be "pst_prcss"

        for(EpptScenarioRun run : _altRuns)
        {
            retval.add(run.getDssContainer().getDvDssFile().getDssPath());
        }
        return retval;
    }

    private void addCodeChangesToDoc(Path baseOutputPath, EpptScenarioRun altRun, Document doc) throws IOException, EpptReportException
    {
        CodeChangesXMLCreator creator = new CodeChangesXMLCreator();
        creator.appendCodeChangesElement(Paths.get(CODE_CHANGES_CSV) ,baseOutputPath, altRun.getOutputPath(),altRun.getName(),doc);
    }

    private void addAssumptionChangesToDoc(FileChangesStatistics fileChangesStatistics, Document doc)
    {
        AssumptionChangesXMLCreator assumpCreater = new AssumptionChangesXMLCreator();
        assumpCreater.appendAssumptionChangesElement(doc, fileChangesStatistics);

    }

    private void addExecutiveReportToDoc(List<FileChangesStatistics> fileChangesStatistics, Document doc) throws Exception
    {
        ExecutiveReportXMLCreator erWriter = new ExecutiveReportXMLCreator();

        List<EpptScenarioRun> allRuns = new ArrayList<>();
        allRuns.add(_baseRun);
        allRuns.addAll(_altRuns);

        DTSProcessor dtsProcessor = new DTSProcessor(_modules);
        Map<EpptScenarioRun, Map<SubModule, List<FlagViolation>>> runsToViolations =
                dtsProcessor.processDSSFiles(allRuns, getPostProcessDSSPathsForAllRuns());//this is a list of the post process dss paths


        erWriter.appendExecutiveReportTableElement(allRuns, runsToViolations, _modules,fileChangesStatistics, true, doc);
    }

    private void addTitlePageToDoc(Document doc)
    {
        List<String> altNames = new ArrayList<>();
        for(EpptScenarioRun altRun : _altRuns)
        {
            altNames.add(altRun.getName());
        }
        ReportHeader rh = new ReportHeader(_author, _subtitle, _baseRun.getName(), altNames);
        ReportHeaderXMLCreator rhWriter = new ReportHeaderXMLCreator();
        rhWriter.appendReportHeaderElement(rh, doc);
    }



    public Document getDoc() throws ParserConfigurationException
    {
        DocumentBuilderFactory documentFactory = DocumentBuilderFactory.newInstance();
        DocumentBuilder documentBuilder = documentFactory.newDocumentBuilder();
        return documentBuilder.newDocument();
    }



    private List<FileChangesStatistics> getFileChangeStatsList() throws Exception
    {
        List<FileChangesStatistics> statsForAllAlternatives = new ArrayList<>();

        Path baseInitDSSPath = _baseRun.getDssContainer().getIvDssFile().getDssPath();
        Path baseStateVarDSSPath = _baseRun.getDssContainer().getSvDssFile().getDssPath();
        Path baseOutputPath = _baseRun.getOutputPath();
        for(EpptScenarioRun altRun : _altRuns)
        {
            AssumptionChangesDataProcessor initProcessor = new AssumptionChangesDataProcessor(Paths.get(INIT_COND_CSV), _tolerance);
            AssumptionChangesStatistics initCondStats =
                    initProcessor.processAssumptionChanges(baseInitDSSPath, altRun.getDssContainer().getIvDssFile().getDssPath());

            AssumptionChangesDataProcessor stateVarProcessor = new AssumptionChangesDataProcessor(Paths.get(STATE_VAR_CSV), _tolerance);
            AssumptionChangesStatistics stateVarStats =
                    stateVarProcessor.processAssumptionChanges(baseStateVarDSSPath, altRun.getDssContainer().getSvDssFile().getDssPath());

            CodeChangesDataProcessor processor = new CodeChangesDataProcessor(Paths.get(CODE_CHANGES_CSV));
            CodeChangesStatistics codeChangeStats = processor.processCodeChanges(baseOutputPath, altRun.getOutputPath());

            FileChangesStatistics fileChangesStatistics = new FileChangesStatistics(initCondStats, stateVarStats, codeChangeStats);
            statsForAllAlternatives.add(fileChangesStatistics);
        }

        return statsForAllAlternatives;
    }

}
