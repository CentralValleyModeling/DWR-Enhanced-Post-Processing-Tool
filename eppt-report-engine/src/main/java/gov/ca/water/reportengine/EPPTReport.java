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
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
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

	private static final String INIT_COND_CSV = "AssumpChangesInitCond" + CSV_EXT;
	private static final String STATE_VAR_CSV = "AssumpChangesStateVar" + CSV_EXT;
	private static final String CODE_CHANGES_CSV = "CodeChangesDSSPaths" + CSV_EXT;

	private static final String MODULES_CSV = CONFIG_DIR + "/ExecutiveReportModulesCSV" + CSV_EXT;
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

	public EPPTReport(Path wyTypeTable,Path wyNameLookup, Path pathToWriteOut, EpptScenarioRun baseRun, List<EpptScenarioRun> altRuns, double tolerance, String author, String subtitle)
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
		try
		{
			LOGGER.at(Level.INFO).log("Begin QAQC Report Data processing");
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
					dtsProcessor.processDSSFiles(allRuns, getPostProcessDSSPathsForAllRuns());

			boolean canShowAssumptionChanges = canShowAssumptionChanges();
			//create the rootnode
			String canShow = "true";
			if(canShowAssumptionChanges == false)
			{
				canShow = "false";
			}
			Element rootElement = doc.createElement("qaqc-report");
			rootElement.setAttribute("alternative-count", Integer.toString(_altRuns.size()));
			rootElement.setAttribute("show-assumption-changes", canShow);
			doc.appendChild(rootElement);

			//create and add the report title page
			LOGGER.at(Level.INFO).log("Generate QAQC Title page data");
			rootElement.appendChild( createTitlePageElem(doc));

			//code changes and assump have to be the same model

			//create and add the executive report
			LOGGER.at(Level.INFO).log("Generate QAQC Executive Summary data");
			rootElement.appendChild( createExecutiveReportElem(runsToFlagViolations, fileChangeStats, doc));

			//create and add the detailed issues
			LOGGER.at(Level.INFO).log("Generate QAQC Detailed Issues data");
			rootElement.appendChild( createDetailedIssueReportElem(runsToFlagViolations, doc));

			//create and add the assumption changes
			if(!fileChangeStats.isEmpty())
			{
				LOGGER.at(Level.INFO).log("Generate QAQC Assumption Changes data");
				rootElement.appendChild( createAssumptionChangesElem(fileChangeStats.get(0), doc));

				//create and add the code changes
				LOGGER.at(Level.INFO).log("Generate QAQC Code Changes data");
				rootElement.appendChild( createCodeChangesElem(_baseRun.getOutputPath(), _altRuns.get(0), doc));
			}
			LOGGER.at(Level.INFO).log("Writing data to XML: {0}", _pathToWriteOut);
			writeXmlFile(doc);
		}
		catch(RuntimeException | IOException | EpptReportException | ParserConfigurationException | TransformerException e)
		{
			throw new QAQCReportException("Error in report generation", e);
		}
	}

	private boolean canShowAssumptionChanges()
	{
		boolean retval = false;
		if(!_altRuns.isEmpty())
		{
			GUILinksAllModelsBO.Model baseModel = _baseRun.getModel();
			GUILinksAllModelsBO.Model altModel = _altRuns.get(0).getModel();
			if(baseModel == altModel)
			{
				retval = true;
			}
		}
		return retval;

	}

	private void writeXmlFile(Document doc) throws TransformerException
	{
		TransformerFactory transformerFactory = TransformerFactory.newInstance();
		Transformer transformer = transformerFactory.newTransformer();
		DOMSource domSource = new DOMSource(doc);

		StreamResult streamResult = new StreamResult(_pathToWriteOut.toString());
		transformer.transform(domSource, streamResult);
	}

	private Element createDetailedIssueReportElem(Map<EpptScenarioRun, Map<SubModule, List<FlagViolation>>> runsToFlagViolations, Document doc) throws EpptReportException
	{
		List<EpptScenarioRun> allRuns = new ArrayList<>();
		allRuns.add(_baseRun);
		allRuns.addAll(_altRuns);
		DetailedIssueProcessor processor =
				new DetailedIssueProcessor(_wyTypeTable, _wyNameLookup, runsToFlagViolations, _modules, _allDetailedIssues, allRuns, true);
		Map<EpptScenarioRun, Map<Module, List<DetailedIssueViolation>>> runsToDetailedViolations = processor.process();

		DetailedIssuesXMLCreator xmlCreator = new DetailedIssuesXMLCreator();
		return xmlCreator.createDetailedIssuesElement(runsToDetailedViolations, _baseRun, _altRuns, _modules, doc);
	}

	private List<Path> getPostProcessDSSPathsForAllRuns()
	{
		List<Path> retval = new ArrayList<>();
		retval.add(_baseRun.getPostProcessDss());

		for(EpptScenarioRun run : _altRuns)
		{
			retval.add(run.getPostProcessDss());
		}
		return retval;
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

	private Element createExecutiveReportElem(Map<EpptScenarioRun, Map<SubModule, List<FlagViolation>>> runsToFlagViolations, List<FileChangesStatistics> fileChangesStatistics, Document doc)
	{
		Element execRootElem = doc.createElement("executive-report");
		ExecutiveReportXMLCreator erWriter = new ExecutiveReportXMLCreator();

		List<EpptScenarioRun> allRuns = new ArrayList<>();
		allRuns.add(_baseRun);
		allRuns.addAll(_altRuns);

		Element executiveReportTableElement = erWriter.createExecutiveReportTableElement(allRuns, runsToFlagViolations, _modules, fileChangesStatistics, true, doc);
		execRootElem.appendChild(executiveReportTableElement);
		return execRootElem;
	}

	private Element createTitlePageElem(Document doc)
	{
		List<String> altNames = new ArrayList<>();
		for(EpptScenarioRun altRun : _altRuns)
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
		for(EpptScenarioRun altRun : _altRuns)
		{

			AssumptionChangesDataProcessor initProcessor = new AssumptionChangesDataProcessor(getInitialConditionsCsv(), _tolerance);
			AssumptionChangesStatistics initCondStats =
					initProcessor.processAssumptionChanges(baseInitDSSPath, altRun.getDssContainer().getIvDssFile().getDssPath());

			AssumptionChangesDataProcessor stateVarProcessor = new AssumptionChangesDataProcessor(getStateVariableCsv(), _tolerance);
			AssumptionChangesStatistics stateVarStats =
					stateVarProcessor.processAssumptionChanges(baseStateVarDSSPath, altRun.getDssContainer().getSvDssFile().getDssPath());

			CodeChangesDataProcessor processor = new CodeChangesDataProcessor(getCodeChangesCsv());
			CodeChangesStatistics codeChangeStats = processor.processCodeChanges(baseOutputPath, altRun.getOutputPath());

			FileChangesStatistics fileChangesStatistics = new FileChangesStatistics(initCondStats, stateVarStats, codeChangeStats);
			statsForAllAlternatives.add(fileChangesStatistics);
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
