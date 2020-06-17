/*
 * Enhanced Post Processing Tool (EPPT) Copyright (c) 2020.
 *
 *  EPPT is copyrighted by the State of California, Department of Water Resources. It is licensed
 *  under the GNU General Public License, version 2. This means it can be
 *  copied, distributed, and modified freely, but you may not restrict others
 *  in their ability to copy, distribute, and modify it. See the license below
 *  for more details.
 *
 *  GNU General Public License
 */

package gov.ca.water.reportengine;

import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.time.Duration;
import java.time.LocalDateTime;
import java.time.temporal.ChronoUnit;
import java.util.ArrayList;
import java.util.HashMap;
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
import gov.ca.water.calgui.EpptInitializationException;
import gov.ca.water.calgui.bo.DetailedIssue;
import gov.ca.water.calgui.bo.GUILinksAllModelsBO;
import gov.ca.water.calgui.busservice.impl.DetailedIssuesReader;
import gov.ca.water.calgui.project.EpptScenarioRun;
import gov.ca.water.calgui.scripts.DssCache;
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
import gov.ca.water.reportengine.standardsummary.EpptChart;
import gov.ca.water.reportengine.standardsummary.StandardSummaryErrors;
import gov.ca.water.reportengine.standardsummary.StandardSummaryReader;
import gov.ca.water.reportengine.standardsummary.StandardSummaryWriter;
import gov.ca.water.reportengine.standardsummary.SummaryReportParameters;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import static gov.ca.water.calgui.constant.Constant.CONFIG_DIR;
import static gov.ca.water.calgui.constant.Constant.CSV_EXT;
import static gov.ca.water.calgui.constant.Constant.QA_QC_DIR;
import static java.util.stream.Collectors.toList;

public class EPPTReport
{
	public static final int EQUAL_TO_CONSTANT = 200;
	public static final int LESS_THAN_CONSTANT = 300;
	public static final int GREATER_THAN_CONSTANT = 100;

	public static final String EXECUTIVE_SUMMARY = "Executive Summary";
	public static final String ASSUMPTION_CHANGES = "Assumption Changes";
	public static final String CODE_CHANGES = "Code Changes";
	public static final String DETAILED_ISSUES = "Detailed Issues";
	public static final String TABLE_OF_CONTENTS = "Table of Contents";
	public static final String COVER_PAGE = "Cover Page";
	public static final String STANDARD_SUMMARY_STATISTICS = "Standard Summary Statistics";

	private static final String INIT_COND_CSV = "AssumptionChangesIV" + CSV_EXT;
	private static final FluentLogger LOGGER = FluentLogger.forEnclosingClass();

	private static final String STATE_VAR_CSV = "AssumptionChangesSV" + CSV_EXT;
	private static final String CODE_CHANGES_CSV = "CodeChanges" + CSV_EXT;

	private static final String MODULES_CSV = QA_QC_DIR + "/Modules" + CSV_EXT;
	public static final String SUMMARY_CSV = QA_QC_DIR + "/Summary" + CSV_EXT;

	private final StandardSummaryErrors _standardSummaryErrors;
	private final Path _pathToWriteOut;
	private final EpptScenarioRun _baseRun;
	private final ReportParameters _reportParameters;
	private final List<EpptScenarioRun> _altRuns = new ArrayList<>();
	private final DssCache _dssCache;
	private List<Module> _modules;


	private List<DetailedIssue> _allDetailedIssues;

	public EPPTReport(Path pathToWriteOut, EpptScenarioRun baseRun, List<EpptScenarioRun> altRuns,
					  ReportParameters reportParameters, StandardSummaryErrors standardSummaryErrors)
	{
		_pathToWriteOut = pathToWriteOut;
		_baseRun = baseRun;
		_reportParameters = reportParameters;
		_standardSummaryErrors = standardSummaryErrors;
		_altRuns.addAll(altRuns);
		_dssCache = new DssCache();
	}

	//what about base only vs alt vs alts
	public void writeReport() throws QAQCReportException
	{
		LocalDateTime start = LocalDateTime.now();
		try
		{
			LOGGER.at(Level.INFO).log("============= Starting Report Processor: %s =============", start);
			validateReportInputs();
			DetailedIssuesReader.createDetailedIssues();
			Document doc = createDoc();
			//create the modules
			ModuleCreator mc = new ModuleCreator();
			_modules = mc.createModules(Paths.get(MODULES_CSV));
			_allDetailedIssues = mc.getAllDetailedIssues();
			checkInterrupt();

			//create the file change stats. One for each alt to the base in order of alts
			List<FileChangesStatistics> fileChangeStats = getFileChangeStatsList();
			checkInterrupt();

			List<EpptScenarioRun> allRuns = new ArrayList<>();
			allRuns.add(_baseRun);
			allRuns.addAll(_altRuns);
			Map<EpptScenarioRun, Map<SubModule, List<FlagViolation>>> runsToFlagViolations = processDtsViolations(allRuns);

			Element rootElement = printRootElement(doc);
			printCoverPage(doc, rootElement);
			//code changes and assump have to be the same model
			printExecutiveSummary(doc, fileChangeStats, runsToFlagViolations, rootElement);
			checkInterrupt();
			printDetailedIssues(doc, runsToFlagViolations, rootElement);
			checkInterrupt();

			//create and add the assumption changes
			if(!fileChangeStats.isEmpty())
			{
				printAssumptionChanges(doc, fileChangeStats, rootElement);
				checkInterrupt();
				printCodeChanges(doc, rootElement);
				checkInterrupt();
			}
			appendSummaryStats(doc, rootElement);

			LOGGER.at(Level.INFO).log("Writing data to XML: %s", _pathToWriteOut);
			writeXmlFile(doc);
			_dssCache.clearCache();
		}
		catch(RuntimeException | IOException | EpptReportException | ParserConfigurationException | TransformerException | EpptInitializationException e)
		{
			throw new QAQCReportException(e.getMessage(), e);
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

	public static void checkInterrupt() throws EpptReportException
	{
		if(Thread.currentThread().isInterrupted())
		{
			throw new EpptReportException("Task interrupted");
		}
	}

	private void printCodeChanges(Document doc, Element rootElement) throws IOException, EpptReportException
	{
		if(canPrintCodeChanges())
		{
			//create and add the code changes
			LOGGER.at(Level.INFO).log("Generate QAQC Code Changes data");
			rootElement.appendChild(createCodeChangesElem(_baseRun.getOutputPath(), _altRuns.get(0), doc));
		}
	}

	private void printAssumptionChanges(Document doc, List<FileChangesStatistics> fileChangeStats, Element rootElement)
	{
		if(canPrintAssumptionChanges())
		{
			LOGGER.at(Level.INFO).log("Generate QA/QC Assumption Changes data");
			rootElement.appendChild(createAssumptionChangesElem(fileChangeStats.get(0), doc));
		}
	}

	private void printDetailedIssues(Document doc, Map<EpptScenarioRun, Map<SubModule, List<FlagViolation>>> runsToFlagViolations,
									 Element rootElement) throws EpptReportException
	{
		if(canPrintDetailedIssues())
		{
			//create and add the detailed issues
			LOGGER.at(Level.INFO).log("Generate QA/QC Detailed Issues data");
			rootElement.appendChild(createDetailedIssueReportElem(runsToFlagViolations, doc));
		}
	}

	private Map<EpptScenarioRun, Map<SubModule, List<FlagViolation>>> processDtsViolations(List<EpptScenarioRun> allRuns) throws EpptReportException
	{
		if(canPrintDetailedIssues() || canPrintExecutiveSummary())
		{

			DTSProcessor dtsProcessor = new DTSProcessor(_modules, _standardSummaryErrors);
			return dtsProcessor.processDSSFiles(allRuns);
		}
		else
		{
			return new HashMap<>();
		}
	}

	private Element printRootElement(Document doc)
	{
		//create the rootnode
		Element rootElement = doc.createElement("qaqc-report");
		rootElement.setAttribute("alternative-count", Integer.toString(_altRuns.size()));
		rootElement.setAttribute("show-assumption-changes", Boolean.toString(canShowAssumptionChanges()));
		boolean printToc = _reportParameters.printToc();
		if(printToc)
		{
			rootElement.setAttribute("print-toc", Boolean.toString(printToc));
		}
		boolean printCoverPage = _reportParameters.printCoverPage();
		if(printCoverPage)
		{
			rootElement.setAttribute("print-title", Boolean.toString(printCoverPage));
		}
		doc.appendChild(rootElement);
		return rootElement;
	}

	private void printCoverPage(Document doc, Element rootElement)
	{
		if(canPrintCoverPage())
		{
			//create and add the report title page
			LOGGER.at(Level.INFO).log("Generate QAQC Title page data");
			rootElement.appendChild(createTitlePageElem(doc));
		}
	}

	private void printExecutiveSummary(Document doc, List<FileChangesStatistics> fileChangeStats,
									   Map<EpptScenarioRun, Map<SubModule, List<FlagViolation>>> runsToFlagViolations, Element rootElement)
	{
		if(canPrintExecutiveSummary())
		{
			//create and add the executive report
			LOGGER.at(Level.INFO).log("Generate QAQC Executive Summary data");
			rootElement.appendChild(createExecutiveReportElem(runsToFlagViolations, fileChangeStats, doc));
		}
	}

	private void appendSummaryStats(Document doc, Element rootElement) throws EpptReportException, EpptInitializationException
	{
		if(canPrintStandardSummary())
		{
			StandardSummaryReader standardSummaryReader = new StandardSummaryReader(Paths.get(SUMMARY_CSV), _standardSummaryErrors, _dssCache);
			Path imagesDir = _pathToWriteOut.getParent().getParent().resolve("Images");
			SummaryReportParameters summaryReportParameters = _reportParameters.getSummaryReportParameters();
			LOGGER.at(Level.INFO).log("Generate Standard Summary Statistic");
			StandardSummaryWriter standardSummaryWriter = new StandardSummaryWriter(doc, _baseRun, _altRuns, summaryReportParameters, imagesDir,
					_standardSummaryErrors);
			List<String> orderedChartIds = standardSummaryReader.getOrderedChartIds();
			Map<String, EpptChart> stringEpptChartMap = standardSummaryReader.readLines();
			checkInterrupt();
			List<EpptChart> collect = orderedChartIds.stream()
													 .map(stringEpptChartMap::get)
													 .filter(Objects::nonNull)
													 .collect(toList());
			rootElement.appendChild(standardSummaryWriter.write(collect));
		}
	}

	private boolean canPrintStandardSummary()
	{
		return !_reportParameters.getDisabledReportModules().contains(STANDARD_SUMMARY_STATISTICS);
	}

	private boolean canPrintExecutiveSummary()
	{
		return !_reportParameters.getDisabledReportModules().contains(EXECUTIVE_SUMMARY);
	}

	private boolean canPrintAssumptionChanges()
	{
		return !_reportParameters.getDisabledReportModules().contains(ASSUMPTION_CHANGES);
	}

	private boolean canPrintCodeChanges()
	{
		return !_reportParameters.getDisabledReportModules().contains(CODE_CHANGES);
	}

	private boolean canPrintDetailedIssues()
	{
		return !_reportParameters.getDisabledReportModules().contains(DETAILED_ISSUES);
	}

	private boolean canPrintTableOfContents()
	{
		return !_reportParameters.getDisabledReportModules().contains(TABLE_OF_CONTENTS);
	}

	private boolean canPrintCoverPage()
	{
		return !_reportParameters.getDisabledReportModules().contains(COVER_PAGE);
	}

	private void validateReportInputs() throws EpptReportException
	{
		LOGGER.at(Level.INFO).log("Validating report inputs");
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
		if(_reportParameters.getSummaryReportParameters().getWaterYearPeriodRanges().isEmpty())
		{
			String message = "No Annual Periods defined.";
			LOGGER.at(Level.SEVERE).log(message);
			throw new EpptReportException(message);
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

		if(run.getLookupDirectory() == null)
		{
			String errorMsg = "Scenario " + run.getName() + ": Water year type table is null";
			LOGGER.at(Level.SEVERE).log(errorMsg);
			throw new EpptReportException(errorMsg);
		}
		if(!run.getLookupDirectory().toFile().exists())
		{
			String errorMsg = "Scenario " + run.getName() + ": Water year type table file could not be found. Path: " + run.getLookupDirectory();
			LOGGER.at(Level.SEVERE).log(errorMsg);
			throw new EpptReportException(errorMsg);
		}

		if(run.getPostProcessDss() == null)
		{
			String errorMsg = "Scenario " + run.getName() + " is  missing the post process dss file.";
			LOGGER.at(Level.SEVERE).log(errorMsg);
			throw new EpptReportException(errorMsg);
		}
		if(!run.getPostProcessDss().toFile().exists())
		{
			String errorMsg = "Scenario " + run.getName() + " is  missing the post process dss file. Invalid path: " + run.getPostProcessDss();
			LOGGER.at(Level.SEVERE).log(errorMsg);
			throw new EpptReportException(errorMsg);
		}
		if(run.getDssContainer().getDtsDssFile().getAPart() == null || run.getDssContainer().getDtsDssFile().getAPart().isEmpty())
		{
			String errorMsg = "Scenario " + run.getName() + " is  missing the A Part for the post process dss file: " + run.getPostProcessDss();
			LOGGER.at(Level.SEVERE).log(errorMsg);
			throw new EpptReportException(errorMsg);
		}
		if(run.getDssContainer().getDtsDssFile().getFPart() == null || run.getDssContainer().getDtsDssFile().getFPart().isEmpty())
		{
			String errorMsg = "Scenario " + run.getName() + " is  missing the F Part for the post process dss file: " + run.getPostProcessDss();
			LOGGER.at(Level.SEVERE).log(errorMsg);
			throw new EpptReportException(errorMsg);
		}


		if(run.getDssContainer().getIvDssFile() == null)
		{
			String errorMsg = "Scenario " + run.getName() + " is  missing the IV dss file.";
			LOGGER.at(Level.SEVERE).log(errorMsg);
			throw new EpptReportException(errorMsg);
		}
		if(!run.getDssContainer().getIvDssFile().getDssPath().toFile().exists())
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
		if(!run.getDssContainer().getSvDssFile().getDssPath().toFile().exists())
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
		if(!run.getDssContainer().getDvDssFile().getDssPath().toFile().exists())
		{
			String errorMsg = "Scenario " + run.getName() + " is  missing the DV dss file. Invalid path: " + run.getDssContainer().getDvDssFile().getDssPath();
			LOGGER.at(Level.SEVERE).log(errorMsg);
			throw new EpptReportException(errorMsg);
		}

		if(run.getDssContainer().getDvDssFile().getAPart() == null || run.getDssContainer().getDvDssFile().getAPart().isEmpty())
		{
			String errorMsg = "Scenario " + run.getName() + " is  missing the A Part for the DV dss file: " + run.getDssContainer().getDvDssFile().getDssPath();
			LOGGER.at(Level.SEVERE).log(errorMsg);
			throw new EpptReportException(errorMsg);
		}
		if(run.getDssContainer().getDvDssFile().getFPart() == null || run.getDssContainer().getDvDssFile().getFPart().isEmpty())
		{
			String errorMsg = "Scenario " + run.getName() + " is  missing the F Part for the DV dss file: " + run.getDssContainer().getDvDssFile().getDssPath();
			LOGGER.at(Level.SEVERE).log(errorMsg);
			throw new EpptReportException(errorMsg);
		}

		if(run.getOutputPath() == null)
		{
			String errorMsg = "Scenario " + run.getName() + " is  missing the output path.";
			LOGGER.at(Level.SEVERE).log(errorMsg);
			throw new EpptReportException(errorMsg);
		}
		if(!run.getOutputPath().toFile().exists())
		{
			String errorMsg = "Scenario " + run.getName() + " is  missing the output path. Invalid path: " + run.getOutputPath();
			LOGGER.at(Level.SEVERE).log(errorMsg);
			throw new EpptReportException(errorMsg);
		}


	}

	private boolean canShowAssumptionChanges()
	{
		boolean retval = false;
		if(!_altRuns.isEmpty())
		{
			GUILinksAllModelsBO.Model baseModel = _baseRun.getModel();
			GUILinksAllModelsBO.Model altModel = _altRuns.get(0).getModel();
			if(Objects.equals(baseModel, altModel))
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
				new DetailedIssueProcessor(_standardSummaryErrors, runsToFlagViolations, _modules, _allDetailedIssues, allRuns, false);
		Map<EpptScenarioRun, Map<Module, List<DetailedIssueViolation>>> runsToDetailedViolations = processor.process();

		DetailedIssuesXMLCreator xmlCreator = new DetailedIssuesXMLCreator();
		return xmlCreator.createDetailedIssuesElement(runsToDetailedViolations, _baseRun, _altRuns, _modules, doc);
	}

	private Element createCodeChangesElem(Path baseOutputPath, EpptScenarioRun altRun, Document doc) throws IOException, EpptReportException
	{
		CodeChangesXMLCreator creator = new CodeChangesXMLCreator();
		return creator.createCodeChangesElement(getCodeChangesCsv(), baseOutputPath, altRun.getOutputPath(), doc);
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
		for(EpptScenarioRun altRun : _altRuns)
		{
			altNames.add(altRun.getName());
		}
		String author = _reportParameters.getAuthor();
		String title = _reportParameters.getTitle();
		String subtitle = _reportParameters.getSubtitle();
		ReportHeader rh = new ReportHeader(author, title, subtitle, _baseRun.getName(), altNames);
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

		if(canPrintExecutiveSummary() || canPrintAssumptionChanges())
		{
			Path baseInitDSSPath = _baseRun.getDssContainer().getIvDssFile().getDssPath();
			Path baseStateVarDSSPath = _baseRun.getDssContainer().getSvDssFile().getDssPath();
			Path baseOutputPath = _baseRun.getOutputPath();
			for(EpptScenarioRun altRun : _altRuns)
			{
				checkInterrupt();

				if(Objects.equals(altRun.getModel(), _baseRun.getModel()))
				{
					double tolerance = _reportParameters.getTolerance();
					LOGGER.at(Level.INFO).log("Processing Assumption Changes Initial Conditions for: %s", altRun.getName());
					AssumptionChangesDataProcessor initProcessor = new AssumptionChangesDataProcessor(getInitialConditionsCsv(), tolerance);
					AssumptionChangesStatistics initCondStats =
							initProcessor.processAssumptionChanges(baseInitDSSPath, altRun.getDssContainer().getIvDssFile().getDssPath());

					checkInterrupt();
					LOGGER.at(Level.INFO).log("Processing Assumption State Variables for: %s", altRun.getName());
					AssumptionChangesDataProcessor stateVarProcessor = new AssumptionChangesDataProcessor(getStateVariableCsv(), tolerance);
					AssumptionChangesStatistics stateVarStats =
							stateVarProcessor.processAssumptionChanges(baseStateVarDSSPath, altRun.getDssContainer().getSvDssFile().getDssPath());

					checkInterrupt();
					LOGGER.at(Level.INFO).log("Processing Code Changes for: %s", altRun.getName());
					CodeChangesDataProcessor processor = new CodeChangesDataProcessor(getCodeChangesCsv());
					CodeChangesStatistics codeChangeStats = processor.processCodeChanges(baseOutputPath, altRun.getOutputPath());

					checkInterrupt();
					FileChangesStatistics fileChangesStatistics = new FileChangesStatistics(initCondStats, stateVarStats, codeChangeStats);
					statsForAllAlternatives.add(fileChangesStatistics);
				}
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
