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

package gov.ca.water.eppt.jasperengine;

import java.io.BufferedWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.time.Duration;
import java.time.LocalDateTime;
import java.time.temporal.ChronoUnit;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.ConsoleHandler;
import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.logging.LogRecord;
import java.util.logging.Logger;
import java.util.stream.Stream;

import gov.ca.water.reportengine.QAQCReportException;
import gov.ca.water.reportengine.ReportRunner;
import net.sf.jasperreports.engine.JRException;
import net.sf.jasperreports.engine.JRPropertiesUtil;
import net.sf.jasperreports.engine.JasperCompileManager;
import net.sf.jasperreports.engine.JasperFillManager;
import net.sf.jasperreports.engine.JasperPrint;
import net.sf.jasperreports.engine.JasperReport;
import net.sf.jasperreports.engine.SimpleJasperReportsContext;
import net.sf.jasperreports.engine.data.JRXmlDataSource;
import net.sf.jasperreports.engine.export.JRPdfExporter;
import net.sf.jasperreports.engine.util.JRLoader;
import net.sf.jasperreports.engine.util.JRXmlUtils;
import net.sf.jasperreports.export.SimpleExporterInput;
import net.sf.jasperreports.export.SimpleOutputStreamExporterOutput;
import net.sf.jasperreports.repo.FileRepositoryPersistenceServiceFactory;
import net.sf.jasperreports.repo.FileRepositoryService;
import net.sf.jasperreports.repo.PersistenceServiceFactory;
import net.sf.jasperreports.repo.RepositoryService;
import org.w3c.dom.Document;

import rma.services.annotations.ServiceProvider;

import static java.util.stream.Collectors.toList;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 04-26-2019
 */
@ServiceProvider(service = ReportRunner.class)
public class JasperReportRunner implements ReportRunner
{
	private static final Logger LOGGER = Logger.getLogger(JasperReportRunner.class.getName());

	public static void main(String[] args) throws Exception
	{
		System.setProperty("java.util.logging.SimpleFormatter.format", "[%1$tF %1$tT] [%4$] %5$s %n");
		Logger parentLogger = Logger.getLogger("");

		Handler[] handlers = parentLogger.getHandlers();
		Arrays.asList(handlers).forEach(parentLogger::removeHandler);
		parentLogger.addHandler(new QaQcReportHandler());
		int exitValue = -1;
		if(args.length != 2)
		{
			LOGGER.log(Level.SEVERE, "input 1: expected jrxml file; input 2: expected output path");
		}
		else
		{
			LocalDateTime start = LocalDateTime.now();
			try
			{
				LOGGER.log(Level.INFO, "============= Starting Report: {0} =============", start);
				LOGGER.log(Level.INFO, "Starting Jasper Report Run. JRXML File: {1} Output File: {0}", args);
				JasperReportRunner runner = new JasperReportRunner();
				runner.runReportWithOutputFile(Paths.get(args[0]), Paths.get(args[1]));
				exitValue = 0;
			}
			catch(Throwable e)
			{
				LOGGER.log(Level.SEVERE, "Error in Jasper Report", e);
				try(BufferedWriter writer = Files.newBufferedWriter((Paths.get(args[0]).getParent().resolve("Error.txt")));
					PrintWriter printWriter = new PrintWriter(writer))
				{
					e.printStackTrace(printWriter);
				}
				System.exit(-1);
			}
			finally
			{
				LocalDateTime end = LocalDateTime.now();
				LOGGER.log(Level.INFO, "============= Report Finished: {0} =============", end);
				long minutes = ChronoUnit.MINUTES.between(start, end);
				long seconds = Duration.between(start, end).minus(minutes, ChronoUnit.MINUTES).getSeconds();
				LOGGER.log(Level.INFO, "============= Report Took: {0}min {1}sec =============", new Object[]{minutes, seconds});
			}
		}
		System.exit(exitValue);
	}

	@Override
	public void runReportWithOutputFile(Path pdfOutputPath, Path jrxmlPath) throws QAQCReportException
	{
		//		Appender console = new ConsoleAppender();
		//		org.apache.log4j.Logger root = org.apache.log4j.Logger.getLogger("net.sf.jasperreports");
		//		root.setLevel(org.apache.log4j.Level.DEBUG);
		//		root.addAppender(console);
		try
		{

			Path subreportDir = compileSubreports(jrxmlPath);
			// compiles jrxml
			Path dataDir = jrxmlPath.getParent().resolve("DWR_QA_QC_Reports/Datasource");
			Path imagesDir = jrxmlPath.getParent().resolve("DWR_QA_QC_Reports/Images");
			SimpleJasperReportsContext context = new SimpleJasperReportsContext();
			JRPropertiesUtil.getInstance(context).setProperty("net.sf.jasperreports.xpath.executer.factory",
					"net.sf.jasperreports.engine.util.xml.JaxenXPathExecuterFactory");
			FileRepositoryService imageRepository = new FileRepositoryService(context,
					imagesDir.toString(), true);
			LOGGER.log(Level.FINE, "Jasper Images Repository {0}", imagesDir);
			FileRepositoryService fileRepository = new FileRepositoryService(context, jrxmlPath.getParent().toString(),
					true);
			LOGGER.log(Level.FINE, "Jasper Subreport Repository {0}", subreportDir);
			FileRepositoryService subreportRepository = new FileRepositoryService(context,
					subreportDir.toString(), true);
			LOGGER.log(Level.FINE, "Jasper Data Repository {0}", dataDir);
			FileRepositoryService dataRepository = new FileRepositoryService(context,
					dataDir.toString(), true);
			LOGGER.log(Level.FINE, "Jasper Parent Repository {0}", dataDir);
			FileRepositoryService parentRepo = new FileRepositoryService(context,
					dataDir.getParent().toString(), true);

			context.setExtensions(RepositoryService.class,
					Arrays.asList(fileRepository, imageRepository, subreportRepository, dataRepository, parentRepo));
			context.setExtensions(PersistenceServiceFactory.class,
					Collections.singletonList(FileRepositoryPersistenceServiceFactory.getInstance()));

			JasperFillManager manager = JasperFillManager.getInstance(context);

			Map<String, Object> parameters = new HashMap<>();
			JasperCompileManager.compileReportToFile(jrxmlPath.toString());
			LOGGER.log(Level.INFO, "Compiling main Jasper Report {0}", jrxmlPath);
			JasperReport jasperReport = JasperCompileManager.compileReport(jrxmlPath.toString());
			LOGGER.log(Level.INFO, "Filling Jasper Report with EPPT XML");
			Document document = JRXmlUtils.parse(JRLoader.getLocationInputStream(dataDir.resolve("QAQC_Datasource.xml").toString()));
			JasperPrint jasperPrint = manager.fill(jasperReport, null, new JRXmlDataSource(context, document));

			LOGGER.log(Level.INFO, "Exporting report to PDF: {0}", pdfOutputPath);
			// fills compiled report with parameters and a connection
			JRPdfExporter exporter = new JRPdfExporter();
			exporter.setExporterInput(new SimpleExporterInput(jasperPrint));
			exporter.setExporterOutput(new SimpleOutputStreamExporterOutput(pdfOutputPath.toFile()));
			exporter.exportReport();
			LOGGER.log(Level.INFO, "Jasper PDF exported");

		}
		catch(JRException | IOException ex)
		{
			throw new QAQCReportException("Unable to generate Jasper Report PDF: " + jrxmlPath, ex);
		}
	}

	private Path compileSubreports(Path jrxmlParentPath) throws IOException, QAQCReportException
	{
		Path subreportDir = jrxmlParentPath.getParent().resolve("DWR_QA_QC_Reports/Subreports");
		if(subreportDir.toFile().isDirectory())
		{
			try(Stream<Path> paths = Files.walk(subreportDir, 5))
			{
				List<Path> subreports = paths.filter(subreport -> subreport.toFile().isFile())
											 .filter(subreport -> subreport.toString().endsWith("jrxml"))
											 .collect(toList());
				for(Path jrxmlFile : subreports)
				{
					try
					{
						Path jasperFile = jrxmlFile.getParent().resolve(jrxmlFile.getFileName().toString().replace("jrxml", "jasper"));
						if(!Files.exists(jasperFile) || Files.getLastModifiedTime(jasperFile).compareTo(Files.getLastModifiedTime(jrxmlFile)) < 0)
						{
							LOGGER.log(Level.INFO, "Compiling Subreport {0}", jrxmlFile);
							JasperCompileManager.compileReportToFile(jrxmlFile.toString(),
									jasperFile.toString());
						}
					}
					catch(JRException e)
					{
						throw new QAQCReportException("Unable to process subreport: " + jrxmlFile, e);
					}
				}
			}
		}
		return subreportDir;
	}

	private static class StdOutHandler extends ConsoleHandler
	{
		private StdOutHandler()
		{
			super();
			setOutputStream(System.out);
		}
	}

	private static class QaQcReportHandler extends ConsoleHandler
	{
		private final StdOutHandler _stdOutHandler;

		private QaQcReportHandler()
		{
			_stdOutHandler = new StdOutHandler();
		}

		@Override
		public void publish(LogRecord record)
		{
			Level level = record.getLevel();
			if(level == null || level.intValue() < Level.WARNING.intValue())
			{
				_stdOutHandler.publish(record);
			}
			else
			{
				super.publish(record);
			}
		}
	}
}
