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

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.stream.Stream;

import net.sf.jasperreports.engine.JRException;
import net.sf.jasperreports.engine.JasperCompileManager;
import net.sf.jasperreports.engine.JasperFillManager;
import net.sf.jasperreports.engine.JasperPrint;
import net.sf.jasperreports.engine.JasperReport;
import net.sf.jasperreports.engine.SimpleJasperReportsContext;
import net.sf.jasperreports.engine.export.JRPdfExporter;
import net.sf.jasperreports.export.SimpleExporterInput;
import net.sf.jasperreports.export.SimpleOutputStreamExporterOutput;
import net.sf.jasperreports.repo.FileRepositoryPersistenceServiceFactory;
import net.sf.jasperreports.repo.FileRepositoryService;
import net.sf.jasperreports.repo.PersistenceServiceFactory;
import net.sf.jasperreports.repo.RepositoryService;

import static java.util.stream.Collectors.toList;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 04-26-2019
 */
public class JasperReportRunner
{
	private static final Logger LOGGER = Logger.getLogger(JasperReportRunner.class.getName());


	public void runReportWithOutputFile(Path pdfOutputPath, Path jrxmlPath)
			throws JRException, IOException
	{
		Path subreportDir = jrxmlPath.getParent().resolve("Subreports");
		if(subreportDir.toFile().isDirectory())
		{
			try(Stream<Path> paths = Files.walk(subreportDir, 1))
			{
				List<Path> subreports = paths.collect(toList());
				for(Path subreport : subreports)
				{
					if(subreport.toFile().isFile() && subreport.toString().endsWith("jrxml"))
					{
						LOGGER.log(Level.FINE, "Compiling Subreport {0}", subreport);
						JasperCompileManager.compileReportToFile(subreport.toString());
					}
				}
			}
		}

		// compiles jrxml
		Path dataDir = jrxmlPath.getParent().resolve("Datasource");
		Path imagesDir = jrxmlPath.getParent().resolve("Images");
		SimpleJasperReportsContext context = new SimpleJasperReportsContext();
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

		context.setExtensions(RepositoryService.class,
				Arrays.asList(fileRepository, imageRepository, subreportRepository, dataRepository));
		context.setExtensions(PersistenceServiceFactory.class,
				Collections.singletonList(FileRepositoryPersistenceServiceFactory.getInstance()));

		JasperFillManager manager = JasperFillManager.getInstance(context);

		Map<String, Object> parameters = new HashMap<>();
		JasperCompileManager.compileReportToFile(jrxmlPath.toString());
		LOGGER.log(Level.INFO, "Compiling main Jasper Report {0}", jrxmlPath);
		JasperReport jasperReport = JasperCompileManager.compileReport(jrxmlPath.toString());
		LOGGER.log(Level.INFO, "Filling Jasper Report with EPPT XML");
		JasperPrint jasperPrint = manager.fill(jasperReport, parameters);

		LOGGER.log(Level.INFO, "Exporting report to PDF: {0}", pdfOutputPath);
		// fills compiled report with parameters and a connection
		JRPdfExporter exporter = new JRPdfExporter();
		exporter.setExporterInput(new SimpleExporterInput(jasperPrint));
		exporter.setExporterOutput(new SimpleOutputStreamExporterOutput(pdfOutputPath.toFile()));
		exporter.exportReport();
		LOGGER.log(Level.FINE, "Jasper PDF exported");
	}
}
