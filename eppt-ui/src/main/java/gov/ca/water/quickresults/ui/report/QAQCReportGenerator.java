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

package gov.ca.water.quickresults.ui.report;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Collections;
import java.util.List;
import java.util.stream.Stream;

import gov.ca.water.calgui.constant.Constant;
import gov.ca.water.calgui.constant.EpptPreferences;
import gov.ca.water.calgui.project.EpptScenarioRun;
import gov.ca.water.calgui.wresl.ProcessOutputConsumer;
import gov.ca.water.reportengine.EPPTReport;
import gov.ca.water.reportengine.QAQCReportException;
import gov.ca.water.reportengine.ReportParameters;

import static java.nio.file.StandardCopyOption.REPLACE_EXISTING;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 05-14-2019
 */
class QAQCReportGenerator
{
	private final ProcessOutputConsumer _consumer;

	QAQCReportGenerator(ProcessOutputConsumer consumer)
	{
		_consumer = consumer;
	}

	void generateQAQCReport(EpptScenarioRun baseRun, EpptScenarioRun altRun, ReportParameters reportParameters, Path outputPdf, boolean forceCopyJrxml)
			throws QAQCReportException
	{
		Path path = writeReportData(baseRun, altRun, reportParameters, forceCopyJrxml);
		QAQCProcessRunner processRunner = new QAQCProcessRunner(outputPdf, path, _consumer);
		processRunner.run();
	}

	private Path writeReportData(EpptScenarioRun baseRun, EpptScenarioRun altRun, ReportParameters reportParameters, boolean forceCopyJrxml)
			throws QAQCReportException
	{
		try
		{
			Path pathToWriteOut = copyJasperPaths(forceCopyJrxml);
			List<EpptScenarioRun> altRuns = Collections.emptyList();
			if(altRun != null)
			{
				altRuns = Collections.singletonList(altRun);
			}
			Path dataFile = pathToWriteOut.resolve("DWR_QA_QC_Reports").resolve("Datasource").resolve("EPPT_Data.xml");
			EPPTReport epptReport = new EPPTReport(dataFile,
					baseRun, altRuns, reportParameters);
//			if(false)
			{
				epptReport.writeReport();
			}
			return pathToWriteOut.resolve("QAQC_Report.jrxml");
		}
		catch(IOException | RuntimeException ex)
		{
			throw new QAQCReportException("Unable to copy Jasper Report files", ex);
		}
	}

	private Path copyJasperPaths(boolean forceCopyJrxml) throws IOException
	{
		String jasperDir = Constant.JASPER_DIR;
		Path lastProjectConfiguration = EpptPreferences.getLastProjectConfiguration();
		Path reports = lastProjectConfiguration.getParent().resolve("Reports");
		if(!reports.toFile().exists() ||
				!reports.resolve("QAQC_Report.jrxml").toFile().exists() ||
				forceCopyJrxml)
		{
			copyFolder(Paths.get(jasperDir), reports);
		}
		return reports;
	}

	private void copyFolder(Path src, Path dest) throws IOException
	{
		try(Stream<Path> walk = Files.walk(src))
		{
			walk.forEach(source -> copy(source, dest.resolve(src.relativize(source))));
		}
	}

	private void copy(Path source, Path dest)
	{
		try
		{
			if(!dest.toFile().exists())
			{
				boolean mkdirs = dest.toFile().mkdirs();
				if(!mkdirs)
				{
					throw new IOException("Unable to create directory for: " + dest);
				}
			}
			if(source.toFile().isFile())
			{
				Files.copy(source, dest, REPLACE_EXISTING);
			}
		}
		catch(IOException e)
		{
			throw new IllegalStateException("Error copying file: " + source, e);
		}
	}
}
