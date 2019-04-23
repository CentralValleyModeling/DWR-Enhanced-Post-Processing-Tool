/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */

package gov.ca.water.eppt.nbui.actions;

import java.awt.Desktop;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
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
import java.util.stream.Collectors;
import java.util.stream.Stream;
import javax.swing.*;

import gov.ca.water.calgui.busservice.IModelRunSvc;
import gov.ca.water.calgui.busservice.impl.ModelRunSvcImpl;
import gov.ca.water.calgui.constant.Constant;
import gov.ca.water.calgui.constant.EpptPreferences;
import gov.ca.water.calgui.presentation.ProgressFrame;
import net.sf.jasperreports.engine.JREmptyDataSource;
import net.sf.jasperreports.engine.JRException;
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
import org.openide.awt.ActionID;
import org.openide.awt.ActionReference;
import org.openide.awt.ActionReferences;
import org.openide.awt.ActionRegistration;
import org.openide.util.NbBundle;
import org.openide.windows.WindowManager;
import org.w3c.dom.Document;

import static java.util.stream.Collectors.toList;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 03-18-2019
 */
@ActionID(
		category = "EPPT",
		id = "gov.ca.water.eppt.nbui.actions.RunWreslScript"
)
@ActionRegistration(
		iconBase = "gov/ca/water/eppt/nbui/actions/run.png",
		displayName = "Run WRESL Script"
)
@ActionReferences(
		{
				@ActionReference(path = "Menu/Tools", position = 0000)
				,
				@ActionReference(path = "Toolbars/EPPT", position = 666)
		})
@NbBundle.Messages("CTL_RunWreslScript=Run WRESL Script")
public class RunWreslScript implements ActionListener
{
	private static final Logger LOGGER = Logger.getLogger(RunWreslScript.class.getName());
	private final IModelRunSvc _modelRunSvc = new ModelRunSvcImpl();

	@Override
	public void actionPerformed(ActionEvent e)
	{
		initReport();
	}

	private void initReport()
	{

	}


	void runReportWithOutputFile(Path outputFile, Path jasperSource)
			throws JRException, IOException
	{
		LOGGER.log(Level.INFO, "Generating report for: {0}", jasperSource);
		LOGGER.log(Level.INFO, "As PDF at location: {0}", outputFile);
		Path subreportDir = jasperSource.getParent().resolve("Subreports");
		if(subreportDir.toFile().isDirectory())
		{
			try(Stream<Path> paths = Files.walk(subreportDir, 1))
			{
				List<Path> subreports = paths.collect(toList());
				for(Path subreport : subreports)
				{
					if(subreport.toFile().isFile() && subreport.toString().endsWith("jrxml"))
					{
						JasperCompileManager.compileReportToFile(subreport.toString());
					}
				}
			}
		}

		// compiles jrxml
		Path dataDir = jasperSource.getParent().resolve("Datasource");
		Path imagesDir = jasperSource.getParent().resolve("Images");
		SimpleJasperReportsContext context = new SimpleJasperReportsContext();
		FileRepositoryService imageRepository = new FileRepositoryService(context,
				imagesDir.toString(), true);
		FileRepositoryService fileRepository = new FileRepositoryService(context, jasperSource.getParent().toString(),
				true);
		FileRepositoryService subreportRepository = new FileRepositoryService(context,
				subreportDir.toString(), true);
		FileRepositoryService dataRepository = new FileRepositoryService(context,
				dataDir.toString(), true);
		context.setExtensions(RepositoryService.class,
				Arrays.asList(fileRepository, imageRepository, subreportRepository, dataRepository));
		context.setExtensions(PersistenceServiceFactory.class,
				Collections.singletonList(FileRepositoryPersistenceServiceFactory.getInstance()));

		JasperFillManager manager = JasperFillManager.getInstance(context);

		Map<String, Object> parameters = new HashMap<>();
		JasperCompileManager.compileReportToFile(jasperSource.toString());
		JasperReport jasperReport = JasperCompileManager.compileReport(jasperSource.toString());
		JasperPrint jasperPrint = manager.fill(jasperReport, parameters);

		// fills compiled report with parameters and a connection
		JRPdfExporter exporter = new JRPdfExporter();
		exporter.setExporterInput(new SimpleExporterInput(jasperPrint));
		exporter.setExporterOutput(new SimpleOutputStreamExporterOutput(outputFile.toFile()));

		exporter.exportReport();
	}

	private void initWresl()
	{
		File selectedFile = chooseScript();
		if(selectedFile != null)
		{
			runScript(selectedFile.toPath());
		}
	}

	private void runScript(Path selectedFile)
	{
		try
		{
			ProgressFrame progressFrame = ProgressFrame.getProgressFrameInstance();
			progressFrame.addScenarioNamesAndAction(selectedFile, Constant.BATCH_RUN);
			progressFrame.setBtnText(Constant.STATUS_BTN_TEXT_STOP);
			progressFrame.setIconImage(WindowManager.getDefault().getMainWindow().getIconImage());
			progressFrame.makeDialogVisible();
			_modelRunSvc.doBatch(Collections.singletonList(selectedFile), false);
		}
		catch(RuntimeException ex)
		{
			LOGGER.log(Level.SEVERE, "Error running WRESL Script: " + selectedFile, ex);
		}
	}

	private File chooseScript()
	{
		JFileChooser fileChooser = new JFileChooser();
		fileChooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
		fileChooser.setCurrentDirectory(EpptPreferences.getScenariosPaths().toFile());
		fileChooser.showOpenDialog(WindowManager.getDefault().getMainWindow());
		fileChooser.setMultiSelectionEnabled(false);
		return fileChooser.getSelectedFile();
	}
}
