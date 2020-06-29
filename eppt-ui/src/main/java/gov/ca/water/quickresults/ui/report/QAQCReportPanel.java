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

package gov.ca.water.quickresults.ui.report;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Desktop;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.YearMonth;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.logging.LogRecord;
import java.util.logging.Logger;
import java.util.stream.Stream;
import javax.swing.*;
import javax.swing.border.TitledBorder;
import javax.swing.event.ChangeEvent;
import javax.swing.text.BadLocationException;
import javax.swing.text.Style;
import javax.swing.text.StyleConstants;
import javax.swing.text.StyledDocument;

import gov.ca.water.calgui.bo.CommonPeriodFilter;
import gov.ca.water.calgui.bo.SimpleFileFilter;
import gov.ca.water.calgui.bo.WaterYearDefinition;
import gov.ca.water.calgui.bo.WaterYearPeriod;
import gov.ca.water.calgui.bo.WaterYearPeriodRange;
import gov.ca.water.calgui.bo.WaterYearPeriodRangesFilter;
import gov.ca.water.calgui.bo.WaterYearType;
import gov.ca.water.calgui.constant.Constant;
import gov.ca.water.calgui.constant.EpptPreferences;
import gov.ca.water.calgui.project.EpptScenarioRun;
import gov.ca.water.calgui.scripts.DssCache;
import gov.ca.water.calgui.wresl.ProcessOutputConsumer;
import gov.ca.water.calgui.wresl.WreslScriptException;
import gov.ca.water.calgui.wresl.WreslScriptRunner;
import gov.ca.water.quickresults.ui.EpptPanel;
import gov.ca.water.quickresults.ui.TextAreaPrintStream;
import gov.ca.water.calgui.project.EpptConfigurationController;
import gov.ca.water.reportengine.EPPTReport;
import gov.ca.water.reportengine.EpptReportException;
import gov.ca.water.reportengine.QAQCReportException;
import gov.ca.water.reportengine.ReportParameters;
import gov.ca.water.reportengine.standardsummary.PercentDiffStyle;
import gov.ca.water.reportengine.standardsummary.StandardSummaryErrors;
import gov.ca.water.reportengine.standardsummary.StandardSummaryReader;
import gov.ca.water.reportengine.standardsummary.SummaryReportParameters;
import org.apache.commons.io.FileUtils;

import hec.heclib.dss.HecDSSFileAccess;
import rma.swing.RmaJDecimalField;

import static java.util.stream.Collectors.toList;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 05-13-2019
 */
public class QAQCReportPanel extends EpptPanel
{
	private static final Logger LOGGER = Logger.getLogger(QAQCReportPanel.class.getName());

	private final StyledDocument _doc;
	private final Style _style;
	private final AtomicInteger _processesRunning = new AtomicInteger(0);
	private final List<JCheckBox> _summaryModules;
	private final EpptConfigurationController _epptConfigurationController;
	private JPanel _panel1;
	private JButton _generateReportButton;
	private JTextPane _qaqcTextPane;
	private JTextField _toleranceTextField;
	private JTextField _reportTitle;
	private JTextField _reportSubtitle;
	private JTextField _pdfOutput;
	private JButton _fileChooserBtn;
	private JCheckBox _excutiveSummaryCheckBox;
	private JCheckBox _assumptionChangesCheckBox;
	private JCheckBox _codeChangesCheckBox;
	private JCheckBox _detailedIssuesCheckBox;
	private JCheckBox _standardSummaryStatiticsCheckBox;
	private JTextField _authorTextField;
	private JProgressBar _progressBar1;
	private JButton _runAltWreslButton;
	private JButton _runBaseWreslButton;
	private JTextField _altScenarioTextField;
	private JTextField _baseScenarioTextField;
	private JTabbedPane _tabbedPane1;
	private JTextPane _baseWreslTextPane;
	private JTextPane _altWreslTextPane;
	private JComboBox<PercentDiffStyle> _percentDiffStyle;
	private JPanel _summaryModulesPanel;
	private JButton _cancelButton;
	private JButton _overwriteJRXMLButton;
	private JButton _openReportButton;
	private JButton _overwriteScriptsButtonAlt;
	private JButton _overwriteScriptsButtonBase;
	private JLabel _baseWreslDiffWarningLabel;
	private JLabel _altWreslDiffWarningLabel;
	private EpptScenarioRun _baseRun;
	private EpptScenarioRun _altRun;
	private final Map<JCheckBox, String> _reportModules = new HashMap<>();
	private Future<?> _baseWreslFuture;
	private Future<?> _altWreslFuture;
	private Future<?> _qaqcReportFuture;
	private final ExecutorService _executor = Executors.newFixedThreadPool(5);
	private WaterYearDefinition _waterYearDefinition;

	public QAQCReportPanel(EpptConfigurationController epptConfigurationController)
	{
		_epptConfigurationController = epptConfigurationController;
		$$$setupUI$$$();
		_generateReportButton.addActionListener(e -> generateReport());
		_runBaseWreslButton.addActionListener(e -> runBaseWresl());
		_runAltWreslButton.addActionListener(e -> runAltWresl());
		setLayout(new BorderLayout());
		add($$$getRootComponent$$$(), BorderLayout.CENTER);
		_authorTextField.setText(EpptPreferences.getUsername());
		((RmaJDecimalField) _toleranceTextField).setValue(0.01);
		_fileChooserBtn.addActionListener(e -> chooseReportFile());
		_doc = (StyledDocument) _qaqcTextPane.getDocument();
		_style = _doc.addStyle("ConsoleStyle", null);
		Logger.getLogger("").addHandler(new ReportHandler());
		_epptConfigurationController.addScenarioChangedListener(this::fillScenarioRuns);
		Path currentProject = EpptPreferences.getLastProjectConfiguration().getParent();
		Path reportPath = currentProject.resolve("Reports").resolve(_epptConfigurationController.getProjectName() + ".pdf");
		_pdfOutput.setText(reportPath.toString());
		_openReportButton.setEnabled(reportPath.toFile().exists());
		_tabbedPane1.setTitleAt(0, reportPath.getFileName().toString() + " QA/QC");
		Arrays.asList(PercentDiffStyle.values()).forEach(_percentDiffStyle::addItem);
		_reportModules.put(_excutiveSummaryCheckBox, EPPTReport.EXECUTIVE_SUMMARY);
		_reportModules.put(_assumptionChangesCheckBox, EPPTReport.ASSUMPTION_CHANGES);
		_reportModules.put(_codeChangesCheckBox, EPPTReport.CODE_CHANGES);
		_reportModules.put(_detailedIssuesCheckBox, EPPTReport.DETAILED_ISSUES);
		_reportModules.put(_standardSummaryStatiticsCheckBox, EPPTReport.STANDARD_SUMMARY_STATISTICS);
		_summaryModules = buildStandardSummaryModules();
		_summaryModulesPanel.setLayout(new BoxLayout(_summaryModulesPanel, BoxLayout.Y_AXIS));
		_summaryModules.forEach(_summaryModulesPanel::add);
		_tabbedPane1.addChangeListener(this::tabChanged);
		_cancelButton.addActionListener(this::cancelRunningTask);
		_standardSummaryStatiticsCheckBox.addActionListener(e -> {
			if(!_standardSummaryStatiticsCheckBox.isSelected())
			{
				_summaryModules.forEach(c -> c.setSelected(false));
			}
			_summaryModules.forEach(c -> c.setEnabled(_standardSummaryStatiticsCheckBox.isSelected()));
		});
		_overwriteJRXMLButton.addActionListener(e -> checkForceCopyJrXml());
		_openReportButton.addActionListener(this::openPdf);
		_overwriteScriptsButtonBase.addActionListener(e -> QaQcFileUtils.createWreslMain(_baseRun, true));
		_overwriteScriptsButtonAlt.addActionListener(e -> QaQcFileUtils.createWreslMain(_altRun, true));
		_baseWreslDiffWarningLabel.setVisible(false);
		_altWreslDiffWarningLabel.setVisible(false);
		Optional<EpptScenarioRun> epptScenarioBase = _epptConfigurationController.getEpptScenarioBase();
		if(epptScenarioBase.isPresent())
		{
			fillScenarioRuns(epptScenarioBase.get(), _epptConfigurationController.getEpptScenarioAlternatives());
		}
	}

	private void tabChanged(ChangeEvent e)
	{
		int selectedIndex = _tabbedPane1.getSelectedIndex();
		boolean taskRunning = false;
		switch(selectedIndex)
		{
			case 0:
				if(_qaqcReportFuture != null)
				{
					taskRunning = !_qaqcReportFuture.isDone();
				}
				break;
			case 1:
				if(_baseWreslFuture != null)
				{
					taskRunning = !_baseWreslFuture.isDone();
				}
				break;
			case 2:
				if(_altWreslFuture != null)
				{
					taskRunning = !_altWreslFuture.isDone();
				}
				break;
		}
		_cancelButton.setEnabled(taskRunning);
	}

	private void cancelRunningTask(ActionEvent e)
	{
		int selectedIndex = _tabbedPane1.getSelectedIndex();
		switch(selectedIndex)
		{
			case 0:
				cancelQaQcTask();
				break;
			case 1:
				cancelBaseWresl();
				break;
			case 2:
				cancelAltWresl();
				break;
		}
	}

	private void cancelAltWresl()
	{
		if(_altWreslFuture != null)
		{
			_altWreslFuture.cancel(true);
		}
	}

	private void cancelBaseWresl()
	{
		if(_baseWreslFuture != null)
		{
			_baseWreslFuture.cancel(true);
		}
	}

	private void cancelQaQcTask()
	{
		if(_qaqcReportFuture != null)
		{
			_qaqcReportFuture.cancel(true);
		}
	}

	private void runAltWresl()
	{
		if(checkIfDssFileIsOpen(_altRun))
		{
			_altWreslFuture = _executor.submit(() -> {
				try
				{
					_tabbedPane1.setSelectedIndex(2);
					startProcessAsync(_runAltWreslButton);
					runWresl(_altRun, _altWreslTextPane);
				}
				finally
				{
					SwingUtilities.invokeLater(() -> {
						if(_tabbedPane1.getSelectedIndex() == 2)
						{
							_cancelButton.setEnabled(false);
						}
					});
					stopProcessAsync(_runAltWreslButton);
				}
			});
		}
	}

	private void runBaseWresl()
	{
		if(checkIfDssFileIsOpen(_baseRun))
		{
			_baseWreslFuture = _executor.submit(() -> {
				try
				{
					_tabbedPane1.setSelectedIndex(1);
					startProcessAsync(_runBaseWreslButton);
					runWresl(_baseRun, _baseWreslTextPane);
				}
				finally
				{
					SwingUtilities.invokeLater(() -> {
						if(_tabbedPane1.getSelectedIndex() == 1)
						{
							_cancelButton.setEnabled(false);
						}
					});
					stopProcessAsync(_runBaseWreslButton);
				}
			});
		}
	}

	private boolean checkIfDssFileIsOpen(EpptScenarioRun epptScenarioRun)
	{
		boolean retval = false;
		if(epptScenarioRun != null)
		{
			Path postProcessDss = epptScenarioRun.getPostProcessDss();
			if(postProcessDss != null)
			{
				HecDSSFileAccess hecDSSFileAccess = new HecDSSFileAccess(postProcessDss.toString());
				if(!hecDSSFileAccess.writeAccess())
				{
					JOptionPane.showMessageDialog(this, "DSS File inaccessible. Ensure it is not being written to in another process:\n" + postProcessDss, "DSS File Error",
							JOptionPane.WARNING_MESSAGE);
				}
				else
				{
					hecDSSFileAccess.close();
					retval = true;
				}
			}
		}
		return retval;
	}

	private void runWresl(EpptScenarioRun scenarioRun, JTextPane textPane)
	{
		try
		{
			textPane.setText("");
			int startYear = _epptConfigurationController.getStartYear();
			int endYear = _epptConfigurationController.getEndYear();
			WaterYearPeriod wreslPeriod = new WaterYearPeriod("WRESL Period");
			WaterYearPeriodRange waterYearPeriodRange = new WaterYearPeriodRange(wreslPeriod, new WaterYearType(startYear, wreslPeriod), new WaterYearType(endYear, wreslPeriod));
			YearMonth startYearMonth = waterYearPeriodRange.getStart(_epptConfigurationController.getWaterYearDefinition());
			YearMonth endYearMonth = waterYearPeriodRange.getEnd(_epptConfigurationController.getWaterYearDefinition());
			LocalDate start = LocalDate.of(startYearMonth.getYear(), startYearMonth.getMonth(), 1);
			start = start.withDayOfMonth(start.lengthOfMonth());
			LocalDate end = LocalDate.of(endYearMonth.getYear(), endYearMonth.getMonth(), 1);
			end = end.withDayOfMonth(end.lengthOfMonth());
			Path wreslMainFile = QaQcFileUtils.createWreslMain(scenarioRun, false);
			WreslScriptRunner wreslScriptRunner = new WreslScriptRunner(scenarioRun, wreslMainFile, new WreslProcessConsumer(textPane));
			wreslScriptRunner.run(start, end);
		}
		catch(WreslScriptException | RuntimeException e1)
		{
			LOGGER.log(Level.SEVERE, "Error in WRESL Script Run", e1);
		}
	}

	private void stopProcessAsync(JButton button)
	{
		SwingUtilities.invokeLater(() -> {
			button.setEnabled(true);
			stopProgress();
		});
	}

	private void startProcessAsync(JButton button)
	{
		SwingUtilities.invokeLater(() -> {
			_cancelButton.setEnabled(true);
			button.setEnabled(false);
			startProgress();
		});
	}

	private void chooseReportFile()
	{
		JFileChooser jFileChooser = new JFileChooser();
		jFileChooser.setCurrentDirectory(new File(_pdfOutput.getText()));
		jFileChooser.setFileFilter(new SimpleFileFilter("PDF"));
		jFileChooser.setDialogTitle("Save Report to PDF");
		jFileChooser.showSaveDialog(this);
		File selectedFile = jFileChooser.getSelectedFile();
		if(selectedFile != null)
		{
			String filePath = selectedFile.toString();
			if(!filePath.toLowerCase().endsWith("pdf"))
			{
				filePath += ".pdf";
			}
			_pdfOutput.setText(filePath);
			_openReportButton.setEnabled(Paths.get(filePath).toFile().exists());
			_tabbedPane1.setTitleAt(0, Paths.get(filePath).getFileName().toString() + " QA/QC");
		}
	}

	private void generateReport()
	{
		_qaqcTextPane.setText("");
		if(_baseRun != null)
		{
			Path reportPath = Paths.get(_pdfOutput.getText());
			boolean exists = reportPath.toFile().exists();
			if(exists)
			{
				int warning = JOptionPane.showConfirmDialog(this, "PDF: " + _pdfOutput.getText() + " already exists. Do you wish to overwrite?", "Warning", JOptionPane.YES_NO_OPTION);
				if(warning == JOptionPane.YES_OPTION)
				{
					try(FileOutputStream f = new FileOutputStream(reportPath.toString()))
					{
						_qaqcReportFuture = _executor.submit(this::generateQAQCReport);
					}
					catch(IOException e)
					{
						LOGGER.log(Level.WARNING, "Unable to delete existing report", e);
						JOptionPane.showMessageDialog(this, "Error while creating the pdf file: " + reportPath.getFileName() + "\nIf the file is already open, please close it and try again.\n" + e.getMessage(),
								"Error", JOptionPane.WARNING_MESSAGE);
					}
				}
			}
			else
			{
				_qaqcReportFuture = _executor.submit(this::generateQAQCReport);
			}
		}
		else
		{
			JOptionPane.showMessageDialog(this, "No Base Scenario Run selected.");
		}
	}

	private void checkForceCopyJrXml()
	{
		Path lastProjectConfiguration = EpptPreferences.getLastProjectConfiguration();
		Path jrXmlPath = lastProjectConfiguration.getParent().resolve("Reports").resolve("QAQC_Report.jrxml");
		int replaceReponse = JOptionPane.showConfirmDialog(this, "Jasper report files exist: " + jrXmlPath + "\n Do you wish to overwrite?", "Warning", JOptionPane.YES_NO_OPTION);
		if(replaceReponse == JOptionPane.YES_OPTION)
		{
			try
			{
				QaQcFileUtils.copyJasperPaths();
			}
			catch(IOException e)
			{
				LOGGER.log(Level.SEVERE, "Error copying JRXML files", e);
			}
		}
	}

	private void generateQAQCReport()
	{
		Path pathToWriteOut = Paths.get(_pdfOutput.getText());
		try
		{
			_tabbedPane1.setSelectedIndex(0);
			_qaqcTextPane.setText("");
			startProcessAsync(_generateReportButton);
			QAQCReportGenerator qaqcReportGenerator = new QAQCReportGenerator(new QaQcProcessConsumer());
			double tolerance = Double.parseDouble(_toleranceTextField.getText());
			String author = _authorTextField.getText();
			EpptPreferences.setUsername(author);
			String title = _reportTitle.getText();
			String subtitle = _reportSubtitle.getText();
			List<Map<EpptScenarioRun, WaterYearPeriodRangesFilter>> waterYearPeriodRanges = _epptConfigurationController.getWaterYearPeriodRanges();
			PercentDiffStyle percentDiffStyle = (PercentDiffStyle) _percentDiffStyle.getSelectedItem();
			List<String> disabledSummaryModules = getDisabledSummaryModules();
			LocalDateTime start = LocalDateTime.of(_epptConfigurationController.getStartYear(), _epptConfigurationController.getWaterYearDefinition().getStartMonth(), 1, 0, 0)
											   .minusDays(2);
			LocalDateTime end = LocalDateTime.of(_epptConfigurationController.getEndYear(), _epptConfigurationController.getWaterYearDefinition().getEndMonth(), 1, 0, 0)
											 .plusMonths(1)
											 .plusDays(2);
			CommonPeriodFilter commonPeriodFilter = new CommonPeriodFilter(start, end);
			SummaryReportParameters summaryReportParameters = new SummaryReportParameters(_waterYearDefinition, waterYearPeriodRanges, percentDiffStyle, disabledSummaryModules, commonPeriodFilter, new DssCache());
			List<String> disabledReportModules = getDisabledReportModules();
			ReportParameters reportParameters = new ReportParameters(tolerance, author, title, subtitle, summaryReportParameters, disabledReportModules, true, true);
			qaqcReportGenerator.generateQAQCReport(_baseRun, _altRun, reportParameters, pathToWriteOut);
		}
		catch(QAQCReportException | RuntimeException | EpptReportException e)
		{
			if(e.getCause() instanceof InterruptedException)
			{
				String msg = "Report generation interrupted";
				LOGGER.log(Level.WARNING, msg, e);
				appendErrorText(msg);
			}
			else
			{
				LOGGER.log(Level.WARNING, "Unable to generate Report PDF", e);
				appendErrorText("Error: " + e);
			}
		}
		finally
		{
			SwingUtilities.invokeLater(() -> {
				if(_tabbedPane1.getSelectedIndex() == 0)
				{
					_cancelButton.setEnabled(false);
				}
				_openReportButton.setEnabled(pathToWriteOut.toFile().exists());
			});
			stopProcessAsync(_generateReportButton);
		}
	}

	private List<String> getDisabledReportModules()
	{
		return _reportModules.entrySet().stream().filter(k -> !k.getKey().isSelected()).map(Map.Entry::getValue).collect(toList());
	}

	private List<String> getDisabledSummaryModules()
	{
		return _summaryModules.stream().filter(jCheckBox -> !jCheckBox.isSelected()).map(JCheckBox::getText).collect(toList());
	}

	private List<JCheckBox> buildStandardSummaryModules()
	{
		StandardSummaryReader reader = new StandardSummaryReader(Paths.get(EPPTReport.SUMMARY_CSV), new StandardSummaryErrors(), new DssCache());
		try
		{
			return reader.getModules().stream().map(text -> new JCheckBox(text, true)).collect(toList());
		}
		catch(EpptReportException e)
		{
			LOGGER.log(Level.SEVERE, "Error reading summary modules", e);
			return new ArrayList<>();
		}
	}

	private void startProgress()
	{
		_processesRunning.incrementAndGet();
		_progressBar1.setVisible(true);
		_progressBar1.setIndeterminate(true);
		repaint();
		revalidate();
	}

	private void stopProgress()
	{
		if(0 == _processesRunning.decrementAndGet())
		{
			_progressBar1.setVisible(false);
			_progressBar1.setIndeterminate(false);
			repaint();
			revalidate();
		}
	}

	private void createUIComponents()
	{
		_toleranceTextField = new RmaJDecimalField();
	}

	/**
	 * Method generated by IntelliJ IDEA GUI Designer
	 * >>> IMPORTANT!! <<<
	 * DO NOT edit this method OR call it in your code!
	 *
	 * @noinspection ALL
	 */
	private void $$$setupUI$$$()
	{
		createUIComponents();
		_panel1 = new JPanel();
		_panel1.setLayout(new BorderLayout(5, 2));
		_panel1.setMinimumSize(new Dimension(1150, 350));
		_panel1.setPreferredSize(new Dimension(1150, 760));
		_panel1.setRequestFocusEnabled(false);
		final JPanel panel1 = new JPanel();
		panel1.setLayout(new BorderLayout(0, 0));
		_panel1.add(panel1, BorderLayout.SOUTH);
		final JPanel panel2 = new JPanel();
		panel2.setLayout(new BorderLayout(0, 0));
		panel1.add(panel2, BorderLayout.CENTER);
		final JPanel panel3 = new JPanel();
		panel3.setLayout(new FlowLayout(FlowLayout.LEFT, 5, 5));
		panel2.add(panel3, BorderLayout.WEST);
		final JPanel panel4 = new JPanel();
		panel4.setLayout(new FlowLayout(FlowLayout.RIGHT, 5, 5));
		panel2.add(panel4, BorderLayout.CENTER);
		_cancelButton = new JButton();
		_cancelButton.setEnabled(false);
		_cancelButton.setPreferredSize(new Dimension(147, 30));
		_cancelButton.setText("Cancel");
		_cancelButton.setToolTipText("Cancel Running Task");
		panel4.add(_cancelButton);
		final JPanel panel5 = new JPanel();
		panel5.setLayout(new BorderLayout(0, 0));
		panel1.add(panel5, BorderLayout.NORTH);
		_progressBar1 = new JProgressBar();
		_progressBar1.setVisible(false);
		panel5.add(_progressBar1, BorderLayout.CENTER);
		final JPanel panel6 = new JPanel();
		panel6.setLayout(new BorderLayout(0, 0));
		_panel1.add(panel6, BorderLayout.CENTER);
		_tabbedPane1 = new JTabbedPane();
		panel6.add(_tabbedPane1, BorderLayout.CENTER);
		final JPanel panel7 = new JPanel();
		panel7.setLayout(new BorderLayout(0, 0));
		_tabbedPane1.addTab("QA/QC", panel7);
		final JPanel panel8 = new JPanel();
		panel8.setLayout(new GridBagLayout());
		panel7.add(panel8, BorderLayout.SOUTH);
		final JLabel label1 = new JLabel();
		label1.setText("Output File:");
		GridBagConstraints gbc;
		gbc = new GridBagConstraints();
		gbc.gridx = 0;
		gbc.gridy = 0;
		gbc.anchor = GridBagConstraints.WEST;
		gbc.insets = new Insets(5, 5, 5, 5);
		panel8.add(label1, gbc);
		final JPanel spacer1 = new JPanel();
		gbc = new GridBagConstraints();
		gbc.gridx = 1;
		gbc.gridy = 0;
		gbc.fill = GridBagConstraints.HORIZONTAL;
		panel8.add(spacer1, gbc);
		final JPanel spacer2 = new JPanel();
		gbc = new GridBagConstraints();
		gbc.gridx = 0;
		gbc.gridy = 1;
		gbc.fill = GridBagConstraints.VERTICAL;
		panel8.add(spacer2, gbc);
		final JPanel panel9 = new JPanel();
		panel9.setLayout(new BorderLayout(0, 0));
		gbc = new GridBagConstraints();
		gbc.gridx = 2;
		gbc.gridy = 0;
		gbc.weightx = 0.8;
		gbc.fill = GridBagConstraints.HORIZONTAL;
		panel8.add(panel9, gbc);
		_fileChooserBtn = new JButton();
		_fileChooserBtn.setPreferredSize(new Dimension(30, 26));
		_fileChooserBtn.setText("...");
		panel9.add(_fileChooserBtn, BorderLayout.EAST);
		_pdfOutput = new JTextField();
		_pdfOutput.setEditable(false);
		_pdfOutput.setPreferredSize(new Dimension(200, 26));
		_pdfOutput.setText("");
		panel9.add(_pdfOutput, BorderLayout.CENTER);
		_openReportButton = new JButton();
		_openReportButton.setMaximumSize(new Dimension(120, 26));
		_openReportButton.setMinimumSize(new Dimension(120, 26));
		_openReportButton.setPreferredSize(new Dimension(146, 30));
		_openReportButton.setText("Open Report");
		_openReportButton.setToolTipText("Open Generated Report");
		gbc = new GridBagConstraints();
		gbc.gridx = 3;
		gbc.gridy = 0;
		gbc.fill = GridBagConstraints.BOTH;
		panel8.add(_openReportButton, gbc);
		_generateReportButton = new JButton();
		_generateReportButton.setPreferredSize(new Dimension(147, 30));
		_generateReportButton.setText("Generate Report");
		_generateReportButton.setToolTipText("Generate QA/QC Report");
		gbc = new GridBagConstraints();
		gbc.gridx = 3;
		gbc.gridy = 2;
		gbc.fill = GridBagConstraints.BOTH;
		panel8.add(_generateReportButton, gbc);
		final JScrollPane scrollPane1 = new JScrollPane();
		scrollPane1.setHorizontalScrollBarPolicy(30);
		panel7.add(scrollPane1, BorderLayout.CENTER);
		_qaqcTextPane = new JTextPane();
		_qaqcTextPane.setEditable(false);
		_qaqcTextPane.setPreferredSize(new Dimension(8, 1382));
		_qaqcTextPane.setText("");
		scrollPane1.setViewportView(_qaqcTextPane);
		final JScrollPane scrollPane2 = new JScrollPane();
		_tabbedPane1.addTab("Base", scrollPane2);
		_baseWreslTextPane = new JTextPane();
		_baseWreslTextPane.setEditable(false);
		_baseWreslTextPane.setEnabled(true);
		scrollPane2.setViewportView(_baseWreslTextPane);
		final JScrollPane scrollPane3 = new JScrollPane();
		_tabbedPane1.addTab("Alternative", scrollPane3);
		_altWreslTextPane = new JTextPane();
		_altWreslTextPane.setEditable(false);
		_altWreslTextPane.setEnabled(true);
		scrollPane3.setViewportView(_altWreslTextPane);
		final JPanel panel10 = new JPanel();
		panel10.setLayout(new BorderLayout(0, 0));
		_panel1.add(panel10, BorderLayout.WEST);
		final JPanel panel11 = new JPanel();
		panel11.setLayout(new BorderLayout(0, 0));
		panel11.setPreferredSize(new Dimension(400, 200));
		panel10.add(panel11, BorderLayout.CENTER);
		panel11.setBorder(BorderFactory.createTitledBorder(BorderFactory.createEtchedBorder(), "QA/QC Report Sections", TitledBorder.DEFAULT_JUSTIFICATION,
				TitledBorder.DEFAULT_POSITION, null, null));
		final JPanel panel12 = new JPanel();
		panel12.setLayout(new BorderLayout(0, 0));
		panel12.setPreferredSize(new Dimension(300, 270));
		panel11.add(panel12, BorderLayout.CENTER);
		final JPanel panel13 = new JPanel();
		panel13.setLayout(new GridBagLayout());
		panel12.add(panel13, BorderLayout.NORTH);
		_excutiveSummaryCheckBox = new JCheckBox();
		_excutiveSummaryCheckBox.setEnabled(true);
		_excutiveSummaryCheckBox.setHorizontalAlignment(2);
		_excutiveSummaryCheckBox.setSelected(true);
		_excutiveSummaryCheckBox.setText("Executive Summary");
		gbc = new GridBagConstraints();
		gbc.gridx = 0;
		gbc.gridy = 0;
		gbc.weightx = 0.5;
		gbc.anchor = GridBagConstraints.NORTHWEST;
		gbc.insets = new Insets(0, 5, 0, 5);
		panel13.add(_excutiveSummaryCheckBox, gbc);
		_assumptionChangesCheckBox = new JCheckBox();
		_assumptionChangesCheckBox.setEnabled(true);
		_assumptionChangesCheckBox.setHorizontalAlignment(2);
		_assumptionChangesCheckBox.setText("Assumption Changes");
		gbc = new GridBagConstraints();
		gbc.gridx = 0;
		gbc.gridy = 1;
		gbc.anchor = GridBagConstraints.NORTHWEST;
		gbc.insets = new Insets(0, 5, 0, 5);
		panel13.add(_assumptionChangesCheckBox, gbc);
		_codeChangesCheckBox = new JCheckBox();
		_codeChangesCheckBox.setEnabled(true);
		_codeChangesCheckBox.setHorizontalAlignment(2);
		_codeChangesCheckBox.setText("Code Changes");
		gbc = new GridBagConstraints();
		gbc.gridx = 0;
		gbc.gridy = 2;
		gbc.anchor = GridBagConstraints.NORTHWEST;
		gbc.insets = new Insets(0, 5, 0, 5);
		panel13.add(_codeChangesCheckBox, gbc);
		_detailedIssuesCheckBox = new JCheckBox();
		_detailedIssuesCheckBox.setEnabled(true);
		_detailedIssuesCheckBox.setHorizontalAlignment(2);
		_detailedIssuesCheckBox.setSelected(true);
		_detailedIssuesCheckBox.setText("Detailed Issues");
		gbc = new GridBagConstraints();
		gbc.gridx = 0;
		gbc.gridy = 3;
		gbc.anchor = GridBagConstraints.NORTHWEST;
		gbc.insets = new Insets(0, 5, 0, 5);
		panel13.add(_detailedIssuesCheckBox, gbc);
		_standardSummaryStatiticsCheckBox = new JCheckBox();
		_standardSummaryStatiticsCheckBox.setEnabled(true);
		_standardSummaryStatiticsCheckBox.setHorizontalAlignment(2);
		_standardSummaryStatiticsCheckBox.setSelected(true);
		_standardSummaryStatiticsCheckBox.setText("Standard Summary Statistics");
		gbc = new GridBagConstraints();
		gbc.gridx = 0;
		gbc.gridy = 4;
		gbc.anchor = GridBagConstraints.NORTHWEST;
		gbc.insets = new Insets(0, 5, 0, 5);
		panel13.add(_standardSummaryStatiticsCheckBox, gbc);
		_summaryModulesPanel = new JPanel();
		_summaryModulesPanel.setLayout(new FlowLayout(FlowLayout.LEFT, 5, 5));
		gbc = new GridBagConstraints();
		gbc.gridx = 0;
		gbc.gridy = 5;
		gbc.fill = GridBagConstraints.BOTH;
		gbc.insets = new Insets(5, 25, 5, 5);
		panel13.add(_summaryModulesPanel, gbc);
		final JPanel panel14 = new JPanel();
		panel14.setLayout(new FlowLayout(FlowLayout.RIGHT, 5, 5));
		panel11.add(panel14, BorderLayout.SOUTH);
		_overwriteJRXMLButton = new JButton();
		_overwriteJRXMLButton.setText("Update Report Templates");
		_overwriteJRXMLButton.setToolTipText("Update Report Templates in project directory with templates in the installer directory");
		panel14.add(_overwriteJRXMLButton);
		final JPanel panel15 = new JPanel();
		panel15.setLayout(new BorderLayout(0, 0));
		panel10.add(panel15, BorderLayout.NORTH);
		final JPanel panel16 = new JPanel();
		panel16.setLayout(new GridBagLayout());
		panel15.add(panel16, BorderLayout.SOUTH);
		final JLabel label2 = new JLabel();
		label2.setText("Author:");
		gbc = new GridBagConstraints();
		gbc.gridx = 0;
		gbc.gridy = 0;
		gbc.weightx = 0.1;
		gbc.fill = GridBagConstraints.HORIZONTAL;
		gbc.insets = new Insets(5, 5, 5, 5);
		panel16.add(label2, gbc);
		_authorTextField = new JTextField();
		gbc = new GridBagConstraints();
		gbc.gridx = 1;
		gbc.gridy = 0;
		gbc.weightx = 0.8;
		gbc.anchor = GridBagConstraints.WEST;
		gbc.fill = GridBagConstraints.HORIZONTAL;
		gbc.insets = new Insets(5, 5, 5, 5);
		panel16.add(_authorTextField, gbc);
		final JLabel label3 = new JLabel();
		label3.setText("QA/QC Report Title:");
		gbc = new GridBagConstraints();
		gbc.gridx = 0;
		gbc.gridy = 1;
		gbc.fill = GridBagConstraints.HORIZONTAL;
		gbc.insets = new Insets(5, 5, 5, 5);
		panel16.add(label3, gbc);
		_reportTitle = new JTextField();
		_reportTitle.setText("EPPT QA/QC Report");
		gbc = new GridBagConstraints();
		gbc.gridx = 1;
		gbc.gridy = 1;
		gbc.weightx = 1.0;
		gbc.anchor = GridBagConstraints.WEST;
		gbc.fill = GridBagConstraints.HORIZONTAL;
		gbc.insets = new Insets(5, 5, 5, 5);
		panel16.add(_reportTitle, gbc);
		final JLabel label4 = new JLabel();
		label4.setText("QA/QC Report Subtitle:");
		gbc = new GridBagConstraints();
		gbc.gridx = 0;
		gbc.gridy = 2;
		gbc.fill = GridBagConstraints.HORIZONTAL;
		gbc.insets = new Insets(5, 5, 5, 5);
		panel16.add(label4, gbc);
		_reportSubtitle = new JTextField();
		_reportSubtitle.setText("");
		gbc = new GridBagConstraints();
		gbc.gridx = 1;
		gbc.gridy = 2;
		gbc.weightx = 1.0;
		gbc.anchor = GridBagConstraints.WEST;
		gbc.fill = GridBagConstraints.HORIZONTAL;
		gbc.insets = new Insets(5, 5, 5, 5);
		panel16.add(_reportSubtitle, gbc);
		final JLabel label5 = new JLabel();
		label5.setText("DSS Compare Tolerance:");
		gbc = new GridBagConstraints();
		gbc.gridx = 0;
		gbc.gridy = 3;
		gbc.fill = GridBagConstraints.HORIZONTAL;
		gbc.insets = new Insets(5, 5, 5, 5);
		panel16.add(label5, gbc);
		_toleranceTextField.setText("");
		gbc = new GridBagConstraints();
		gbc.gridx = 1;
		gbc.gridy = 3;
		gbc.anchor = GridBagConstraints.WEST;
		gbc.fill = GridBagConstraints.HORIZONTAL;
		gbc.insets = new Insets(5, 5, 5, 5);
		panel16.add(_toleranceTextField, gbc);
		final JLabel label6 = new JLabel();
		label6.setText("Percent Diff Format:");
		gbc = new GridBagConstraints();
		gbc.gridx = 0;
		gbc.gridy = 4;
		gbc.fill = GridBagConstraints.HORIZONTAL;
		gbc.insets = new Insets(5, 5, 5, 5);
		panel16.add(label6, gbc);
		_percentDiffStyle = new JComboBox();
		_percentDiffStyle.setPreferredSize(new Dimension(91, 26));
		gbc = new GridBagConstraints();
		gbc.gridx = 1;
		gbc.gridy = 4;
		gbc.anchor = GridBagConstraints.WEST;
		gbc.fill = GridBagConstraints.HORIZONTAL;
		gbc.insets = new Insets(5, 5, 5, 5);
		panel16.add(_percentDiffStyle, gbc);
		final JPanel panel17 = new JPanel();
		panel17.setLayout(new GridBagLayout());
		panel15.add(panel17, BorderLayout.CENTER);
		panel17.setBorder(BorderFactory.createTitledBorder(null, "Scenario Runs", TitledBorder.DEFAULT_JUSTIFICATION, TitledBorder.DEFAULT_POSITION, null, null));
		final JLabel label7 = new JLabel();
		label7.setText("Base:");
		gbc = new GridBagConstraints();
		gbc.gridx = 0;
		gbc.gridy = 0;
		gbc.anchor = GridBagConstraints.NORTH;
		gbc.fill = GridBagConstraints.HORIZONTAL;
		gbc.insets = new Insets(10, 5, 5, 5);
		panel17.add(label7, gbc);
		final JPanel spacer3 = new JPanel();
		gbc = new GridBagConstraints();
		gbc.gridx = 1;
		gbc.gridy = 0;
		gbc.fill = GridBagConstraints.HORIZONTAL;
		panel17.add(spacer3, gbc);
		final JPanel spacer4 = new JPanel();
		gbc = new GridBagConstraints();
		gbc.gridx = 0;
		gbc.gridy = 1;
		gbc.fill = GridBagConstraints.VERTICAL;
		panel17.add(spacer4, gbc);
		_baseScenarioTextField = new JTextField();
		_baseScenarioTextField.setEditable(false);
		_baseScenarioTextField.setOpaque(true);
		_baseScenarioTextField.setPreferredSize(new Dimension(251, 26));
		_baseScenarioTextField.setText("Base");
		gbc = new GridBagConstraints();
		gbc.gridx = 2;
		gbc.gridy = 0;
		gbc.weightx = 0.8;
		gbc.fill = GridBagConstraints.HORIZONTAL;
		gbc.insets = new Insets(5, 5, 5, 5);
		panel17.add(_baseScenarioTextField, gbc);
		_runBaseWreslButton = new JButton();
		_runBaseWreslButton.setEnabled(false);
		_runBaseWreslButton.setMaximumSize(new Dimension(120, 26));
		_runBaseWreslButton.setMinimumSize(new Dimension(120, 26));
		_runBaseWreslButton.setPreferredSize(new Dimension(90, 26));
		_runBaseWreslButton.setText("Run WRESL");
		_runBaseWreslButton.setToolTipText("Run EPPT WRESL Script");
		gbc = new GridBagConstraints();
		gbc.gridx = 3;
		gbc.gridy = 0;
		gbc.anchor = GridBagConstraints.EAST;
		gbc.insets = new Insets(2, 2, 2, 2);
		panel17.add(_runBaseWreslButton, gbc);
		_baseWreslDiffWarningLabel = new JLabel();
		_baseWreslDiffWarningLabel.setMaximumSize(new Dimension(251, 16));
		_baseWreslDiffWarningLabel.setMinimumSize(new Dimension(251, 16));
		_baseWreslDiffWarningLabel.setPreferredSize(new Dimension(251, 16));
		_baseWreslDiffWarningLabel.setText("*WRESL Script Directory differs from installation");
		gbc = new GridBagConstraints();
		gbc.gridx = 2;
		gbc.gridy = 1;
		gbc.weightx = 0.8;
		gbc.fill = GridBagConstraints.HORIZONTAL;
		gbc.insets = new Insets(5, 5, 5, 5);
		panel17.add(_baseWreslDiffWarningLabel, gbc);
		final JLabel label8 = new JLabel();
		label8.setText("Alternative:");
		gbc = new GridBagConstraints();
		gbc.gridx = 0;
		gbc.gridy = 2;
		gbc.anchor = GridBagConstraints.NORTH;
		gbc.fill = GridBagConstraints.HORIZONTAL;
		gbc.insets = new Insets(10, 5, 5, 5);
		panel17.add(label8, gbc);
		_overwriteScriptsButtonBase = new JButton();
		_overwriteScriptsButtonBase.setMaximumSize(new Dimension(120, 26));
		_overwriteScriptsButtonBase.setMinimumSize(new Dimension(120, 26));
		_overwriteScriptsButtonBase.setPreferredSize(new Dimension(120, 26));
		_overwriteScriptsButtonBase.setText("Overwrite Scripts");
		_overwriteScriptsButtonBase.setToolTipText("Overwrite Scripts in project directory with scripts from the installer directory");
		gbc = new GridBagConstraints();
		gbc.gridx = 4;
		gbc.gridy = 0;
		gbc.weightx = 0.2;
		gbc.anchor = GridBagConstraints.EAST;
		gbc.insets = new Insets(2, 2, 2, 2);
		panel17.add(_overwriteScriptsButtonBase, gbc);
		_runAltWreslButton = new JButton();
		_runAltWreslButton.setEnabled(false);
		_runAltWreslButton.setMaximumSize(new Dimension(120, 26));
		_runAltWreslButton.setMinimumSize(new Dimension(120, 26));
		_runAltWreslButton.setPreferredSize(new Dimension(90, 26));
		_runAltWreslButton.setText("Run WRESL");
		_runAltWreslButton.setToolTipText("Run EPPT WRESL Script");
		gbc = new GridBagConstraints();
		gbc.gridx = 3;
		gbc.gridy = 2;
		gbc.weightx = 0.2;
		gbc.anchor = GridBagConstraints.EAST;
		gbc.insets = new Insets(2, 2, 2, 2);
		panel17.add(_runAltWreslButton, gbc);
		_overwriteScriptsButtonAlt = new JButton();
		_overwriteScriptsButtonAlt.setMaximumSize(new Dimension(120, 26));
		_overwriteScriptsButtonAlt.setMinimumSize(new Dimension(120, 26));
		_overwriteScriptsButtonAlt.setPreferredSize(new Dimension(120, 26));
		_overwriteScriptsButtonAlt.setText("Overwrite Scripts");
		_overwriteScriptsButtonAlt.setToolTipText("Overwrite Scripts in project directory with scripts from the installer directory");
		gbc = new GridBagConstraints();
		gbc.gridx = 4;
		gbc.gridy = 2;
		gbc.weightx = 0.2;
		gbc.anchor = GridBagConstraints.EAST;
		gbc.insets = new Insets(2, 2, 2, 2);
		panel17.add(_overwriteScriptsButtonAlt, gbc);
		_altScenarioTextField = new JTextField();
		_altScenarioTextField.setEditable(false);
		_altScenarioTextField.setPreferredSize(new Dimension(251, 26));
		_altScenarioTextField.setText("Alternative");
		gbc = new GridBagConstraints();
		gbc.gridx = 2;
		gbc.gridy = 2;
		gbc.weightx = 0.8;
		gbc.fill = GridBagConstraints.HORIZONTAL;
		gbc.insets = new Insets(5, 5, 5, 5);
		panel17.add(_altScenarioTextField, gbc);
		_altWreslDiffWarningLabel = new JLabel();
		_altWreslDiffWarningLabel.setMaximumSize(new Dimension(251, 16));
		_altWreslDiffWarningLabel.setMinimumSize(new Dimension(251, 16));
		_altWreslDiffWarningLabel.setPreferredSize(new Dimension(251, 16));
		_altWreslDiffWarningLabel.setText("*WRESL Script Directory differs from installation");
		_altWreslDiffWarningLabel.setVisible(true);
		gbc = new GridBagConstraints();
		gbc.gridx = 2;
		gbc.gridy = 3;
		gbc.gridwidth = 2;
		gbc.weightx = 0.8;
		gbc.fill = GridBagConstraints.HORIZONTAL;
		gbc.insets = new Insets(5, 5, 5, 5);
		panel17.add(_altWreslDiffWarningLabel, gbc);
	}

	/**
	 * @noinspection ALL
	 */
	public JComponent $$$getRootComponent$$$()
	{
		return _panel1;
	}

	public void fillScenarioRuns(EpptScenarioRun baseRun, List<EpptScenarioRun> alternatives)
	{
		if(Objects.equals(_baseRun, baseRun) && alternatives.contains(_altRun))
		{
			return;
		}

		_waterYearDefinition = _epptConfigurationController.getWaterYearDefinition();
		_qaqcTextPane.setText("");
		_baseRun = baseRun;
		if(alternatives.isEmpty())
		{
			_altRun = null;
			_altScenarioTextField.setText("");
			_tabbedPane1.setTitleAt(2, "Alternative WRESL");
			_tabbedPane1.setEnabledAt(2, false);
		}
		else
		{
			_altRun = alternatives.get(0);
			_altScenarioTextField.setText(_altRun.getName());
			_tabbedPane1.setTitleAt(2, _altRun.getName() + " WRESL");
			_tabbedPane1.setEnabledAt(2, true);
		}

		if(baseRun == null)
		{
			_baseScenarioTextField.setText("");
			_tabbedPane1.setTitleAt(1, "Base WRESL");
			_tabbedPane1.setEnabledAt(1, false);
		}
		else
		{
			_baseScenarioTextField.setText(baseRun.getName());
			_tabbedPane1.setTitleAt(1, baseRun.getName() + " WRESL");
			_tabbedPane1.setEnabledAt(1, true);
		}
		Path currentProject = EpptPreferences.getLastProjectConfiguration().getParent();
		Path reportPath = currentProject.resolve("Reports").resolve(_epptConfigurationController.getProjectName() + ".pdf");
		_pdfOutput.setText(reportPath.toString());
		_openReportButton.setEnabled(reportPath.toFile().exists());
		_tabbedPane1.setTitleAt(0, reportPath.getFileName().toString() + " QA/QC");
		_tabbedPane1.setSelectedIndex(0);
		updateCompareState();
		updateEnabledState();
		showWreslScriptChangedWarning(_baseWreslDiffWarningLabel, _baseRun);
		showWreslScriptChangedWarning(_altWreslDiffWarningLabel, _altRun);
	}

	static void showWreslScriptChangedWarning(JLabel jLabel, EpptScenarioRun scenarioRun)
	{
		boolean show = false;
		if(scenarioRun != null)
		{
			try
			{
				Path scenarioWreslDirectory = scenarioRun.getWreslDirectory();
				Path scenarioLookupDirectory = scenarioRun.getLookupDirectory();
				Path installerWreslDirectory = Paths.get(Constant.WRESL_DIR).resolve(scenarioRun.getModel().toString());
				Path installerLookupDirectory = Paths.get(Constant.WRESL_DIR).resolve(scenarioRun.getModel().toString()).resolve(Constant.LOOKUP_DIRECTORY);
				List<Path> scenarioWreslFiles;
				try(Stream<Path> walk = Files.walk(scenarioWreslDirectory))
				{
					scenarioWreslFiles = walk.filter(p -> !p.startsWith(scenarioWreslDirectory.resolve(Constant.LOOKUP_DIRECTORY))).collect(toList());
				}
				List<Path> scenarioLookupFiles;
				try(Stream<Path> walk = Files.walk(scenarioLookupDirectory))
				{
					scenarioLookupFiles = walk.collect(toList());
				}
				List<Path> installerWreslFiles;
				try(Stream<Path> walk = Files.walk(installerWreslDirectory))
				{
					installerWreslFiles = walk.filter(p -> !p.startsWith(installerWreslDirectory.resolve(Constant.LOOKUP_DIRECTORY))).collect(toList());
				}
				List<Path> installerLookupFiles;
				try(Stream<Path> walk = Files.walk(installerLookupDirectory))
				{
					installerLookupFiles = walk.collect(toList());
				}
				show = compareDirectories(scenarioLookupDirectory, scenarioLookupFiles, installerLookupDirectory, installerLookupFiles) || compareDirectories(
						scenarioWreslDirectory, scenarioWreslFiles, installerWreslDirectory, installerWreslFiles);
			}
			catch(IOException e)
			{
				LOGGER.log(Level.SEVERE, "Unable to determine if WRESL files differ from installer for scenario run: " + scenarioRun, e);
			}
		}
		jLabel.setVisible(show);
	}

	private static boolean compareDirectories(Path scenarioRoot, List<Path> scenarioDirectoryPaths, Path installerRoot, List<Path> installerDirectoryPaths) throws IOException
	{
		boolean differ = installerDirectoryPaths.stream().map(other -> installerRoot.relativize(other)).map(scenarioRoot::resolve).anyMatch(p -> !scenarioDirectoryPaths.contains(p));
		if(!differ)
		{
			for(Path installerPath : installerDirectoryPaths)
			{
				if(installerPath.toFile().isFile())
				{
					Path scenarioPath = scenarioRoot.resolve(installerRoot.relativize(installerPath));
					if(!scenarioPath.equals(installerPath))
					{
						boolean contentEquals = FileUtils.contentEquals(installerPath.toFile(), scenarioPath.toFile());
						if(!contentEquals)
						{
							differ = true;
							break;
						}
					}
				}
			}
		}
		return differ;
	}

	private void updateCompareState()
	{
		boolean canCompare = false;
		if(_baseRun != null && _altRun != null)
		{
			canCompare = Objects.equals(_baseRun.getModel(), _altRun.getModel());
		}
		if(!canCompare)
		{
			_codeChangesCheckBox.setSelected(false);
			_assumptionChangesCheckBox.setSelected(false);
		}
		_codeChangesCheckBox.setEnabled(canCompare);
		_assumptionChangesCheckBox.setEnabled(canCompare);
	}

	private void updateEnabledState()
	{
		_runBaseWreslButton.setEnabled(_baseRun != null);
		_generateReportButton.setEnabled(_baseRun != null);
		_runAltWreslButton.setEnabled(_altRun != null);
	}


	private void appendErrorText(String str)
	{
		SwingUtilities.invokeLater(() -> {
			StyleConstants.setForeground(_style, Color.red);
			appendText(str);
		});
	}

	private void appendNormalText(String str)
	{
		SwingUtilities.invokeLater(() -> {
			StyleConstants.setForeground(_style, Color.black);
			appendText(str);
		});
	}

	private void appendText(String str)
	{
		try
		{
			int length = _doc.getLength();
			_doc.insertString(length, "\n" + str, _style);
			_qaqcTextPane.setCaretPosition(length + 1);
		}
		catch(BadLocationException e)
		{
			LOGGER.log(Level.SEVERE, "Error appending text", e);
		}
	}

	private void openPdf(ActionEvent e)
	{
		Path path = Paths.get(_pdfOutput.getText());
		if(path.toFile().exists())
		{
			try
			{
				Desktop.getDesktop().open(path.toFile());
			}
			catch(IOException ex)
			{
				LOGGER.log(Level.SEVERE, "Unable to open file: " + path, ex);
			}
		}
	}

	public void toggleWarningText()
	{
		_baseWreslDiffWarningLabel.setVisible(!_baseWreslDiffWarningLabel.isVisible());
		_altWreslDiffWarningLabel.setVisible(!_altWreslDiffWarningLabel.isVisible());
	}

	private final class ReportHandler extends Handler
	{

		@Override
		public void publish(LogRecord record)
		{
			String loggerName = record.getLoggerName();
			if(loggerName != null && loggerName.startsWith(EPPTReport.class.getPackage().getName()))
			{
				if(record.getLevel().equals(Level.WARNING) || record.getLevel().equals(Level.SEVERE))
				{
					Throwable thrown = record.getThrown();
					if(thrown == null)
					{
						appendErrorText(record.getMessage());
					}
					else
					{
						appendThrowable(thrown);
					}
				}
				else
				{
					appendNormalText(record.getMessage());
				}
			}
		}

		private void appendThrowable(Throwable thrown)
		{
			if(thrown != null)
			{
				Throwable cause = thrown.getCause();
				if(cause != null)
				{
					appendThrowable(cause);
				}
				else
				{
					if(thrown instanceof RuntimeException)
					{
						appendErrorText("\t" + thrown.toString());
					}
					else
					{
						appendErrorText("\t" + thrown.getMessage());
					}
				}
			}
		}

		@Override
		public void flush()
		{
			LOGGER.log(Level.FINE, "NO-OP");
		}

		@Override
		public void close()
		{
			LOGGER.log(Level.FINE, "NO-OP");
		}
	}

	private class QaQcProcessConsumer implements ProcessOutputConsumer
	{

		private TextAreaPrintStream _textAreaPrintStreamQaQc;

		@Override
		public void runStarted(EpptScenarioRun scenarioRun, Process process)
		{
			_textAreaPrintStreamQaQc = new TextAreaPrintStream(_qaqcTextPane, process.getInputStream(), process.getErrorStream());
		}

		@Override
		public void runFinished(Process process)
		{
			if(process.exitValue() == 0)
			{
				try
				{
					Desktop.getDesktop().open(new File(_pdfOutput.getText()));
				}
				catch(IOException e)
				{
					LOGGER.log(Level.WARNING, "Desktop not supported. PDF will not be displayed", e);
				}
			}
			if(_textAreaPrintStreamQaQc != null)
			{
				_textAreaPrintStreamQaQc.close();
			}
		}
	}

	private static final class WreslProcessConsumer implements ProcessOutputConsumer
	{
		private final JTextPane _textPane;
		private TextAreaPrintStream _printStream;

		private WreslProcessConsumer(JTextPane textPane)
		{
			_textPane = textPane;
		}

		@Override
		public void runStarted(EpptScenarioRun scenarioRun, Process process)
		{
			_printStream = new TextAreaPrintStream(_textPane, process.getInputStream(), process.getErrorStream());
		}

		@Override
		public void runFinished(Process process)
		{
			if(_printStream != null)
			{
				_printStream.close();
			}
		}
	}

	@Override
	public String getJavaHelpId()
	{
		return "4.5_QAQCReport.htm";
	}
}
