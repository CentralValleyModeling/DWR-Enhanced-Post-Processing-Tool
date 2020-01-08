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
import java.nio.file.Path;
import java.nio.file.Paths;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.Month;
import java.time.ZoneId;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.NavigableMap;
import java.util.Objects;
import java.util.TreeMap;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.logging.LogRecord;
import java.util.logging.Logger;
import javax.swing.*;
import javax.swing.event.ChangeEvent;
import javax.swing.text.BadLocationException;
import javax.swing.text.Style;
import javax.swing.text.StyleConstants;
import javax.swing.text.StyledDocument;

import gov.ca.water.calgui.EpptInitializationException;
import gov.ca.water.calgui.bo.CommonPeriodFilter;
import gov.ca.water.calgui.bo.SimpleFileFilter;
import gov.ca.water.calgui.bo.WaterYearDefinition;
import gov.ca.water.calgui.bo.WaterYearIndex;
import gov.ca.water.calgui.bo.WaterYearPeriod;
import gov.ca.water.calgui.bo.WaterYearPeriodRange;
import gov.ca.water.calgui.bo.WaterYearType;
import gov.ca.water.calgui.busservice.impl.WaterYearDefinitionSvc;
import gov.ca.water.calgui.busservice.impl.WaterYearTableReader;
import gov.ca.water.calgui.constant.EpptPreferences;
import gov.ca.water.calgui.project.EpptScenarioRun;
import gov.ca.water.calgui.scripts.DssCache;
import gov.ca.water.calgui.scripts.DssMissingRecordException;
import gov.ca.water.calgui.scripts.DssReader;
import gov.ca.water.calgui.wresl.ProcessOutputConsumer;
import gov.ca.water.calgui.wresl.WreslScriptException;
import gov.ca.water.calgui.wresl.WreslScriptRunner;
import gov.ca.water.quickresults.ui.TextAreaPrintStream;
import gov.ca.water.quickresults.ui.projectconfig.ProjectConfigurationPanel;
import gov.ca.water.reportengine.EPPTReport;
import gov.ca.water.reportengine.EpptReportException;
import gov.ca.water.reportengine.QAQCReportException;
import gov.ca.water.reportengine.ReportParameters;
import gov.ca.water.reportengine.standardsummary.PercentDiffStyle;
import gov.ca.water.reportengine.standardsummary.StandardSummaryErrors;
import gov.ca.water.reportengine.standardsummary.StandardSummaryReader;
import gov.ca.water.reportengine.standardsummary.SummaryReportParameters;

import hec.heclib.dss.HecDSSFileAccess;
import rma.swing.RmaJDateTimeField;
import rma.swing.RmaJDecimalField;
import rma.swing.RmaJIntegerField;
import rma.swing.RmaJPanel;

import static java.util.stream.Collectors.toList;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 05-13-2019
 */
public class QAQCReportPanel extends RmaJPanel
{
	private static final Logger LOGGER = Logger.getLogger(QAQCReportPanel.class.getName());
	private static final QAQCReportPanel INSTANCE = new QAQCReportPanel();

	private final StyledDocument _doc;
	private final Style _style;
	private final AtomicInteger _processesRunning = new AtomicInteger(0);
	private final List<JCheckBox> _summaryModules;
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
	private JCheckBox _tableOfContentsCheckBox;
	private JCheckBox _coverPageCheckBox;
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
	private WaterYearPeriodsPanel _waterYearPeriodsPanel;
	private JComboBox<WaterYearIndex> _waterYearIndexCombo;
	private JComboBox<WaterYearDefinition> _waterYearDefinitionCombo;
	private JComboBox<PercentDiffStyle> _percentDiffStyle;
	private RmaJIntegerField _longTermEndYear;
	private RmaJIntegerField _longTermStartYear;
	private JPanel _summaryModulesPanel;
	private JButton _cancelButton;
	private RmaJDateTimeField _startDateChooser;
	private RmaJDateTimeField _endDateChooser;
	private JButton _overwriteJRXMLButton;
	private JButton _openReportButton;
	private JButton _overwriteScriptsButton;
	private JButton _overwriteScriptsButton1;
	private EpptScenarioRun _baseRun;
	private EpptScenarioRun _altRun;
	private final Map<JCheckBox, String> _reportModules = new HashMap<>();
	private Future<?> _baseWreslFuture;
	private Future<?> _altWreslFuture;
	private Future<?> _qaqcReportFuture;
	private final ExecutorService _executor = Executors.newFixedThreadPool(5);

	private QAQCReportPanel()
	{
		_waterYearPeriodsPanel = new WaterYearPeriodsPanel();
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
		ProjectConfigurationPanel.getProjectConfigurationPanel().addScenarioChangedListener(this::fillScenarioRuns);
		ProjectConfigurationPanel projectConfigurationPanel = ProjectConfigurationPanel.getProjectConfigurationPanel();
		Path currentProject = EpptPreferences.getLastProjectConfiguration().getParent();
		Path reportPath = currentProject.resolve("Reports").resolve(projectConfigurationPanel.getProjectName() + ".pdf");
		_pdfOutput.setText(reportPath.toString());
		_openReportButton.setEnabled(reportPath.toFile().exists());
		_tabbedPane1.setTitleAt(0, reportPath.getFileName().toString() + " QA/QC");
		WaterYearDefinitionSvc.getWaterYearDefinitionSvc().getDefinitions()
							  .forEach(_waterYearDefinitionCombo::addItem);
		_waterYearIndexCombo.addActionListener(this::waterYearIndexChanged);
		_waterYearDefinitionCombo.addActionListener(this::waterYearDefinitionChanged);
		_longTermStartYear.setValue(1921);
		_longTermEndYear.setValue(2003);
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
		_standardSummaryStatiticsCheckBox.addActionListener(e ->
		{
			if(!_standardSummaryStatiticsCheckBox.isSelected())
			{
				_summaryModules.forEach(c -> c.setSelected(false));
			}
			_summaryModules.forEach(c -> c.setEnabled(_standardSummaryStatiticsCheckBox.isSelected()));
		});
		_overwriteJRXMLButton.addActionListener(e -> checkForceCopyJrXml());
		_openReportButton.addActionListener(this::openPdf);
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

	private void waterYearDefinitionChanged(ActionEvent e)
	{
		Object waterYearDefinition = _waterYearDefinitionCombo.getSelectedItem();
		if(waterYearDefinition instanceof WaterYearDefinition)
		{
			_waterYearPeriodsPanel.fillWithDefinition((WaterYearDefinition) waterYearDefinition);
		}
	}

	private void waterYearIndexChanged(ActionEvent e)
	{
		Object waterYearIndex = _waterYearIndexCombo.getSelectedItem();
		Object waterYearDefinition = _waterYearDefinitionCombo.getSelectedItem();
		if(waterYearIndex instanceof WaterYearIndex && waterYearDefinition instanceof WaterYearDefinition)
		{
			_waterYearPeriodsPanel.fillWithIndex((WaterYearIndex) waterYearIndex, (WaterYearDefinition) waterYearDefinition);
		}
	}

	private void runAltWresl()
	{
		if(checkIfDssFileIsOpen(_altRun))
		{
			_altWreslFuture = _executor.submit(() ->
			{
				try
				{
					_tabbedPane1.setSelectedIndex(2);
					startProcessAsync(_runAltWreslButton);
					runWresl(_altRun, _altWreslTextPane);
				}
				finally
				{
					SwingUtilities.invokeLater(() ->
					{
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
			_baseWreslFuture = _executor.submit(() ->
			{
				try
				{
					_tabbedPane1.setSelectedIndex(1);
					startProcessAsync(_runBaseWreslButton);
					runWresl(_baseRun, _baseWreslTextPane);
				}
				finally
				{
					SwingUtilities.invokeLater(() ->
					{
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
					JOptionPane.showConfirmDialog(this,
							"DSS File inaccessible. Ensure it is not being written to in another process:\n" + postProcessDss,
							"DSS File Error", JOptionPane.OK_OPTION, JOptionPane.WARNING_MESSAGE);
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
			ProjectConfigurationPanel projectConfigurationPanel = ProjectConfigurationPanel.getProjectConfigurationPanel();
			LocalDate startMonth = projectConfigurationPanel.getStartMonth();
			LocalDate endMonth = projectConfigurationPanel.getEndMonth();
			LocalDate start = LocalDate.of(startMonth.getYear(), startMonth.getMonth(), 1);
			start = start.withDayOfMonth(start.lengthOfMonth());
			LocalDate end = LocalDate.of(endMonth.getYear(), endMonth.getMonth(), 1);
			end = end.withDayOfMonth(end.lengthOfMonth());
			WreslScriptRunner wreslScriptRunner = new WreslScriptRunner(scenarioRun, new WreslProcessConsumer(textPane));
			wreslScriptRunner.run(start, end);
		}
		catch(WreslScriptException | RuntimeException e1)
		{
			LOGGER.log(Level.SEVERE, "Error in WRESL Script Run", e1);
		}
	}

	private void stopProcessAsync(JButton button)
	{
		SwingUtilities.invokeLater(() ->
		{
			button.setEnabled(true);
			stopProgress();
		});
	}

	private void startProcessAsync(JButton button)
	{
		SwingUtilities.invokeLater(() ->
		{
			_cancelButton.setEnabled(true);
			button.setEnabled(false);
			startProgress();
		});
	}

	public static QAQCReportPanel getInstance()
	{
		return INSTANCE;
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
				int warning = JOptionPane.showConfirmDialog(this, "PDF: " + _pdfOutput.getText() + " already exists. Do you wish to overwrite?",
						"Warning", JOptionPane.YES_NO_OPTION);
				if(warning == JOptionPane.YES_OPTION)
				{
					try(FileOutputStream f = new FileOutputStream(reportPath.toString()))
					{
						_qaqcReportFuture = _executor.submit(this::generateQAQCReport);
					}
					catch(IOException e)
					{
						LOGGER.log(Level.WARNING, "Unable to delete existing report", e);
						JOptionPane.showMessageDialog(this, "Error while creating the pdf file: " + reportPath.getFileName()
										+ "\nIf the file is already open, please close it and try again.\n" + e.getMessage(),
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
		int replaceReponse = JOptionPane.showConfirmDialog(this, "Jasper report files exist: " + jrXmlPath + "\n Do you wish to overwrite?",
				"Warning", JOptionPane.YES_NO_OPTION);
		if(replaceReponse == JOptionPane.YES_OPTION)
		{
			try
			{
				QAQCReportGenerator.copyJasperPaths();
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
			WaterYearPeriodRange longTermRange = getLongTermRange();
			Map<WaterYearPeriod, List<WaterYearPeriodRange>> waterYearPeriodRanges = _waterYearPeriodsPanel.getWaterYearPeriodRanges();
			PercentDiffStyle percentDiffStyle = (PercentDiffStyle) _percentDiffStyle.getSelectedItem();
			WaterYearDefinition waterYearDefinition = (WaterYearDefinition) _waterYearDefinitionCombo.getSelectedItem();
			WaterYearIndex waterYearIndex = (WaterYearIndex) _waterYearIndexCombo.getSelectedItem();
			List<String> disabledSummaryModules = getDisabledSummaryModules();
			Date startDate = _startDateChooser.getDate();
			Date endDate = _endDateChooser.getDate();
			CommonPeriodFilter commonPeriodFilter = new CommonPeriodFilter(LocalDateTime.ofInstant(startDate.toInstant(), ZoneId.systemDefault()),
					LocalDateTime.ofInstant(endDate.toInstant(), ZoneId.systemDefault()));
			SummaryReportParameters summaryReportParameters = new SummaryReportParameters(waterYearDefinition, waterYearIndex,
					longTermRange, waterYearPeriodRanges, percentDiffStyle, disabledSummaryModules, commonPeriodFilter, new DssCache());
			List<String> disabledReportModules = getDisabledReportModules();
			ReportParameters reportParameters = new ReportParameters(tolerance, author, title, subtitle, summaryReportParameters,
					disabledReportModules, true, true);
			qaqcReportGenerator.generateQAQCReport(_baseRun, _altRun, reportParameters,
					pathToWriteOut);
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
				LOGGER.log(Level.SEVERE, "Unable to generate Report PDF", e);
				appendErrorText("Error: " + e);
			}
		}
		finally
		{
			SwingUtilities.invokeLater(() ->
			{
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
		return _reportModules.entrySet().stream().filter(k -> !k.getKey().isSelected()).map(
				Map.Entry::getValue).collect(toList());
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
			return reader.getModules()
						 .stream()
						 .map(text -> new JCheckBox(text, true))
						 .collect(toList());
		}
		catch(EpptReportException e)
		{
			LOGGER.log(Level.SEVERE, "Error reading summary modules", e);
			return new ArrayList<>();
		}
	}

	private WaterYearPeriodRange getLongTermRange()
	{
		WaterYearPeriod longTerm = new WaterYearPeriod("Long Term");
		int startYear = _longTermStartYear.getValue();
		int endYear = _longTermEndYear.getValue();
		return new WaterYearPeriodRange(longTerm, new WaterYearType(startYear, longTerm), new WaterYearType(endYear, longTerm));
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
		_overwriteJRXMLButton = new JButton();
		_overwriteJRXMLButton.setText("Update Report Templates");
		panel3.add(_overwriteJRXMLButton);
		final JPanel panel4 = new JPanel();
		panel4.setLayout(new FlowLayout(FlowLayout.RIGHT, 5, 5));
		panel2.add(panel4, BorderLayout.CENTER);
		_generateReportButton = new JButton();
		_generateReportButton.setText("Generate Report");
		panel4.add(_generateReportButton);
		_cancelButton = new JButton();
		_cancelButton.setEnabled(false);
		_cancelButton.setPreferredSize(new Dimension(115, 24));
		_cancelButton.setText("Cancel");
		panel4.add(_cancelButton);
		final JPanel panel5 = new JPanel();
		panel5.setLayout(new BorderLayout(0, 0));
		panel1.add(panel5, BorderLayout.NORTH);
		_progressBar1 = new JProgressBar();
		_progressBar1.setVisible(false);
		panel5.add(_progressBar1, BorderLayout.CENTER);
		final JPanel panel6 = new JPanel();
		panel6.setLayout(new BorderLayout(0, 0));
		panel6.setPreferredSize(new Dimension(850, 510));
		_panel1.add(panel6, BorderLayout.NORTH);
		final JPanel panel7 = new JPanel();
		panel7.setLayout(new BorderLayout(0, 0));
		panel7.setPreferredSize(new Dimension(560, 300));
		panel6.add(panel7, BorderLayout.WEST);
		final JPanel panel8 = new JPanel();
		panel8.setLayout(new GridBagLayout());
		panel7.add(panel8, BorderLayout.NORTH);
		final JLabel label1 = new JLabel();
		label1.setText("Alternative Scenario Run:");
		GridBagConstraints gbc;
		gbc = new GridBagConstraints();
		gbc.gridx = 0;
		gbc.gridy = 1;
		gbc.anchor = GridBagConstraints.NORTH;
		gbc.fill = GridBagConstraints.HORIZONTAL;
		gbc.insets = new Insets(10, 5, 5, 5);
		panel8.add(label1, gbc);
		final JPanel panel9 = new JPanel();
		panel9.setLayout(new BorderLayout(5, 5));
		gbc = new GridBagConstraints();
		gbc.gridx = 1;
		gbc.gridy = 1;
		gbc.fill = GridBagConstraints.BOTH;
		gbc.insets = new Insets(5, 5, 5, 5);
		panel8.add(panel9, gbc);
		final JPanel panel10 = new JPanel();
		panel10.setLayout(new BorderLayout(5, 5));
		panel9.add(panel10, BorderLayout.SOUTH);
		final JLabel label2 = new JLabel();
		label2.setPreferredSize(new Dimension(251, 26));
		label2.setText("*WRESL Script Directory differs from installation");
		panel10.add(label2, BorderLayout.WEST);
		_overwriteScriptsButton = new JButton();
		_overwriteScriptsButton.setPreferredSize(new Dimension(125, 26));
		_overwriteScriptsButton.setText("Overwrite Scripts");
		panel10.add(_overwriteScriptsButton, BorderLayout.EAST);
		final JPanel panel11 = new JPanel();
		panel11.setLayout(new BorderLayout(0, 0));
		panel9.add(panel11, BorderLayout.CENTER);
		_altScenarioTextField = new JTextField();
		_altScenarioTextField.setEditable(false);
		_altScenarioTextField.setText("Alternative");
		panel11.add(_altScenarioTextField, BorderLayout.CENTER);
		_runAltWreslButton = new JButton();
		_runAltWreslButton.setEnabled(false);
		_runAltWreslButton.setPreferredSize(new Dimension(125, 26));
		_runAltWreslButton.setText("Run WRESL");
		panel11.add(_runAltWreslButton, BorderLayout.EAST);
		final JLabel label3 = new JLabel();
		label3.setText("Base Scenario Run:");
		gbc = new GridBagConstraints();
		gbc.gridx = 0;
		gbc.gridy = 0;
		gbc.anchor = GridBagConstraints.NORTH;
		gbc.fill = GridBagConstraints.HORIZONTAL;
		gbc.insets = new Insets(10, 5, 5, 5);
		panel8.add(label3, gbc);
		final JPanel panel12 = new JPanel();
		panel12.setLayout(new BorderLayout(5, 5));
		gbc = new GridBagConstraints();
		gbc.gridx = 1;
		gbc.gridy = 0;
		gbc.fill = GridBagConstraints.BOTH;
		gbc.insets = new Insets(5, 5, 5, 5);
		panel8.add(panel12, gbc);
		final JPanel panel13 = new JPanel();
		panel13.setLayout(new BorderLayout(5, 5));
		panel12.add(panel13, BorderLayout.SOUTH);
		final JLabel label4 = new JLabel();
		label4.setPreferredSize(new Dimension(251, 26));
		label4.setText("*WRESL Script Directory differs from installation");
		panel13.add(label4, BorderLayout.WEST);
		_overwriteScriptsButton1 = new JButton();
		_overwriteScriptsButton1.setPreferredSize(new Dimension(125, 26));
		_overwriteScriptsButton1.setText("Overwrite Scripts");
		panel13.add(_overwriteScriptsButton1, BorderLayout.EAST);
		final JPanel panel14 = new JPanel();
		panel14.setLayout(new BorderLayout(0, 0));
		panel12.add(panel14, BorderLayout.NORTH);
		_baseScenarioTextField = new JTextField();
		_baseScenarioTextField.setEditable(false);
		_baseScenarioTextField.setText("Base");
		panel14.add(_baseScenarioTextField, BorderLayout.CENTER);
		_runBaseWreslButton = new JButton();
		_runBaseWreslButton.setEnabled(false);
		_runBaseWreslButton.setPreferredSize(new Dimension(125, 26));
		_runBaseWreslButton.setText("Run WRESL");
		panel14.add(_runBaseWreslButton, BorderLayout.EAST);
		final JLabel label5 = new JLabel();
		label5.setText("Output File:");
		gbc = new GridBagConstraints();
		gbc.gridx = 0;
		gbc.gridy = 5;
		gbc.fill = GridBagConstraints.HORIZONTAL;
		gbc.insets = new Insets(5, 5, 5, 5);
		panel8.add(label5, gbc);
		final JPanel panel15 = new JPanel();
		panel15.setLayout(new BorderLayout(2, 0));
		gbc = new GridBagConstraints();
		gbc.gridx = 1;
		gbc.gridy = 5;
		gbc.fill = GridBagConstraints.BOTH;
		gbc.insets = new Insets(5, 5, 5, 5);
		panel8.add(panel15, gbc);
		_pdfOutput = new JTextField();
		_pdfOutput.setEditable(false);
		_pdfOutput.setPreferredSize(new Dimension(200, 24));
		_pdfOutput.setText("");
		panel15.add(_pdfOutput, BorderLayout.CENTER);
		_fileChooserBtn = new JButton();
		_fileChooserBtn.setPreferredSize(new Dimension(30, 24));
		_fileChooserBtn.setText("...");
		panel15.add(_fileChooserBtn, BorderLayout.EAST);
		final JLabel label6 = new JLabel();
		label6.setText("DSS Compare Tolerance:");
		gbc = new GridBagConstraints();
		gbc.gridx = 0;
		gbc.gridy = 7;
		gbc.fill = GridBagConstraints.HORIZONTAL;
		gbc.insets = new Insets(5, 5, 5, 5);
		panel8.add(label6, gbc);
		_toleranceTextField.setText("");
		gbc = new GridBagConstraints();
		gbc.gridx = 1;
		gbc.gridy = 7;
		gbc.anchor = GridBagConstraints.WEST;
		gbc.fill = GridBagConstraints.HORIZONTAL;
		gbc.insets = new Insets(5, 5, 5, 5);
		panel8.add(_toleranceTextField, gbc);
		final JLabel label7 = new JLabel();
		label7.setText("Author:");
		gbc = new GridBagConstraints();
		gbc.gridx = 0;
		gbc.gridy = 2;
		gbc.weightx = 0.1;
		gbc.fill = GridBagConstraints.HORIZONTAL;
		gbc.insets = new Insets(5, 5, 5, 5);
		panel8.add(label7, gbc);
		_authorTextField = new JTextField();
		gbc = new GridBagConstraints();
		gbc.gridx = 1;
		gbc.gridy = 2;
		gbc.weightx = 0.8;
		gbc.anchor = GridBagConstraints.WEST;
		gbc.fill = GridBagConstraints.HORIZONTAL;
		gbc.insets = new Insets(5, 5, 5, 5);
		panel8.add(_authorTextField, gbc);
		final JLabel label8 = new JLabel();
		label8.setText("QA/QC Report Subtitle:");
		gbc = new GridBagConstraints();
		gbc.gridx = 0;
		gbc.gridy = 4;
		gbc.fill = GridBagConstraints.HORIZONTAL;
		gbc.insets = new Insets(5, 5, 5, 5);
		panel8.add(label8, gbc);
		_reportSubtitle = new JTextField();
		_reportSubtitle.setText("");
		gbc = new GridBagConstraints();
		gbc.gridx = 1;
		gbc.gridy = 4;
		gbc.weightx = 1.0;
		gbc.anchor = GridBagConstraints.WEST;
		gbc.fill = GridBagConstraints.HORIZONTAL;
		gbc.insets = new Insets(5, 5, 5, 5);
		panel8.add(_reportSubtitle, gbc);
		final JLabel label9 = new JLabel();
		label9.setText("Percent Diff Format:");
		gbc = new GridBagConstraints();
		gbc.gridx = 0;
		gbc.gridy = 8;
		gbc.fill = GridBagConstraints.HORIZONTAL;
		gbc.insets = new Insets(5, 5, 5, 5);
		panel8.add(label9, gbc);
		final JLabel label10 = new JLabel();
		label10.setText("Long Term Start Year:");
		gbc = new GridBagConstraints();
		gbc.gridx = 0;
		gbc.gridy = 9;
		gbc.fill = GridBagConstraints.HORIZONTAL;
		gbc.insets = new Insets(5, 5, 5, 5);
		panel8.add(label10, gbc);
		final JLabel label11 = new JLabel();
		label11.setText("Long Term End Year:");
		gbc = new GridBagConstraints();
		gbc.gridx = 0;
		gbc.gridy = 10;
		gbc.fill = GridBagConstraints.HORIZONTAL;
		gbc.insets = new Insets(5, 5, 5, 5);
		panel8.add(label11, gbc);
		_percentDiffStyle = new JComboBox();
		gbc = new GridBagConstraints();
		gbc.gridx = 1;
		gbc.gridy = 8;
		gbc.anchor = GridBagConstraints.WEST;
		gbc.fill = GridBagConstraints.HORIZONTAL;
		gbc.insets = new Insets(5, 5, 5, 5);
		panel8.add(_percentDiffStyle, gbc);
		_longTermStartYear = new RmaJIntegerField();
		gbc = new GridBagConstraints();
		gbc.gridx = 1;
		gbc.gridy = 9;
		gbc.fill = GridBagConstraints.HORIZONTAL;
		gbc.insets = new Insets(5, 5, 5, 5);
		panel8.add(_longTermStartYear, gbc);
		_longTermEndYear = new RmaJIntegerField();
		gbc = new GridBagConstraints();
		gbc.gridx = 1;
		gbc.gridy = 10;
		gbc.fill = GridBagConstraints.HORIZONTAL;
		gbc.insets = new Insets(5, 5, 5, 5);
		panel8.add(_longTermEndYear, gbc);
		final JLabel label12 = new JLabel();
		label12.setText("Common Period:");
		gbc = new GridBagConstraints();
		gbc.gridx = 0;
		gbc.gridy = 11;
		gbc.fill = GridBagConstraints.HORIZONTAL;
		gbc.insets = new Insets(5, 5, 5, 5);
		panel8.add(label12, gbc);
		final JPanel panel16 = new JPanel();
		panel16.setLayout(new FlowLayout(FlowLayout.LEFT, 0, 5));
		gbc = new GridBagConstraints();
		gbc.gridx = 1;
		gbc.gridy = 11;
		gbc.fill = GridBagConstraints.BOTH;
		gbc.insets = new Insets(5, 5, 5, 5);
		panel8.add(panel16, gbc);
		_startDateChooser = new RmaJDateTimeField();
		_startDateChooser.setEnabled(true);
		_startDateChooser.setPreferredSize(new Dimension(120, 26));
		_startDateChooser.setText("01Nov1921 0000");
		panel16.add(_startDateChooser);
		final JLabel label13 = new JLabel();
		label13.setText("  Start ");
		panel16.add(label13);
		_endDateChooser = new RmaJDateTimeField();
		_endDateChooser.setPreferredSize(new Dimension(120, 26));
		_endDateChooser.setText("01Oct2003 0100");
		panel16.add(_endDateChooser);
		final JLabel label14 = new JLabel();
		label14.setText(" End");
		panel16.add(label14);
		_openReportButton = new JButton();
		_openReportButton.setPreferredSize(new Dimension(125, 26));
		_openReportButton.setText("Open Report");
		gbc = new GridBagConstraints();
		gbc.gridx = 1;
		gbc.gridy = 6;
		gbc.anchor = GridBagConstraints.EAST;
		gbc.insets = new Insets(0, 0, 0, 5);
		panel8.add(_openReportButton, gbc);
		final JLabel label15 = new JLabel();
		label15.setText("QA/QC Report Title:");
		gbc = new GridBagConstraints();
		gbc.gridx = 0;
		gbc.gridy = 3;
		gbc.fill = GridBagConstraints.HORIZONTAL;
		gbc.insets = new Insets(5, 5, 5, 5);
		panel8.add(label15, gbc);
		_reportTitle = new JTextField();
		_reportTitle.setText("EPPT QA/QC Report");
		gbc = new GridBagConstraints();
		gbc.gridx = 1;
		gbc.gridy = 3;
		gbc.weightx = 1.0;
		gbc.anchor = GridBagConstraints.WEST;
		gbc.fill = GridBagConstraints.HORIZONTAL;
		gbc.insets = new Insets(5, 5, 5, 5);
		panel8.add(_reportTitle, gbc);
		final JPanel panel17 = new JPanel();
		panel17.setLayout(new BorderLayout(0, 0));
		panel17.setPreferredSize(new Dimension(400, 200));
		panel6.add(panel17, BorderLayout.CENTER);
		panel17.setBorder(BorderFactory.createTitledBorder(BorderFactory.createEtchedBorder(), "QA/QC Report Controls"));
		final JPanel panel18 = new JPanel();
		panel18.setLayout(new BorderLayout(0, 0));
		panel18.setPreferredSize(new Dimension(300, 270));
		panel17.add(panel18, BorderLayout.WEST);
		final JPanel panel19 = new JPanel();
		panel19.setLayout(new GridBagLayout());
		panel18.add(panel19, BorderLayout.NORTH);
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
		panel19.add(_excutiveSummaryCheckBox, gbc);
		_assumptionChangesCheckBox = new JCheckBox();
		_assumptionChangesCheckBox.setEnabled(true);
		_assumptionChangesCheckBox.setHorizontalAlignment(2);
		_assumptionChangesCheckBox.setText("Assumption Changes");
		gbc = new GridBagConstraints();
		gbc.gridx = 0;
		gbc.gridy = 1;
		gbc.anchor = GridBagConstraints.NORTHWEST;
		gbc.insets = new Insets(0, 5, 0, 5);
		panel19.add(_assumptionChangesCheckBox, gbc);
		_codeChangesCheckBox = new JCheckBox();
		_codeChangesCheckBox.setEnabled(true);
		_codeChangesCheckBox.setHorizontalAlignment(2);
		_codeChangesCheckBox.setText("Code Changes");
		gbc = new GridBagConstraints();
		gbc.gridx = 0;
		gbc.gridy = 2;
		gbc.anchor = GridBagConstraints.NORTHWEST;
		gbc.insets = new Insets(0, 5, 0, 5);
		panel19.add(_codeChangesCheckBox, gbc);
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
		panel19.add(_detailedIssuesCheckBox, gbc);
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
		panel19.add(_standardSummaryStatiticsCheckBox, gbc);
		_summaryModulesPanel = new JPanel();
		_summaryModulesPanel.setLayout(new FlowLayout(FlowLayout.LEFT, 5, 5));
		gbc = new GridBagConstraints();
		gbc.gridx = 0;
		gbc.gridy = 5;
		gbc.fill = GridBagConstraints.BOTH;
		gbc.insets = new Insets(5, 25, 5, 5);
		panel19.add(_summaryModulesPanel, gbc);
		final JPanel panel20 = new JPanel();
		panel20.setLayout(new BorderLayout(0, 0));
		panel17.add(panel20, BorderLayout.CENTER);
		final JPanel panel21 = new JPanel();
		panel21.setLayout(new BorderLayout(0, 0));
		panel21.setPreferredSize(new Dimension(350, 300));
		panel20.add(panel21, BorderLayout.WEST);
		final JPanel panel22 = new JPanel();
		panel22.setLayout(new GridBagLayout());
		panel21.add(panel22, BorderLayout.NORTH);
		final JLabel label16 = new JLabel();
		label16.setText("Water Year Definition:");
		gbc = new GridBagConstraints();
		gbc.gridx = 0;
		gbc.gridy = 0;
		gbc.anchor = GridBagConstraints.WEST;
		gbc.insets = new Insets(5, 5, 5, 5);
		panel22.add(label16, gbc);
		final JLabel label17 = new JLabel();
		label17.setText("Water Year Index:");
		gbc = new GridBagConstraints();
		gbc.gridx = 0;
		gbc.gridy = 1;
		gbc.anchor = GridBagConstraints.WEST;
		gbc.insets = new Insets(5, 5, 5, 5);
		panel22.add(label17, gbc);
		_waterYearDefinitionCombo = new JComboBox();
		_waterYearDefinitionCombo.setEnabled(true);
		gbc = new GridBagConstraints();
		gbc.gridx = 1;
		gbc.gridy = 0;
		gbc.weightx = 0.8;
		gbc.anchor = GridBagConstraints.WEST;
		gbc.fill = GridBagConstraints.HORIZONTAL;
		gbc.insets = new Insets(5, 5, 5, 5);
		panel22.add(_waterYearDefinitionCombo, gbc);
		_waterYearIndexCombo = new JComboBox();
		_waterYearIndexCombo.setEnabled(false);
		gbc = new GridBagConstraints();
		gbc.gridx = 1;
		gbc.gridy = 1;
		gbc.weightx = 0.8;
		gbc.anchor = GridBagConstraints.WEST;
		gbc.fill = GridBagConstraints.HORIZONTAL;
		gbc.insets = new Insets(5, 5, 5, 5);
		panel22.add(_waterYearIndexCombo, gbc);
		_waterYearPeriodsPanel.setEnabled(true);
		panel21.add(_waterYearPeriodsPanel, BorderLayout.CENTER);
		_tabbedPane1 = new JTabbedPane();
		_panel1.add(_tabbedPane1, BorderLayout.CENTER);
		final JScrollPane scrollPane1 = new JScrollPane();
		_tabbedPane1.addTab("QA/QC", scrollPane1);
		_qaqcTextPane = new JTextPane();
		_qaqcTextPane.setEditable(false);
		_qaqcTextPane.setPreferredSize(new Dimension(800, 250));
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
			_waterYearIndexCombo.removeAllItems();
		}
		else
		{
			_baseScenarioTextField.setText(baseRun.getName());
			_tabbedPane1.setTitleAt(1, baseRun.getName() + " WRESL");
			_tabbedPane1.setEnabledAt(1, true);
			fillWaterYearIndex();
		}
		Path currentProject = EpptPreferences.getLastProjectConfiguration().getParent();
		Path reportPath = currentProject.resolve("Reports").resolve(
				ProjectConfigurationPanel.getProjectConfigurationPanel().getProjectName() + ".pdf");
		_pdfOutput.setText(reportPath.toString());
		_openReportButton.setEnabled(reportPath.toFile().exists());
		_tabbedPane1.setTitleAt(0, reportPath.getFileName().toString() + " QA/QC");
		_tabbedPane1.setSelectedIndex(0);
		updateCompareState();
		updateEnabledState();
		initializeCommonPeriod();
	}

	private void initializeCommonPeriod()
	{
		if(_baseRun == null)
		{
			_startDateChooser.setText("");
			_endDateChooser.setText("");
		}
		else if(_altRun == null)
		{
			_startDateChooser.setDate(getStartDate(_baseRun));
			_endDateChooser.setDate(getEndDate(_baseRun));
		}
		else
		{
			Date startDate = getStartDate(_baseRun);
			Date endDate = getEndDate(_baseRun);
			Date altStartDate = getStartDate(_altRun);
			Date altEndDate = getEndDate(_altRun);
			if(startDate.before(altStartDate))
			{
				startDate = altStartDate;
			}
			if(endDate.after(altEndDate))
			{
				endDate = altEndDate;
			}
			_startDateChooser.setDate(startDate);
			_endDateChooser.setDate(endDate);
		}
	}

	private Date getEndDate(EpptScenarioRun baseRun)
	{
		NavigableMap<LocalDateTime, Double> guiLinkData = new TreeMap<>();
		try
		{
			WaterYearDefinition selectedItem = (WaterYearDefinition) _waterYearDefinitionCombo.getSelectedItem();
			guiLinkData = new DssReader(baseRun, selectedItem, new DssCache()).getGuiLinkData(102);
		}
		catch(DssMissingRecordException e)
		{
			LOGGER.log(Level.FINE, "Missing GUILink for ID 102", e);
		}
		if(!guiLinkData.isEmpty())
		{
			LocalDateTime localDateTime = guiLinkData.lastKey();
			return Date.from(localDateTime.atZone(ZoneId.systemDefault()).toInstant());
		}
		else
		{
			return new Date();
		}
	}

	private Date getStartDate(EpptScenarioRun baseRun)
	{
		NavigableMap<LocalDateTime, Double> guiLinkData = new TreeMap<>();
		WaterYearDefinition selectedItem = (WaterYearDefinition) _waterYearDefinitionCombo.getSelectedItem();
		try
		{
			guiLinkData = new DssReader(baseRun, selectedItem, new DssCache()).getGuiLinkData(102);
		}
		catch(DssMissingRecordException e)
		{
			LOGGER.log(Level.FINE, "Missing GUILink for ID 102", e);
		}
		if(!guiLinkData.isEmpty())
		{
			LocalDateTime localDateTime = guiLinkData.firstKey();
			return Date.from(localDateTime.atZone(ZoneId.systemDefault()).toInstant());
		}
		else
		{
			return new Date();
		}
	}

	private void fillWaterYearIndex()
	{
		Path waterYearTable = _baseRun.getWaterYearTable();
		WaterYearTableReader waterYearTableReader = new WaterYearTableReader(waterYearTable);
		try
		{
			waterYearTableReader.read()
								.forEach(_waterYearIndexCombo::addItem);
			_waterYearIndexCombo.setSelectedIndex(0);
		}
		catch(EpptInitializationException e)
		{
			String msg = "Error processing water year table for the base scenario for the QA/QC report, please ensure the path is correct: "
					+ waterYearTable;
			LOGGER.log(Level.WARNING, msg, e);
		}
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
		_waterYearIndexCombo.setEnabled(_baseRun != null);
		_runBaseWreslButton.setEnabled(_baseRun != null);
		_generateReportButton.setEnabled(_baseRun != null);
		_runAltWreslButton.setEnabled(_altRun != null);
	}


	private void appendErrorText(String str)
	{
		SwingUtilities.invokeLater(() ->
		{
			StyleConstants.setForeground(_style, Color.red);
			appendText(str);
		});
	}

	private void appendNormalText(String str)
	{
		SwingUtilities.invokeLater(() ->
		{
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
}
