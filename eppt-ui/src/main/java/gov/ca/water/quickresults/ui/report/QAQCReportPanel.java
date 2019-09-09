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
import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.Month;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Function;
import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.logging.LogRecord;
import java.util.logging.Logger;
import java.util.stream.Collectors;
import javax.swing.*;
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
import gov.ca.water.reportengine.standardsummary.StandardSummaryReader;
import gov.ca.water.reportengine.standardsummary.SummaryReportParameters;

import rma.swing.RmaJDecimalField;
import rma.swing.RmaJIntegerField;
import rma.swing.RmaJPanel;

import static java.util.stream.Collectors.toList;
import static java.util.stream.Collectors.toMap;

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
	private EpptScenarioRun _baseRun;
	private EpptScenarioRun _altRun;
	private final Map<JCheckBox, String> _reportModules = new HashMap<>();

	private QAQCReportPanel()
	{
		_waterYearPeriodsPanel = new WaterYearPeriodsPanel();
		$$$setupUI$$$();
		_generateReportButton.addActionListener(e -> generateReport());
		_runBaseWreslButton.addActionListener(e -> CompletableFuture.runAsync(this::runBaseWresl));
		_runAltWreslButton.addActionListener(e -> CompletableFuture.runAsync(this::runAltWresl));
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
		_tabbedPane1.setTitleAt(0, reportPath.getFileName().toString() + " QA/QC");
		WaterYearDefinitionSvc.getWaterYearDefinitionSvc().getDefinitions()
							  .forEach(_waterYearDefinitionCombo::addItem);
		_waterYearIndexCombo.addActionListener(this::waterYearIndexChanged);
		_waterYearDefinitionCombo.addActionListener(this::waterYearDefinitionChanged);
		_longTermStartYear.setValue(1921);
		_longTermEndYear.setValue(2003);
		Arrays.asList(PercentDiffStyle.values()).forEach(_percentDiffStyle::addItem);
		_reportModules.put(_coverPageCheckBox, EPPTReport.COVER_PAGE);
		_reportModules.put(_tableOfContentsCheckBox, EPPTReport.TABLE_OF_CONTENTS);
		_reportModules.put(_excutiveSummaryCheckBox, EPPTReport.EXECUTIVE_SUMMARY);
		_reportModules.put(_assumptionChangesCheckBox, EPPTReport.ASSUMPTION_CHANGES);
		_reportModules.put(_codeChangesCheckBox, EPPTReport.CODE_CHANGES);
		_reportModules.put(_detailedIssuesCheckBox, EPPTReport.DETAILED_ISSUES);
		_reportModules.put(_standardSummaryStatiticsCheckBox, EPPTReport.STANDARD_SUMMARY_STATISTICS);
		_summaryModules = buildStandardSummaryModules();
		_summaryModulesPanel.setLayout(new BoxLayout(_summaryModulesPanel, BoxLayout.Y_AXIS));
		_summaryModules.forEach(_summaryModulesPanel::add);
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
		try
		{
			_tabbedPane1.setSelectedIndex(2);
			startProcessAsync(_runAltWreslButton);
			runWresl(_altRun, _altWreslTextPane);
		}
		finally
		{
			stopProcessAsync(_runAltWreslButton);
		}
	}

	private void runBaseWresl()
	{
		try
		{
			_tabbedPane1.setSelectedIndex(1);
			startProcessAsync(_runBaseWreslButton);
			runWresl(_baseRun, _baseWreslTextPane);
		}
		finally
		{
			stopProcessAsync(_runBaseWreslButton);
		}
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
			_tabbedPane1.setTitleAt(0, Paths.get(filePath).getFileName().toString() + " QA/QC");
		}
	}

	private void generateReport()
	{
		_qaqcTextPane.setText("");
		if(_baseRun != null)
		{
			boolean exists = Paths.get(_pdfOutput.getText()).toFile().exists();
			if(exists)
			{
				int warning = JOptionPane.showConfirmDialog(this, "PDF: " + _pdfOutput.getText() + " already exists. Do you wish to overwrite?",
						"Warning", JOptionPane.YES_NO_OPTION);
				if(warning == JOptionPane.YES_OPTION)
				{
					CompletableFuture.runAsync(this::generateQAQCReport);
				}
			}
			else
			{
				CompletableFuture.runAsync(this::generateQAQCReport);
			}
		}
		else
		{
			JOptionPane.showMessageDialog(this, "No Base Scenario Run selected.");
		}
	}

	private void generateQAQCReport()
	{
		try
		{
			_tabbedPane1.setSelectedIndex(0);
			_qaqcTextPane.setText("");
			startProcessAsync(_generateReportButton);
			Path pathToWriteOut = Paths.get(_pdfOutput.getText());
			QAQCReportGenerator qaqcReportGenerator = new QAQCReportGenerator(new QaQcProcessConsumer());
			double tolerance = Double.parseDouble(_toleranceTextField.getText());
			String author = _authorTextField.getText();
			EpptPreferences.setUsername(author);
			String subtitle = _reportSubtitle.getText();
			WaterYearPeriodRange longTermRange = getLongTermRange();
			Map<WaterYearPeriod, List<WaterYearPeriodRange>> waterYearPeriodRanges = _waterYearPeriodsPanel.getWaterYearPeriodRanges();
			PercentDiffStyle percentDiffStyle = (PercentDiffStyle) _percentDiffStyle.getSelectedItem();
			WaterYearDefinition waterYearDefinition = (WaterYearDefinition) _waterYearDefinitionCombo.getSelectedItem();
			WaterYearIndex waterYearIndex = (WaterYearIndex) _waterYearIndexCombo.getSelectedItem();
			List<String> disabledSummaryModules = getDisabledSummaryModules();
			CommonPeriodFilter commonPeriodFilter = new CommonPeriodFilter(LocalDateTime.of(1950, Month.JULY, 1, 0, 0),
					LocalDateTime.of(1999, Month.JULY, 1, 0, 0));
			SummaryReportParameters summaryReportParameters = new SummaryReportParameters(waterYearDefinition, waterYearIndex,
					longTermRange, waterYearPeriodRanges, percentDiffStyle, disabledSummaryModules, commonPeriodFilter);
			List<String> disabledReportModules = getDisabledReportModules();
			ReportParameters reportParameters = new ReportParameters(tolerance, author, subtitle, summaryReportParameters,
					disabledReportModules);
			qaqcReportGenerator.generateQAQCReport(_baseRun, _altRun, reportParameters,
					pathToWriteOut);
		}
		catch(QAQCReportException | RuntimeException | EpptReportException e)
		{
			LOGGER.log(Level.SEVERE, "Unable to generate Report PDF", e);
			appendErrorText("Error: " + e);
		}
		finally
		{
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
		StandardSummaryReader reader = new StandardSummaryReader(Paths.get(EPPTReport.SUMMARY_CSV));
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
		_panel1.setMinimumSize(new Dimension(600, 350));
		_panel1.setPreferredSize(new Dimension(1000, 800));
		_panel1.setRequestFocusEnabled(false);
		final JPanel panel1 = new JPanel();
		panel1.setLayout(new BorderLayout(0, 0));
		_panel1.add(panel1, BorderLayout.SOUTH);
		final JPanel panel2 = new JPanel();
		panel2.setLayout(new FlowLayout(FlowLayout.RIGHT, 5, 5));
		panel1.add(panel2, BorderLayout.CENTER);
		_generateReportButton = new JButton();
		_generateReportButton.setText("Generate Report");
		panel2.add(_generateReportButton);
		final JPanel panel3 = new JPanel();
		panel3.setLayout(new BorderLayout(0, 0));
		panel1.add(panel3, BorderLayout.NORTH);
		_progressBar1 = new JProgressBar();
		_progressBar1.setVisible(false);
		panel3.add(_progressBar1, BorderLayout.CENTER);
		final JPanel panel4 = new JPanel();
		panel4.setLayout(new BorderLayout(0, 0));
		panel4.setPreferredSize(new Dimension(850, 400));
		_panel1.add(panel4, BorderLayout.NORTH);
		final JPanel panel5 = new JPanel();
		panel5.setLayout(new BorderLayout(0, 0));
		panel5.setPreferredSize(new Dimension(475, 204));
		panel4.add(panel5, BorderLayout.WEST);
		final JPanel panel6 = new JPanel();
		panel6.setLayout(new GridBagLayout());
		panel5.add(panel6, BorderLayout.NORTH);
		final JLabel label1 = new JLabel();
		label1.setText("Alternative Scenario Run:");
		GridBagConstraints gbc;
		gbc = new GridBagConstraints();
		gbc.gridx = 0;
		gbc.gridy = 1;
		gbc.fill = GridBagConstraints.HORIZONTAL;
		gbc.insets = new Insets(5, 5, 5, 5);
		panel6.add(label1, gbc);
		final JPanel panel7 = new JPanel();
		panel7.setLayout(new BorderLayout(0, 0));
		gbc = new GridBagConstraints();
		gbc.gridx = 1;
		gbc.gridy = 1;
		gbc.fill = GridBagConstraints.BOTH;
		gbc.insets = new Insets(5, 5, 5, 5);
		panel6.add(panel7, gbc);
		_runAltWreslButton = new JButton();
		_runAltWreslButton.setEnabled(false);
		_runAltWreslButton.setText("Run WRESL");
		panel7.add(_runAltWreslButton, BorderLayout.EAST);
		_altScenarioTextField = new JTextField();
		_altScenarioTextField.setEditable(false);
		panel7.add(_altScenarioTextField, BorderLayout.CENTER);
		final JLabel label2 = new JLabel();
		label2.setText("Base Scenario Run:");
		gbc = new GridBagConstraints();
		gbc.gridx = 0;
		gbc.gridy = 0;
		gbc.fill = GridBagConstraints.HORIZONTAL;
		gbc.insets = new Insets(5, 5, 5, 5);
		panel6.add(label2, gbc);
		final JPanel panel8 = new JPanel();
		panel8.setLayout(new BorderLayout(0, 0));
		gbc = new GridBagConstraints();
		gbc.gridx = 1;
		gbc.gridy = 0;
		gbc.fill = GridBagConstraints.BOTH;
		gbc.insets = new Insets(5, 5, 5, 5);
		panel6.add(panel8, gbc);
		_runBaseWreslButton = new JButton();
		_runBaseWreslButton.setEnabled(false);
		_runBaseWreslButton.setText("Run WRESL");
		panel8.add(_runBaseWreslButton, BorderLayout.EAST);
		_baseScenarioTextField = new JTextField();
		_baseScenarioTextField.setEditable(false);
		panel8.add(_baseScenarioTextField, BorderLayout.CENTER);
		final JLabel label3 = new JLabel();
		label3.setText("Output File:");
		gbc = new GridBagConstraints();
		gbc.gridx = 0;
		gbc.gridy = 4;
		gbc.fill = GridBagConstraints.HORIZONTAL;
		gbc.insets = new Insets(5, 5, 5, 5);
		panel6.add(label3, gbc);
		final JPanel panel9 = new JPanel();
		panel9.setLayout(new BorderLayout(2, 0));
		gbc = new GridBagConstraints();
		gbc.gridx = 1;
		gbc.gridy = 4;
		gbc.fill = GridBagConstraints.BOTH;
		gbc.insets = new Insets(5, 5, 5, 5);
		panel6.add(panel9, gbc);
		_pdfOutput = new JTextField();
		_pdfOutput.setEditable(false);
		_pdfOutput.setPreferredSize(new Dimension(200, 24));
		_pdfOutput.setText("");
		panel9.add(_pdfOutput, BorderLayout.CENTER);
		_fileChooserBtn = new JButton();
		_fileChooserBtn.setPreferredSize(new Dimension(30, 24));
		_fileChooserBtn.setText("...");
		panel9.add(_fileChooserBtn, BorderLayout.EAST);
		final JLabel label4 = new JLabel();
		label4.setText("DSS Compare Tolerance:");
		gbc = new GridBagConstraints();
		gbc.gridx = 0;
		gbc.gridy = 5;
		gbc.fill = GridBagConstraints.HORIZONTAL;
		gbc.insets = new Insets(5, 5, 5, 5);
		panel6.add(label4, gbc);
		_toleranceTextField.setText("");
		gbc = new GridBagConstraints();
		gbc.gridx = 1;
		gbc.gridy = 5;
		gbc.anchor = GridBagConstraints.WEST;
		gbc.fill = GridBagConstraints.HORIZONTAL;
		gbc.insets = new Insets(5, 5, 5, 5);
		panel6.add(_toleranceTextField, gbc);
		final JLabel label5 = new JLabel();
		label5.setText("Author:");
		gbc = new GridBagConstraints();
		gbc.gridx = 0;
		gbc.gridy = 2;
		gbc.weightx = 0.1;
		gbc.fill = GridBagConstraints.HORIZONTAL;
		gbc.insets = new Insets(5, 5, 5, 5);
		panel6.add(label5, gbc);
		_authorTextField = new JTextField();
		gbc = new GridBagConstraints();
		gbc.gridx = 1;
		gbc.gridy = 2;
		gbc.weightx = 0.8;
		gbc.anchor = GridBagConstraints.WEST;
		gbc.fill = GridBagConstraints.HORIZONTAL;
		gbc.insets = new Insets(5, 5, 5, 5);
		panel6.add(_authorTextField, gbc);
		final JLabel label6 = new JLabel();
		label6.setText("QA/QC Report Subtitle:");
		gbc = new GridBagConstraints();
		gbc.gridx = 0;
		gbc.gridy = 3;
		gbc.fill = GridBagConstraints.HORIZONTAL;
		gbc.insets = new Insets(5, 5, 5, 5);
		panel6.add(label6, gbc);
		_reportSubtitle = new JTextField();
		_reportSubtitle.setText("Subtitle");
		gbc = new GridBagConstraints();
		gbc.gridx = 1;
		gbc.gridy = 3;
		gbc.weightx = 1.0;
		gbc.anchor = GridBagConstraints.WEST;
		gbc.fill = GridBagConstraints.HORIZONTAL;
		gbc.insets = new Insets(5, 5, 5, 5);
		panel6.add(_reportSubtitle, gbc);
		final JLabel label7 = new JLabel();
		label7.setText("Percent Full Format:");
		gbc = new GridBagConstraints();
		gbc.gridx = 0;
		gbc.gridy = 6;
		gbc.fill = GridBagConstraints.HORIZONTAL;
		gbc.insets = new Insets(5, 5, 5, 5);
		panel6.add(label7, gbc);
		final JLabel label8 = new JLabel();
		label8.setText("Long Term Start Year:");
		gbc = new GridBagConstraints();
		gbc.gridx = 0;
		gbc.gridy = 7;
		gbc.fill = GridBagConstraints.HORIZONTAL;
		gbc.insets = new Insets(5, 5, 5, 5);
		panel6.add(label8, gbc);
		final JLabel label9 = new JLabel();
		label9.setText("Long Term End Year:");
		gbc = new GridBagConstraints();
		gbc.gridx = 0;
		gbc.gridy = 8;
		gbc.fill = GridBagConstraints.HORIZONTAL;
		gbc.insets = new Insets(5, 5, 5, 5);
		panel6.add(label9, gbc);
		_percentDiffStyle = new JComboBox();
		gbc = new GridBagConstraints();
		gbc.gridx = 1;
		gbc.gridy = 6;
		gbc.anchor = GridBagConstraints.WEST;
		gbc.fill = GridBagConstraints.HORIZONTAL;
		gbc.insets = new Insets(5, 5, 5, 5);
		panel6.add(_percentDiffStyle, gbc);
		_longTermStartYear = new RmaJIntegerField();
		gbc = new GridBagConstraints();
		gbc.gridx = 1;
		gbc.gridy = 7;
		gbc.fill = GridBagConstraints.HORIZONTAL;
		gbc.insets = new Insets(5, 5, 5, 5);
		panel6.add(_longTermStartYear, gbc);
		_longTermEndYear = new RmaJIntegerField();
		gbc = new GridBagConstraints();
		gbc.gridx = 1;
		gbc.gridy = 8;
		gbc.fill = GridBagConstraints.HORIZONTAL;
		gbc.insets = new Insets(5, 5, 5, 5);
		panel6.add(_longTermEndYear, gbc);
		final JPanel panel10 = new JPanel();
		panel10.setLayout(new BorderLayout(0, 0));
		panel10.setPreferredSize(new Dimension(500, 200));
		panel4.add(panel10, BorderLayout.CENTER);
		panel10.setBorder(BorderFactory.createTitledBorder(BorderFactory.createEtchedBorder(), "QA/QC Report Controls"));
		final JPanel panel11 = new JPanel();
		panel11.setLayout(new BorderLayout(0, 0));
		panel11.setPreferredSize(new Dimension(300, 270));
		panel10.add(panel11, BorderLayout.WEST);
		final JPanel panel12 = new JPanel();
		panel12.setLayout(new GridBagLayout());
		panel11.add(panel12, BorderLayout.NORTH);
		_excutiveSummaryCheckBox = new JCheckBox();
		_excutiveSummaryCheckBox.setEnabled(true);
		_excutiveSummaryCheckBox.setHorizontalAlignment(2);
		_excutiveSummaryCheckBox.setSelected(true);
		_excutiveSummaryCheckBox.setText("Excutive Summary");
		gbc = new GridBagConstraints();
		gbc.gridx = 0;
		gbc.gridy = 2;
		gbc.weightx = 0.5;
		gbc.anchor = GridBagConstraints.NORTHWEST;
		gbc.insets = new Insets(0, 5, 0, 5);
		panel12.add(_excutiveSummaryCheckBox, gbc);
		_assumptionChangesCheckBox = new JCheckBox();
		_assumptionChangesCheckBox.setEnabled(true);
		_assumptionChangesCheckBox.setHorizontalAlignment(2);
		_assumptionChangesCheckBox.setText("Assumption Changes");
		gbc = new GridBagConstraints();
		gbc.gridx = 0;
		gbc.gridy = 3;
		gbc.anchor = GridBagConstraints.NORTHWEST;
		gbc.insets = new Insets(0, 5, 0, 5);
		panel12.add(_assumptionChangesCheckBox, gbc);
		_codeChangesCheckBox = new JCheckBox();
		_codeChangesCheckBox.setEnabled(true);
		_codeChangesCheckBox.setHorizontalAlignment(2);
		_codeChangesCheckBox.setText("Code Changes");
		gbc = new GridBagConstraints();
		gbc.gridx = 0;
		gbc.gridy = 4;
		gbc.anchor = GridBagConstraints.NORTHWEST;
		gbc.insets = new Insets(0, 5, 0, 5);
		panel12.add(_codeChangesCheckBox, gbc);
		_detailedIssuesCheckBox = new JCheckBox();
		_detailedIssuesCheckBox.setEnabled(true);
		_detailedIssuesCheckBox.setHorizontalAlignment(2);
		_detailedIssuesCheckBox.setSelected(true);
		_detailedIssuesCheckBox.setText("Detailed Issues");
		gbc = new GridBagConstraints();
		gbc.gridx = 0;
		gbc.gridy = 5;
		gbc.anchor = GridBagConstraints.NORTHWEST;
		gbc.insets = new Insets(0, 5, 0, 5);
		panel12.add(_detailedIssuesCheckBox, gbc);
		_standardSummaryStatiticsCheckBox = new JCheckBox();
		_standardSummaryStatiticsCheckBox.setEnabled(true);
		_standardSummaryStatiticsCheckBox.setHorizontalAlignment(2);
		_standardSummaryStatiticsCheckBox.setSelected(true);
		_standardSummaryStatiticsCheckBox.setText("Standard Summary Statitics");
		gbc = new GridBagConstraints();
		gbc.gridx = 0;
		gbc.gridy = 6;
		gbc.anchor = GridBagConstraints.NORTHWEST;
		gbc.insets = new Insets(0, 5, 0, 5);
		panel12.add(_standardSummaryStatiticsCheckBox, gbc);
		_coverPageCheckBox = new JCheckBox();
		_coverPageCheckBox.setEnabled(true);
		_coverPageCheckBox.setHorizontalAlignment(2);
		_coverPageCheckBox.setSelected(true);
		_coverPageCheckBox.setText("Cover Page");
		gbc = new GridBagConstraints();
		gbc.gridx = 0;
		gbc.gridy = 0;
		gbc.anchor = GridBagConstraints.NORTHWEST;
		gbc.insets = new Insets(0, 5, 0, 5);
		panel12.add(_coverPageCheckBox, gbc);
		_tableOfContentsCheckBox = new JCheckBox();
		_tableOfContentsCheckBox.setEnabled(true);
		_tableOfContentsCheckBox.setHorizontalAlignment(2);
		_tableOfContentsCheckBox.setSelected(true);
		_tableOfContentsCheckBox.setText("Table of Contents");
		gbc = new GridBagConstraints();
		gbc.gridx = 0;
		gbc.gridy = 1;
		gbc.anchor = GridBagConstraints.NORTHWEST;
		gbc.insets = new Insets(0, 5, 0, 5);
		panel12.add(_tableOfContentsCheckBox, gbc);
		_summaryModulesPanel = new JPanel();
		_summaryModulesPanel.setLayout(new FlowLayout(FlowLayout.LEFT, 5, 5));
		gbc = new GridBagConstraints();
		gbc.gridx = 0;
		gbc.gridy = 7;
		gbc.fill = GridBagConstraints.BOTH;
		gbc.insets = new Insets(5, 25, 5, 5);
		panel12.add(_summaryModulesPanel, gbc);
		final JPanel panel13 = new JPanel();
		panel13.setLayout(new BorderLayout(0, 0));
		panel10.add(panel13, BorderLayout.CENTER);
		final JPanel panel14 = new JPanel();
		panel14.setLayout(new BorderLayout(0, 0));
		panel14.setPreferredSize(new Dimension(350, 300));
		panel13.add(panel14, BorderLayout.WEST);
		final JPanel panel15 = new JPanel();
		panel15.setLayout(new GridBagLayout());
		panel14.add(panel15, BorderLayout.NORTH);
		final JLabel label10 = new JLabel();
		label10.setText("Water Year Definition:");
		gbc = new GridBagConstraints();
		gbc.gridx = 0;
		gbc.gridy = 0;
		gbc.anchor = GridBagConstraints.WEST;
		gbc.insets = new Insets(5, 5, 5, 5);
		panel15.add(label10, gbc);
		final JLabel label11 = new JLabel();
		label11.setText("Water Year Index:");
		gbc = new GridBagConstraints();
		gbc.gridx = 0;
		gbc.gridy = 1;
		gbc.anchor = GridBagConstraints.WEST;
		gbc.insets = new Insets(5, 5, 5, 5);
		panel15.add(label11, gbc);
		_waterYearDefinitionCombo = new JComboBox();
		_waterYearDefinitionCombo.setEnabled(true);
		gbc = new GridBagConstraints();
		gbc.gridx = 1;
		gbc.gridy = 0;
		gbc.weightx = 0.8;
		gbc.anchor = GridBagConstraints.WEST;
		gbc.fill = GridBagConstraints.HORIZONTAL;
		gbc.insets = new Insets(5, 5, 5, 5);
		panel15.add(_waterYearDefinitionCombo, gbc);
		_waterYearIndexCombo = new JComboBox();
		_waterYearIndexCombo.setEnabled(false);
		gbc = new GridBagConstraints();
		gbc.gridx = 1;
		gbc.gridy = 1;
		gbc.weightx = 0.8;
		gbc.anchor = GridBagConstraints.WEST;
		gbc.fill = GridBagConstraints.HORIZONTAL;
		gbc.insets = new Insets(5, 5, 5, 5);
		panel15.add(_waterYearIndexCombo, gbc);
		_waterYearPeriodsPanel.setEnabled(true);
		panel14.add(_waterYearPeriodsPanel, BorderLayout.CENTER);
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
		_tabbedPane1.setSelectedIndex(0);
		updateCompareState();
		updateEnabledState();
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
			LOGGER.log(Level.SEVERE, "Error processing water year table for the base scenario", e);
		}
	}

	private void updateCompareState()
	{
		boolean canCompare = false;
		if(_baseRun != null && _altRun != null)
		{
			canCompare = Objects.equals(_baseRun.getModel(), _altRun.getModel());
		}
		_codeChangesCheckBox.setSelected(canCompare);
		_assumptionChangesCheckBox.setSelected(canCompare);
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
					appendErrorText(record.getMessage());
					Throwable thrown = record.getThrown();
					appendThrowable(thrown);
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
				appendErrorText("\t" + thrown.getMessage());
				Throwable cause = thrown.getCause();
				if(cause != null)
				{
					appendThrowable(cause);
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
