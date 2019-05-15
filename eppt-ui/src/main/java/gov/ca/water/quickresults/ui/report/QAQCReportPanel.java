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
import java.util.List;
import java.util.Objects;
import java.util.concurrent.CompletableFuture;
import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.logging.LogRecord;
import java.util.logging.Logger;
import javax.swing.*;
import javax.swing.text.BadLocationException;
import javax.swing.text.Style;
import javax.swing.text.StyleConstants;
import javax.swing.text.StyledDocument;

import gov.ca.water.calgui.bo.SimpleFileFilter;
import gov.ca.water.calgui.constant.EpptPreferences;
import gov.ca.water.calgui.project.EpptScenarioRun;
import gov.ca.water.calgui.wresl.ProcessOutputConsumer;
import gov.ca.water.quickresults.ui.TextAreaPrintStream;
import gov.ca.water.quickresults.ui.projectconfig.ProjectConfigurationPanel;
import gov.ca.water.reportengine.EPPTReport;
import gov.ca.water.reportengine.QAQCReportException;

import rma.swing.RmaJDecimalField;
import rma.swing.RmaJPanel;

import static gov.ca.water.calgui.constant.Constant.CSV_EXT;
import static gov.ca.water.calgui.constant.Constant.TABLE_EXT;
import static gov.ca.water.calgui.constant.Constant.WRESL_DIR;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 05-13-2019
 */
public class QAQCReportPanel extends RmaJPanel implements ProcessOutputConsumer
{
	private static final Logger LOGGER = Logger.getLogger(QAQCReportPanel.class.getName());
	private static final String WY_TYPES_TABLE = WRESL_DIR + "/lookup/wytypes" + TABLE_EXT;
	private static final String WY_TYPES_NAME_LOOKUP = WRESL_DIR + "/WYTypesLookup" + CSV_EXT;

	private final StyledDocument _doc;
	private final Style _style;
	private JPanel _panel1;
	private JButton _generateReportButton;
	private JTextPane _textPane1 = new JTextPane();
	private JComboBox<EpptScenarioRun> _altComboBox;
	private JComboBox<EpptScenarioRun> _baseComboBox;
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
	private JButton _wyTableBtn;
	private JTextField _waterYearLookup;
	private JButton _wyLookupBtn;
	private JTextField _waterYearTable;
	private TextAreaPrintStream _textAreaPrintStream;
	private boolean _ignoreSelectionChange;

	public QAQCReportPanel()
	{
		$$$setupUI$$$();
		addListeners();
		setLayout(new BorderLayout());
		add($$$getRootComponent$$$(), BorderLayout.CENTER);
		_authorTextField.setText(System.getProperty("user.name"));
		((RmaJDecimalField) _toleranceTextField).setValue(0.01);
		_fileChooserBtn.addActionListener(e -> chooseReportFile());
		_doc = (StyledDocument) _textPane1.getDocument();
		_style = _doc.addStyle("ConsoleStyle", null);
		Logger.getLogger("").addHandler(new ReportHandler());
		_waterYearTable.setText(Paths.get(WY_TYPES_TABLE).normalize().toString());
		_waterYearLookup.setText(Paths.get(WY_TYPES_NAME_LOOKUP).normalize().toString());
		_wyLookupBtn.addActionListener(e -> chooseWaterYearLookup());
		_wyTableBtn.addActionListener(e -> chooseWaterYearTable());
		_baseComboBox.addActionListener(this::scenarioComboboxChanged);
		_altComboBox.addActionListener(this::scenarioComboboxChanged);
	}

	private void scenarioComboboxChanged(ActionEvent e)
	{
		if(!_ignoreSelectionChange)
		{
			_ignoreSelectionChange = true;
			Object selectedBaseItem = _baseComboBox.getSelectedItem();
			Object selectedAltItem = _altComboBox.getSelectedItem();
			_baseComboBox.removeAllItems();
			_altComboBox.removeAllItems();
			_altComboBox.addItem(null);
			ProjectConfigurationPanel projectConfigurationPanel = ProjectConfigurationPanel.getProjectConfigurationPanel();
			List<EpptScenarioRun> allEpptScenarioRuns = projectConfigurationPanel.getAllEpptScenarioRuns();
			allEpptScenarioRuns.forEach(s -> _baseComboBox.addItem(s));
			allEpptScenarioRuns.forEach(s -> _altComboBox.addItem(s));
			if(selectedAltItem != null)
			{
				_baseComboBox.removeItem(selectedAltItem);
			}
			if(selectedBaseItem != null)
			{
				_altComboBox.removeItem(selectedBaseItem);
			}
			_baseComboBox.setSelectedItem(selectedBaseItem);
			_altComboBox.setSelectedItem(selectedAltItem);
			_ignoreSelectionChange = false;
		}
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
			_pdfOutput.setText(selectedFile.toString());
		}
	}

	private void chooseWaterYearTable()
	{
		JFileChooser jFileChooser = new JFileChooser();
		jFileChooser.setCurrentDirectory(Paths.get(_waterYearTable.getText()).toFile());
		jFileChooser.setFileFilter(new SimpleFileFilter("TABLE"));
		jFileChooser.setDialogTitle("Choose Water Year Table File");
		jFileChooser.showOpenDialog(this);
		File selectedFile = jFileChooser.getSelectedFile();
		if(selectedFile != null)
		{
			_waterYearTable.setText(selectedFile.toString());
		}
	}

	private void chooseWaterYearLookup()
	{
		JFileChooser jFileChooser = new JFileChooser();
		jFileChooser.setCurrentDirectory(Paths.get(_waterYearLookup.getText()).toFile());
		jFileChooser.setFileFilter(new SimpleFileFilter("CSV"));
		jFileChooser.setDialogTitle("Choose Water Year Lookup File");
		jFileChooser.showOpenDialog(this);
		File selectedFile = jFileChooser.getSelectedFile();
		if(selectedFile != null)
		{
			_waterYearLookup.setText(selectedFile.toString());
		}
	}

	private void addListeners()
	{
		_generateReportButton.addActionListener(e -> generateReport());
		_altComboBox.addActionListener(e ->
		{
			boolean canCompare = false;
			EpptScenarioRun baseRun = (EpptScenarioRun) _baseComboBox.getSelectedItem();
			EpptScenarioRun altRun = (EpptScenarioRun) _altComboBox.getSelectedItem();
			if(baseRun != null && altRun != null)
			{
				canCompare = Objects.equals(baseRun.getModel(), altRun.getModel());
			}
			_codeChangesCheckBox.setSelected(canCompare);
			_assumptionChangesCheckBox.setSelected(canCompare);
		});
	}

	private void generateReport()
	{
		_textPane1.setText("");
		EpptScenarioRun baseRun = (EpptScenarioRun) _baseComboBox.getSelectedItem();
		if(baseRun != null)
		{
			CompletableFuture.runAsync(this::generateQAQCReport);
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
			SwingUtilities.invokeLater(() -> _generateReportButton.setEnabled(false));
			Path pathToWriteOut = Paths.get(_pdfOutput.getText());
			EpptScenarioRun baseRun = (EpptScenarioRun) _baseComboBox.getSelectedItem();
			EpptScenarioRun altRun = (EpptScenarioRun) _altComboBox.getSelectedItem();
			QAQCReportGenerator qaqcReportGenerator = new QAQCReportGenerator(this);
			double tolerance = Double.parseDouble(_toleranceTextField.getText());
			String author = _authorTextField.getText();
			String subtitle = _reportSubtitle.getText();
			Path waterYearTablePath = Paths.get(_waterYearTable.getText());
			Path waterYearLookupPath = Paths.get(_waterYearLookup.getText());
			qaqcReportGenerator.generateQAQCReport(waterYearTablePath, waterYearLookupPath, baseRun, altRun, tolerance, author, subtitle,
					pathToWriteOut);
		}
		catch(QAQCReportException | RuntimeException e)
		{
			LOGGER.log(Level.SEVERE, "Unable to generate Report PDF", e);
			appendErrorText("Error: " + e);
		}
		finally
		{
			SwingUtilities.invokeLater(() -> _generateReportButton.setEnabled(true));
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
		_panel1.setMinimumSize(new Dimension(100, 350));
		_panel1.setPreferredSize(new Dimension(700, 600));
		_panel1.setRequestFocusEnabled(true);
		final JPanel panel1 = new JPanel();
		panel1.setLayout(new FlowLayout(FlowLayout.RIGHT, 5, 5));
		_panel1.add(panel1, BorderLayout.SOUTH);
		_generateReportButton = new JButton();
		_generateReportButton.setText("Generate Report");
		panel1.add(_generateReportButton);
		final JPanel panel2 = new JPanel();
		panel2.setLayout(new BorderLayout(0, 0));
		_panel1.add(panel2, BorderLayout.NORTH);
		final JPanel panel3 = new JPanel();
		panel3.setLayout(new GridBagLayout());
		panel2.add(panel3, BorderLayout.CENTER);
		final JLabel label1 = new JLabel();
		label1.setText("Base Scenario Run:");
		GridBagConstraints gbc;
		gbc = new GridBagConstraints();
		gbc.gridx = 0;
		gbc.gridy = 2;
		gbc.anchor = GridBagConstraints.WEST;
		gbc.insets = new Insets(5, 5, 5, 5);
		panel3.add(label1, gbc);
		final JLabel label2 = new JLabel();
		label2.setText("Alternative Scenario Run:");
		gbc = new GridBagConstraints();
		gbc.gridx = 0;
		gbc.gridy = 3;
		gbc.anchor = GridBagConstraints.WEST;
		gbc.insets = new Insets(5, 5, 5, 5);
		panel3.add(label2, gbc);
		_altComboBox = new JComboBox();
		gbc = new GridBagConstraints();
		gbc.gridx = 1;
		gbc.gridy = 3;
		gbc.anchor = GridBagConstraints.WEST;
		gbc.fill = GridBagConstraints.HORIZONTAL;
		gbc.insets = new Insets(5, 5, 5, 5);
		panel3.add(_altComboBox, gbc);
		_baseComboBox = new JComboBox();
		gbc = new GridBagConstraints();
		gbc.gridx = 1;
		gbc.gridy = 2;
		gbc.anchor = GridBagConstraints.WEST;
		gbc.fill = GridBagConstraints.HORIZONTAL;
		gbc.ipadx = 60;
		gbc.insets = new Insets(5, 5, 5, 5);
		panel3.add(_baseComboBox, gbc);
		final JLabel label3 = new JLabel();
		label3.setText("DSS Compare Tolerance:");
		gbc = new GridBagConstraints();
		gbc.gridx = 0;
		gbc.gridy = 4;
		gbc.anchor = GridBagConstraints.WEST;
		gbc.insets = new Insets(5, 5, 5, 5);
		panel3.add(label3, gbc);
		_toleranceTextField.setText("");
		gbc = new GridBagConstraints();
		gbc.gridx = 1;
		gbc.gridy = 4;
		gbc.anchor = GridBagConstraints.WEST;
		gbc.fill = GridBagConstraints.HORIZONTAL;
		gbc.insets = new Insets(5, 5, 5, 5);
		panel3.add(_toleranceTextField, gbc);
		_reportSubtitle = new JTextField();
		_reportSubtitle.setText("Subtitle");
		gbc = new GridBagConstraints();
		gbc.gridx = 1;
		gbc.gridy = 0;
		gbc.weightx = 1.0;
		gbc.anchor = GridBagConstraints.WEST;
		gbc.fill = GridBagConstraints.HORIZONTAL;
		gbc.insets = new Insets(5, 5, 5, 5);
		panel3.add(_reportSubtitle, gbc);
		final JLabel label4 = new JLabel();
		label4.setText("QA/QC Report Subtitle:");
		gbc = new GridBagConstraints();
		gbc.gridx = 0;
		gbc.gridy = 0;
		gbc.anchor = GridBagConstraints.WEST;
		gbc.insets = new Insets(5, 5, 5, 5);
		panel3.add(label4, gbc);
		final JLabel label5 = new JLabel();
		label5.setText("Output File:");
		gbc = new GridBagConstraints();
		gbc.gridx = 0;
		gbc.gridy = 5;
		gbc.anchor = GridBagConstraints.WEST;
		gbc.insets = new Insets(5, 5, 5, 5);
		panel3.add(label5, gbc);
		final JPanel panel4 = new JPanel();
		panel4.setLayout(new BorderLayout(2, 0));
		gbc = new GridBagConstraints();
		gbc.gridx = 1;
		gbc.gridy = 5;
		gbc.fill = GridBagConstraints.BOTH;
		gbc.insets = new Insets(5, 5, 5, 5);
		panel3.add(panel4, gbc);
		_pdfOutput = new JTextField();
		_pdfOutput.setEditable(false);
		_pdfOutput.setPreferredSize(new Dimension(200, 24));
		_pdfOutput.setText("");
		panel4.add(_pdfOutput, BorderLayout.CENTER);
		_fileChooserBtn = new JButton();
		_fileChooserBtn.setPreferredSize(new Dimension(30, 24));
		_fileChooserBtn.setText("...");
		panel4.add(_fileChooserBtn, BorderLayout.EAST);
		final JLabel label6 = new JLabel();
		label6.setText("WY Types Table:");
		gbc = new GridBagConstraints();
		gbc.gridx = 0;
		gbc.gridy = 6;
		gbc.anchor = GridBagConstraints.WEST;
		gbc.insets = new Insets(5, 5, 5, 5);
		panel3.add(label6, gbc);
		final JLabel label7 = new JLabel();
		label7.setText("WY Name Lookups:");
		gbc = new GridBagConstraints();
		gbc.gridx = 0;
		gbc.gridy = 7;
		gbc.anchor = GridBagConstraints.WEST;
		gbc.insets = new Insets(5, 5, 5, 5);
		panel3.add(label7, gbc);
		final JPanel panel5 = new JPanel();
		panel5.setLayout(new BorderLayout(0, 0));
		gbc = new GridBagConstraints();
		gbc.gridx = 1;
		gbc.gridy = 6;
		gbc.fill = GridBagConstraints.BOTH;
		gbc.insets = new Insets(5, 5, 5, 5);
		panel3.add(panel5, gbc);
		_waterYearTable = new JTextField();
		_waterYearTable.setEditable(false);
		_waterYearTable.setPreferredSize(new Dimension(200, 24));
		_waterYearTable.setText("");
		panel5.add(_waterYearTable, BorderLayout.CENTER);
		_wyTableBtn = new JButton();
		_wyTableBtn.setPreferredSize(new Dimension(30, 24));
		_wyTableBtn.setText("...");
		panel5.add(_wyTableBtn, BorderLayout.EAST);
		final JPanel panel6 = new JPanel();
		panel6.setLayout(new BorderLayout(0, 0));
		gbc = new GridBagConstraints();
		gbc.gridx = 1;
		gbc.gridy = 7;
		gbc.fill = GridBagConstraints.BOTH;
		gbc.insets = new Insets(5, 5, 5, 5);
		panel3.add(panel6, gbc);
		_waterYearLookup = new JTextField();
		_waterYearLookup.setEditable(false);
		_waterYearLookup.setPreferredSize(new Dimension(200, 24));
		panel6.add(_waterYearLookup, BorderLayout.CENTER);
		_wyLookupBtn = new JButton();
		_wyLookupBtn.setPreferredSize(new Dimension(30, 24));
		_wyLookupBtn.setSelected(true);
		_wyLookupBtn.setText("...");
		panel6.add(_wyLookupBtn, BorderLayout.EAST);
		final JLabel label8 = new JLabel();
		label8.setText("Author:");
		gbc = new GridBagConstraints();
		gbc.gridx = 0;
		gbc.gridy = 1;
		gbc.anchor = GridBagConstraints.WEST;
		gbc.insets = new Insets(5, 5, 5, 5);
		panel3.add(label8, gbc);
		_authorTextField = new JTextField();
		gbc = new GridBagConstraints();
		gbc.gridx = 1;
		gbc.gridy = 1;
		gbc.anchor = GridBagConstraints.WEST;
		gbc.fill = GridBagConstraints.HORIZONTAL;
		gbc.insets = new Insets(5, 5, 5, 5);
		panel3.add(_authorTextField, gbc);
		final JPanel panel7 = new JPanel();
		panel7.setLayout(new GridBagLayout());
		panel7.setPreferredSize(new Dimension(300, 200));
		panel2.add(panel7, BorderLayout.EAST);
		panel7.setBorder(BorderFactory.createTitledBorder(BorderFactory.createEtchedBorder(), "Modules"));
		_excutiveSummaryCheckBox = new JCheckBox();
		_excutiveSummaryCheckBox.setEnabled(false);
		_excutiveSummaryCheckBox.setHorizontalAlignment(2);
		_excutiveSummaryCheckBox.setSelected(true);
		_excutiveSummaryCheckBox.setText("Excutive Summary");
		gbc = new GridBagConstraints();
		gbc.gridx = 0;
		gbc.gridy = 2;
		gbc.weightx = 0.5;
		gbc.anchor = GridBagConstraints.NORTHWEST;
		gbc.insets = new Insets(5, 5, 5, 5);
		panel7.add(_excutiveSummaryCheckBox, gbc);
		_assumptionChangesCheckBox = new JCheckBox();
		_assumptionChangesCheckBox.setEnabled(false);
		_assumptionChangesCheckBox.setHorizontalAlignment(2);
		_assumptionChangesCheckBox.setText("Assumption Changes");
		gbc = new GridBagConstraints();
		gbc.gridx = 0;
		gbc.gridy = 3;
		gbc.anchor = GridBagConstraints.NORTHWEST;
		gbc.insets = new Insets(5, 5, 5, 5);
		panel7.add(_assumptionChangesCheckBox, gbc);
		_codeChangesCheckBox = new JCheckBox();
		_codeChangesCheckBox.setEnabled(false);
		_codeChangesCheckBox.setHorizontalAlignment(2);
		_codeChangesCheckBox.setText("Code Changes");
		gbc = new GridBagConstraints();
		gbc.gridx = 0;
		gbc.gridy = 4;
		gbc.anchor = GridBagConstraints.NORTHWEST;
		gbc.insets = new Insets(5, 5, 5, 5);
		panel7.add(_codeChangesCheckBox, gbc);
		_detailedIssuesCheckBox = new JCheckBox();
		_detailedIssuesCheckBox.setEnabled(false);
		_detailedIssuesCheckBox.setHorizontalAlignment(2);
		_detailedIssuesCheckBox.setSelected(true);
		_detailedIssuesCheckBox.setText("Detailed Issues");
		gbc = new GridBagConstraints();
		gbc.gridx = 0;
		gbc.gridy = 5;
		gbc.anchor = GridBagConstraints.NORTHWEST;
		gbc.insets = new Insets(5, 5, 5, 5);
		panel7.add(_detailedIssuesCheckBox, gbc);
		_tableOfContentsCheckBox = new JCheckBox();
		_tableOfContentsCheckBox.setEnabled(false);
		_tableOfContentsCheckBox.setHorizontalAlignment(2);
		_tableOfContentsCheckBox.setSelected(true);
		_tableOfContentsCheckBox.setText("Table of Contents");
		gbc = new GridBagConstraints();
		gbc.gridx = 0;
		gbc.gridy = 1;
		gbc.anchor = GridBagConstraints.NORTHWEST;
		gbc.insets = new Insets(5, 5, 5, 5);
		panel7.add(_tableOfContentsCheckBox, gbc);
		_coverPageCheckBox = new JCheckBox();
		_coverPageCheckBox.setEnabled(false);
		_coverPageCheckBox.setHorizontalAlignment(2);
		_coverPageCheckBox.setSelected(true);
		_coverPageCheckBox.setText("Cover Page");
		gbc = new GridBagConstraints();
		gbc.gridx = 0;
		gbc.gridy = 0;
		gbc.anchor = GridBagConstraints.NORTHWEST;
		gbc.insets = new Insets(5, 5, 5, 5);
		panel7.add(_coverPageCheckBox, gbc);
		_standardSummaryStatiticsCheckBox = new JCheckBox();
		_standardSummaryStatiticsCheckBox.setEnabled(false);
		_standardSummaryStatiticsCheckBox.setHorizontalAlignment(2);
		_standardSummaryStatiticsCheckBox.setText("Standard Summary Statitics");
		gbc = new GridBagConstraints();
		gbc.gridx = 0;
		gbc.gridy = 6;
		gbc.anchor = GridBagConstraints.NORTHWEST;
		gbc.insets = new Insets(5, 5, 5, 5);
		panel7.add(_standardSummaryStatiticsCheckBox, gbc);
		final JPanel panel8 = new JPanel();
		panel8.setLayout(new BorderLayout(0, 0));
		panel8.setPreferredSize(new Dimension(100, 0));
		gbc = new GridBagConstraints();
		gbc.gridx = 1;
		gbc.gridy = 0;
		gbc.fill = GridBagConstraints.BOTH;
		panel7.add(panel8, gbc);
		final JScrollPane scrollPane1 = new JScrollPane();
		_panel1.add(scrollPane1, BorderLayout.CENTER);
		_textPane1 = new JTextPane();
		_textPane1.setEditable(false);
		_textPane1.setPreferredSize(new Dimension(800, 250));
		scrollPane1.setViewportView(_textPane1);
	}

	/**
	 * @noinspection ALL
	 */
	public JComponent $$$getRootComponent$$$()
	{
		return _panel1;
	}

	public void fillComboScenarioRuns()
	{
		_ignoreSelectionChange = true;
		_textPane1.setText("");
		_baseComboBox.removeAllItems();
		_altComboBox.removeAllItems();
		ProjectConfigurationPanel projectConfigurationPanel = ProjectConfigurationPanel.getProjectConfigurationPanel();
		List<EpptScenarioRun> allEpptScenarioRuns = projectConfigurationPanel.getAllEpptScenarioRuns();
		EpptScenarioRun baseScenario = projectConfigurationPanel.getBaseScenario();
		allEpptScenarioRuns.forEach(s -> _baseComboBox.addItem(s));
		_altComboBox.addItem(null);
		allEpptScenarioRuns.forEach(s -> _altComboBox.addItem(s));
		if(baseScenario != null)
		{
			_baseComboBox.setSelectedItem(baseScenario);
		}
		_altComboBox.setSelectedItem(null);
		_altComboBox.removeItem(baseScenario);
		_ignoreSelectionChange = false;
		Path currentProject = EpptPreferences.getLastProjectConfiguration();
		Path reports = currentProject.getParent().resolve("Reports");
		_pdfOutput.setText(reports.toString() + projectConfigurationPanel.getProjectName() + ".pdf");
	}

	@Override
	public void runStarted(EpptScenarioRun scenarioRun, Process process)
	{
		_textAreaPrintStream = new TextAreaPrintStream(_textPane1, process.getInputStream(), process.getErrorStream());
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
			_textPane1.setCaretPosition(length + 1);
		}
		catch(BadLocationException e)
		{
			LOGGER.log(Level.SEVERE, "Error appending text", e);
		}
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
		if(_textAreaPrintStream != null)
		{
			_textAreaPrintStream.close();
		}
	}

	private final class ReportHandler extends Handler
	{

		@Override
		public void publish(LogRecord record)
		{
			String loggerName = record.getLoggerName();
			if(loggerName.startsWith(EPPTReport.class.getPackage().getName()))
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
}
