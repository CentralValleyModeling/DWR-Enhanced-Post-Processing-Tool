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
import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Collections;
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
import gov.ca.water.calgui.project.EpptScenarioRun;
import gov.ca.water.calgui.wresl.ProcessOutputConsumer;
import gov.ca.water.quickresults.ui.TextAreaPrintStream;
import gov.ca.water.quickresults.ui.projectconfig.ProjectConfigurationPanel;
import gov.ca.water.reportengine.EPPTReport;
import gov.ca.water.reportengine.QAQCReportException;

import rma.swing.RmaJDecimalField;
import rma.swing.RmaJPanel;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 05-13-2019
 */
public class QAQCReportPanel extends RmaJPanel implements ProcessOutputConsumer
{
	private static final Logger LOGGER = Logger.getLogger(QAQCReportPanel.class.getName());

	private final StyledDocument _doc;
	private final Style _style;
	private JPanel _panel1;
	private JButton _generateReportButton;
	private JTextPane _textPane1 = new JTextPane();
	private JComboBox<EpptScenarioRun> _altComboBox;
	private JComboBox<EpptScenarioRun> _baseComboBox;
	private JTextField _toleranceTextField;
	private JTextField _reportSubtitle;
	private JTextField _textField1;
	private JButton __fileChooserBtn;
	private JCheckBox _excutiveSummaryCheckBox;
	private JCheckBox _assumptionChangesCheckBox;
	private JCheckBox _codeChangesCheckBox;
	private JCheckBox _detailedIssuesCheckBox;
	private JCheckBox _tableOfContentsCheckBox;
	private JCheckBox _coverPageCheckBox;
	private JCheckBox _standardSummaryStatiticsCheckBox;
	private JTextField _authorTextField;
	private TextAreaPrintStream _textAreaPrintStream;

	public QAQCReportPanel()
	{
		$$$setupUI$$$();
		addListeners();
		setLayout(new BorderLayout());
		add($$$getRootComponent$$$(), BorderLayout.CENTER);
		_authorTextField.setText(System.getProperty("user.name"));
		((RmaJDecimalField) _toleranceTextField).setValue(0.01);
		__fileChooserBtn.addActionListener(e -> chooseReportFile());
		_doc = (StyledDocument) _textPane1.getDocument();
		_style = _doc.addStyle("ConsoleStyle", null);
		Logger.getLogger("").addHandler(new Handler()
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
						  }
						  else
						  {
							  appendNormalText(record.getMessage());
						  }
					  }
				  }

				  @Override
				  public void flush()
				  {
					  LOGGER.log(Level.FINE, "NO-OP");
				  }

				  @Override
				  public void close() throws SecurityException
				  {
					  LOGGER.log(Level.FINE, "NO-OP");
				  }
			  });
	}

	private void chooseReportFile()
	{
		JFileChooser jFileChooser = new JFileChooser();
		jFileChooser.setCurrentDirectory(Paths.get(_textField1.getText()).getParent().toFile());
		jFileChooser.setFileFilter(new SimpleFileFilter("PDF"));
		jFileChooser.setDialogTitle("Save Report to PDF");
		jFileChooser.showSaveDialog(this);
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
			Path pathToWriteOut = null;
			EpptScenarioRun baseRun = (EpptScenarioRun) _baseComboBox.getSelectedItem();
			List<EpptScenarioRun> altRuns = Collections.emptyList();
			EpptScenarioRun altRun = (EpptScenarioRun) _altComboBox.getSelectedItem();
			if(altRun != null)
			{
				altRuns = Collections.singletonList(altRun);
			}
			double tolerance = Double.valueOf(_toleranceTextField.getText());
			String author = _authorTextField.getText();
			String subtitle = _reportSubtitle.getText();
			EPPTReport epptReport = new EPPTReport(pathToWriteOut, baseRun, altRuns, tolerance, author, subtitle);
			epptReport.writeReport();
			QAQCProcessRunner processRunner = new QAQCProcessRunner(Paths.get("Ouptut.pdf"),
					Paths.get("C:\\Git\\DWR\\EPPT\\DWR-Enhanced-Post-Processing-Tool\\eppt-jasper-engine\\src\\test\\resources\\QAQC_Report.jrxml"),
					this);
			processRunner.run();
		}
		catch(QAQCReportException | RuntimeException e)
		{
			LOGGER.log(Level.SEVERE, "Unable to generate Report PDF", e);
			appendErrorText("Error: " + e);
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
		_panel1.setPreferredSize(new Dimension(700, 500));
		_panel1.setRequestFocusEnabled(true);
		final JPanel panel1 = new JPanel();
		panel1.setLayout(new FlowLayout(FlowLayout.RIGHT, 5, 5));
		_panel1.add(panel1, BorderLayout.SOUTH);
		_generateReportButton = new JButton();
		_generateReportButton.setText("Generate Report");
		panel1.add(_generateReportButton);
		final JPanel panel2 = new JPanel();
		panel2.setLayout(new BorderLayout(0, 5));
		_panel1.add(panel2, BorderLayout.NORTH);
		final JPanel panel3 = new JPanel();
		panel3.setLayout(new GridBagLayout());
		panel2.add(panel3, BorderLayout.CENTER);
		final JLabel label1 = new JLabel();
		label1.setText("Base Scenario Run:");
		GridBagConstraints gbc;
		gbc = new GridBagConstraints();
		gbc.gridx = 0;
		gbc.gridy = 1;
		gbc.anchor = GridBagConstraints.WEST;
		gbc.insets = new Insets(5, 5, 5, 5);
		panel3.add(label1, gbc);
		final JLabel label2 = new JLabel();
		label2.setText("Alternative Scenario Run:");
		gbc = new GridBagConstraints();
		gbc.gridx = 0;
		gbc.gridy = 2;
		gbc.anchor = GridBagConstraints.WEST;
		gbc.insets = new Insets(5, 5, 5, 5);
		panel3.add(label2, gbc);
		_altComboBox = new JComboBox();
		gbc = new GridBagConstraints();
		gbc.gridx = 1;
		gbc.gridy = 2;
		gbc.anchor = GridBagConstraints.WEST;
		gbc.fill = GridBagConstraints.HORIZONTAL;
		gbc.insets = new Insets(5, 5, 5, 5);
		panel3.add(_altComboBox, gbc);
		_baseComboBox = new JComboBox();
		gbc = new GridBagConstraints();
		gbc.gridx = 1;
		gbc.gridy = 1;
		gbc.anchor = GridBagConstraints.WEST;
		gbc.fill = GridBagConstraints.HORIZONTAL;
		gbc.ipadx = 60;
		gbc.insets = new Insets(5, 5, 5, 5);
		panel3.add(_baseComboBox, gbc);
		final JLabel label3 = new JLabel();
		label3.setText("DSS Compare Tolerance:");
		gbc = new GridBagConstraints();
		gbc.gridx = 0;
		gbc.gridy = 3;
		gbc.anchor = GridBagConstraints.WEST;
		gbc.insets = new Insets(5, 5, 5, 5);
		panel3.add(label3, gbc);
		_toleranceTextField.setText("");
		gbc = new GridBagConstraints();
		gbc.gridx = 1;
		gbc.gridy = 3;
		gbc.anchor = GridBagConstraints.WEST;
		gbc.fill = GridBagConstraints.HORIZONTAL;
		gbc.insets = new Insets(5, 5, 5, 5);
		panel3.add(_toleranceTextField, gbc);
		_reportSubtitle = new JTextField();
		_reportSubtitle.setText("Subtitle");
		gbc = new GridBagConstraints();
		gbc.gridx = 1;
		gbc.gridy = 0;
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
		gbc.gridy = 4;
		gbc.anchor = GridBagConstraints.WEST;
		gbc.insets = new Insets(5, 5, 5, 5);
		panel3.add(label5, gbc);
		final JPanel panel4 = new JPanel();
		panel4.setLayout(new BorderLayout(2, 0));
		gbc = new GridBagConstraints();
		gbc.gridx = 1;
		gbc.gridy = 4;
		gbc.fill = GridBagConstraints.BOTH;
		gbc.insets = new Insets(5, 5, 5, 5);
		panel3.add(panel4, gbc);
		_textField1 = new JTextField();
		_textField1.setEditable(false);
		_textField1.setPreferredSize(new Dimension(200, 24));
		panel4.add(_textField1, BorderLayout.CENTER);
		__fileChooserBtn = new JButton();
		__fileChooserBtn.setPreferredSize(new Dimension(30, 24));
		__fileChooserBtn.setText("...");
		panel4.add(__fileChooserBtn, BorderLayout.EAST);
		final JLabel label6 = new JLabel();
		label6.setText("Author:");
		gbc = new GridBagConstraints();
		gbc.gridx = 0;
		gbc.gridy = 5;
		gbc.anchor = GridBagConstraints.WEST;
		gbc.insets = new Insets(5, 5, 5, 5);
		panel3.add(label6, gbc);
		_authorTextField = new JTextField();
		gbc = new GridBagConstraints();
		gbc.gridx = 1;
		gbc.gridy = 5;
		gbc.anchor = GridBagConstraints.WEST;
		gbc.fill = GridBagConstraints.HORIZONTAL;
		gbc.insets = new Insets(5, 5, 5, 5);
		panel3.add(_authorTextField, gbc);
		final JPanel panel5 = new JPanel();
		panel5.setLayout(new GridBagLayout());
		panel5.setPreferredSize(new Dimension(300, 200));
		panel2.add(panel5, BorderLayout.EAST);
		panel5.setBorder(BorderFactory.createTitledBorder(BorderFactory.createEtchedBorder(), "Modules"));
		_excutiveSummaryCheckBox = new JCheckBox();
		_excutiveSummaryCheckBox.setEnabled(false);
		_excutiveSummaryCheckBox.setHorizontalAlignment(2);
		_excutiveSummaryCheckBox.setSelected(true);
		_excutiveSummaryCheckBox.setText("Excutive Summary");
		gbc = new GridBagConstraints();
		gbc.gridx = 0;
		gbc.gridy = 2;
		gbc.anchor = GridBagConstraints.WEST;
		panel5.add(_excutiveSummaryCheckBox, gbc);
		_assumptionChangesCheckBox = new JCheckBox();
		_assumptionChangesCheckBox.setEnabled(false);
		_assumptionChangesCheckBox.setHorizontalAlignment(2);
		_assumptionChangesCheckBox.setText("Assumption Changes");
		gbc = new GridBagConstraints();
		gbc.gridx = 0;
		gbc.gridy = 3;
		gbc.anchor = GridBagConstraints.WEST;
		panel5.add(_assumptionChangesCheckBox, gbc);
		_codeChangesCheckBox = new JCheckBox();
		_codeChangesCheckBox.setEnabled(false);
		_codeChangesCheckBox.setHorizontalAlignment(2);
		_codeChangesCheckBox.setText("Code Changes");
		gbc = new GridBagConstraints();
		gbc.gridx = 0;
		gbc.gridy = 4;
		gbc.anchor = GridBagConstraints.WEST;
		panel5.add(_codeChangesCheckBox, gbc);
		_detailedIssuesCheckBox = new JCheckBox();
		_detailedIssuesCheckBox.setEnabled(false);
		_detailedIssuesCheckBox.setHorizontalAlignment(2);
		_detailedIssuesCheckBox.setSelected(true);
		_detailedIssuesCheckBox.setText("Detailed Issues");
		gbc = new GridBagConstraints();
		gbc.gridx = 0;
		gbc.gridy = 5;
		gbc.anchor = GridBagConstraints.WEST;
		panel5.add(_detailedIssuesCheckBox, gbc);
		_tableOfContentsCheckBox = new JCheckBox();
		_tableOfContentsCheckBox.setEnabled(false);
		_tableOfContentsCheckBox.setHorizontalAlignment(2);
		_tableOfContentsCheckBox.setSelected(true);
		_tableOfContentsCheckBox.setText("Table of Contents");
		gbc = new GridBagConstraints();
		gbc.gridx = 0;
		gbc.gridy = 1;
		gbc.anchor = GridBagConstraints.WEST;
		panel5.add(_tableOfContentsCheckBox, gbc);
		_coverPageCheckBox = new JCheckBox();
		_coverPageCheckBox.setEnabled(false);
		_coverPageCheckBox.setHorizontalAlignment(2);
		_coverPageCheckBox.setSelected(true);
		_coverPageCheckBox.setText("Cover Page");
		gbc = new GridBagConstraints();
		gbc.gridx = 0;
		gbc.gridy = 0;
		gbc.anchor = GridBagConstraints.WEST;
		panel5.add(_coverPageCheckBox, gbc);
		_standardSummaryStatiticsCheckBox = new JCheckBox();
		_standardSummaryStatiticsCheckBox.setEnabled(false);
		_standardSummaryStatiticsCheckBox.setHorizontalAlignment(2);
		_standardSummaryStatiticsCheckBox.setText("Standard Summary Statitics");
		gbc = new GridBagConstraints();
		gbc.gridx = 0;
		gbc.gridy = 6;
		gbc.anchor = GridBagConstraints.WEST;
		panel5.add(_standardSummaryStatiticsCheckBox, gbc);
		final JPanel panel6 = new JPanel();
		panel6.setLayout(new BorderLayout(0, 0));
		panel6.setPreferredSize(new Dimension(100, 0));
		gbc = new GridBagConstraints();
		gbc.gridx = 1;
		gbc.gridy = 0;
		gbc.fill = GridBagConstraints.BOTH;
		panel5.add(panel6, gbc);
		final JScrollPane scrollPane1 = new JScrollPane();
		_panel1.add(scrollPane1, BorderLayout.CENTER);
		_textPane1 = new JTextPane();
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
			_textField1.setText(baseScenario.getOutputPath().resolve("QAQC_EPPT_" + baseScenario.getName() + ".pdf").toString());
		}
		_altComboBox.setSelectedItem(null);
	}

	@Override
	public void runStarted(EpptScenarioRun scenarioRun, Process process)
	{
		_textAreaPrintStream = new TextAreaPrintStream(_textPane1, process.getInputStream(), process.getErrorStream());
	}


	public void appendErrorText(String str)
	{
		SwingUtilities.invokeLater(() ->
		{
			StyleConstants.setForeground(_style, Color.red);
			appendText(str);
		});
	}

	public void appendNormalText(String str)
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
				Desktop.getDesktop().open(Paths.get("Ouptut.pdf").toFile());
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
}
