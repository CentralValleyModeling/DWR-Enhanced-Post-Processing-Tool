/*
 * Enhanced Post Processing Tool (EPPT) Copyright (c) 2020.
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
import java.awt.FlowLayout;
import java.awt.GridLayout;
import java.nio.file.Path;
import java.time.LocalDate;
import java.time.YearMonth;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.stream.Collectors;
import javax.swing.*;
import javax.swing.border.TitledBorder;

import gov.ca.water.calgui.bo.WaterYearDefinition;
import gov.ca.water.calgui.bo.WaterYearPeriod;
import gov.ca.water.calgui.bo.WaterYearPeriodRange;
import gov.ca.water.calgui.bo.WaterYearType;
import gov.ca.water.calgui.project.EpptScenarioRun;
import gov.ca.water.calgui.wresl.ProcessOutputConsumer;
import gov.ca.water.calgui.wresl.WreslScriptException;
import gov.ca.water.calgui.wresl.WreslScriptRunner;
import gov.ca.water.quickresults.ui.TextAreaPrintStream;
import gov.ca.water.calgui.project.EpptConfigurationController;

import rma.swing.RmaJPanel;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 07-05-2019
 */
public class WreslPanel extends RmaJPanel implements ProcessOutputConsumer
{
	private static final Logger LOGGER = Logger.getLogger(WreslPanel.class.getName());

	private final JTabbedPane _tabbedPane;
	private final List<TextAreaPrintStream> _textAreaPrintStreams = new ArrayList<>();
	private final JButton _startButton = new JButton("Run WRESL");
	private final JButton _stopButton = new JButton("Stop All WRESL Scripts");
	private final List<Process> _processes = new ArrayList<>();
	private final List<EpptScenarioRunCheckbox> _scenarioRunCheckboxes = new ArrayList<>();
	private final JProgressBar _progressBar;
	private final JPanel _southPanel;
	private final EpptConfigurationController _epptConfigurationController;
	private JPanel _scenarioPanel;

	public WreslPanel(EpptConfigurationController epptConfigurationController)
	{
		_epptConfigurationController = epptConfigurationController;
		setLayout(new BorderLayout());
		_tabbedPane = new JTabbedPane();
		_stopButton.addActionListener(e -> destroyProcesses());
		_startButton.addActionListener(e -> runSelectedScenarios());
		add(_tabbedPane, BorderLayout.CENTER);
		_southPanel = new JPanel();
		_southPanel.setLayout(new BorderLayout());
		_southPanel.add(_startButton, BorderLayout.CENTER);
		_progressBar = new JProgressBar();
		_progressBar.setVisible(false);
		_southPanel.add(_progressBar, BorderLayout.NORTH);
		add(_southPanel, BorderLayout.SOUTH);
	}

	public void buildScenarioPanel(List<EpptScenarioRun> scenarioRuns)
	{
		if(_scenarioPanel != null)
		{
			remove(_scenarioPanel);
		}
		_scenarioPanel = new JPanel(new FlowLayout(FlowLayout.LEFT));
		JPanel panel = new JPanel();
		panel.setBorder(new TitledBorder("Run Scenarios"));
		panel.setLayout(new GridLayout(0, 3));
		List<EpptScenarioRunCheckbox> scenarioRunCheckboxes = scenarioRuns
				.stream()
				.map(EpptScenarioRunCheckbox::new)
				.collect(Collectors.toList());
		_scenarioRunCheckboxes.clear();
		_scenarioRunCheckboxes.addAll(scenarioRunCheckboxes);
		_scenarioRunCheckboxes.forEach(comp -> addScenarioRunCheckboxPanel(comp, panel));
		_scenarioPanel.add(panel);
		add(_scenarioPanel, BorderLayout.NORTH);
	}

	private void runSelectedScenarios()
	{
		_tabbedPane.removeAll();
		_scenarioRunCheckboxes.stream()
							  .filter(JCheckBox::isSelected)
							  .map(EpptScenarioRunCheckbox::getScenarioRun)
							  .forEach(this::runScenario);
	}

	@Override
	public void runStarted(EpptScenarioRun scenarioRun, Process process)
	{
		SwingUtilities.invokeLater(() ->
		{
			_progressBar.setIndeterminate(true);
			_progressBar.setVisible(true);
			_processes.add(process);
			_southPanel.remove(_startButton);
			_southPanel.remove(_stopButton);
			_southPanel.add(_stopButton, BorderLayout.SOUTH);
			JTextPane textArea = new JTextPane();
			textArea.setEditable(false);
			textArea.setBackground(Color.WHITE);
			JScrollPane jsp = new JScrollPane(textArea);
			TextAreaPrintStream textAreaPrintStream = new TextAreaPrintStream(textArea, process.getInputStream(), process.getErrorStream());
			_tabbedPane.addTab(scenarioRun.getName(), jsp);
			_textAreaPrintStreams.add(textAreaPrintStream);
			repaint();
			revalidate();
		});
	}

	@Override
	public void runFinished(Process process)
	{
		SwingUtilities.invokeLater(() ->
		{
			_processes.remove(process);
			if(_processes.isEmpty())
			{
				destroyProcesses();
			}
		});
	}

	public void destroyProcesses()
	{
		_progressBar.setIndeterminate(false);
		_progressBar.setVisible(false);
		_southPanel.remove(_startButton);
		_southPanel.remove(_stopButton);
		_southPanel.add(_startButton, BorderLayout.SOUTH);
		repaint();
		revalidate();
		_textAreaPrintStreams.forEach(TextAreaPrintStream::close);
		_processes.forEach(Process::destroyForcibly);
	}

	private void runScenario(EpptScenarioRun epptScenarioRun)
	{
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
		runWresl(epptScenarioRun, start, end);
	}


	private void runWresl(EpptScenarioRun scenarioRun, LocalDate start, LocalDate end)
	{
		CompletableFuture.runAsync(() ->
		{
			try
			{
				Path wreslMain = QaQcFileUtils.createWreslMain(scenarioRun, false);
				WreslScriptRunner wreslScriptRunner = new WreslScriptRunner(scenarioRun, wreslMain,this);
				wreslScriptRunner.run(start, end);
			}
			catch(WreslScriptException | RuntimeException e1)
			{
				LOGGER.log(Level.SEVERE, "Error in WRESL Script Run", e1);
			}
		}).whenComplete((v, t) ->
		{
			if(t != null)
			{
				LOGGER.log(Level.SEVERE, "Error in WRESL Script Run", t);
			}
		});
	}

	private void addScenarioRunCheckboxPanel(EpptScenarioRunCheckbox comp, JPanel parent)
	{
		JLabel warnText = new JLabel();
		warnText.setText("*WRESL Script Directory differs from installation");
		EpptScenarioRun scenarioRun = comp.getScenarioRun();
		parent.add(comp);
		JButton button = new JButton("Overwrite WRESL Scripts");
		button.addActionListener(e->QaQcFileUtils.createWreslMain(scenarioRun, true));
		parent.add(button);
		parent.add(warnText);
		QAQCReportPanel.showWreslScriptChangedWarning(warnText, comp._scenarioRun);
	}

	private static final class EpptScenarioRunCheckbox extends JCheckBox
	{
		private final EpptScenarioRun _scenarioRun;

		private EpptScenarioRunCheckbox(EpptScenarioRun scenarioRun)
		{
			super(scenarioRun.getName());
			_scenarioRun = scenarioRun;
		}

		private EpptScenarioRun getScenarioRun()
		{
			return _scenarioRun;
		}
	}
}
