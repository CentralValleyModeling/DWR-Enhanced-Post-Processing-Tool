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

package gov.ca.water.quickresults.ui;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Frame;
import java.awt.GridLayout;
import java.io.InputStream;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.stream.Collectors;
import javax.swing.*;
import javax.swing.border.TitledBorder;

import gov.ca.water.calgui.project.EpptScenarioRun;
import gov.ca.water.calgui.wresl.ProcessOutputConsumer;
import gov.ca.water.calgui.wresl.WreslScriptException;
import gov.ca.water.calgui.wresl.WreslScriptRunner;
import gov.ca.water.quickresults.ui.projectconfig.ProjectConfigurationPanel;
import org.jfree.data.time.Month;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 05-10-2019
 */
public class WreslRunDialog extends JDialog implements ProcessOutputConsumer
{
	private static final Logger LOGGER = Logger.getLogger(WreslRunDialog.class.getName());

	private final JTabbedPane _tabbedPane;
	private final List<TextAreaPrintStream> _textAreaPrintStreams = new ArrayList<>();
	private final JButton _startButton = new JButton("Run WRESL");
	private final JButton _stopButton = new JButton("Stop all WRESL Scripts");
	private final List<Process> _processes = new ArrayList<>();
	private final List<EpptScenarioRunCheckbox> _scenarioRunCheckboxes = new ArrayList<>();
	private JPanel _scenarioPanel;

	public WreslRunDialog(Frame frame)
	{
		super(frame, "Run WRESL Script", false);
		setSize(new Dimension(1200, 500));
		setLocationRelativeTo(frame);
		setLayout(new BorderLayout());
		_tabbedPane = new JTabbedPane();
		_stopButton.addActionListener(e -> destroyProcesses());
		_startButton.addActionListener(e->runSelectedScenarios());
		add(_tabbedPane, BorderLayout.CENTER);
		add(_startButton, BorderLayout.SOUTH);
		Runtime.getRuntime().addShutdownHook(new Thread(this::destroyProcesses));
	}

	public void buildScenarioPanel(List<EpptScenarioRun> scenarioRuns)
	{
		if(_scenarioPanel != null)
		{
			remove(_scenarioPanel);
		}
		_scenarioPanel = new JPanel();
		_scenarioPanel.setBorder(new TitledBorder("Run Scenarios"));
		_scenarioPanel.setLayout(new GridLayout(0,1));
		List<EpptScenarioRunCheckbox> scenarioRunCheckboxes = scenarioRuns
				.stream()
				.map(EpptScenarioRunCheckbox::new)
				.collect(Collectors.toList());
		_scenarioRunCheckboxes.clear();
		_scenarioRunCheckboxes.addAll(scenarioRunCheckboxes);
		_scenarioRunCheckboxes.forEach(_scenarioPanel::add);
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
			_processes.add(process);
			remove(_startButton);
			remove(_stopButton);
			add(_stopButton, BorderLayout.SOUTH);
			JTextPane textArea = new JTextPane();
			textArea.setEditable(false);
			textArea.setBackground(Color.WHITE);
			JScrollPane jsp = new JScrollPane(textArea);
			TextAreaPrintStream textAreaPrintStream = new TextAreaPrintStream(textArea, process.getInputStream(), process.getErrorStream());
			_tabbedPane.addTab(scenarioRun.getName(), jsp);
			_textAreaPrintStreams.add(textAreaPrintStream);
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
				remove(_startButton);
				remove(_stopButton);
				add(_startButton, BorderLayout.SOUTH);
				revalidate();
			}
		});
	}

	private void destroyProcesses()
	{
		_textAreaPrintStreams.forEach(TextAreaPrintStream::close);
		_processes.forEach(Process::destroyForcibly);
		remove(_startButton);
		remove(_stopButton);
		add(_startButton, BorderLayout.SOUTH);
		revalidate();
	}

	@Override
	public void dispose()
	{
		destroyProcesses();
		super.dispose();
	}

	private void runScenario(EpptScenarioRun epptScenarioRun)
	{

		ProjectConfigurationPanel projectConfigurationPanel = ProjectConfigurationPanel.getProjectConfigurationPanel();
		Month startMonth = projectConfigurationPanel.getStartMonth();
		Month endMonth = projectConfigurationPanel.getEndMonth();
		LocalDate start = LocalDate.of(startMonth.getYearValue(), startMonth.getMonth(), 1);
		start = start.withDayOfMonth(start.lengthOfMonth());
		LocalDate end = LocalDate.of(endMonth.getYearValue(), endMonth.getMonth(), 1);
		end = end.withDayOfMonth(end.lengthOfMonth());
		runWresl(epptScenarioRun, start, end);
	}


	private void runWresl(EpptScenarioRun scenarioRun, LocalDate start, LocalDate end)
	{
		CompletableFuture.runAsync(() ->
		{
			try
			{
				WreslScriptRunner wreslScriptRunner = new WreslScriptRunner(scenarioRun, this);
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
