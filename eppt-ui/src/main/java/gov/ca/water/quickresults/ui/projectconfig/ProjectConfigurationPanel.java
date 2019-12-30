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

package gov.ca.water.quickresults.ui.projectconfig;

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.Container;
import java.awt.Frame;
import java.awt.KeyboardFocusManager;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.nio.file.Path;
import java.time.LocalDate;
import java.time.temporal.TemporalAdjusters;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import javax.swing.*;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.text.DefaultFormatter;

import gov.ca.water.calgui.bo.ResultUtilsBO;
import gov.ca.water.calgui.busservice.ScenarioChangeListener;
import gov.ca.water.calgui.constant.Constant;
import gov.ca.water.calgui.constant.EpptPreferences;
import gov.ca.water.calgui.project.EpptProject;
import gov.ca.water.calgui.project.EpptScenarioRun;
import gov.ca.water.calgui.project.EpptScenarioRunValidator;
import gov.ca.water.calgui.project.PlotConfigurationState;
import gov.ca.water.quickresults.ui.EpptPanel;
import gov.ca.water.quickresults.ui.projectconfig.scenarioconfig.ScenarioRunEditor;
import gov.ca.water.quickresults.ui.projectconfig.scenariotable.ScenarioTablePanel;
import gov.ca.water.quickresults.ui.quickresults.PlotConfigurationStateBuilder;
import javafx.application.Platform;
import org.apache.log4j.Logger;

import rma.util.RMAUtil;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 02-21-2019
 */
public final class ProjectConfigurationPanel extends EpptPanel
{
	private static final Logger LOGGER = Logger.getLogger(ProjectConfigurationPanel.class.getName());
	private static final String SCENARIO_CONFIGURATION_XML_FILE = "Project_Configuration.xml";
	private static ProjectConfigurationPanel instance;
	private final Set<ScenarioChangeListener> _scenarioChangeListeners = new HashSet<>();
	private final ProjectConfigurationIO _projectConfigurationIO = new ProjectConfigurationIO();
	private final ScenarioTablePanel _scenarioTablePanel = new ScenarioTablePanel();
	private boolean _ignoreModifiedEvents = false;

	private ProjectConfigurationPanel()
	{
		try
		{
			super.setLayout(new BorderLayout());
			Container swixmlProjectConfigurationPanel = renderSwixml(SCENARIO_CONFIGURATION_XML_FILE);
			super.add(swixmlProjectConfigurationPanel, BorderLayout.CENTER);
			initComponents();
			initModels();
			initListeners();
		}
		catch(Exception e)
		{
			LOGGER.error("Error setting up quick results swing xml: " + SCENARIO_CONFIGURATION_XML_FILE, e);
			throw new IllegalStateException(e);
		}
	}

	/**
	 * This method is for unit testing purposes only
	 *
	 * @return a new ProjectConfigurationPanel with UI initialized
	 */
	static ProjectConfigurationPanel createProjectConfigurationPanel()
	{
		return new ProjectConfigurationPanel();
	}

	public static synchronized ProjectConfigurationPanel getProjectConfigurationPanel()
	{
		if(instance == null)
		{
			synchronized(ProjectConfigurationPanel.class)
			{
				if(instance == null)
				{
					instance = new ProjectConfigurationPanel();
				}
			}
		}
		return instance;
	}

	private void initComponents()
	{
		initScenarioTree();
		revalidate();
	}

	private void initScenarioTree()
	{
		Component component = getSwingEngine().find("ScenarioTree");
		if(component instanceof JPanel)
		{
			JPanel treePanel = (JPanel) component;
			treePanel.setLayout(new BorderLayout());
			treePanel.add(_scenarioTablePanel, BorderLayout.CENTER);
		}
	}

	private void setSummaryTableEnabled(boolean selected, Container container)
	{
		container.setEnabled(selected);
		for(Component component : container.getComponents())
		{
			component.setEnabled(selected);
			if(component instanceof Container)
			{
				setSummaryTableEnabled(selected, (Container) component);
			}
		}
	}

	private void initModels()
	{
		initializeSpinners();
	}

	private void initListeners()
	{
		JCheckBox summaryTableCheckbox = (JCheckBox) getSwingEngine().find("RepckbSummaryTable");
		summaryTableCheckbox.addActionListener(e ->
		{
			boolean selected = summaryTableCheckbox.isSelected();
			Container controls3 = (Container) getSwingEngine().find("controls3");
			setSummaryTableEnabled(selected, controls3);
		});

		DocumentListener documentListener = new DocumentListener()
		{
			@Override
			public void insertUpdate(DocumentEvent e)
			{
				ProjectConfigurationPanel.this.setModified(true);
			}

			@Override
			public void removeUpdate(DocumentEvent e)
			{
				ProjectConfigurationPanel.this.setModified(true);
			}

			@Override
			public void changedUpdate(DocumentEvent e)
			{
				ProjectConfigurationPanel.this.setModified(true);
			}
		};
		JTextField projectNameField = (JTextField) getSwingEngine().find("prj_name");
		JTextField descriptionField = (JTextField) getSwingEngine().find("prj_desc");
		projectNameField.getDocument().addDocumentListener(documentListener);
		descriptionField.getDocument().addDocumentListener(documentListener);
		JSpinner spnSM = (JSpinner) getSwingEngine().find("spnStartMonth");
		spnSM.addChangeListener(e -> setModified(true));
		JSpinner spnEM = (JSpinner) getSwingEngine().find("spnEndMonth");
		spnEM.addChangeListener(e -> setModified(true));
		JSpinner spnSY = (JSpinner) getSwingEngine().find("spnStartYear");
		spnSY.addChangeListener(e -> setModified(true));
		JSpinner spnEY = (JSpinner) getSwingEngine().find("spnEndYear");
		spnEY.addChangeListener(e -> setModified(true));
		JCheckBox tafCheckBox = ((JCheckBox) getSwingEngine().find("chkTAF"));
		tafCheckBox.addActionListener(e -> setModified(true));
	}

	private void initializeSpinners()
	{
		// Set up month spinners on result page
		JSpinner spnSM = (JSpinner) getSwingEngine().find("spnStartMonth");
		ResultUtilsBO.SetMonthModelAndIndex(spnSM, 9, null, true);
		JSpinner spnEM = (JSpinner) getSwingEngine().find("spnEndMonth");
		ResultUtilsBO.SetMonthModelAndIndex(spnEM, 8, null, true);
		// Set up year spinners
		JSpinner spnSY = (JSpinner) getSwingEngine().find("spnStartYear");
		ResultUtilsBO.SetNumberModelAndIndex(spnSY, 1921, 1921, 2003, 1, "####", null, true);
		JSpinner spnEY = (JSpinner) getSwingEngine().find("spnEndYear");
		ResultUtilsBO.SetNumberModelAndIndex(spnEY, 2003, 1921, 2003, 1, "####", null, true);
		makeSpinnerCommitOnEdit(spnSM);
		makeSpinnerCommitOnEdit(spnEM);
		makeSpinnerCommitOnEdit(spnSY);
		makeSpinnerCommitOnEdit(spnEY);
		JFormattedTextField textField = ((JSpinner.DefaultEditor) spnSM.getEditor()).getTextField();
		textField.addKeyListener(new MyKeyAdapter(textField));
		JFormattedTextField textField1 = ((JSpinner.DefaultEditor) spnEM.getEditor()).getTextField();
		textField1.addKeyListener(new MyKeyAdapter(textField1));
		JFormattedTextField textField2 = ((JSpinner.DefaultEditor) spnSY.getEditor()).getTextField();
		textField2.addKeyListener(new MyKeyAdapter(textField2));
		JFormattedTextField textField3 = ((JSpinner.DefaultEditor) spnEY.getEditor()).getTextField();
		textField3.addKeyListener(new MyKeyAdapter(textField3));
	}

	private void makeSpinnerCommitOnEdit(JSpinner spinner)
	{
		JComponent comp = spinner.getEditor();
		if(comp.getComponents().length > 0)
		{
			Component component = comp.getComponent(0);
			if(component instanceof JFormattedTextField)
			{
				JFormattedTextField field = (JFormattedTextField) component;
				JFormattedTextField.AbstractFormatter formatter = field.getFormatter();
				if(formatter instanceof DefaultFormatter)
				{
					DefaultFormatter defaultFormatter = (DefaultFormatter) formatter;
					defaultFormatter.setCommitsOnValidEdit(true);
				}
			}
		}
	}

	@Override
	public String getJavaHelpId()
	{
		return "3.1_ProjectConfiguration.htm";
	}

	JPanel getControls2()
	{
		return (JPanel) getSwingEngine().find("controls2");
	}


	void updateRadioState()
	{
		SwingUtilities.invokeLater(() ->
		{
			_scenarioChangeListeners.forEach(this::postScenarioChanged);
			EpptScenarioRun base = _scenarioTablePanel.getBaseScenarioRun();
			List<EpptScenarioRun> alternatives = _scenarioTablePanel.getAlternativeScenarioRuns();
			getRadioButtonDiff().setEnabled(!alternatives.isEmpty() && base != null);
			getRadioButtonComparison().setEnabled(!alternatives.isEmpty() && base != null);
			if(alternatives.isEmpty())
			{
				getRadioButtonBase().setSelected(true);
				getRadioButtonComparison().setSelected(false);
				getRadioButtonDiff().setSelected(false);
			}
		});
	}

	void clearAllScenarios()
	{
		if(JOptionPane.YES_OPTION == JOptionPane.showConfirmDialog(SwingUtilities.windowForComponent(this),
				"Are you sure you want to delete all Scenario Runs?\nThis operation cannot be undone.",
				"Clear All", JOptionPane.YES_NO_OPTION, JOptionPane.WARNING_MESSAGE))
		{
			_scenarioTablePanel.clearScenarios();
			setModified(true);
		}
	}

	public Path getSelectedDssPath()
	{
		return _scenarioTablePanel.getSelectedDssFile();
	}

	private void postScenarioChanged(ScenarioChangeListener scenarioChangeListener)
	{
		EpptScenarioRun base = _scenarioTablePanel.getBaseScenarioRun();
		List<EpptScenarioRun> alternatives = _scenarioTablePanel.getAlternativeScenarioRuns();
		if(scenarioChangeListener != null)
		{
			try
			{
				scenarioChangeListener.fillScenarioRuns(base, alternatives);
			}
			catch(RuntimeException e)
			{
				LOGGER.error(e);
			}
		}
	}

	private void checkValidScenarioRuns()
	{
		for(EpptScenarioRun epptScenarioRun : _scenarioTablePanel.getAllScenarioRuns())
		{
			EpptScenarioRunValidator validator = new EpptScenarioRunValidator(epptScenarioRun);
			if(!validator.isValid())
			{
				StringBuilder builder = new StringBuilder("Scenario Run: ")
						.append(epptScenarioRun.getName())
						.append(" is invalid. Would you like to edit?");
				validator.getErrors().forEach(s -> builder.append("\n").append(s));
				Frame frame = Frame.getFrames()[0];
				int response = JOptionPane.showConfirmDialog(frame, builder.toString(), "Misconfigured Scenario Run",
						JOptionPane.YES_NO_OPTION);
				if(response == JOptionPane.YES_OPTION)
				{
					try
					{
						SwingUtilities.invokeAndWait(() ->
						{
							ScenarioRunEditor scenarioRunEditor = new ScenarioRunEditor(frame);
							scenarioRunEditor.fillPanel(epptScenarioRun);
							scenarioRunEditor.setVisible(true);
							replaceScenario(epptScenarioRun, scenarioRunEditor.createRun());
						});
					}
					catch(InterruptedException e)
					{
						LOGGER.info("Thread interrupted", e);
						Thread.currentThread().interrupt();
					}
					catch(InvocationTargetException e)
					{
						LOGGER.error("Error updating Scenario run: " + epptScenarioRun, e);
					}
				}
			}
		}
	}

	public void addScenarioChangedListener(ScenarioChangeListener scenarioChangeListener)
	{
		_scenarioChangeListeners.add(scenarioChangeListener);
	}

	void deleteScenario()
	{
		EpptScenarioRun selectedScenario = _scenarioTablePanel.getSelectedScenario();
		if(selectedScenario != null)
		{
			int clear = JOptionPane.showConfirmDialog(SwingUtilities.windowForComponent(this),
					"Are you sure you want to delete Scenario Runs: " + selectedScenario
							+ "?\nThis operation cannot be undone.",
					"Clear", JOptionPane.YES_NO_OPTION, JOptionPane.WARNING_MESSAGE);
			if(JOptionPane.YES_OPTION == clear)
			{
				_scenarioTablePanel.deleteSelectedScenarioRun();
				setModified(true);
			}
		}
	}

	public String quickStateString()
	{
		PlotConfigurationStateBuilder builder = new PlotConfigurationStateBuilder(getSwingEngine());
		return builder.createQuickStateString();
	}

	public PlotConfigurationState plotConfigurationState()
	{
		PlotConfigurationStateBuilder builder = new PlotConfigurationStateBuilder(getSwingEngine());
		return builder.createPlotConfigurationState();
	}

	@Override
	public void setModified(boolean b)
	{
		if(!_ignoreModifiedEvents)
		{
			updateRadioState();
			super.setModified(b);
			RMAUtil.setParentModified(this);
		}
	}

	public LocalDate getStartMonth()
	{
		JSpinner monthSpinner = (JSpinner) getSwingEngine().find("spnStartMonth");
		int month = ResultUtilsBO.getResultUtilsInstance().monthToInt(monthSpinner.getValue().toString());
		JSpinner yearSpinner = (JSpinner) getSwingEngine().find("spnStartYear");
		int year = Integer.parseInt(yearSpinner.getValue().toString());
		return LocalDate.of(year, month, 1);
	}

	private void setStartMonth(LocalDate start)
	{
		JSpinner monthSpinner = (JSpinner) getSwingEngine().find("spnStartMonth");
		monthSpinner.setValue(ResultUtilsBO.getResultUtilsInstance().intToMonth(start.getMonth().getValue()));
		JSpinner yearSpinner = (JSpinner) getSwingEngine().find("spnStartYear");
		yearSpinner.setValue(start.getYear());
	}

	public void saveConfigurationToPath(Path selectedPath, String projectName, String projectDescription)
			throws IOException
	{
		boolean mkdirs = selectedPath.getParent().toFile().mkdirs();
		if(!mkdirs)
		{
			LOGGER.debug("Path not created: " + selectedPath);
		}
		_projectConfigurationIO.saveConfiguration(selectedPath, projectName, projectDescription);
		EpptPreferences.setLastProjectConfiguration(selectedPath);
	}

	public void saveAsConfigurationToPath(Path newProjectPath, String projectName, String projectDescription)
			throws IOException
	{
		boolean mkdirs = newProjectPath.toFile().mkdirs();
		if(!mkdirs)
		{
			LOGGER.debug("Path not created: " + newProjectPath);
		}
		Path projectFile = newProjectPath.resolve(projectName + "." + Constant.EPPT_EXT);
		Path oldProjectPath = EpptPreferences.getLastProjectConfiguration().getParent();
		_scenarioTablePanel.relativizeScenariosToNewProject(newProjectPath, oldProjectPath);
		_projectConfigurationIO.saveConfiguration(projectFile, projectName, projectDescription);
		EpptPreferences.setLastProjectConfiguration(projectFile);
	}

	public void loadProjectConfiguration(Path selectedPath) throws IOException
	{
		if(selectedPath.toFile().exists())
		{
			try
			{
				_ignoreModifiedEvents = true;

				EpptProject project = _projectConfigurationIO.loadConfiguration(
						selectedPath);
				EpptPreferences.setLastProjectConfiguration(selectedPath);
				JTextField projectNameField = (JTextField) getSwingEngine().find("prj_name");
				JTextField descriptionField = (JTextField) getSwingEngine().find("prj_desc");
				projectNameField.setText(project.getName());
				descriptionField.setText(project.getDescription());
				setStartMonth(project.getStartMonth());
				setEndMonth(project.getEndMonth());
				updateSelectedComponents(project.getSelectedComponents());
				_scenarioTablePanel.clearScenarios();
				List<EpptScenarioRun> scenarioRuns = project.getScenarioRuns();
				boolean hasBase = scenarioRuns.stream().anyMatch(EpptScenarioRun::isBaseSelected);
				boolean hasAlt = scenarioRuns.stream().anyMatch(EpptScenarioRun::isAltSelected);

				if(!hasBase && !scenarioRuns.isEmpty())
				{
					scenarioRuns.get(0).setBaseSelected(true);
				}
				if(!hasAlt && scenarioRuns.size() > 1)
				{
					scenarioRuns.get(1).setAltSelected(true);
				}
				addScenarios(scenarioRuns);

				//Need to ensure this is called after scenarios are added to TreeTable model
				Platform.runLater(() -> SwingUtilities.invokeLater(this::updateRadioState));
			}
			catch(RuntimeException ex)
			{
				LOGGER.error("Error loading project configuration", ex);
			}
			finally
			{
				_ignoreModifiedEvents = false;
			}
		}
	}

	private void addScenarios(List<EpptScenarioRun> scenarioRuns)
	{
		scenarioRuns.forEach(_scenarioTablePanel::addScenarioRun);
		Platform.runLater(() ->
		{
			checkValidScenarioRuns();
			SwingUtilities.invokeLater(ProjectConfigurationPanel.this::updateRadioState);
		});
	}

	private void updateSelectedComponents(Map<String, Boolean> selectedComponents)
	{
		for(Map.Entry<String, Boolean> entry : selectedComponents.entrySet())
		{
			String key = entry.getKey();
			Boolean selected = entry.getValue();
			Component component = getSwingEngine().find(key);
			if(component instanceof JCheckBox)
			{
				((JCheckBox) component).setSelected(selected);
			}
			else if(component instanceof JRadioButton)
			{
				((JRadioButton) component).setSelected(selected);
			}
		}
	}

	public boolean isTaf()
	{
		return ((JCheckBox) getSwingEngine().find("chkTAF")).isSelected();
	}

	private JRadioButton getRadioButtonBase()
	{
		return (JRadioButton) getSwingEngine().find("rdbp000");
	}

	private JRadioButton getRadioButtonComparison()
	{
		return (JRadioButton) getSwingEngine().find("rdbp001");
	}

	private JRadioButton getRadioButtonDiff()
	{
		return (JRadioButton) getSwingEngine().find("rdbp002");
	}

	public LocalDate getEndMonth()
	{
		JSpinner monthSpinner = (JSpinner) getSwingEngine().find("spnEndMonth");
		int month = ResultUtilsBO.getResultUtilsInstance().monthToInt(monthSpinner.getValue().toString());
		JSpinner yearSpinner = (JSpinner) getSwingEngine().find("spnEndYear");
		int year = Integer.parseInt(yearSpinner.getValue().toString());
		return (LocalDate) TemporalAdjusters.lastDayOfMonth().adjustInto(LocalDate.of(year, month, 1));
	}

	private void setEndMonth(LocalDate end)
	{
		JSpinner monthSpinner = (JSpinner) getSwingEngine().find("spnEndMonth");
		monthSpinner.setValue(ResultUtilsBO.getResultUtilsInstance().intToMonth(end.getMonth().getValue()));
		JSpinner yearSpinner = (JSpinner) getSwingEngine().find("spnEndYear");
		yearSpinner.setValue(end.getYear());
	}

	public String getProjectName()
	{
		JTextField projectNameField = (JTextField) getSwingEngine().find("prj_name");
		return projectNameField.getText();
	}

	public String getProjectDescription()
	{
		JTextField descriptionField = (JTextField) getSwingEngine().find("prj_desc");
		return descriptionField.getText();
	}

	public void resetProjectConfiguration() throws Exception
	{
		removeAll();
		Container swixmlProjectConfigurationPanel = renderSwixml(SCENARIO_CONFIGURATION_XML_FILE);
		super.add(swixmlProjectConfigurationPanel, BorderLayout.CENTER);
		initComponents();
		initModels();
		_scenarioTablePanel.clearScenarios();
		setActionListener(getActionListener());
		JSplitPane splitPane = (JSplitPane)getSwingEngine().find("split_pane");
		splitPane.setDividerLocation(350);
	}

	public EpptScenarioRun getBaseScenario()
	{
		return _scenarioTablePanel.getBaseScenarioRun();
	}

	EpptScenarioRun getSelectedScenario()
	{
		return _scenarioTablePanel.getSelectedScenario();
	}

	void addScenario(EpptScenarioRun scenarioRun)
	{
		_scenarioTablePanel.addScenarioRun(scenarioRun);
		updateRadioState();
		setModified(true);
	}

	public List<EpptScenarioRun> getEpptScenarioAlternatives()
	{
		return _scenarioTablePanel.getAlternativeScenarioRuns();
	}

	public List<EpptScenarioRun> getAllEpptScenarioRuns()
	{
		return new ArrayList<>(_scenarioTablePanel.getAllScenarioRuns());
	}

	public ScenarioTablePanel getScenarioTablePanel()
	{
		return _scenarioTablePanel;
	}

	void replaceScenario(EpptScenarioRun oldScenarioRun, EpptScenarioRun newScenarioRun)
	{
		_scenarioTablePanel.updateScenario(oldScenarioRun, newScenarioRun);
		if(!Objects.equals(oldScenarioRun, newScenarioRun))
		{
			setModified(true);
		}
	}

	void moveSelectedScenarioUp()
	{
		_scenarioTablePanel.moveSelectedScenarioUp();
		setModified(true);
	}

	void moveSelectedScenarioDown()
	{
		_scenarioTablePanel.moveSelectedScenarioDown();
		setModified(true);
	}

	private static final class MyKeyAdapter extends KeyAdapter
	{
		private final Component _component;

		private MyKeyAdapter(Component component)
		{
			_component = component;
		}

		@Override
		public void keyPressed(KeyEvent evt)
		{
			int key = evt.getKeyCode();
			if(key == KeyEvent.VK_ENTER
					|| key == KeyEvent.VK_TAB)
			{
				_component.transferFocus();
				SwingUtilities.invokeLater(() ->
				{
					Component focusOwner = KeyboardFocusManager.getCurrentKeyboardFocusManager().getFocusOwner();
					if(focusOwner instanceof JTextField)
					{
						JTextField textField = (JTextField) focusOwner;
						textField.selectAll();
					}
				});
			}
		}
	}
}
