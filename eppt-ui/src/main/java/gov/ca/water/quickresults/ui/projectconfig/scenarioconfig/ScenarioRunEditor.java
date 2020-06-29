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

package gov.ca.water.quickresults.ui.projectconfig.scenarioconfig;

import java.awt.*;
import java.awt.event.ActionEvent;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.swing.*;

import gov.ca.water.calgui.constant.Constant;
import gov.ca.water.calgui.constant.EpptPreferences;
import gov.ca.water.calgui.project.EpptScenarioRun;
import gov.ca.water.calgui.project.EpptScenarioRunValidator;
import javafx.scene.paint.Color;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 04-08-2019
 */
public class ScenarioRunEditor extends JDialog implements LoadingDss
{
	private static final Logger LOGGER = Logger.getLogger(ScenarioRunEditor.class.getName());
	private final ScenarioEditorPanel _scenarioEditorPanel;
	private final JProgressBar _progressBar = new JProgressBar();
	private final List<EpptScenarioRun> _scenarioRuns;
	private boolean _canceled = true;
	private EpptScenarioRun _originalScenarioRun;
	private final JLabel _warningLabel = new JLabel("*Please double-check the information in the highlighted fields");

	public ScenarioRunEditor(Frame frame, List<EpptScenarioRun> scenarioRuns)
	{
		super(frame, "New Scenario Run", true);
		_scenarioRuns = scenarioRuns;
		_progressBar.setVisible(false);
		_scenarioEditorPanel = new ScenarioEditorPanel(this, scenarioRuns);
		setPreferredSize(new Dimension(900, 700));
		setMinimumSize(new Dimension(650, 500));
		initComponents();
		pack();
		setLocationRelativeTo(frame);
	}

	public void fillPanel(EpptScenarioRun scenarioRun)
	{
		_originalScenarioRun = scenarioRun;
		setTitle("Edit Scenario Run: " + scenarioRun.getName());
		_scenarioEditorPanel.fillPanel(scenarioRun);
	}

	public void fillPanelForCopy(EpptScenarioRun scenarioRun, Color plotlyDefaultColor)
	{
		setTitle("Copy Scenario Run: " + scenarioRun.getName());
		_scenarioEditorPanel.fillPanelForCopy(scenarioRun, plotlyDefaultColor);
		_warningLabel.setVisible(true);
		_warningLabel.setForeground(java.awt.Color.BLUE);
	}

	private void initComponents()
	{
		setLayout(new BorderLayout(5, 5));
		add(_scenarioEditorPanel.$$$getRootComponent$$$(), BorderLayout.CENTER);
		buildOkCancelButtons();
	}

	private void buildOkCancelButtons()
	{
		JPanel jPanel = new JPanel();
		jPanel.setLayout(new BorderLayout());
		jPanel.add(_progressBar, BorderLayout.NORTH);
		JButton okButton = new JButton("OK");
		okButton.setDefaultCapable(true);
		getRootPane().setDefaultButton(okButton);
		JButton cancelButton = new JButton("Cancel");
		JPanel buttonPanel = new JPanel();
		buttonPanel.setLayout(new FlowLayout(FlowLayout.RIGHT));
		buttonPanel.add(okButton);
		okButton.setPreferredSize(cancelButton.getPreferredSize());
		buttonPanel.add(cancelButton);
		jPanel.add(buttonPanel, BorderLayout.CENTER);
		add(jPanel, BorderLayout.SOUTH);
		JPanel warningPanel = new JPanel(new FlowLayout(FlowLayout.LEFT));
		jPanel.add(warningPanel, BorderLayout.WEST);
		_warningLabel.setVisible(false);
		warningPanel.add(_warningLabel);
		okButton.addActionListener(this::okPerformed);
		cancelButton.addActionListener(this::cancelPerformed);
	}

	@Override
	public void loadingStart(String text)
	{
		_progressBar.setVisible(true);
		_progressBar.setIndeterminate(true);
		_progressBar.setToolTipText(text);
	}

	@Override
	public void loadingFinished()
	{
		_progressBar.setVisible(false);
		_progressBar.setIndeterminate(false);
	}

	private void okPerformed(ActionEvent e)
	{
		EpptScenarioRun run = createRun();
		EpptScenarioRunValidator epptScenarioRunValidator = new EpptScenarioRunValidator(run);
		if(epptScenarioRunValidator.isValid())
		{
			boolean duplicateName = _scenarioRuns
					.stream()
					.filter(s -> s != _originalScenarioRun)
					.map(EpptScenarioRun::getName)
					.anyMatch(s -> s.equalsIgnoreCase(run.getName()));
			if(duplicateName)
			{
				JOptionPane.showMessageDialog(this, "Duplicate Scenario Run Name: " + run.getName(),
						"Error", JOptionPane.WARNING_MESSAGE);
			}
			else
			{
				try
				{
					copyWaterYearIndexModelCsv(run);
					_canceled = false;
					dispose();
				}
				catch(IOException ex)
				{
					LOGGER.log(Level.SEVERE, "Unable to copy water year index model file into project directory", ex);
				}
			}
		}
		else
		{
			StringBuilder builder = new StringBuilder("Scenario Run is not valid: ");
			epptScenarioRunValidator.getErrors().forEach(s -> builder.append("\n").append(s));
			JOptionPane.showMessageDialog(this, builder.toString(), "Error", JOptionPane.WARNING_MESSAGE);
		}
	}

	private void copyWaterYearIndexModelCsv(EpptScenarioRun epptScenarioRun) throws IOException
	{
		Path path = _scenarioEditorPanel.getWaterYearIndexModelCsv();
		Path scenarioDirectoryFile = EpptPreferences.getLastProjectConfiguration()
									  .getParent()
									  .resolve(epptScenarioRun.getName())
									  .resolve(Paths.get(Constant.MODEL_WATER_YEAR_INDEX_FILE).getFileName());
		if(!scenarioDirectoryFile.getParent().toFile().exists())
		{
			Files.createDirectories(scenarioDirectoryFile.getParent());
		}
		Files.write(scenarioDirectoryFile, Files.readAllBytes(path));
		_scenarioEditorPanel.cleanUpTempFile();
	}

	private void cancelPerformed(ActionEvent e)
	{
		_scenarioEditorPanel.cleanUpTempFile();
		dispose();
	}

	@Override
	public void dispose()
	{
		_progressBar.setIndeterminate(false);
		_progressBar.setVisible(false);
		_scenarioEditorPanel.shutdown();
		setVisible(false);
		super.dispose();
	}

	/**
	 * @return null if canceled, builds a new Scenario Run otherwise
	 */
	public EpptScenarioRun createRun()
	{
		return _scenarioEditorPanel.createRun();
	}

	public boolean isCanceled()
	{
		return _canceled;
	}
}
