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
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Frame;
import java.awt.event.ActionEvent;
import java.io.File;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import javax.swing.*;
import javax.swing.border.TitledBorder;
import javax.swing.plaf.basic.BasicArrowButton;

import gov.ca.water.calgui.bo.GUILinksAllModelsBO;
import gov.ca.water.calgui.bo.SimpleFileFilter;
import gov.ca.water.calgui.constant.EpptPreferences;
import gov.ca.water.calgui.project.EpptDssContainer;
import gov.ca.water.calgui.project.EpptScenarioRun;
import gov.ca.water.calgui.project.NamedDssPath;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 04-08-2019
 */
class ScenarioRunEditor extends JDialog
{
	private final JTextField _nameField;
	private final JTextField _descriptionField;
	private final JComboBox<GUILinksAllModelsBO.Model> _modelCombobox;
	private final JTextField _outputTextField;
	private final JTextField _wreslTextField;
	private final JTextField _ivTextField;
	private final JTextField _dvTextField;
	private final JTextField _svTextField;
	private final JTextField _ivAliasTextField;
	private final JTextField _dvAliasTextField;
	private final JTextField _svAliasTextField;
	private final DefaultListModel<NamedDssPath> _extraDssListModel;
	private boolean _canceled = true;

	ScenarioRunEditor(Frame frame)
	{
		super(frame, "New Scenario Run", true);
		setPreferredSize(new Dimension(650, 500));
		setMinimumSize(new Dimension(650, 500));
		_nameField = new JTextField();
		_descriptionField = new JTextField();
		_modelCombobox = new JComboBox<>();
		_outputTextField = new JTextField();
		_wreslTextField = new JTextField();
		_dvTextField = new JTextField();
		_svTextField = new JTextField();
		_ivTextField = new JTextField();
		_dvAliasTextField = new JTextField();
		_svAliasTextField = new JTextField();
		_ivAliasTextField = new JTextField();
		_extraDssListModel = new DefaultListModel<>();
		initComponents();
		initModelCombo();
		pack();
		setLocationRelativeTo(frame);
		_nameField.requestFocus();
		_nameField.selectAll();
	}

	void fillPanel(EpptScenarioRun scenarioRun)
	{
		setTitle("Edit Scenario Run: " + scenarioRun.getName());
		_nameField.setText(scenarioRun.getName());
		_descriptionField.setText(scenarioRun.getDescription());
		_modelCombobox.setSelectedItem(scenarioRun.getModel());
		_outputTextField.setText(scenarioRun.getOutputPath().toString());
		_wreslTextField.setText(scenarioRun.getWreslMain().toString());
		EpptDssContainer dssContainer = scenarioRun.getDssContainer();
		NamedDssPath dvDssFile = dssContainer.getDvDssFile();
		if(dvDssFile != null)
		{
			_dvTextField.setText(dvDssFile.getDssPath().toString());
			_dvAliasTextField.setText(dvDssFile.getAliasName());
		}
		NamedDssPath svDssFile = dssContainer.getSvDssFile();
		if(svDssFile != null)
		{
			_svTextField.setText(svDssFile.getDssPath().toString());
			_svAliasTextField.setText(svDssFile.getAliasName());
		}
		NamedDssPath ivDssFile = dssContainer.getIvDssFile();
		if(ivDssFile != null)
		{
			_ivTextField.setText(ivDssFile.getDssPath().toString());
			_ivAliasTextField.setText(ivDssFile.getAliasName());
		}
		dssContainer.getExtraDssFiles().forEach(_extraDssListModel::addElement);
	}

	private void initComponents()
	{
		setLayout(new BorderLayout(5, 5));
		buildScenarioFields();
		buildDssFields();
		buildOkCancelButtons();
	}

	private void buildDssFields()
	{
		JPanel defaultDssPanel = buildDefaultDssPanel();
		JPanel extraDssPanel = buildExtraDssPanel();

		JPanel dssPanel = new JPanel();
		dssPanel.setLayout(new BorderLayout());
		dssPanel.setBorder(new TitledBorder("DSS Paths"));
		dssPanel.add(defaultDssPanel, BorderLayout.NORTH);
		dssPanel.add(extraDssPanel, BorderLayout.CENTER);

		add(dssPanel, BorderLayout.CENTER);
	}

	private JPanel buildDefaultDssPanel()
	{
		JPanel defaultDssPanel = new JPanel();
		JPanel dvPanel = createDssSelectorPanel("DV DSS:", _dvTextField, _dvAliasTextField);
		JPanel svPanel = createDssSelectorPanel("SV DSS:", _svTextField, _svAliasTextField);
		JPanel ivPanel = createDssSelectorPanel("IV DSS:", _ivTextField, _ivAliasTextField);
		defaultDssPanel.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));
		defaultDssPanel.setLayout(new BoxLayout(defaultDssPanel, BoxLayout.Y_AXIS));
		defaultDssPanel.add(dvPanel);
		defaultDssPanel.add(Box.createRigidArea(new Dimension(5, 5)));
		defaultDssPanel.add(svPanel);
		defaultDssPanel.add(Box.createRigidArea(new Dimension(5, 5)));
		defaultDssPanel.add(ivPanel);
		return defaultDssPanel;
	}

	private JPanel buildExtraDssPanel()
	{
		JPanel extraDssPanel = new JPanel();
		extraDssPanel.setBorder(new TitledBorder("Extra DSS"));
		JList<NamedDssPath> dssTree = new JList<>();
		JPanel treeButtonPanel = new JPanel();
		treeButtonPanel.setLayout(new FlowLayout(FlowLayout.RIGHT));
		JButton plusButton = new JButton("+");
		JButton minusButton = new JButton("-");
		JButton upButton = new JButton("<html>&#9650;</html>");
		JButton downButton = new JButton("<html>&#9660;</html>");
		plusButton.addActionListener(this::addExtraDss);
		minusButton.addActionListener(e -> removeSelectedDss(dssTree.getSelectedValue()));
		upButton.addActionListener(e -> moveSelectedDssUp(dssTree.getSelectedIndex()));
		downButton.addActionListener(e -> moveSelectedDssDown(dssTree.getSelectedIndex()));
		treeButtonPanel.add(plusButton);
		treeButtonPanel.add(minusButton);
		treeButtonPanel.add(upButton);
		treeButtonPanel.add(downButton);
		dssTree.setModel(_extraDssListModel);
		extraDssPanel.setLayout(new BorderLayout());
		extraDssPanel.add(treeButtonPanel, BorderLayout.SOUTH);
		extraDssPanel.add(dssTree, BorderLayout.CENTER);
		return extraDssPanel;
	}

	private void moveSelectedDssDown(int selectedValue)
	{
		if(selectedValue >= 0 && selectedValue < _extraDssListModel.size() - 1)
		{
			NamedDssPath remove = _extraDssListModel.remove(selectedValue);
			_extraDssListModel.add(selectedValue + 1, remove);
		}
	}

	private void moveSelectedDssUp(int selectedValue)
	{
		if(selectedValue >= 1)
		{
			NamedDssPath remove = _extraDssListModel.remove(selectedValue);
			_extraDssListModel.add(selectedValue - 1, remove);
		}
	}

	private void removeSelectedDss(NamedDssPath selectedValue)
	{
		if(selectedValue != null)
		{
			_extraDssListModel.removeElement(selectedValue);
		}
	}

	private void addExtraDss(ActionEvent e)
	{
		JFileChooser fileChooser = new JFileChooser();
		fileChooser.setFileFilter(new SimpleFileFilter("DSS"));
		fileChooser.setCurrentDirectory(EpptPreferences.getProjectsPath().toFile());
		fileChooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
		fileChooser.showDialog(this, "Select");
		File selectedFile = fileChooser.getSelectedFile();
		if(selectedFile != null)
		{
			_extraDssListModel.addElement(
					new NamedDssPath(selectedFile.toPath(), selectedFile.toPath().getFileName().toString()));
		}
	}

	private JPanel createDssSelectorPanel(String labelText, JTextField fileField, JTextField aliasField)
	{
		Dimension dimension = new Dimension(70, 8);
		JLabel label = new JLabel(labelText);
		label.setPreferredSize(dimension);

		JPanel filePanel = new JPanel();
		filePanel.setLayout(new BorderLayout(0, 5));
		filePanel.add(fileField, BorderLayout.CENTER);
		JButton extraDssButton = new JButton("...");
		extraDssButton.addActionListener(e -> selectDss(fileField));
		filePanel.add(extraDssButton, BorderLayout.EAST);
		filePanel.add(label, BorderLayout.WEST);

		JPanel aliasPanel = new JPanel();
		aliasPanel.setLayout(new BorderLayout(5, 5));
		aliasPanel.add(aliasField, BorderLayout.CENTER);
		JLabel aliasLabel = new JLabel("Alias:");
		aliasPanel.add(aliasLabel, BorderLayout.WEST);

		JPanel outerPanel = new JPanel();
		outerPanel.setLayout(new BorderLayout(5, 5));
		outerPanel.add(filePanel, BorderLayout.CENTER);
		aliasPanel.setPreferredSize(new Dimension(200, fileField.getPreferredSize().height));
		outerPanel.add(aliasPanel, BorderLayout.EAST);

		return outerPanel;
	}

	private void buildScenarioFields()
	{

		JPanel descriptorPanel = new JPanel();
		descriptorPanel.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));
		descriptorPanel.setLayout(new BoxLayout(descriptorPanel, BoxLayout.Y_AXIS));

		JPanel namePanel = new JPanel();
		JLabel nameLabel = new JLabel("Name: ");
		namePanel.setLayout(new BorderLayout(5, 5));
		namePanel.add(nameLabel, BorderLayout.WEST);
		namePanel.add(_nameField, BorderLayout.CENTER);

		JPanel descriptionPanel = new JPanel();
		JLabel descriptionLabel = new JLabel("Description: ");
		descriptionPanel.setLayout(new BorderLayout(5, 5));
		descriptionPanel.add(descriptionLabel, BorderLayout.WEST);
		descriptionPanel.add(_descriptionField, BorderLayout.CENTER);

		JPanel modelPanel = new JPanel();
		JLabel modelLabel = new JLabel("Model: ");
		modelPanel.setLayout(new BorderLayout(5, 5));
		modelPanel.add(modelLabel, BorderLayout.WEST);
		modelPanel.add(_modelCombobox, BorderLayout.CENTER);

		JPanel outputPanel = new JPanel();
		JLabel outputLabel = new JLabel("Output Path: ");
		outputPanel.setLayout(new BorderLayout(5, 5));
		outputPanel.add(outputLabel, BorderLayout.WEST);
		outputPanel.add(_outputTextField, BorderLayout.CENTER);
		JButton outputButton = new JButton("...");
		outputPanel.add(outputButton, BorderLayout.EAST);
		outputButton.addActionListener(this::selectOutputPath);

		JPanel wreslPanel = new JPanel();
		JLabel wreslLabel = new JLabel("WRESL Main: ");
		wreslPanel.setLayout(new BorderLayout(5, 5));
		wreslPanel.add(wreslLabel, BorderLayout.WEST);
		wreslPanel.add(_wreslTextField, BorderLayout.CENTER);
		JButton wrelMainButton = new JButton("...");
		wreslPanel.add(wrelMainButton, BorderLayout.EAST);
		wrelMainButton.addActionListener(this::selectWreslMain);

		Dimension dimension = new Dimension(70, 14);
		modelLabel.setPreferredSize(dimension);
		nameLabel.setPreferredSize(dimension);
		outputLabel.setPreferredSize(dimension);
		descriptionLabel.setPreferredSize(dimension);
		wreslLabel.setPreferredSize(dimension);

		descriptorPanel.add(namePanel);
		descriptorPanel.add(Box.createRigidArea(new Dimension(5, 5)));
		descriptorPanel.add(descriptionPanel);
		descriptorPanel.add(Box.createRigidArea(new Dimension(5, 5)));
		descriptorPanel.add(modelPanel);
		descriptorPanel.add(Box.createRigidArea(new Dimension(5, 5)));
		descriptorPanel.add(outputPanel);
		descriptorPanel.add(Box.createRigidArea(new Dimension(5, 5)));
		descriptorPanel.add(wreslPanel);
		add(descriptorPanel, BorderLayout.NORTH);

		String time = Long.toString(new Date().getTime()).substring(6);
		_nameField.setText("NewScenarioRun " + time);
	}

	private void selectWreslMain(ActionEvent actionEvent)
	{
		JFileChooser fileChooser = new JFileChooser();
		fileChooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
		fileChooser.setCurrentDirectory(EpptPreferences.getWreslDirectory().toFile());
		fileChooser.showDialog(this, "Select");
		File selectedFile = fileChooser.getSelectedFile();
		if(selectedFile != null)
		{
			_wreslTextField.setText(selectedFile.toString());
		}
	}

	private void selectOutputPath(ActionEvent actionEvent)
	{
		JFileChooser fileChooser = new JFileChooser("Select Scenario Run Directory");
		fileChooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
		fileChooser.setCurrentDirectory(EpptPreferences.getProjectsPath().toFile());
		fileChooser.showDialog(this, "Select");
		File selectedFile = fileChooser.getSelectedFile();
		if(selectedFile != null)
		{
			_outputTextField.setText(selectedFile.toString());
		}
	}

	private void selectDss(JTextField textField)
	{
		JFileChooser fileChooser = new JFileChooser("Select Scenario Run Directory");
		fileChooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
		fileChooser.setCurrentDirectory(EpptPreferences.getProjectsPath().toFile());
		fileChooser.setFileFilter(new SimpleFileFilter("DSS"));
		fileChooser.showDialog(this, "Select");
		File selectedFile = fileChooser.getSelectedFile();
		if(selectedFile != null)
		{
			textField.setText(selectedFile.toString());
		}
	}

	private void initModelCombo()
	{
		for(GUILinksAllModelsBO.Model model : GUILinksAllModelsBO.Model.values())
		{
			_modelCombobox.addItem(model);
		}
		_modelCombobox.setSelectedItem(GUILinksAllModelsBO.Model.values().get(0));
	}

	private void buildOkCancelButtons()
	{
		JButton okButton = new JButton("OK");
		JButton cancelButton = new JButton("Cancel");
		JPanel buttonPanel = new JPanel();
		buttonPanel.setLayout(new FlowLayout(FlowLayout.RIGHT));
		buttonPanel.add(okButton);
		okButton.setPreferredSize(cancelButton.getPreferredSize());
		buttonPanel.add(cancelButton);
		add(buttonPanel, BorderLayout.SOUTH);
		okButton.addActionListener(this::okPerformed);
		cancelButton.addActionListener(this::cancelPerformed);
	}

	void okPerformed(ActionEvent e)
	{
		_canceled = false;
		setVisible(false);
		dispose();
	}

	private void cancelPerformed(ActionEvent e)
	{
		setVisible(false);
		dispose();
	}

	/**
	 * @return null if canceled, builds a new Scenario Run otherwise
	 */
	EpptScenarioRun createRun()
	{
		EpptScenarioRun retval = null;
		if(!_canceled && validateRun())
		{
			String name = _nameField.getText();
			String description = _descriptionField.getText();
			GUILinksAllModelsBO.Model model = (GUILinksAllModelsBO.Model) _modelCombobox.getSelectedItem();
			Path outputPath = Paths.get(_outputTextField.getText());
			Path wreslMain = Paths.get(_wreslTextField.getText());
			EpptDssContainer dssContainer = createDssContainer();
			retval = new EpptScenarioRun(name, description, model, outputPath,
					wreslMain, dssContainer);
		}
		return retval;
	}

	private EpptDssContainer createDssContainer()
	{
		NamedDssPath dvDssFile = getNamedDssPath(_dvTextField, _dvAliasTextField);
		NamedDssPath svDssFile = getNamedDssPath(_svTextField, _svAliasTextField);
		NamedDssPath ivDssFile = getNamedDssPath(_ivTextField, _ivAliasTextField);
		List<NamedDssPath> extraDssFiles = new ArrayList<>();
		for(int i = 0; i < _extraDssListModel.getSize(); i++)
		{
			extraDssFiles.add(_extraDssListModel.get(i));
		}
		return new EpptDssContainer(dvDssFile, svDssFile, ivDssFile, extraDssFiles);
	}

	private NamedDssPath getNamedDssPath(JTextField pathTextField, JTextField aliasTextField)
	{
		NamedDssPath ivDssFile = null;
		String ivTextFieldText = pathTextField.getText();
		if(ivTextFieldText != null && !ivTextFieldText.isEmpty())
		{
			Path ivPath = Paths.get(ivTextFieldText);
			String aliasText = aliasTextField.getText();
			if(aliasText.isEmpty())
			{
				aliasText = ivPath.getFileName().toString();
			}
			ivDssFile = new NamedDssPath(ivPath, aliasText);
		}
		return ivDssFile;
	}

	private boolean validateRun()
	{
		return !_nameField.getText().isEmpty();
	}
}
