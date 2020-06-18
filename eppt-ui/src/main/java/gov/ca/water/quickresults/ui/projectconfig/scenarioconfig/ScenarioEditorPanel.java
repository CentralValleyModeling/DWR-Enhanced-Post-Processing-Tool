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
import java.beans.PropertyChangeEvent;
import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Date;
import java.util.List;
import java.util.Objects;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.swing.*;
import javax.swing.border.EmptyBorder;
import javax.swing.border.TitledBorder;
import javax.swing.table.DefaultTableCellRenderer;
import javax.swing.table.TableCellEditor;
import javax.swing.table.TableCellRenderer;

import gov.ca.water.calgui.bo.GUILinksAllModelsBO;
import gov.ca.water.calgui.bo.SimpleFileFilter;
import gov.ca.water.calgui.constant.Constant;
import gov.ca.water.calgui.constant.EpptPreferences;
import gov.ca.water.calgui.project.EpptDssContainer;
import gov.ca.water.calgui.project.EpptScenarioRun;
import gov.ca.water.calgui.techservice.impl.NonSymlinkFilter;
import javafx.scene.paint.Color;

import rma.swing.RmaJColorChooserButton;
import rma.swing.RmaJComboBox;
import rma.swing.RmaJDescriptionField;
import rma.swing.RmaJTable;
import rma.swing.table.RmaCellEditor;

import static gov.ca.water.quickresults.ui.projectconfig.scenarioconfig.ScenarioDssTableModel.ALIAS_COLUMN;
import static gov.ca.water.quickresults.ui.projectconfig.scenarioconfig.ScenarioDssTableModel.A_PART_COLUMN;
import static java.util.stream.Collectors.toList;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 05-29-2019
 */
public class ScenarioEditorPanel
{
	private static JFileChooser fileChooser;
	private static final Logger LOGGER = Logger.getLogger(ScenarioEditorPanel.class.getName());
	private final ScenarioDssTableModel _scenarioDssTableModel;
	private JPanel _panel1;
	private JTable _dssTable;
	private JButton _addButton;
	private JButton _removeButton;
	private JTextField _nameField;
	private JTextField _descriptionField;
	private JTextField _outputTextField;
	private JTextField _wreslTextField;
	private JButton _wreslButton;
	private JComboBox<GUILinksAllModelsBO.Model> _modelCombobox;
	private JButton _outputPathButton;
	private JTextField _waterYearTable;
	private JButton _wyTableBtn;
	private JTextField _colorHexTextField;
	private RmaJColorChooserButton _colorChooserButton;
	private JTextField _waterYearIndexPathField;
	private JButton _editWaterYearIndexButton;
	private JLabel _scenarioNameLabel;
	private Path _tempWaterYearIndexModelPath;

	ScenarioEditorPanel(LoadingDss loadingDss, List<EpptScenarioRun> scenarioRuns)
	{
		// GUI initializer generated by IntelliJ IDEA GUI Designer
		// >>> IMPORTANT!! <<<
		// DO NOT EDIT OR ADD ANY CODE HERE!
		$$$setupUI$$$();
		_scenarioDssTableModel = new ScenarioDssTableModel(loadingDss);
		initComponents(scenarioRuns);
		initModels();
		initListeners();
		_nameField.requestFocus();
		_dssTable.getColumnModel().getColumn(1).setPreferredWidth(400);
	}

	private void initComponents(List<EpptScenarioRun> scenarioRuns)
	{
		String time = Long.toString(new Date().getTime()).substring(6);
		_nameField.setText("NewScenarioRun " + time);
		_nameField.selectAll();
		_waterYearTable.setText(Paths.get(Constant.WRESL_DIR).resolve("CalLite").resolve(Constant.LOOKUP_DIRECTORY).toString());
		_wreslTextField.setText(Paths.get(Constant.WRESL_DIR).resolve("CalLite").toString());
		Color plotlyDefaultColor = Constant.getColorNotInList(scenarioRuns.stream().map(EpptScenarioRun::getColor).collect(toList()));
		String hex = Constant.colorToHex(plotlyDefaultColor);
		_colorHexTextField.setText(hex);
		java.awt.Color decode = java.awt.Color.decode(hex.substring(0, 7));
		decode = new java.awt.Color(decode.getRed(), decode.getGreen(), decode.getBlue(), Integer.parseInt(hex.substring(7, 9), 16));
		_colorChooserButton.setColor(decode);
		_waterYearIndexPathField.setText(Constant.MODEL_WATER_YEAR_INDEX_FILE);
	}

	private void initListeners()
	{
		_addButton.addActionListener(this::addExtraDss);
		_removeButton.addActionListener(this::removeExtraDss);
		_outputPathButton.addActionListener(this::selectOutputPath);
		_wreslButton.addActionListener(this::selectWreslMain);
		_wyTableBtn.addActionListener(e -> chooseWaterYearTable());
		_colorChooserButton.addSampleListener(this::chooseColor);
		_modelCombobox.addActionListener(e -> modelComboChanged());
		_editWaterYearIndexButton.addActionListener(e -> editWaterYearIndexModelFile());
	}

	private void editWaterYearIndexModelFile()
	{
		setupTempWaterIndexFile();
		try
		{
			Desktop.getDesktop().open(_tempWaterYearIndexModelPath.toFile());
		}
		catch(IOException e)
		{
			LOGGER.log(Level.SEVERE, "Unable to open temporary file for editing the: " + _tempWaterYearIndexModelPath + " Will default to installation version", e);
		}
	}

	private void setupTempWaterIndexFile()
	{
		Path modelWaterYearIndexFile = _tempWaterYearIndexModelPath;
		if(modelWaterYearIndexFile == null)
		{
			modelWaterYearIndexFile = EpptPreferences.getLastProjectConfiguration().resolve(_nameField.getText()).resolve(Constant.MODEL_WATER_YEAR_INDEX_FILE);
		}
		Object selectedItem = _modelCombobox.getSelectedItem();
		if(!modelWaterYearIndexFile.toFile().exists() && selectedItem != null)
		{
			modelWaterYearIndexFile = Paths.get(Constant.CONFIG_DIR).resolve(selectedItem.toString()).resolve(Constant.MODEL_WATER_YEAR_INDEX_FILE);
		}
		try
		{
			_tempWaterYearIndexModelPath = Files.createTempFile("WaterYearIndexModel", Constant.CSV_EXT);
			Files.write(_tempWaterYearIndexModelPath, Files.readAllBytes(modelWaterYearIndexFile));
		}
		catch(IOException e)
		{
			LOGGER.log(Level.SEVERE, "Unable to create temporary file for editing the: " + Constant.MODEL_WATER_YEAR_INDEX_FILE + " Will default to installation version", e);
		}
	}

	private void modelComboChanged()
	{
		Object selectedItem = _modelCombobox.getSelectedItem();
		if(selectedItem instanceof GUILinksAllModelsBO.Model)
		{
			_waterYearTable.setText(Paths.get(Constant.WRESL_DIR).resolve(selectedItem.toString()).resolve(Constant.LOOKUP_DIRECTORY).toString());
			_wreslTextField.setText(Paths.get(Constant.WRESL_DIR).resolve(selectedItem.toString()).toString());
		}

	}

	private void chooseColor(PropertyChangeEvent actionEvent)
	{
		if(Objects.equals(RmaJColorChooserButton.PROPERTY_COLOR, actionEvent.getPropertyName()))
		{
			Object newValue = actionEvent.getNewValue();
			if(newValue instanceof java.awt.Color)
			{
				java.awt.Color color = (java.awt.Color) newValue;
				_colorHexTextField.setText(Constant.colorToHex(color));
				_colorChooserButton.setColor(color);
			}
		}
	}

	private void chooseWaterYearTable()
	{
		Window window = SwingUtilities.windowForComponent($$$getRootComponent$$$());
		JFileChooser jFileChooser = getFileChooser("Select Lookup Directory");
		jFileChooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
		jFileChooser.setFileFilter(new NonSymlinkFilter());
		if(JFileChooser.APPROVE_OPTION == jFileChooser.showOpenDialog(window))
		{
			File selectedFile = jFileChooser.getSelectedFile();
			if(selectedFile != null)
			{
				if(selectedFile.toPath().resolve(Constant.WY_TYPES_TABLE).toFile().exists())
				{
					_waterYearTable.setText(selectedFile.toString());
				}
				else
				{
					JOptionPane.showMessageDialog(window, "Selected directory must contain a file named: " + Constant.WY_TYPES_TABLE, "Invalid Lookup Directory",
							JOptionPane.WARNING_MESSAGE);
				}
			}
		}
	}

	private void removeExtraDss(ActionEvent actionEvent)
	{
		int selectedRow = _dssTable.getSelectedRow();
		if(selectedRow >= 0 && _scenarioDssTableModel.getRowType(selectedRow) == ScenarioDssTableModel.RowType.EXTRA)
		{
			_scenarioDssTableModel.removeExtraDss(selectedRow);
		}
	}

	private static JFileChooser getFileChooser(String s)
	{
		if(fileChooser == null)
		{
			fileChooser = new JFileChooser();
			fileChooser.setCurrentDirectory(EpptPreferences.getLastProjectConfiguration().getParent().toFile());
		}
		fileChooser.setDialogTitle(s);
		return fileChooser;
	}

	private void initModels()
	{
		for(GUILinksAllModelsBO.Model model : GUILinksAllModelsBO.Model.values())
		{
			_modelCombobox.addItem(model);
		}
		_modelCombobox.setSelectedItem(GUILinksAllModelsBO.Model.values().get(0));
		_dssTable.setModel(_scenarioDssTableModel);
		_dssTable.getColumnModel().getColumn(ScenarioDssTableModel.DSS_PATH_COLUMN).setCellEditor(new FileChooserEditor());
		_dssTable.getColumnModel().getColumn(ScenarioDssTableModel.DSS_PATH_COLUMN).setCellRenderer(new FileChooserRenderer());
		_dssTable.getColumnModel().getColumn(A_PART_COLUMN).setCellEditor(new ComboBoxCellEditor(new RmaJComboBox()));
		_dssTable.getColumnModel().getColumn(ScenarioDssTableModel.F_PART_COLUMN).setCellEditor(new ComboBoxCellEditor(new RmaJComboBox()));
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
		_panel1.setLayout(new BorderLayout(0, 0));
		_panel1.setPreferredSize(new Dimension(700, 500));
		final JPanel panel1 = new JPanel();
		panel1.setLayout(new BorderLayout(0, 0));
		_panel1.add(panel1, BorderLayout.CENTER);
		panel1.setBorder(
				BorderFactory.createTitledBorder(BorderFactory.createEtchedBorder(), "Required Files", TitledBorder.DEFAULT_JUSTIFICATION, TitledBorder.DEFAULT_POSITION, null,
						null));
		final JPanel panel2 = new JPanel();
		panel2.setLayout(new FlowLayout(FlowLayout.RIGHT, 5, 5));
		panel1.add(panel2, BorderLayout.SOUTH);
		_addButton = new JButton();
		_addButton.setPreferredSize(new Dimension(45, 24));
		_addButton.setText("+");
		panel2.add(_addButton);
		_removeButton = new JButton();
		_removeButton.setPreferredSize(new Dimension(45, 24));
		_removeButton.setText("-");
		panel2.add(_removeButton);
		final JScrollPane scrollPane1 = new JScrollPane();
		panel1.add(scrollPane1, BorderLayout.CENTER);
		scrollPane1.setViewportView(_dssTable);
		final JPanel panel3 = new JPanel();
		panel3.setLayout(new BorderLayout(0, 0));
		_panel1.add(panel3, BorderLayout.NORTH);
		panel3.setBorder(
				BorderFactory.createTitledBorder(BorderFactory.createEtchedBorder(), "General Description", TitledBorder.DEFAULT_JUSTIFICATION, TitledBorder.DEFAULT_POSITION,
						null, null));
		final JPanel panel4 = new JPanel();
		panel4.setLayout(new GridBagLayout());
		panel3.add(panel4, BorderLayout.CENTER);
		_scenarioNameLabel = new JLabel();
		_scenarioNameLabel.setText("Scenario Name:");
		GridBagConstraints gbc;
		gbc = new GridBagConstraints();
		gbc.gridx = 0;
		gbc.gridy = 0;
		gbc.anchor = GridBagConstraints.WEST;
		gbc.insets = new Insets(5, 5, 5, 5);
		panel4.add(_scenarioNameLabel, gbc);
		_nameField = new JTextField();
		gbc = new GridBagConstraints();
		gbc.gridx = 1;
		gbc.gridy = 0;
		gbc.gridwidth = 2;
		gbc.weightx = 1.0;
		gbc.anchor = GridBagConstraints.WEST;
		gbc.fill = GridBagConstraints.HORIZONTAL;
		gbc.insets = new Insets(5, 5, 5, 5);
		panel4.add(_nameField, gbc);
		final JLabel label1 = new JLabel();
		label1.setText("Description:");
		gbc = new GridBagConstraints();
		gbc.gridx = 0;
		gbc.gridy = 1;
		gbc.anchor = GridBagConstraints.WEST;
		gbc.insets = new Insets(5, 5, 5, 5);
		panel4.add(label1, gbc);
		final JLabel label2 = new JLabel();
		label2.setText("Model:");
		gbc = new GridBagConstraints();
		gbc.gridx = 0;
		gbc.gridy = 2;
		gbc.anchor = GridBagConstraints.WEST;
		gbc.insets = new Insets(5, 5, 5, 5);
		panel4.add(label2, gbc);
		_descriptionField = new JTextField();
		gbc = new GridBagConstraints();
		gbc.gridx = 1;
		gbc.gridy = 1;
		gbc.gridwidth = 2;
		gbc.weightx = 1.0;
		gbc.anchor = GridBagConstraints.WEST;
		gbc.fill = GridBagConstraints.HORIZONTAL;
		gbc.insets = new Insets(5, 5, 5, 5);
		panel4.add(_descriptionField, gbc);
		final JLabel label3 = new JLabel();
		label3.setText("Study Main Directory:");
		gbc = new GridBagConstraints();
		gbc.gridx = 0;
		gbc.gridy = 3;
		gbc.anchor = GridBagConstraints.WEST;
		gbc.insets = new Insets(5, 5, 5, 5);
		panel4.add(label3, gbc);
		_outputTextField = new JTextField();
		gbc = new GridBagConstraints();
		gbc.gridx = 1;
		gbc.gridy = 3;
		gbc.weightx = 1.0;
		gbc.anchor = GridBagConstraints.WEST;
		gbc.fill = GridBagConstraints.HORIZONTAL;
		gbc.insets = new Insets(5, 5, 5, 5);
		panel4.add(_outputTextField, gbc);
		_modelCombobox = new JComboBox();
		gbc = new GridBagConstraints();
		gbc.gridx = 1;
		gbc.gridy = 2;
		gbc.gridwidth = 2;
		gbc.weightx = 1.0;
		gbc.anchor = GridBagConstraints.WEST;
		gbc.fill = GridBagConstraints.HORIZONTAL;
		gbc.insets = new Insets(5, 5, 5, 5);
		panel4.add(_modelCombobox, gbc);
		_outputPathButton = new JButton();
		_outputPathButton.setPreferredSize(new Dimension(45, 24));
		_outputPathButton.setText("...");
		gbc = new GridBagConstraints();
		gbc.gridx = 2;
		gbc.gridy = 3;
		gbc.fill = GridBagConstraints.HORIZONTAL;
		gbc.insets = new Insets(5, 5, 5, 5);
		panel4.add(_outputPathButton, gbc);
		final JLabel label4 = new JLabel();
		label4.setText("Scenario Color");
		gbc = new GridBagConstraints();
		gbc.gridx = 0;
		gbc.gridy = 5;
		gbc.anchor = GridBagConstraints.WEST;
		gbc.insets = new Insets(5, 5, 5, 5);
		panel4.add(label4, gbc);
		_colorHexTextField = new JTextField();
		gbc = new GridBagConstraints();
		gbc.gridx = 1;
		gbc.gridy = 5;
		gbc.weightx = 1.0;
		gbc.anchor = GridBagConstraints.WEST;
		gbc.fill = GridBagConstraints.HORIZONTAL;
		gbc.insets = new Insets(5, 5, 5, 5);
		panel4.add(_colorHexTextField, gbc);
		_colorChooserButton = new RmaJColorChooserButton();
		_colorChooserButton.setPreferredSize(new Dimension(45, 24));
		gbc = new GridBagConstraints();
		gbc.gridx = 2;
		gbc.gridy = 5;
		gbc.insets = new Insets(5, 5, 5, 5);
		panel4.add(_colorChooserButton, gbc);
		final JLabel label5 = new JLabel();
		label5.setText("Water Year Index File:");
		gbc = new GridBagConstraints();
		gbc.gridx = 0;
		gbc.gridy = 4;
		gbc.anchor = GridBagConstraints.WEST;
		gbc.insets = new Insets(5, 5, 5, 5);
		panel4.add(label5, gbc);
		_waterYearIndexPathField = new JTextField();
		_waterYearIndexPathField.setEditable(false);
		gbc = new GridBagConstraints();
		gbc.gridx = 1;
		gbc.gridy = 4;
		gbc.weightx = 1.0;
		gbc.anchor = GridBagConstraints.WEST;
		gbc.fill = GridBagConstraints.HORIZONTAL;
		gbc.insets = new Insets(5, 5, 5, 5);
		panel4.add(_waterYearIndexPathField, gbc);
		_editWaterYearIndexButton = new JButton();
		_editWaterYearIndexButton.setPreferredSize(new Dimension(45, 24));
		_editWaterYearIndexButton.setText("Edit");
		gbc = new GridBagConstraints();
		gbc.gridx = 2;
		gbc.gridy = 4;
		gbc.fill = GridBagConstraints.HORIZONTAL;
		gbc.insets = new Insets(5, 5, 5, 5);
		panel4.add(_editWaterYearIndexButton, gbc);
		final JPanel panel5 = new JPanel();
		panel5.setLayout(new BorderLayout(0, 0));
		_panel1.add(panel5, BorderLayout.SOUTH);
		panel5.setBorder(BorderFactory.createTitledBorder(BorderFactory.createEtchedBorder(), "QA/QC Report Resources", TitledBorder.DEFAULT_JUSTIFICATION,
				TitledBorder.DEFAULT_POSITION, null, null));
		final JPanel panel6 = new JPanel();
		panel6.setLayout(new GridBagLayout());
		panel5.add(panel6, BorderLayout.CENTER);
		_wreslTextField = new JTextField();
		gbc = new GridBagConstraints();
		gbc.gridx = 1;
		gbc.gridy = 0;
		gbc.weightx = 1.0;
		gbc.fill = GridBagConstraints.HORIZONTAL;
		gbc.insets = new Insets(5, 5, 5, 5);
		panel6.add(_wreslTextField, gbc);
		_wreslButton = new JButton();
		_wreslButton.setPreferredSize(new Dimension(45, 24));
		_wreslButton.setText("...");
		gbc = new GridBagConstraints();
		gbc.gridx = 2;
		gbc.gridy = 0;
		gbc.fill = GridBagConstraints.HORIZONTAL;
		gbc.insets = new Insets(5, 5, 5, 5);
		panel6.add(_wreslButton, gbc);
		final JLabel label6 = new JLabel();
		label6.setText("Lookup Directory:");
		gbc = new GridBagConstraints();
		gbc.gridx = 0;
		gbc.gridy = 1;
		gbc.anchor = GridBagConstraints.WEST;
		gbc.insets = new Insets(5, 5, 5, 5);
		panel6.add(label6, gbc);
		_waterYearTable = new JTextField();
		gbc = new GridBagConstraints();
		gbc.gridx = 1;
		gbc.gridy = 1;
		gbc.anchor = GridBagConstraints.WEST;
		gbc.fill = GridBagConstraints.HORIZONTAL;
		gbc.insets = new Insets(5, 5, 5, 5);
		panel6.add(_waterYearTable, gbc);
		_wyTableBtn = new JButton();
		_wyTableBtn.setPreferredSize(new Dimension(45, 24));
		_wyTableBtn.setRolloverEnabled(false);
		_wyTableBtn.setText("...");
		gbc = new GridBagConstraints();
		gbc.gridx = 2;
		gbc.gridy = 1;
		gbc.fill = GridBagConstraints.HORIZONTAL;
		gbc.insets = new Insets(5, 5, 5, 5);
		panel6.add(_wyTableBtn, gbc);
		final JLabel label7 = new JLabel();
		label7.setText("EPPT WRESL Directory:");
		gbc = new GridBagConstraints();
		gbc.gridx = 0;
		gbc.gridy = 0;
		gbc.anchor = GridBagConstraints.WEST;
		gbc.insets = new Insets(5, 5, 5, 5);
		panel6.add(label7, gbc);
	}

	/**
	 * @noinspection ALL
	 */
	public JComponent $$$getRootComponent$$$()
	{
		return _panel1;
	}

	private void createUIComponents()
	{
		_dssTable = new RmaJTable();
	}

	private void addExtraDss(ActionEvent e)
	{
		_scenarioDssTableModel.addExtraDss();
	}

	private void selectWreslMain(ActionEvent actionEvent)
	{
		Window window = SwingUtilities.windowForComponent($$$getRootComponent$$$());
		JFileChooser jFileChooser = getFileChooser("Select WRESL Directory");
		jFileChooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
		jFileChooser.setFileFilter(new NonSymlinkFilter());
		if(JFileChooser.APPROVE_OPTION == jFileChooser.showDialog(window, "Select"))
		{
			File selectedFile = jFileChooser.getSelectedFile();
			if(selectedFile != null)
			{
				if(selectedFile.toPath().resolve(Constant.WRESL_MAIN).toFile().exists())
				{
					_wreslTextField.setText(selectedFile.toString());
				}
				else
				{
					JOptionPane.showMessageDialog(window, "Selected directory must contain a file named: " + Constant.WRESL_MAIN, "Invalid WRESL Directory",
							JOptionPane.WARNING_MESSAGE);
				}
			}
		}
	}

	private void selectOutputPath(ActionEvent actionEvent)
	{
		JFileChooser jFileChooser = getFileChooser("Select Scenario Run Directory");
		jFileChooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
		jFileChooser.setFileFilter(new NonSymlinkFilter());
		if(JFileChooser.APPROVE_OPTION == jFileChooser.showDialog(SwingUtilities.windowForComponent($$$getRootComponent$$$()), "Select"))
		{
			File selectedFile = jFileChooser.getSelectedFile();
			if(selectedFile != null)
			{
				jFileChooser.setCurrentDirectory(selectedFile);
				_outputTextField.setText(selectedFile.toString());
			}
		}
	}

	private void selectDss(RmaJDescriptionField textField)
	{
		JFileChooser jFileChooser = getFileChooser("Select DSS File");
		jFileChooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
		jFileChooser.setFileFilter(new SimpleFileFilter("DSS"));
		if(JFileChooser.APPROVE_OPTION == jFileChooser.showDialog(SwingUtilities.windowForComponent($$$getRootComponent$$$()), "Select"))
		{
			File selectedFile = jFileChooser.getSelectedFile();
			if(selectedFile != null)
			{
				String fileName = selectedFile.toString();
				if(!fileName.toLowerCase().endsWith(".dss"))
				{
					fileName += ".dss";
				}
				textField.setText(fileName);
			}
		}
	}

	/**
	 * @return null if canceled, builds a new Scenario Run otherwise
	 */
	EpptScenarioRun createRun()
	{
		TableCellEditor editor = _dssTable.getCellEditor();
		if(editor != null)
		{
			editor.stopCellEditing();
		}
		String name = _nameField.getText();
		String description = _descriptionField.getText();
		GUILinksAllModelsBO.Model model = (GUILinksAllModelsBO.Model) _modelCombobox.getSelectedItem();
		Path outputPath = relativizeToProject(_outputTextField.getText());
		Path wreslMain = relativizeToInstaller(_wreslTextField.getText());
		EpptDssContainer dssContainer = _scenarioDssTableModel.createDssContainer(name, outputPath);
		Path waterYearTablePath = relativizeToInstaller(_waterYearTable.getText());
		Color web;
		String text = _colorHexTextField.getText();
		try
		{
			web = Color.web(text);
		}
		catch(IllegalArgumentException ex)
		{
			LOGGER.log(Level.SEVERE, "Invalid hex color: " + text, ex);
			web = Constant.getPlotlyDefaultColor(0);
		}
		return new EpptScenarioRun(name, description, model, outputPath, wreslMain, waterYearTablePath, dssContainer, web);
	}

	private Path relativizeToProject(String text)
	{
		Path retval = Paths.get(text);
		if(!retval.isAbsolute())
		{
			retval = EpptPreferences.getLastProjectConfiguration().getParent().resolve(retval);
		}
		return retval;
	}

	private Path relativizeToInstaller(String text)
	{
		Path retval = Paths.get(text);
		if(!retval.isAbsolute())
		{
			retval = Paths.get("").toAbsolutePath().resolve(retval);
		}
		return retval;

	}

	void fillPanel(EpptScenarioRun scenarioRun)
	{
		_nameField.setText(scenarioRun.getName());
		_descriptionField.setText(scenarioRun.getDescription());
		_modelCombobox.setSelectedItem(scenarioRun.getModel());
		Path outputPath = scenarioRun.getOutputPath();
		if(outputPath != null)
		{
			_outputTextField.setText(outputPath.toString());
		}
		Path wreslMain = scenarioRun.getWreslDirectory();
		if(wreslMain != null)
		{
			_wreslTextField.setText(wreslMain.toString());
		}
		EpptDssContainer dssContainer = scenarioRun.getDssContainer();
		_scenarioDssTableModel.fillModel(dssContainer);
		_waterYearTable.setText(scenarioRun.getLookupDirectory().toString());
		String hex = Constant.colorToHex(scenarioRun.getColor());
		_colorHexTextField.setText(hex);
		java.awt.Color decode = java.awt.Color.decode(hex.substring(0, 7));
		decode = new java.awt.Color(decode.getRed(), decode.getGreen(), decode.getBlue(), Integer.parseInt(hex.substring(7, 9), 16));
		_colorChooserButton.setColor(decode);
		setupTempWaterIndexFile();
	}

	void fillPanelForCopy(EpptScenarioRun scenarioRun, Color plotlyDefaultColor)
	{
		fillPanel(scenarioRun);
		_nameField.setText(scenarioRun.getName() + " (Copy)");
		String hex = Constant.colorToHex(plotlyDefaultColor);
		_colorHexTextField.setText(hex);
		java.awt.Color decode = java.awt.Color.decode(hex.substring(0, 7));
		decode = new java.awt.Color(decode.getRed(), decode.getGreen(), decode.getBlue(), Integer.parseInt(hex.substring(7, 9), 16));
		_colorChooserButton.setColor(decode);
		java.awt.Color highlightColor = java.awt.Color.BLUE;
		_scenarioNameLabel.setForeground(highlightColor);
		((RmaJTable) _dssTable).setModifiedForegroundColor(highlightColor);
		_dssTable.getColumnModel().getColumn(ALIAS_COLUMN).setHeaderRenderer(new DefaultTableCellRenderer()
		{
			@Override
			public Component getTableCellRendererComponent(JTable jTable, Object o, boolean b, boolean b1, int i, int i1)
			{
				Component retval = super.getTableCellRendererComponent(jTable, o, b, b1, i, i1);
				setForeground(highlightColor);
				return retval;
			}
		});
	}

	void shutdown()
	{
		_scenarioDssTableModel.shutdown();
	}

	public Path getWaterYearIndexModelCsv()
	{

		Path modelWaterYearIndexFile = _tempWaterYearIndexModelPath;
		if(modelWaterYearIndexFile == null)
		{
			modelWaterYearIndexFile = EpptPreferences.getLastProjectConfiguration().resolve(_nameField.getText()).resolve(Constant.MODEL_WATER_YEAR_INDEX_FILE);
		}
		Object selectedItem = _modelCombobox.getSelectedItem();
		if(!modelWaterYearIndexFile.toFile().exists() && selectedItem != null)
		{
			modelWaterYearIndexFile = Paths.get(Constant.CONFIG_DIR).resolve(selectedItem.toString()).resolve(Constant.MODEL_WATER_YEAR_INDEX_FILE);
		}
		return modelWaterYearIndexFile;
	}

	public void cleanUpTempFile()
	{
		if(_tempWaterYearIndexModelPath != null)
		{
			try
			{
				Files.deleteIfExists(_tempWaterYearIndexModelPath);
			}
			catch(IOException e)
			{
				LOGGER.log(Level.WARNING, "Unable to clean up temp file: " + _tempWaterYearIndexModelPath, e);
			}
		}
	}

	private final class FileChooserEditor extends RmaCellEditor
	{

		private RmaJDescriptionField _descriptionField = new RmaJDescriptionField()
		{
			@Override
			public void displayChooserDialog()
			{
				selectDss(_descriptionField);
				stopCellEditing();
			}
		};

		private FileChooserEditor()
		{
			super(new JTextField());
			FileChooserEditor.this.setClickCountToStart(1);
		}

		@Override
		public Object getCellEditorValue()
		{
			Object retval;
			if(_descriptionField != null)
			{
				retval = _descriptionField.getText();
			}
			else
			{
				retval = super.getCellEditorValue();
			}
			return retval;
		}

		@Override
		public Component getTableCellEditorComponent(JTable table, Object value, boolean isSelected, int row, int column)
		{
			if(value == null)
			{
				_descriptionField.setText("");
			}
			else
			{
				_descriptionField.setText(value.toString());
			}
			return _descriptionField;
		}
	}

	public static final class FileChooserRenderer extends RmaJDescriptionField implements TableCellRenderer
	{

		private FileChooserRenderer()
		{
			FileChooserRenderer.this.setBorder(new EmptyBorder(1, 1, 1, 1));
		}

		@Override
		public Component getTableCellRendererComponent(final JTable table, Object value, boolean isSelected, boolean hasFocus, int row, int column)
		{
			if(value != null)
			{
				String str = value.toString();
				setText(str);
			}
			else
			{
				setText("");
			}
			if(isSelected)
			{
				setForeground(table.getSelectionForeground());
				setBackground(table.getSelectionBackground());
			}
			else
			{
				setForeground(table.getForeground());
				setBackground(table.getBackground());
			}
			return this;
		}
	}

	private final class ComboBoxCellEditor extends RmaCellEditor implements TableCellRenderer
	{
		private final RmaJComboBox<String> _comboBox;

		private ComboBoxCellEditor(RmaJComboBox comboBox)
		{
			super(comboBox);
			comboBox.setEditable(true);
			_comboBox = comboBox;
		}

		@Override
		public boolean stopCellEditing()
		{
			Component editorComponent = _comboBox.getEditor().getEditorComponent();
			if(editorComponent instanceof JTextField)
			{
				JTextField textField = (JTextField) editorComponent;
				String text = textField.getText();
				_comboBox.setSelectedItem(text);
			}
			return super.stopCellEditing();
		}

		@Override
		public Component getTableCellEditorComponent(JTable table, Object value, boolean isSelected, int row, int column)
		{
			Component tableCellEditorComponent = super.getTableCellEditorComponent(table, value, isSelected, row, column);
			if(tableCellEditorComponent == _comboBox)
			{
				Object selectedItem = _comboBox.getSelectedItem();
				_comboBox.removeAllItems();
				if(column == A_PART_COLUMN)
				{
					Set<String> aParts = _scenarioDssTableModel.getAPartsForRow(row);
					aParts.forEach(_comboBox::addItem);
				}
				else if(column == ScenarioDssTableModel.F_PART_COLUMN)
				{
					Set<String> fParts = _scenarioDssTableModel.getFPartsForRow(row);
					fParts.forEach(_comboBox::addItem);
				}
				_comboBox.setSelectedItem(selectedItem);
			}
			return _comboBox;
		}

		@Override
		public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected, boolean hasFocus, int row, int column)
		{
			_comboBox.setSelectedItem(value);
			return _comboBox;
		}

	}

}
