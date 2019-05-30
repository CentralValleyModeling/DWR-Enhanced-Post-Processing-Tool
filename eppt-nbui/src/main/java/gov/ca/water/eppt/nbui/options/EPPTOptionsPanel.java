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
package gov.ca.water.eppt.nbui.options;

import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.io.File;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Objects;
import javax.swing.*;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;

import gov.ca.water.calgui.constant.EpptPreferences;

import hec.client.FileChooserFld;

final class EPPTOptionsPanel extends JPanel
{

	private final EPPTOptionsOptionsPanelController _controller;
	private final JComboBox<Object> _resultsOutputComboBox;
	private final FileChooserFld _projectDirectoryFileChooserField;
	private final JTextField _wrimsDirectoryField;
	private boolean _resetPreferences;

	EPPTOptionsPanel(EPPTOptionsOptionsPanelController controller)
	{
		this._controller = controller;
		_resultsOutputComboBox = new JComboBox<>();
		_projectDirectoryFileChooserField = new FileChooserFld();
		_wrimsDirectoryField = new JTextField();
		initComponents();
		initListeners();
	}

	private void initListeners()
	{
		_resultsOutputComboBox.addActionListener(e ->
		{
			_resetPreferences = false;
			_controller.changed();
		});
		DocumentListener documentListener = new DocumentListener()
		{
			@Override
			public void insertUpdate(DocumentEvent e)
			{
				_resetPreferences = false;
				_controller.changed();
			}

			@Override
			public void removeUpdate(DocumentEvent e)
			{
				_resetPreferences = false;
				_controller.changed();
			}

			@Override
			public void changedUpdate(DocumentEvent e)
			{
				_resetPreferences = false;
				_controller.changed();
			}
		};
		_projectDirectoryFileChooserField.getDocument().addDocumentListener(documentListener);
		_wrimsDirectoryField.getDocument().addDocumentListener(documentListener);
	}

	private void initComponents()
	{
		JButton wrimsDirButton = new JButton("...");
		Dimension buttonDimensions = new Dimension(60, wrimsDirButton.getPreferredSize().height);
		wrimsDirButton.setPreferredSize(buttonDimensions);
		wrimsDirButton.addActionListener(this::chooseWrimsDir);
		JButton resetButton = new JButton("Default");
		resetButton.addActionListener(this::resetPreferences);
		resetButton.setPreferredSize(buttonDimensions);
		_projectDirectoryFileChooserField.setOpenDirectory();
		_projectDirectoryFileChooserField.setPreferredSize(
				new Dimension(300, _projectDirectoryFileChooserField.getPreferredSize().height));
		_resultsOutputComboBox.addItem("properties");
		_resultsOutputComboBox.addItem("editor");
		_resultsOutputComboBox.addItem("explorer");

		setLayout(new GridBagLayout());

		JPanel panel = new JPanel();
		panel.setLayout(new GridBagLayout());
		panel.setPreferredSize(new Dimension(500, 120));
		add(panel, new GridBagConstraints(1,
				1, 1, 1, .1, .5,
				GridBagConstraints.NORTHWEST, GridBagConstraints.NONE,
				new Insets(5, 5, 5, 5), 5, 5));
		panel.add(new JLabel("EPPT Project Directory:"), new GridBagConstraints(1,
				1, 1, 1, .1, .5,
				GridBagConstraints.WEST, GridBagConstraints.BOTH,
				new Insets(5, 5, 5, 5), 5, 5));
		panel.add(_projectDirectoryFileChooserField, new GridBagConstraints(2,
				1, 2, 1, 1.0, .5,
				GridBagConstraints.EAST, GridBagConstraints.HORIZONTAL,
				new Insets(5, 5, 5, 5), 5, 5));
		panel.add(new JLabel("Default Results Output Location:"), new GridBagConstraints(1,
				2, 1, 1, .1, .5,
				GridBagConstraints.WEST, GridBagConstraints.BOTH,
				new Insets(5, 5, 5, 5), 5, 5));
		panel.add(_resultsOutputComboBox, new GridBagConstraints(2,
				2, 2, 1, 1.0, .5,
				GridBagConstraints.EAST, GridBagConstraints.HORIZONTAL,
				new Insets(5, 5, 5, 5), 5, 5));
		panel.add(new JLabel("WRIMS Installation Directory:"), new GridBagConstraints(1,
				3, 1, 1, .1, .5,
				GridBagConstraints.WEST, GridBagConstraints.BOTH,
				new Insets(5, 5, 5, 5), 5, 5));
		panel.add(_wrimsDirectoryField, new GridBagConstraints(2,
				3, 1, 1, 1.0, .5,
				GridBagConstraints.EAST, GridBagConstraints.HORIZONTAL,
				new Insets(5, 5, 5, 5), 5, 5));
		panel.add(wrimsDirButton, new GridBagConstraints(3,
				3, 1, 1, .1, .5,
				GridBagConstraints.EAST, GridBagConstraints.HORIZONTAL,
				new Insets(5, 5, 5, 5), 5, 5));
		panel.add(resetButton, new GridBagConstraints(3,
				4, 1, 1, .1, .5,
				GridBagConstraints.EAST, GridBagConstraints.HORIZONTAL,
				new Insets(5, 5, 5, 5), 5, 5));
		revalidate();
	}

	private void resetPreferences(ActionEvent e)
	{
		_projectDirectoryFileChooserField.setText(EpptPreferences.getDefaultProjectPath().toString());
		_projectDirectoryFileChooserField.setDefaultPath(EpptPreferences.getDefaultProjectPath().toString());
		_resultsOutputComboBox.setSelectedItem(EpptPreferences.getDefaultResultsOutputLocation());
		_wrimsDirectoryField.setText(EpptPreferences.getDefaultWrimsPath().toString());
		EpptPreferences.removeProjectsPathPreference();
		EpptPreferences.removeWrimsPathPreference();
		EpptPreferences.removeResultsOutputLocation();
		_resetPreferences = true;
	}

	private void chooseWrimsDir(ActionEvent e)
	{
		JFileChooser fileChooser = new JFileChooser(_wrimsDirectoryField.getText());
		fileChooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
		fileChooser.showOpenDialog(this);
		File selectedFile = fileChooser.getSelectedFile();
		if(selectedFile != null)
		{
			_wrimsDirectoryField.setText(selectedFile.toString());
		}
	}

	void load()
	{
		_projectDirectoryFileChooserField.setText(EpptPreferences.getProjectsPath().toString());
		_projectDirectoryFileChooserField.setDefaultPath(EpptPreferences.getProjectsPath().toString());
		_resultsOutputComboBox.setSelectedItem(EpptPreferences.getResultsOutputLocation());
		_wrimsDirectoryField.setText(EpptPreferences.getWrimsPath().toString());
	}

	void store()
	{
		if(!_resetPreferences)
		{
			EpptPreferences.setProjectsPath(_projectDirectoryFileChooserField.getText());
			EpptPreferences.setResultsOutputLocation(Objects.toString(_resultsOutputComboBox.getSelectedItem()));
			EpptPreferences.setWrimsPath(Paths.get(_wrimsDirectoryField.getText()));
		}
	}

	boolean valid()
	{
		String text = _projectDirectoryFileChooserField.getText();
		boolean projectDirExists = Paths.get(text).toFile().isDirectory() && Paths.get(text).toFile().exists();
		Path wrimsDir = Paths.get(_wrimsDirectoryField.getText());
		boolean wrimsDirExists = wrimsDir.toFile().exists();

		if(wrimsDirExists)
		{
			Path javaExe = wrimsDir.resolve("jre").resolve("bin").resolve("java.exe");
			Path wrimsLib = wrimsDir.resolve("lib");
			Path wrimsSys = wrimsLib.resolve("sys");
			wrimsDirExists = javaExe.toFile().exists() && wrimsLib.toFile().exists() && wrimsSys.toFile().exists();
		}
		return projectDirExists && wrimsDirExists;
	}
}
