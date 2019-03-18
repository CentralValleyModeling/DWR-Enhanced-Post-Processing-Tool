/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package gov.ca.water.eppt.nbui.options;

import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.nio.file.Paths;
import javax.swing.*;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;

import gov.ca.water.calgui.constant.EpptPreferences;

import hec.client.FileChooserFld;

final class EPPTOptionsPanel extends javax.swing.JPanel
{

	private final EPPTOptionsOptionsPanelController _controller;
	private final JComboBox<Object> _resultsOutputComboBox;
	private final FileChooserFld _projectDirectoryFileChooserField;

	EPPTOptionsPanel(EPPTOptionsOptionsPanelController controller)
	{
		this._controller = controller;
		_resultsOutputComboBox = new JComboBox<>();
		_projectDirectoryFileChooserField = new FileChooserFld();
		initComponents();
		initListeners();
	}

	private void initListeners()
	{
		_resultsOutputComboBox.addActionListener(e -> _controller.changed());
		_projectDirectoryFileChooserField.getDocument().addDocumentListener(new DocumentListener()
		{
			@Override
			public void insertUpdate(DocumentEvent e)
			{
				_controller.changed();
			}

			@Override
			public void removeUpdate(DocumentEvent e)
			{
				_controller.changed();
			}

			@Override
			public void changedUpdate(DocumentEvent e)
			{
				_controller.changed();
			}
		});
	}

	private void initComponents()
	{
		_projectDirectoryFileChooserField.setOpenDirectory();
		_projectDirectoryFileChooserField.setPreferredSize(
				new Dimension(300, _projectDirectoryFileChooserField.getPreferredSize().height));
		_resultsOutputComboBox.addItem("properties");
		_resultsOutputComboBox.addItem("editor");
		_resultsOutputComboBox.addItem("explorer");

		setLayout(new GridBagLayout());

		JPanel panel = new JPanel();
		panel.setLayout(new GridBagLayout());
		panel.setPreferredSize(new Dimension(500, 100));
		add(panel, new GridBagConstraints(1,
				1, 1, 1, .1, .5,
				GridBagConstraints.NORTHWEST, GridBagConstraints.NONE,
				new Insets(5, 5, 5, 5), 5, 5));
		panel.add(new JLabel("EPPT Project Directory:"), new GridBagConstraints(1,
				1, 1, 1, .1, .5,
				GridBagConstraints.WEST, GridBagConstraints.BOTH,
				new Insets(5, 5, 5, 5), 5, 5));
		panel.add(_projectDirectoryFileChooserField, new GridBagConstraints(2,
				1, 1, 1, 1.0, .5,
				GridBagConstraints.EAST, GridBagConstraints.HORIZONTAL,
				new Insets(5, 5, 5, 5), 5, 5));
		panel.add(new JLabel("Default Results Output Location:"), new GridBagConstraints(1,
				2, 1, 1, .1, .5,
				GridBagConstraints.WEST, GridBagConstraints.BOTH,
				new Insets(5, 5, 5, 5), 5, 5));
		panel.add(_resultsOutputComboBox, new GridBagConstraints(2,
				2, 1, 1, 1.0, .5,
				GridBagConstraints.EAST, GridBagConstraints.HORIZONTAL,
				new Insets(5, 5, 5, 5), 5, 5));
		revalidate();
	}

	void load()
	{
		_projectDirectoryFileChooserField.setText(EpptPreferences.getProjectsPath().toString());
		_projectDirectoryFileChooserField.setDefaultPath(EpptPreferences.getProjectsPath().toString());
		_resultsOutputComboBox.setSelectedItem(EpptPreferences.getResultsOutputLocation());
	}

	void store()
	{
		EpptPreferences.setProjectsPath(_projectDirectoryFileChooserField.getText());
		EpptPreferences.setResultsOutputLocation(_resultsOutputComboBox.getSelectedItem().toString());
	}

	boolean valid()
	{
		String text = _projectDirectoryFileChooserField.getText();
		return Paths.get(text).toFile().isDirectory() && Paths.get(text).toFile().exists();
	}
}
