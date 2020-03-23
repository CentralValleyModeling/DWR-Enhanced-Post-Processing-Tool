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
import java.io.IOException;
import java.io.InputStream;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLClassLoader;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;
import java.util.Objects;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.stream.Stream;
import javax.swing.*;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;

import gov.ca.water.calgui.EpptInitializationException;
import gov.ca.water.calgui.busservice.impl.DssPatternUpdater;
import gov.ca.water.calgui.constant.EpptPreferences;
import gov.ca.water.calgui.techservice.impl.DialogSvcImpl;
import gov.ca.water.calgui.techservice.impl.NonSymlinkFilter;

import hec.client.FileChooserFld;
import rma.swing.RmaJPanel;

import static java.util.stream.Collectors.toList;

final class EPPTOptionsPanel extends RmaJPanel
{
	private static final Logger LOGGER = Logger.getLogger(EPPTOptionsPanel.class.getName());
	private final EPPTOptionsOptionsPanelController _controller;
	private final JComboBox<Object> _resultsOutputComboBox;
	private final FileChooserFld _projectDirectoryFileChooserField;
	private final JTextField _wrimsDirectoryField;
	private final JLabel _wrimsVersionLabel = new JLabel();
	private final JButton _updateDssPatternButton;
	private boolean _resetPreferences;
	private JCheckBox _usePlotlyCheckbox;

	EPPTOptionsPanel(EPPTOptionsOptionsPanelController controller)
	{
		this._controller = controller;
		_resultsOutputComboBox = new JComboBox<>();
		_projectDirectoryFileChooserField = new FileChooserFld();
		_wrimsDirectoryField = new JTextField();
		_updateDssPatternButton = new JButton("Refresh DSS Pattern Matcher");
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
		DocumentListener documentListener = new MyDocumentListener();
		_projectDirectoryFileChooserField.getDocument().addDocumentListener(documentListener);
		_wrimsDirectoryField.getDocument().addDocumentListener(documentListener);
		_updateDssPatternButton.addActionListener(e ->
		{
			try
			{
				DssPatternUpdater.initPatterns();
			}
			catch(EpptInitializationException ex)
			{
				DialogSvcImpl.getDialogSvcInstance()
							 .getOK("Error updating DSS Pattern Matcher:\n" + ex.getMessage(), JOptionPane.WARNING_MESSAGE);
				LOGGER.log(Level.WARNING, "Error updating DSS Pattern Matcher", ex);
			}
		});
	}

	private void initComponents()
	{
		JButton wrimsDirButton = new JButton("...");
		wrimsDirButton.addActionListener(this::chooseWrimsDir);
		JButton resetButton = new JButton("Default");
		resetButton.addActionListener(this::resetPreferences);
		_projectDirectoryFileChooserField.setOpenDirectory();
		_projectDirectoryFileChooserField.setPreferredSize(
				new Dimension(300, 30));
		_resultsOutputComboBox.addItem("properties");
		_resultsOutputComboBox.addItem("editor");
		_resultsOutputComboBox.addItem("explorer");
		_usePlotlyCheckbox = new JCheckBox("Use Plotly for Plots");

		setLayout(new GridBagLayout());

		JPanel panel = new JPanel();
		panel.setLayout(new GridBagLayout());
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
		panel.add(new JLabel("WRIMS Version: "), new GridBagConstraints(1,
				4, 1, 1, .1, .5,
				GridBagConstraints.WEST, GridBagConstraints.BOTH,
				new Insets(5, 5, 5, 5), 5, 5));
		panel.add(_wrimsVersionLabel, new GridBagConstraints(2,
				4, 2, 1, .1, .5,
				GridBagConstraints.WEST, GridBagConstraints.BOTH,
				new Insets(5, 5, 5, 5), 5, 5));
		panel.add(_updateDssPatternButton, new GridBagConstraints(1,
				5, 3, 1, .1, .5,
				GridBagConstraints.EAST, GridBagConstraints.NONE,
				new Insets(5, 5, 5, 5), 5, 5));
		panel.add(resetButton, new GridBagConstraints(1,
				6, 1, 1, .1, .5,
				GridBagConstraints.WEST, GridBagConstraints.NONE,
				new Insets(5, 5, 5, 5), 5, 5));
		revalidate();
	}

	private void resetPreferences(ActionEvent e)
	{
		_projectDirectoryFileChooserField.setText(EpptPreferences.getDefaultProjectPath().toString());
		_projectDirectoryFileChooserField.setDefaultPath(EpptPreferences.getDefaultProjectPath().toString());
		_resultsOutputComboBox.setSelectedItem(EpptPreferences.getDefaultResultsOutputLocation());
		_usePlotlyCheckbox.setSelected(false);
		_wrimsDirectoryField.setText(EpptPreferences.getDefaultWrimsPath().toString());
		EpptPreferences.removeProjectsPathPreference();
		EpptPreferences.removeWrimsPathPreference();
		EpptPreferences.removeResultsOutputLocation();
		_resetPreferences = true;
		_updateDssPatternButton.getAction().actionPerformed(null);
	}

	private void chooseWrimsDir(ActionEvent e)
	{
		JFileChooser fileChooser = new JFileChooser(_wrimsDirectoryField.getText());
		fileChooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
		fileChooser.setFileFilter(new NonSymlinkFilter());
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
		_usePlotlyCheckbox.setSelected(EpptPreferences.usePlotly());
		updateWrimsVersion();
	}

	private void updateWrimsVersion()
	{
		_wrimsVersionLabel.setText("N/A");
		String text = _wrimsDirectoryField.getText();
		Path wrimsLibDir = Paths.get(text).resolve("lib");
		if(wrimsLibDir.toFile().exists() && wrimsLibDir.toFile().isDirectory())
		{
			try(Stream<Path> walk = Files.walk(wrimsLibDir, 1))
			{
				List<URL> urls = walk.filter(f -> f.toString().endsWith(".jar"))
									 .map(this::pathToUrl)
									 .filter(Objects::nonNull)
									 .collect(toList());
				try(URLClassLoader urlClassLoader = new URLClassLoader(urls.toArray(new URL[0])))
				{
					URL resource = urlClassLoader.findResource("wrimsv2/version.props");
					if(resource != null)
					{
						Properties properties = new Properties();
						try(InputStream inputStream = resource.openStream())
						{
							properties.load(inputStream);
							Object version = properties.get("version");
							if(version != null)
							{
								_wrimsVersionLabel.setText(version.toString());
							}
						}
					}
				}
			}
			catch(IOException ex)
			{
				LOGGER.log(Level.WARNING, "Unable to locate WRIMS jar to find version number", ex);
			}
		}
	}

	private URL pathToUrl(Path path)
	{
		try
		{
			return path.toUri().toURL();
		}
		catch(MalformedURLException e)
		{
			LOGGER.log(Level.FINE, "Cannot load jar: " + path, e);
			return null;
		}
	}

	void store()
	{
		if(!_resetPreferences)
		{
			EpptPreferences.setProjectsPath(_projectDirectoryFileChooserField.getText());
			EpptPreferences.setResultsOutputLocation(Objects.toString(_resultsOutputComboBox.getSelectedItem()));
			Path path = Paths.get(_wrimsDirectoryField.getText());
			if(!path.toAbsolutePath().startsWith(Paths.get("").toAbsolutePath()))
			{
				EpptPreferences.setWrimsPath(path);
			}
			else
			{
				EpptPreferences.removeWrimsPathPreference();
			}
			EpptPreferences.setUsePlotly(_usePlotlyCheckbox.isSelected());
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

	private final class MyDocumentListener implements DocumentListener
	{
		@Override
		public void insertUpdate(DocumentEvent e)
		{
			_resetPreferences = false;
			updateWrimsVersion();
			_controller.changed();
		}

		@Override
		public void removeUpdate(DocumentEvent e)
		{
			insertUpdate(e);
		}

		@Override
		public void changedUpdate(DocumentEvent e)
		{
			insertUpdate(e);
		}
	}
}
