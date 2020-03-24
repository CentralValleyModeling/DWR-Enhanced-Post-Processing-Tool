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

package gov.ca.water.eppt.nbui.actions;

import java.awt.BorderLayout;
import java.awt.Frame;
import java.io.File;
import java.nio.file.Path;
import java.nio.file.Paths;

import javax.swing.*;
import javax.swing.border.EmptyBorder;

import gov.ca.water.calgui.constant.EpptPreferences;
import gov.ca.water.calgui.techservice.impl.NonSymlinkFilter;
import org.openide.windows.WindowManager;

import hec.gui.NameDialog;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 12-27-2019
 */
class NewProjectDialog extends NameDialog
{
	private final JTextField _filePathTextField;

	NewProjectDialog()
	{
		super(WindowManager.getDefault().getMainWindow(), true);
		JPanel fileChooserPanel = new JPanel();
		fileChooserPanel.setBorder(new EmptyBorder(5, 7, 5, 0));
		fileChooserPanel.setLayout(new BorderLayout(5, 5));
		JLabel jLabel = new JLabel("Projects Directory:");
		fileChooserPanel.add(jLabel, BorderLayout.WEST);
		_filePathTextField = new JTextField(EpptPreferences.getProjectsPath().toString());
		fileChooserPanel.add(_filePathTextField, BorderLayout.CENTER);
		JButton button = new JButton("...");
		button.addActionListener(e ->
		{
			JFileChooser fileChooser = new JFileChooser(EpptPreferences.getProjectsPath().toString());
			fileChooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
			fileChooser.setFileFilter(new NonSymlinkFilter());
			fileChooser.setDialogTitle("Choose Projects Directory");
			int saveDialog = fileChooser.showSaveDialog(WindowManager.getDefault().getMainWindow());
			if(saveDialog == JFileChooser.APPROVE_OPTION)
			{
				File selectedFile = fileChooser.getSelectedFile();
				_filePathTextField.setText(selectedFile.toString());
			}
		});
		fileChooserPanel.add(button, BorderLayout.EAST);
		addAuxilaryPanel(fileChooserPanel);
	}

	Path getProjectDirectory()
	{
		String name = getName();
		return Paths.get(_filePathTextField.getText()).resolve(name);
	}
}
