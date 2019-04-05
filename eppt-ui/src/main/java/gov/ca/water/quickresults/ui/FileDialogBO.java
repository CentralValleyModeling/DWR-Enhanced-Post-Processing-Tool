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

//! Custom file chooser for selection of different CalLite file types

import java.awt.Component;
import java.awt.HeadlessException;
import java.awt.Window;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import javax.swing.*;

import gov.ca.water.calgui.bo.SimpleFileFilter;
import gov.ca.water.calgui.constant.Constant;
import gov.ca.water.calgui.constant.EpptPreferences;
import gov.ca.water.calgui.techservice.IErrorHandlingSvc;
import gov.ca.water.calgui.techservice.impl.ErrorHandlingSvcImpl;
import org.apache.log4j.Logger;

/**
 * Supports selection of different types of CalLite files from customized
 * JFileChoosers.
 *
 * @author tslawecki
 */
public class FileDialogBO implements ActionListener
{

	private static final Logger LOG = Logger.getLogger(FileDialogBO.class.getName());
	private final String _theFileExt;
	private final JFileChooser _fc = new JFileChooser();
	private JTextField _theTextField;
	private Component _parentComponent;
	private IErrorHandlingSvc _errorHandlingSvc = new ErrorHandlingSvcImpl();


	/**
	 * Constructor for use with DSS files, result is appended to list and placed
	 * in textfield.
	 *
	 * @param aTextField
	 */
	public FileDialogBO(JTextField aTextField, Component mainFrame)
	{
		this(aTextField, "DSS", mainFrame);
	}

	/**
	 * Constructor for use with arbitrary files, result is appended to list and
	 * placed in textfield.
	 *
	 * @param aTextField
	 * @param aFileExt
	 */
	public FileDialogBO(JTextField aTextField, String aFileExt, Component mainFrame)
	{
		_parentComponent = mainFrame;
		_theTextField = aTextField;
		_theFileExt = aFileExt;
		setup();
	}

	private void setup()
	{

		try
		{
			// Set up file chooser

			if("DSS".equals(_theFileExt))
			{
				_fc.setFileFilter(new SimpleFileFilter("DSS"));
				_fc.setCurrentDirectory(EpptPreferences.getScenariosPaths().toFile());
			}
			else
			{
				_fc.setFileFilter(new SimpleFileFilter(_theFileExt));
				if("PDF".equals(_theFileExt) || "CLS".equals(_theFileExt))
				{
					_fc.setCurrentDirectory(EpptPreferences.getReportsPath().toFile());
				}
				else
				{
					_fc.setCurrentDirectory(new File(Constant.CONFIG_DIR));
				}
			}
		}
		catch(RuntimeException e)
		{
			LOG.error(e.getMessage());
			String messageText = "Unable to set up file dialog.";
			_errorHandlingSvc.businessErrorHandler(messageText, e);
		}
	}

	@Override
	public void actionPerformed(ActionEvent e)
	{
		try
		{
			setup();
			selectFile();
		}
		catch(HeadlessException e1)
		{
			LOG.error(e1.getMessage());
			String messageText = "Unable to show file chooser.";
			_errorHandlingSvc.businessErrorHandler(messageText, e1);
		}
	}

	private void selectFile()
	{
		// Otherwise, show dialog
		int rc;
		if(_theFileExt == null || "inp".equalsIgnoreCase(_theFileExt))
		{
			_fc.setFileSelectionMode(JFileChooser.FILES_ONLY);
			Window window = SwingUtilities.windowForComponent(_parentComponent);
			rc = _fc.showOpenDialog(window);
		}
		else
		{
			_fc.setFileSelectionMode(JFileChooser.FILES_ONLY);
			Window window = SwingUtilities.windowForComponent(_parentComponent);
			rc = _fc.showDialog(window,
					"DSS".equals(_theFileExt) ? "Open" : "Save");
		}
		File file;
		if(rc == 0)
		{
			file = _fc.getSelectedFile();
			if("PDF".equals(_theFileExt) && !file.getName().toLowerCase().endsWith(".pdf"))
			{
				file = new File(file.getPath() + ".PDF");
			}
			if("CLS".equals(_theFileExt) && !file.getName().toLowerCase().endsWith(".cls"))
			{
				file = new File(file.getPath() + ".CLS");
			}
			if(_theTextField != null)
			{
				_theTextField.setText(file.getName());
				_theTextField.setToolTipText(file.getPath());
			}
		}
	}


}
