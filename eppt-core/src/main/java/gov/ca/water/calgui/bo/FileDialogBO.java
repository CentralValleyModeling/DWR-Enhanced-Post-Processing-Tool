/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */

package gov.ca.water.calgui.bo;

//! Custom file chooser for selection of different CalLite file types

import java.awt.Component;
import java.awt.Container;
import java.awt.HeadlessException;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.HashMap;
import java.util.Map;
import javax.swing.*;

import gov.ca.water.calgui.constant.Constant;
import gov.ca.water.calgui.constant.EpptPreferences;
import gov.ca.water.calgui.tech_service.IErrorHandlingSvc;
import gov.ca.water.calgui.tech_service.impl.ErrorHandlingSvcImpl;
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
	private final DefaultListModel _lmScenNames;
	private JFileChooser _fc = new JFileChooser2();
	private int _dialogRC;
	private JTextField _theTextField;
	private boolean _theMultipleFlag = false;
	private String _theFileExt = null;
	private final JFrame _mainFrame;
	private IErrorHandlingSvc _errorHandlingSvc = new ErrorHandlingSvcImpl();


	/**
	 * Constructor for use with DSS files, result is appended to list and placed
	 * in textfield.
	 *
	 * @param aTextField
	 */
	public FileDialogBO(JTextField aTextField, JFrame mainFrame)
	{
		_mainFrame = mainFrame;
		_lmScenNames = null;
		_theFileExt = "DSS";
		_theTextField = aTextField;
		setup();
	}

	/**
	 * Constructor for use with arbitrary files, result is appended to list and
	 * placed in textfield.
	 *
	 * @param aTextField
	 * @param aFileExt
	 */
	public FileDialogBO(JTextField aTextField, String aFileExt, JFrame mainFrame)
	{
		_mainFrame = mainFrame;
		_lmScenNames = null;
		_theTextField = aTextField;
		_theFileExt = aFileExt;
		setup();
	}

	/**
	 * Constructor used for DSS files, result is appended to list, radiobuttons
	 * are enabled when list length greater than 1, button is enabled when list
	 * is not empty;
	 *
	 */
	public FileDialogBO(DefaultListModel<RBListItemBO> lmScenNames, boolean isMultiple, JFrame mainFrame)
	{
		_mainFrame = mainFrame;
		_theFileExt = "DSS";
		_theTextField = null;
		_lmScenNames = lmScenNames;
		setup();
		_theMultipleFlag = isMultiple;
	}

	public DefaultListModel getLmScenNames()
	{
		return _lmScenNames;
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
			else if("DSS2".equals(_theFileExt))
			{
				_theFileExt = "DSS";
				_fc.setFileFilter(new SimpleFileFilter("DSS"));
				_fc.setCurrentDirectory(EpptPreferences.getModelDssPath().toFile());
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
		catch(Exception e)
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
			Object obj = null;
			if(e != null)
			{
				obj = e.getSource();
			}
			if((obj != null) && (((Component) obj).getName() != null)
					&& "btnDelScenario".equals(((Component) obj).getName()))
			{
				deleteScenario();
			}
			else if("btnClearScenario".equals(e.getActionCommand()))
			{
				clearAllScenarios();
			}
			else
			{
				addScenario();
			}
		}
		catch(HeadlessException e1)
		{
			LOG.error(e1.getMessage());
			String messageText = "Unable to show file chooser.";
			_errorHandlingSvc.businessErrorHandler(messageText, e1);
		}
	}

	private void addScenario()
	{
		// Otherwise, show dialog
		int rc;
		if(_theMultipleFlag)
		{
			UIManager.put("FileChooser.openDialogTitleText", "Select Scenarios");
			_fc.setMultiSelectionEnabled(true);
			_dialogRC = _fc.showDialog(_mainFrame, "Select");
			if(_lmScenNames != null && _dialogRC != 1)
			{
				for(File file : _fc.getSelectedFiles())
				{
					addFileToList(file);
				}
			}
		}
		else
		{
			if(_theFileExt == null)
			{
				rc = _fc.showOpenDialog(_mainFrame);
			}
			else
			{
				rc = _fc.showDialog(_mainFrame,
						"DSS".equals(_theFileExt) ? "Open" : "Save");
			}
			_dialogRC = rc;
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
				if(_lmScenNames != null)
				{
					addFileToList(file);
				}
				else if(_theTextField != null)
				{
					_theTextField.setText(file.getName());
					_theTextField.setToolTipText(file.getPath());
				}
			}
		}
	}

	private void clearAllScenarios()
	{
		_lmScenNames.clear();
	}

	private void deleteScenario()
	{
		// If invoked by QR DelScenario button, delete a scenario from
		// Quick
		// Results scenario list
		if((_lmScenNames != null) && _lmScenNames.getSize() > 0)
		{
			int todel = -1;
			for(int i = 0; i < _lmScenNames.getSize(); i++)
			{
				if(((RBListItemBO) _lmScenNames.getElementAt(i)).isSelected())
				{
					todel = i;
				}
			}
			if(todel > 0)
			{
				((RBListItemBO) _lmScenNames.getElementAt(todel - 1)).setSelected(true);
			}
			else if(todel < _lmScenNames.getSize() - 1)
			{
				((RBListItemBO) _lmScenNames.getElementAt(todel + 1)).setSelected(true);
			}
			_lmScenNames.remove(todel);
		}
	}

	/**
	 * Adds file to list of scenarios if not already in list. Currently used to
	 * manage list of scenarios on Quick Result dashboard
	 *
	 * @param file
	 */
	public void addFileToList(File file)
	{
		try
		{
			boolean match = false;
			for(int i = 0; i < _lmScenNames.getSize(); i++)
			{
				RBListItemBO rbli = (RBListItemBO) _lmScenNames.getElementAt(i);
				match = match || (rbli.toString().equals(file.getPath()));
			}

			if(!match)
			{
				_lmScenNames.addElement(new RBListItemBO(file.getPath(), file.getName()));
				if(_lmScenNames.getSize() == 1)
				{
					((RBListItemBO) _lmScenNames.getElementAt(0)).setSelected(true);
				}
			}
		}
		catch(Exception e)
		{
			LOG.error(e.getMessage());
			String messageText = "Unable to update list.";
			_errorHandlingSvc.businessErrorHandler(messageText, e);
		}
	}


	/**
	 * Custom class shows scenario description in tooltip
	 *
	 * @author tslawecki
	 */
	private class FileNameRenderer extends DefaultListCellRenderer
	{

		private static final long serialVersionUID = -3040003845509293885L;

		private final JFileChooser2 _theOwner;
		private final Map<String, String> _theToolTips = new HashMap<>();

		private FileNameRenderer(JFileChooser2 jFileChooser)
		{
			_theOwner = jFileChooser;
		}

		@Override
		public Component getListCellRendererComponent(JList list, Object value, int index, boolean isSelected,
													  boolean cellHasFocus)
		{
			JLabel lbl = (JLabel) super.getListCellRendererComponent(list, value, index, isSelected, cellHasFocus);
			if(!_theOwner._toolTipFlag)
			{
				_theToolTips.clear();
				File folder = new File(System.getProperty("user.dir") + "\\Scenarios"); // change
				// to
				// read
				// current
				// directory
				File[] listOfFiles = folder.listFiles();
				if(listOfFiles != null)
				{
					for(final File listOfFile : listOfFiles)
					{
						if(listOfFile.isFile() && listOfFile.getName().toLowerCase().endsWith(".txt"))
						{
							try(FileInputStream fin = new FileInputStream(listOfFile);
								BufferedReader br = new BufferedReader(new InputStreamReader(fin)))
							{
								String theKey = br.readLine().toLowerCase();
								String theValue = br.readLine() + "\n" + br.readLine() + "\n" + br.readLine();
								_theToolTips.put(theKey.toLowerCase(), theValue);
							}
							catch(IOException e1)
							{
								LOG.debug(e1.getMessage());

							}
						}
						// need to flag when directory
						_theOwner._toolTipFlag = true;
						// changes
					}
				}
			}
			File file = new File(String.valueOf(value));
			String tooltip = _theToolTips.getOrDefault(file.getName().toLowerCase(),
					"No scenario information for this file");
			lbl.setToolTipText(tooltip);
			lbl.setText(file.getName());
			return lbl;
		}
	}

	/**
	 * Custom FileChooser that puts scenario description for *.cls (assumedin
	 * *.txt) in tooltip
	 *
	 * @author tslawecki
	 */
	private class JFileChooser2 extends javax.swing.JFileChooser
	{
		private static final long serialVersionUID = -150877374751505363L;

		private boolean _toolTipFlag = false;

		private Component findJList(Component comp)
		{

			if(comp instanceof JList)
			{
				return comp;
			}
			if(comp instanceof Container)
			{
				Component[] components = ((Container) comp).getComponents();
				for(final Component component : components)
				{
					Component child = findJList(component);
					if(child != null)
					{
						return child;
					}
				}
			}
			return null;
		}

		@Override
		public int showOpenDialog(Component c)
		{
			JList myList = (JList) findJList(this);
			if(myList != null)
			{
				myList.setCellRenderer(new FileNameRenderer(this));
			}
			_toolTipFlag = false;
			return super.showOpenDialog(c);
		}
	}

}
