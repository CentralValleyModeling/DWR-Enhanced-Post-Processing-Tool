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
package vista.app;

import java.awt.BorderLayout;
import java.util.ArrayList;
import javax.swing.*;

import vista.db.dss.DSSUtil;
import vista.gui.Changeable;
import vista.gui.DialogButtonPanel;

/**
 * A modal dialog to establish connection with server
 *
 * @author Nicky Sandhu
 * @version $Id: OpenConnectionDialog.java,v 1.1 2003/10/02 20:48:36 redwood Exp
 * $
 */
public class OpenConnectionDialog extends JDialog implements Changeable
{
	/**
	 *
	 */
	private static final boolean DEBUG = false;
	/**
	 *
	 */
	private static ArrayList<String> _directoryHistory, _serverHistory;
	private View _view;
	/**
	 * server and directory
	 */
	private JComboBox _serverField, _databaseField;

	/**
	 * sets up a blocking dialog displaying server and directory names
	 */
	public OpenConnectionDialog(JFrame parent, View view)
	{
		super(parent, true);
		_view = view;
		// set up connection panel
		JPanel connectionPanel = new JPanel();
		connectionPanel.setLayout(new BorderLayout());
		// add server panel
		if(_serverHistory == null)
		{
			_serverHistory = new ArrayList<String>();
			_serverHistory.add("");
		}
		_serverField = new JComboBox(_serverHistory.toArray());
		_serverField.setEditable(true);
		_serverField.setSelectedIndex(_serverField.getItemCount() - 1);
		JPanel serverPanel = new JPanel();
		serverPanel.setLayout(new BorderLayout());
		serverPanel.add(new JLabel("SERVER IP/DOMAIN NAME: "),
				BorderLayout.WEST);
		serverPanel.add(_serverField, BorderLayout.CENTER);
		// add directory panel
		if(_directoryHistory == null)
		{
			_directoryHistory = new ArrayList<String>();
			_directoryHistory.add(DSSUtil.getDefaultDirectory());
		}
		_databaseField = new JComboBox(_directoryHistory.toArray());
		_databaseField.setEditable(true);
		_databaseField.setSelectedIndex(_databaseField.getItemCount() - 1);
		JPanel directoryPanel = new JPanel();
		directoryPanel.setLayout(new BorderLayout());
		directoryPanel.add(new JLabel("Database: "), BorderLayout.WEST);
		directoryPanel.add(_databaseField, BorderLayout.CENTER);
		// add server and directory panels
		connectionPanel.add(serverPanel, BorderLayout.NORTH);
		connectionPanel.add(directoryPanel, BorderLayout.SOUTH);
		// add connection panel
		super.getContentPane().setLayout(new BorderLayout());
		super.getContentPane().add(connectionPanel, BorderLayout.CENTER);
		super.getContentPane().add(new DialogButtonPanel(this),
				BorderLayout.SOUTH);
		pack();
	}

	/**
	 * gets server name.
	 */
	public String getServer()
	{
		String server = (String) _serverField.getSelectedItem();
		return server;
	}

	/**
	 * returns directory name.
	 */
	public String getDirectory()
	{
		String str = (String) _databaseField.getSelectedItem();
		return str;
	}

	/**
	 * Apply the changes (OK/Apply button pressed)
	 */
	public void applyChanges()
	{
		if(DEBUG)
		{
			System.out.println("Making connection to " + getServer());
		}
		if(DEBUG)
		{
			System.out.println("Directory " + getDirectory());
		}
		final String server = getServer();
		final String dir = getDirectory();
		if(!_serverHistory.contains(server))
		{
			_serverHistory.add(server);
		}
		if(!_directoryHistory.contains(server))
		{
			_directoryHistory.add(dir);
		}
	}

	/**
	 * Done with making changes (OK/Cancel button pressed)
	 */
	public void doneChanges()
	{
		this.dispose();
	}
}
