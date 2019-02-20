/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */

package calsim.gui;

import java.awt.FileDialog;
import javax.swing.*;

import calsim.app.AppUtils;
import calsim.app.Project;
import calsim.debug.DebugSetting;
import vista.gui.VistaUtils;
import wrims.schematic.Schematic;
import wrims.schematic.SchematicUtils;

/**
 * The menu bar for the Node/Arc toggle button in the main panel
 *
 * @author Yan-Ping Zuo
 * @version $Id: NodeArcMenuBar.java,v 1.1.2.19 2001/10/23 16:28:45 jfenolio Exp $
 */

public class NodeArcMenuBar
{
	public static final boolean DEBUG = false;
	/*
	 * private variables
	 */
	private JTextField _nodetextfield;
	private JTextField _arctextfield;
	private MainPanel _mainPanel;
	private MainMenuBar _mainMenuBar;
	private JMenuBar _menuBar;

	/**
	 * constructor
	 * parameters: main panel in the CalsimOASGUI frame
	 */
	public NodeArcMenuBar(MainPanel mp)
	{
		if(mp != null)
		{
			_mainPanel = mp;
			_mainMenuBar = _mainPanel.getMainMenuBar();
			_menuBar = createMenuBar();
		}
	}

	/**
	 * create menu bar with menus
	 */
	public JMenuBar createMenuBar()
	{
		JMenuBar mbar = new JMenuBar();
		mbar.add(_mainMenuBar.getFileMenu());
		mbar.add(_mainMenuBar.getEditMenu());
		mbar.add(createNodeMenu());
		mbar.add(createArcMenu());
		mbar.add(createToolsMenu());
		mbar.add(_mainMenuBar.getHelpMenu());
		if(DebugSetting.DEBUG_FILEINPUT)
		{
			mbar.add(_mainMenuBar.getDebugMenu());
		}
		return mbar;
	}

	/**
	 * Return the Node/Arc menu bar
	 */
	public JMenuBar getMenuBar()
	{
		return _menuBar;
	}

	/**
	 * create Node menu
	 */
	JMenu createNodeMenu()
	{
		JMenu menu = new JMenu("Node      ");
		_nodetextfield = new JTextField("6", 10);  // get a default number from app ??
		_nodetextfield.setRequestFocusEnabled(true);
		_nodetextfield.requestFocus();   //not works, needs to be fixed ??
		JMenuItem storageitem = new JMenuItem("Storage EOP");
		JMenuItem inflowitem = new JMenuItem("Local Inflow");
		JMenuItem evapitem = new JMenuItem("Evaporation");
		JMenuItem surfaceitem = new JMenuItem("Surface Area");
		JMenuItem spillitem = new JMenuItem("Non-recoverable Spill");
		JMenuItem massbalanceitem = new JMenuItem("Mass Balance");
		storageitem.addActionListener(new GuiTaskListener("Retrieving storage...")
		{
			public void doWork()
			{
				nodeStorage(_nodetextfield.getText());
			}
		});
		//
		inflowitem.addActionListener(new GuiTaskListener("Retrieving inflow...")
		{
			public void doWork()
			{
				nodeInflow(_nodetextfield.getText());
			}
		});
		evapitem.addActionListener(new GuiTaskListener("Retrieving evaporation...")
		{
			public void doWork()
			{
				nodeEvap(_nodetextfield.getText());
			}
		});
		surfaceitem.addActionListener(new GuiTaskListener("Retrieving surface area...")
		{
			public void doWork()
			{
				nodeSurface(_nodetextfield.getText());
			}
		});
		spillitem.addActionListener(new GuiTaskListener("Retrieving spill...")
		{
			public void doWork()
			{
				nodeSpill(_nodetextfield.getText());
			}
		});
		massbalanceitem.addActionListener(new GuiTaskListener("Retrieving mass balance...")
		{
			public void doWork()
			{
				massBalance(_nodetextfield.getText());
			}
		});
		menu.add(_nodetextfield);
		menu.add(storageitem);
		menu.add(inflowitem);
		menu.add(evapitem);
		menu.add(surfaceitem);
		menu.add(spillitem);
		menu.add(massbalanceitem);
		menu.setToolTipText("Single time series of a node");
		menu.setMnemonic('n');
		return menu;
	}

	/**
	 * create Arc menu
	 */
	JMenu createArcMenu()
	{
		JMenu menu = new JMenu("Arc       ");
		_arctextfield = new JTextField("6", 10);   //get default number from app ??
		JMenuItem channelitem = new JMenuItem("Channel Flow");
		JMenuItem deliveryitem = new JMenuItem("Delivery");
		JMenuItem returnitem = new JMenuItem("Return Flow");
		JMenuItem minitem = new JMenuItem("Minimum Flow");
		channelitem.addActionListener(new GuiTaskListener("Retrieving channel flow...")
		{
			public void doWork()
			{
				channelFlow(_arctextfield.getText());
			}
		});
		deliveryitem.addActionListener(new GuiTaskListener("Retrieving delivery...")
		{
			public void doWork()
			{
				delivery(_arctextfield.getText());
			}
		});
		returnitem.addActionListener(new GuiTaskListener("Retrieving return flow...")
		{
			public void doWork()
			{
				returnFlow(_arctextfield.getText());
			}
		});
		minitem.addActionListener(new GuiTaskListener("Retrieving min flow...")
		{
			public void doWork()
			{
				minFlow(_arctextfield.getText());
			}
		});
		menu.add(_arctextfield);
		menu.add(channelitem);
		menu.add(deliveryitem);
		menu.add(returnitem);
		menu.add(minitem);
		menu.setToolTipText("Single time series of a arc");
		menu.setMnemonic('a');
		return menu;
	}

	/**
	 * create Tool menu
	 */
	JMenu createToolsMenu()
	{
		JMenu menu = new JMenu("Tools     ");

		JMenuItem schematicitem = new JMenuItem("Schematic Window");
		schematicitem.addActionListener(new GuiTaskListener("Starting schematic...")
		{
			public void doWork()
			{
				SchematicUtils.schematic = new Schematic();
			}
		});
		menu.add(schematicitem);
		menu.setToolTipText("Node/arc schematic of physical system");
		menu.setMnemonic('t');
		return menu;
	}

	/**
	 *
	 */
	void nodeStorage(String nodeId)
	{
		String bpart = "S" + nodeId.trim();
		String cpart = "STORAGE";
		GuiUtils.displayData(_mainPanel, bpart, cpart);
	}

	/**
	 * 1. Compose B-part and C-part and send them to package app.
	 * 2. Change the status bar and cursor.
	 * 3. If the filter produces more than one match from any one file or there is no match, error
	 * messages will be displayed in a dialog box.
	 */
	void nodeInflow(String nodeId)
	{
		String bpart = "I" + nodeId.trim();
		String cpart = "FLOW-INFLOW";
		GuiUtils.displayData(_mainPanel, bpart, cpart);
	}

	/**
	 * 1. Compose B-part and C-part and send them to package app.
	 * 2. Change the status bar and cursor.
	 * 3. If the filter produces more than one match from any one file or there is no match, error
	 * messages will be displayed in a dialog box.
	 */
	void nodeEvap(String nodeId)
	{
		String bpart = "E" + nodeId.trim();
		String cpart = "EVAPORATION";
		GuiUtils.displayData(_mainPanel, bpart, cpart);
	}

	/**
	 * 1. Compose B-part and C-part and send them to package app.
	 * 2. Change the status bar and cursor.
	 * 3. If the filter produces more than one match from any one file or there is no match, error
	 * messages will be displayed in a dialog box.
	 */
	void nodeSurface(String nodeId)
	{
		if(DEBUG)
		{
			System.out.println("nodeSurface");
		}
		String bpart = "A" + nodeId.trim();
		String cpart = "SURFACE-AREA";
		GuiUtils.displayData(_mainPanel, bpart, cpart);
	}

	/**
	 * 1. Compose B-part and C-part and send them to package app.
	 * 2. Change the status bar and cursor.
	 * 3. If the filter produces more than one match from any one file or there is no match, error
	 * messages will be displayed in a dialog box.
	 */
	void nodeSpill(String nodeId)
	{
		if(DEBUG)
		{
			System.out.println("nodeSpill");
		}
		String bpart = "F" + nodeId.trim();
		String cpart = "FLOW-SPILL-NON-RECOV";
		GuiUtils.displayData(_mainPanel, bpart, cpart);
	}

	/**
	 * 1. Display a file dialog box for user to choose a connectivity.cvs
	 * 2. Send the file name and the node number to package app.
	 * 3. Change the status bar and cursor.
	 */
	void massBalance(String nodenum)
	{
		if(DEBUG)
		{
			System.out.println("massBalance");
		}
		//send node number to app
		try
		{
			Project prj = AppUtils.getCurrentProject();
			if(prj.getNetwork() == null)
			{
				String netfile = VistaUtils.getFilenameFromDialog(_mainPanel, FileDialog.LOAD,
						"csv", "Network file");
				if(netfile == null)
				{
					return;
				}
				prj.setNetwork(netfile);
			}
			int nodeId = new Integer(nodenum);
			GuiUtils.displayMassBalanceData(_mainPanel, nodeId);
		}
		catch(Exception e)
		{
			VistaUtils.displayException(_mainPanel, e);
		}
	}

	/**
	 * 1. Compose B-part and C-part and send them to package app.
	 * 2. Change the status bar and cursor.
	 * 3. If the filter produces more than one match from any one file or there is no match, error
	 * messages will be displayed in a dialog box.
	 */
	void channelFlow(String nodeId)
	{
		if(DEBUG)
		{
			System.out.println("channelFlow");
		}
		String bpart = "C" + nodeId.trim();
		String cpart = "FLOW-CHANNEL";
		GuiUtils.displayData(_mainPanel, bpart, cpart);
	}

	/**
	 * 1. Compose B-part and C-part and send them to package app.
	 * 2. Change the status bar and cursor.
	 * 3. If the filter produces more than one match from any one file or there is no match, error
	 * messages will be displayed in a dialog box.
	 */
	void delivery(String nodeId)
	{
		if(DEBUG)
		{
			System.out.println("delivery");
		}
		String bpart = "D" + nodeId.trim();
		String cpart = "FLOW-DELIVERY";
		GuiUtils.displayData(_mainPanel, bpart, cpart);
	}

	/**
	 * 1. Compose B-part and C-part and send them to package app.
	 * 2. Change the status bar and cursor.
	 * 3. If the filter produces more than one match from any one file or there is no match, error
	 * messages will be displayed in a dialog box.
	 */
	void returnFlow(String nodeId)
	{
		if(DEBUG)
		{
			System.out.println("returnFlow");
		}
		String bpart = "R" + nodeId.trim();
		String cpart = "FLOW-RETURN";
		GuiUtils.displayData(_mainPanel, bpart, cpart);
	}

	/**
	 * 1. Compose B-part and C-part and send them to package app.
	 * 2. Change the status bar and cursor.
	 * 3. If the filter produces more than one match from any one file or there is no match, error
	 * messages will be displayed in a dialog box.
	 */
	void minFlow(String nodeId)
	{
		if(DEBUG)
		{
			System.out.println("minFlow");
		}
		String bpart = "C" + nodeId.trim() + "_MIF";
		String cpart = "FLOW-MIN-INSTREAM";
		GuiUtils.displayData(_mainPanel, bpart, cpart);
	}
} //end of class NodeArcMenuBar
