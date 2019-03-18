/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */

package calsim.gui;
//import java.awt.*;

import javax.swing.*;
//import java.io.*;

/**
 * The tabbed pane of Calsim OAS GUI, which contains main panel, schematic editor, etc.
 *
 * @author YanPing Zuo
 * @version $Id: TabbedPane.java,v 1.1.2.14 2001/07/12 02:00:01 amunevar Exp $
 */

public class TabbedPane
{
	public static boolean DEBUG = true;
	/**
	 *
	 */
	private JTabbedPane _tabbedPane;

	/**
	 * constructor
	 * create a tabbed pane, which contains several tabs of main panel, schematic editor, etc.
	 */
	public TabbedPane()
	{
		_tabbedPane = createTabbedPane();
	}

	/**
	 * create the tabbed pane
	 */
	JTabbedPane createTabbedPane()
	{
		_tabbedPane = new JTabbedPane();
		_tabbedPane.addTab("Study   ", null, GuiUtils.createStudyTab(), "Study tabbed pane");
		_tabbedPane.addTab("Output    ", null, GuiUtils.createMainPanel(), "OutPut panel");
		return _tabbedPane;
	}

	/**
	 * Return the Node/Arc/General menu bar
	 */
	public JTabbedPane getTabbedPane()
	{
		return _tabbedPane;
	}
}

