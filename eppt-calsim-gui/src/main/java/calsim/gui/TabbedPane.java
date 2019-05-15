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

