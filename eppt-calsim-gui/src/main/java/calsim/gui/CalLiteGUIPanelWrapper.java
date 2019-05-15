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

/**
 *
 */
package calsim.gui;

import java.awt.BorderLayout;
import javax.swing.*;

/**
 * Class creates a <b>JPanel</b> version of the Calsim OAS GUI. The code is
 * modified from TabbedPane.java. <br>
 * <br>
 * "tabbed pane of Calsim OAS GUI, which contains main panel, schematic editor, etc."
 * <br>
 * "@version $Id: TabbedPane.java,v 1.1.2.14 2001/07/12 02:00:01 amunevar Exp $"
 *
 * @author tslawecki
 *
 */
public class CalLiteGUIPanelWrapper
{

	/**
	 *
	 */
	private JTabbedPane _tabbedPane;
	private JPanel _panel; // LimnoTech

	/**
	 * constructor - creates a panel <s>tabbed pane</s>, which contains several
	 * tabs of main panel, schematic editor, etc.
	 */
	public CalLiteGUIPanelWrapper()
	{
		_panel = createPanel();
	}

	/**
	 * Parallel creation of panel only -- LimnoTech
	 *
	 * @return
	 */
	private JPanel createPanel()
	{
		_panel = new JPanel();
		_panel.setLayout(new BorderLayout());
		_panel.add(GuiUtils.createMainPanelCLG(), BorderLayout.CENTER);
		return _panel;

	}

	/**
	 * Return the Node/Arc/General menu bar
	 */
	public JTabbedPane getTabbedPane()
	{
		return _tabbedPane;
	}

	public JPanel getPanel()
	{
		return _panel;
	}
}
