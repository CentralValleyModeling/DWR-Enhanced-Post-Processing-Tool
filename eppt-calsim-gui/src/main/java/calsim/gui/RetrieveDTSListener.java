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

import javax.swing.*;

import calsim.app.AppUtils;
import calsim.app.DerivedTimeSeries;
//import java.awt.event.*;
//import vista.gui.*;

/**
 * Retrieves a DTS and displays in correct view
 *
 * @author Nicky Sandhu
 * @version $Id: RetrieveDTSListener.java,v 1.1.2.9 2001/07/12 01:59:57 amunevar Exp $
 */
public class RetrieveDTSListener extends GuiTaskListener
{
	public static boolean DEBUG = true;
	private JMenuItem _mi = null;
	private String _dtsName = null;

	/**
	 *
	 */
	public RetrieveDTSListener()
	{
		super("Retrieving...");
	}

	/**
	 *
	 */
	public RetrieveDTSListener(JMenuItem mi, String dtsName)
	{
		super("Retrieving " + dtsName + "...");
		_mi = mi;
		_dtsName = dtsName;
	}

	/**
	 * Invoked when a menu item is clickedon.
	 */
	public void doWork()
	{
		JMenuItem mi = null;
		String dtsName = null;
		if(_mi == null)
		{
			mi = (JMenuItem) super.getComponent();
		}
		else
		{
			mi = _mi;
		}
		if(_dtsName == null)
		{
			dtsName = mi.getText().toUpperCase();
		}
		else
		{
			dtsName = _dtsName;
		}
		try
		{
			// first try the current project
			DerivedTimeSeries dts = AppUtils.getCurrentProject().getDTS(dtsName);
			if(dts == null)
			{
				dts = AppUtils.getGlobalDTS(dtsName);
			}
			if(dts == null)
			{
				return;
			}
			GuiUtils.displayDTS(dts);
		}
		catch(Exception ex)
		{
			vista.gui.VistaUtils.displayException(mi, ex);
		}
	}
}
