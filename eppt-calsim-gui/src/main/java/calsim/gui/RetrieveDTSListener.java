/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
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
