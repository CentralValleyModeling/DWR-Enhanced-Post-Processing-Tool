/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */

package calsim.gui;

import javax.swing.*;

import calsim.app.AppUtils;
import calsim.app.MultipleTimeSeries;
//import java.awt.event.*;
//import vista.gui.*;

/**
 * Retrieves a MTS and displays in correct view
 *
 * @author Nicky Sandhu
 * @version $Id: RetrieveMTSListener.java,v 1.1.2.6 2001/07/12 01:59:58 amunevar Exp $
 */
public class RetrieveMTSListener extends GuiTaskListener
{
	public static boolean DEBUG = true;

	/**
	 *
	 */
	public RetrieveMTSListener()
	{
		super("Retrieving...");
	}

	/**
	 * Invoked when a menu item is clickedon.
	 */
	public void doWork()
	{
		JMenuItem mi = (JMenuItem) super.getComponent();
		String mtsName = mi.getText().toUpperCase().trim();
		try
		{
			if(DEBUG)
			{
				System.out.println("finding mts " + mtsName);
			}
			MultipleTimeSeries mts = AppUtils.findMTS(mtsName);
			if(mts == null)
			{
				return;
			}
			if(DEBUG)
			{
				System.out.println("Retrieving Data for " + mts.getName());
			}
			GuiUtils.displayMTS(mts);
		}
		catch(Exception ex)
		{
			vista.gui.VistaUtils.displayException(mi, ex);
		}
	}
}
