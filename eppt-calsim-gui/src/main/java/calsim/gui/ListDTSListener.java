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
 * List all the dts listeners
 *
 * @author Nicky Sandhu
 * @version $Id: ListDTSListener.java,v 1.1.2.6 2000/12/20 20:07:15 amunevar Exp $
 */
public class ListDTSListener extends GuiTaskListener
{
	/**
	 *
	 */
	public ListDTSListener()
	{
		super("Listing...");
	}

	/**
	 * Invoked when a menu item is clickedon.
	 */
	public void doWork()
	{
		JMenuItem mi = (JMenuItem) super.getComponent();
		String dtsName = mi.getText().toUpperCase();
		try
		{
			DerivedTimeSeries dts = AppUtils.getCurrentProject().getDTS(dtsName);
			if(dts == null)
			{
				dts = AppUtils.getGlobalDTS(dtsName);
			}
			if(dts == null)
			{
				return;
			}
			JFrame fr = new DefaultFrame(new DTSTable(dts));
			fr.setSize(400, 250);
			fr.setVisible(true);
		}
		catch(Exception ex)
		{
			vista.gui.VistaUtils.displayException(mi, ex);
		}
	}
}
