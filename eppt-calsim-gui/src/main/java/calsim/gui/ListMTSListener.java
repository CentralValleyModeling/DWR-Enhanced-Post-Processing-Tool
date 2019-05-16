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
import calsim.app.MultipleTimeSeries;
//import java.awt.event.*;
//import vista.gui.*;

/**
 * Shows the MTS in a MTS Table
 *
 * @author Nicky Sandhu
 * @version $Id: ListMTSListener.java,v 1.1.2.4 2000/12/20 20:07:15 amunevar Exp $
 */
public class ListMTSListener extends GuiTaskListener
{
	public static boolean DEBUG = true;

	/**
	 *
	 */
	public ListMTSListener()
	{
		super("Listing...");
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
				System.out.println("Showing in a table: " + mts.getName());
			}
			JFrame fr = new DefaultFrame(new MTSTable(mts));
			fr.setSize(400, 250);
			fr.setVisible(true);
		}
		catch(Exception ex)
		{
			vista.gui.VistaUtils.displayException(mi, ex);
		}
	}
}
