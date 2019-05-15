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
import vista.gui.CursorChangeListener;

//import java.awt.event.*;

/**
 * Shows the MTS in a MTS Table
 *
 * @author Nicky Sandhu
 * @version $Id: DeleteMTSListener.java,v 1.1.2.3 2000/12/20 20:07:07 amunevar Exp $
 */
public class DeleteMTSListener extends CursorChangeListener
{
	public static boolean DEBUG = true;

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
			MultipleTimeSeries mts = AppUtils.getCurrentProject().getMTS(mtsName);
			if(mts == null)
			{
				return;
			}
			AppUtils.getCurrentProject().remove(mts);
		}
		catch(Exception ex)
		{
			vista.gui.VistaUtils.displayException(mi, ex);
		}
	}
}
