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
import vista.gui.CursorChangeListener;

//import java.awt.event.*;

/**
 * List all the dts listeners
 *
 * @author Nicky Sandhu
 * @version $Id: DeleteDTSListener.java,v 1.1.2.3 2000/12/20 20:07:06 amunevar Exp $
 */
public class DeleteDTSListener extends CursorChangeListener
{
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
				return;
			}
			AppUtils.getCurrentProject().remove(dts);
		}
		catch(Exception ex)
		{
			vista.gui.VistaUtils.displayException(mi, ex);
		}
	}
}
