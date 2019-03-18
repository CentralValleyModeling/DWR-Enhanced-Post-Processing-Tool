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
