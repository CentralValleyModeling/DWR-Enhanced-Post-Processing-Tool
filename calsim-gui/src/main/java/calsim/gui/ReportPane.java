/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */

package calsim.gui;

import javax.swing.*;

/**
 * Just a way to force it to set page width??
 *
 * @author Nicky Sandhu
 * @version $Id: ReportPane.java,v 1.1.2.3 2001/07/12 01:59:55 amunevar Exp $
 */
public class ReportPane extends JTextPane
{
	/**
	 *
	 */
	public boolean getScrollableTracksViewportWidth()
	{
		return false;
	}
}
