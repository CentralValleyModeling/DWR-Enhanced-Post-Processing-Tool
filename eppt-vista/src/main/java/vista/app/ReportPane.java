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

package vista.app;

import javax.swing.*;

/**
 * Just a way to force it to set page width??
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
