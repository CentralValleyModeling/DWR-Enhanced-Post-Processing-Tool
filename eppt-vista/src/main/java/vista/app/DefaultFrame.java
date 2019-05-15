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

import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;
import javax.swing.*;

/**
 * A default quitable frame.
 *
 * @author Nicky Sandhu
 * @version $Id: DefaultFrame.java,v 1.1 2003/10/02 20:48:28 redwood Exp $
 */
public class DefaultFrame extends JFrame
{
	/**
	 * adds the given component to the center of the frame.
	 */
	public DefaultFrame()
	{
		WindowListener l = new DefaultWindowListener();
		addWindowListener(l);
	}

	/**
	 *
	 */
	private class DefaultWindowListener extends WindowAdapter
	{
		/**
		 *
		 */
		public final void windowClosing(WindowEvent e)
		{
			setVisible(false);
			dispose();
		}
	} // end of DefaultWindowListener

}
