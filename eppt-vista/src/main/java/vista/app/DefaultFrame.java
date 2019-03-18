/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
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
public class DefaultFrame extends JFrame {
	/**
	 * adds the given component to the center of the frame.
	 */
	public DefaultFrame() {
		WindowListener l = new DefaultWindowListener();
		addWindowListener(l);
	}

	/**
   *
   */
	private class DefaultWindowListener extends WindowAdapter {
		/**
   *
   */
		public final void windowClosing(WindowEvent e) {
			setVisible(false);
			dispose();
		}
	} // end of DefaultWindowListener

}
