/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */

package calsim.gui;

import java.awt.BorderLayout;
import javax.swing.*;
//import java.awt.event.*;

/**
 * A default frame understands MPanel's. It queries
 * the panel for its menu bar and adds it to itself.
 *
 * @author Nicky Sandhu
 * @version $Id: DefaultFrame.java,v 1.1.4.5 2000/12/20 20:07:05 amunevar Exp $
 */
public class DefaultFrame extends JFrame
{
	/**
	 * creates a frame with the given MPanel and sets
	 * its menu bar accordingly
	 *
	 * @see MPanel
	 */
	public DefaultFrame(MPanel panel)
	{
		getContentPane().setLayout(new BorderLayout());
		//setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
		setMPanel(panel);
    /*
    addWindowListener(new WindowAdapter() {
        public void windowClosing(WindowEvent evt){
          cleanup();
        }
        public void windowClosed(WindowEvent evt){
          cleanup();
        }
    });
    */
	}

	public void cleanup()
	{
		getContentPane().removeAll();
		super.dispose();
	}

	/**
	 * sets the main panel to this MPanel
	 *
	 * @see MPanel
	 */
	public void setMPanel(MPanel panel)
	{
		getContentPane().removeAll();
		getContentPane().add(panel);
		setJMenuBar(panel.getJMenuBar());
		setTitle(panel.getFrameTitle());
	}
}
