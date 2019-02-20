/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */

package calsim.gui;

import java.awt.*;
import javax.swing.*;
//import java.awt.event.*;

/**
 * A default frame understands MPanel's. It queries
 * the panel for its menu bar and adds it to itself.
 *
 * @author Nicky Sandhu
 * @version $Id: DefaultInternalFrame.java,v 1.1.2.1 2001/04/18 21:07:40 jfenolio Exp $
 */
public class DefaultInternalFrame extends JInternalFrame
{
	/**
	 * creates a frame with the given MPanel and sets
	 * its menu bar accordingly
	 *
	 * @see MPanel
	 */
	public DefaultInternalFrame(MPanel panel)
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
		//CB    getContentPane().add(panel);
		getContentPane().add(panel, BorderLayout.CENTER);  //CB - but still CANNOT SEE TABLE
		//    setJMenuBar(panel.getJMenuBar());
		setTitle(panel.getFrameTitle());
	}
}
