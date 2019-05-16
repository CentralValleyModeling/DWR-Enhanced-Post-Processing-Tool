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

import vista.gui.CursorChangeListener;
//import java.awt.*;
//import java.awt.event.*;
//import javax.swing.*;

/**
 * Task listener for the Gui package which determines cursor changes and status messages
 *
 * @author Nicky Sandhu ,Armin Munevar
 * @version $Id: GuiTaskListener.java,v 1.1.2.8 2001/07/12 01:59:38 amunevar Exp $
 */
public abstract class GuiTaskListener extends CursorChangeListener
{
	//  private Frame _fr;
	//  private Component _glass;
	//  private Cursor _oldCursor;
	private String _preMsg, _postMsg;

	//  private MouseListener _ml = new MouseAdapter(){
	//    public void mousePressed(MouseEvent e) {}
	//  };
	public GuiTaskListener(String preMsg)
	{
		this(preMsg, "Done.");
	}

	public GuiTaskListener(String preMsg, String postMsg)
	{
		_preMsg = preMsg;
		_postMsg = postMsg;
		// set this to false if you don't want to use threads. Sets this
		// so for all GuiTaskListener instances. Also does it dynamically.
		//    USE_THREADS=true;
	}

	/**
	 *
	 */
	public void doPreWork()
	{
		super.doPreWork();
		GuiUtils.setStatus(_preMsg);
	}

	/**
	 *
	 */
	public void doPostWork()
	{
		super.doPostWork();
		GuiUtils.setStatus(_postMsg);
	}
}


