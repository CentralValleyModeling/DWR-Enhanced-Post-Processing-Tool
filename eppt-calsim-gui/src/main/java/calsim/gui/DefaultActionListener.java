/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */

package calsim.gui;

import java.awt.Component;
import java.awt.Cursor;
import java.awt.Frame;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import javax.swing.*;

import vista.gui.WorkerActionListener;

/**
 * Default action listener
 *
 * @author Nicky Sandhu
 * @version $Id: DefaultActionListener.java,v 1.1.2.3 2001/07/12 01:59:34 amunevar Exp $
 */
public abstract class DefaultActionListener extends WorkerActionListener
{
	private Component _comp;
	private Frame _fr;
	private Component _glass;
	private Cursor _oldCursor;
	//  private String _preMsg, _postMsg;
	//  private String _postMsg;
	private MouseListener _ml = new MouseAdapter()
	{
		public void mousePressed(MouseEvent e)
		{
		}
	};

	/**
	 *
	 */
	public DefaultActionListener(Component comp, String preMsg)
	{
		this(comp, preMsg, "Done.");
	}

	/**
	 *
	 */
	public DefaultActionListener(Component comp, String preMsg, String postMsg)
	{
		_comp = comp;
		//    _preMsg = preMsg;
		//   _postMsg = postMsg;
		// set this to false if you don't want to use threads. Sets this
		// so for all GuiTaskListener instances. Also does it dynamically.
		USE_THREADS = true;
	}

	public void doPreWork()
	{
		if(_comp == null)
		{
			return;
		}
		_fr = JOptionPane.getFrameForComponent(_comp);
		if(_fr instanceof JFrame)
		{
			JFrame jfr = (JFrame) _fr;
			_glass = jfr.getGlassPane();
			_glass.addMouseListener(_ml);
			_oldCursor = jfr.getCursor();
			_glass.setVisible(true);
			_glass.setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
		}
		else
		{
			_fr.setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
		}
	}

	public void doPostWork()
	{
		if(_comp == null)
		{
			return;
		}
		if(_fr instanceof JFrame)
		{
			_glass.setCursor(_oldCursor);
			_glass.removeMouseListener(_ml);
			_glass.setVisible(false);
		}
		else
		{
			_fr.setCursor(_oldCursor);
		}
	}
}
