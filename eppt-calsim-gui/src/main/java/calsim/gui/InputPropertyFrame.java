/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */

package calsim.gui;
//import calsim.schematic.*;
//import calsim.schematic.input.*;

import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import javax.swing.*;

//import java.awt.*;
//import java.io.*;
//import vista.gui.VistaUtils;
//import calsim.app.AppProps;
//import calsim.gym.*;

/**
 * The input frame of Calsim  GUI
 *
 * @author YanPing Zuo
 * @version $Id: InputPropertyFrame.java,v 1.1.2.3 2001/07/12 01:59:42 amunevar Exp $
 */

public class InputPropertyFrame
{
	public static boolean DEBUG = true;
	public static int FRAME_WIDTH = 850;
	public static int FRAME_HEIGHT = 700;

	static
	{
		loadProps();
	}

	private Object _obj;

	/**
	 * constructor
	 */
	public InputPropertyFrame(Object obj)
	{
		_obj = obj;
		JFrame fr = new JFrame("" + _obj);/*
    fr.setIconImage(Toolkit.getDefaultToolkit().
                    createImage(VistaUtils.getImageAsBytes("/calsim/gui/calsimoas.gif")));*/
		fr.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		fr.setSize(FRAME_WIDTH, FRAME_HEIGHT);
		fr.setVisible(true);
		fr.setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE);
	}

	/**
	 *
	 */
	public static void loadProps()
	{
		//FRAME_WIDTH = new Integer(AppProps.getProperty("CalsimGui.FRAME_WIDTH")).intValue();
		//FRAME_HEIGHT = new Integer(AppProps.getProperty("CalsimGui.FRAME_HEIGHT")).intValue();
	}
}

