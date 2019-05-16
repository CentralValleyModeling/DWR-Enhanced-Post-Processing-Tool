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
//import calsim.schematic.*;
//import calsim.schematic.input.*;

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

