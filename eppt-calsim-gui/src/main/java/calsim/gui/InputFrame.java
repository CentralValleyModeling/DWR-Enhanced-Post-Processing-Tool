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

import java.awt.Color;
import java.awt.Container;
import java.io.IOException;
import javax.swing.*;

import calsim.app.AppProps;
import calsim.gym.Network;
import calsim.schematic.CalsimSchematic;
import calsim.schematic.NetworkSchematicData;
import vista.gui.VistaUtils;

//import calsim.schematic.input.*;

/**
 * The input frame of Calsim  GUI
 *
 * @author YanPing Zuo
 * @version $Id: InputFrame.java,v 1.1.2.3 2001/07/12 01:59:41 amunevar Exp $
 */

public class InputFrame
{
	public static boolean DEBUG = true;
	public static int FRAME_WIDTH = 950;
	public static int FRAME_HEIGHT = 800;

	static
	{
		loadProps();
	}

	/**
	 * constructor
	 * create the main frame, retrieve the menu bar from the main panel,
	 * and display the main panel.
	 */
	public InputFrame()
	{
		JFrame fr = new JFrame("Input Frame");/*
    fr.setIconImage(Toolkit.getDefaultToolkit().
                    createImage(VistaUtils.getImageAsBytes("/calsim/gui/calsimoas.gif")));*/
		Container pane = fr.getContentPane();
		try
		{
			String preName = "/calsimoas/calsim/testdata/sample1";
			Network net1 = Network.read(preName + ".net");
			NetworkSchematicData nsd1 = new NetworkSchematicData(net1, preName + ".xy");
			CalsimSchematic cs = new CalsimSchematic(nsd1);
			cs.setBackgroundColor(Color.white);
			pane.add(new InputSchematicPanel(cs));
		}
		catch(IOException ioe)
		{
			VistaUtils.displayException(fr, ioe);
		}
		fr.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		fr.setSize(FRAME_WIDTH, FRAME_HEIGHT);
		fr.setVisible(true);
		fr.setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE);
	}

	/**
	 * the main method
	 */
	public static void main(String[] args)
	{
		new InputFrame();
	}

	/**
	 *
	 */
	public static void loadProps()
	{
		FRAME_WIDTH = new Integer(AppProps.getProperty("CalsimGui.FRAME_WIDTH")).intValue();
		FRAME_HEIGHT = new Integer(AppProps.getProperty("CalsimGui.FRAME_HEIGHT")).intValue();
	}

	/**
	 *
	 */
	public static void saveProps()
	{
		AppProps.setProperty("CalsimGui.FRAME_WIDTH", new Integer(FRAME_WIDTH).toString());
		AppProps.setProperty("CalsimGui.FRAME_HEIGHT", new Integer(FRAME_HEIGHT).toString());
	}
}

