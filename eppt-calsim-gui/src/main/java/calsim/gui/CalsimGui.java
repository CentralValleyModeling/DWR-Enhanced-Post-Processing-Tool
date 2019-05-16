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

import java.awt.BorderLayout;
import java.awt.Container;
import java.awt.Toolkit;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import javax.swing.*;

import calsim.app.AppProps;
import vista.gui.VistaUtils;

//import java.io.*;

//import calsim.installer.CalsimBuilder;

/**
 * The main frme of Calsim  GUI
 *
 * @author YanPing Zuo, Armin Munever
 * @version $Id: CalsimGui.java,v 1.1.2.8 2001/10/23 16:28:35 jfenolio Exp $
 */

public class CalsimGui
{
	//public final String CALSIM_HOME = "D:\\WRIMS1Development\\Calsim1.3\\calsim"; // CB for IDE use (non-batch file)

	public static boolean DEBUG = true;
	public static int FRAME_WIDTH = 800;
	public static int FRAME_HEIGHT = 600;

	static
	{
		loadProps();
	}

	/**
	 * constructor
	 * create the main frame, retrieve the menu bar from the main panel,
	 * and display the main panel.
	 */
	public CalsimGui(String prjFile)
	{
		//    JFrame fr = new JFrame(GuiUtils.getProgramName()+GuiUtils.getVersionNo());
		JFrame fr = new JFrame(GuiUtils.getProgramName()); // version is already in About screen
		fr.setIconImage(Toolkit.getDefaultToolkit().
				createImage(VistaUtils.getImageAsBytes("/calsim/gui/calsimoas.gif")));
		Container pane = fr.getContentPane();
		pane.setLayout(new BorderLayout());
		TabbedPane tabbedPane = new TabbedPane();
		pane.add(tabbedPane.getTabbedPane(), BorderLayout.CENTER);
		pane.add(GuiUtils.getStatusPanel(), BorderLayout.SOUTH);
		//
		if(prjFile != null)
		{
			GuiUtils.getMainPanel().getMainMenuBar().openProject(prjFile);
		}
		else
		{
			GuiUtils.getMainPanel().getMessagePanel().updateMessagePanel();
		}
		//
		fr.addWindowListener(new WindowAdapter()
		{
			public void windowClosing(WindowEvent evt)
			{
				exit();
			}
		});
		fr.setSize(810, FRAME_HEIGHT);
		fr.setVisible(true);
		fr.setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE);
	}

	/**
	 * the main method
	 */
	public static void main(String[] args)
	{
		String prjFile = null;
		if(args.length == 1)
		{
			prjFile = args[0];
		}
		try
		{
			UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
		}
		catch(Exception e)
		{
			System.out.println(e.getMessage());
		}
		new CalsimGui(prjFile);
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

	/**
	 * close the window and exit
	 * if user made any change without saving, a warning will be displayed.
	 */
	void exit()
	{
		//
		saveProps();
		GuiUtils.getMainPanel().getMainMenuBar().fileExit();
		//
	}
	/**
	 * Private variables
	 */
	//  private MainPanel _panel;
}

