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

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import javax.swing.*;

import vista.graph.Graph;
import vista.gui.VistaUtils;

/**
 * This class constructs a frame and provides the context with which to interact
 * with the Graph object. The Graph object itself is contained with the
 * GraphCanvas object
 * 
 * @see Graph
 * @see HEC.DSS.GraphCanvas
 * @author Nicky Sandhu
 * @version $Id: DataGraph.java,v 1.1 2003/10/02 20:48:25 redwood Exp $
 */
public class DataGraphFrame extends JFrame{
	/**
	 * for debuggin'
	 */
	public static boolean DEBUG = false;
	private DataSetGraph dataGraph;

	/**
	 * shows graph in a frame with frame title and shows it if isVisible is true
	 * 
	 * @wbp.parser.constructor
	 */
	public DataGraphFrame(Graph graph, String frameTitle, boolean isVisible) {
		init(graph, isVisible, frameTitle);
	}

	/**
	 * Constructor
	 */
	public DataGraphFrame(Graph graph, boolean isVisible) {
		init(graph, isVisible, "");
	}

	/**
	 * Constructor
	 */
	public DataGraphFrame(Graph graph, String frameTitle) {
		init(graph, true, frameTitle);
	}

	/**
    *
    */
	public void cleanup() {
		if (DEBUG)
			System.out.println("Disposing of data graph");
		getContentPane().removeAll();
		dataGraph.cleanUp();
		if (DEBUG)
			System.out.println("Removed all components");
		if (DEBUG)
			System.out.println("Set _gC to null");
	}

	/**
   *
   */
	private void init(Graph graph, boolean isVisible, String frameTitle) {
		setTitle(frameTitle);
		setIconImage(Toolkit.getDefaultToolkit().createImage(
				VistaUtils.getImageAsBytes("/vista/planning.gif")));
		dataGraph = new DataSetGraph(graph);
		// add graph canvas to frame and set its listeners
		this.getContentPane().setLayout(new BorderLayout());
		this.getContentPane().add(dataGraph, BorderLayout.CENTER);
		dataGraph.addZoomControlBar(this);
		// set curve filters for the graph
		AppUtils.setCurveFilter(graph, AppUtils.getCurrentCurveFilter());
		// set up menus and their listeners
		JMenuBar mb = new JMenuBar();
		JMenu mainMenu = dataGraph.getMainMenu();
		JMenuItem quitItem = new JMenuItem("Quit");
		quitItem.addActionListener(new QuitListener());
		mainMenu.add(quitItem);
		JMenu displayMenu = dataGraph.getDisplayMenu();
		mb.add(mainMenu);
		mb.add(displayMenu);
		setJMenuBar(mb);
		//
		this.addWindowListener(new QuitListener());
		// position frame and show
		this.pack();
		Toolkit tk = getToolkit();
		Dimension screenSize = tk.getScreenSize();
		Dimension frameSize = getSize();
		this.setLocation(screenSize.width - frameSize.width, screenSize.height
				- frameSize.height);
		this.setVisible(isVisible);
		//this.repaint();
	}


	/**
	 * quits on quit command
	 */
	protected class QuitListener extends WindowAdapter implements
			ActionListener {
		public void actionPerformed(ActionEvent e) {
			if (DEBUG)
				System.out.println("Quit event");
			dispose();
			cleanup();
		}

		public void windowClosed(WindowEvent evt) {
			if (DEBUG)
				System.out.println("Window closed event");
			cleanup();
		}

		public void windowClosing(WindowEvent evt) {
			if (DEBUG)
				System.out.println("Window closing event");
			cleanup();
		}
	}

}
