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

import vista.gui.VistaUtils;
import vista.set.Session;
import vista.set.TimeSeriesMath;

/**
 * The main GUI for the application
 * 
 * @author Nicky Sandhu
 * @version $Id: MainGUI.java,v 1.1 2003/10/02 20:48:33 redwood Exp $
 */
public class MainGUI extends DefaultFrame {
	/**
	 * The main method for the GUI. The command line for the gui is vista [-s
	 * session_file_name]
	 */
	public static void main(String[] args) {
		try {
			VistaUtils.showStartUpIcon();
			setDebugMode();
			new MainGUI(args);
		} catch (Exception e) {
			e.printStackTrace(System.err);
			System.err.println(e.getMessage());
		}
	}

	/**
	 * Constructor
	 */
	public MainGUI(String[] args) {
		if (args != null && args.length == 0)
			args = null;
		TimeSeriesMath.DUMB_PATCH = false;
		new SessionFrame(args);
	}

	/**
	 * prints message on usage if incorrect
	 */
	private void printUsageMsg() {
		System.out.println("Usage: program dssfile");
	}

	/**
	 * context for this gui
	 */
	private static SessionContext _sc = new SessionContext(new Session());

	/**
	 * gets the context ( contains data ) for this gui
	 */
	public static final SessionContext getContext() {
		return _sc;
	}

	/**
   *
   */
	public static void setDebugMode() {
		if (MainProperties.getProperty("debug").equals("true")) {
			Runtime.getRuntime().traceMethodCalls(true);
		}
	}

	/**
	 * returns the symbol table in use
	 */
	public static SymbolTable getSymbolTable() {
		return _st;
	}

	/**
	 * sets the symbol table to this one
	 */
	public static void setSymbolTable(SymbolTable st) {
		_st = st;
	}

	private static SymbolTable _st = new SymbolTable();
}
