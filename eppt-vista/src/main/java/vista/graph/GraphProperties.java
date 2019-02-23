/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package vista.graph;

import java.util.Properties;

import vista.gui.VistaUtils;

/**
   *
   */
public class GraphProperties {
	/**
   *
   */
	public static Properties properties;
	static {
		properties = new Properties();
		// load default properties
		properties.put("GraphInfo.timeUnitString", "TIME");

		properties.put("GraphFrame.LOAD_ATTR", "Load Attributes...");
		properties.put("GraphFrame.SAVE_ATTR", "Save Attributes...");

		properties.put("GraphFrame.gifSelectionMsg", "Select Gif File...");
		properties
				.put("GraphFrame.psSelectionMsg", "Select PostScript File...");
		properties.put("GraphFrame.PPMSelectionMsg", "Select PPM File...");
		properties.put("GraphFrame.jpegSelectionMsg", "Select Jpeg File...");
		properties.put("GraphFrame.GIF_FILE", "graph.gif");
		properties.put("GraphFrame.PS_FILE", "graph.ps");
		properties.put("GraphFrame.PPM_FILE", "graph.ppm");

		properties.put("GraphFrame.MAIN_MENU", "MAIN");
		properties.put("GraphFrame.EDIT_MENU", "Edit");
		properties.put("GraphFrame.EDIT_GRAPH", "Graph...");
		properties.put("GraphFrame.PRINT", "Print");
		properties.put("GraphFrame.QUIT", "Quit");
		properties.put("GraphFrame.SAVE2GIF", "Save2Gif");
		properties.put("GraphFrame.SAVE2PS", "Save2PS");
		properties.put("GraphFrame.SAVE2PPM", "Save2PPM");
		properties.put("GraphFrame.SAVE2JPEG", "Save2Jpeg");
		properties.put("DSSPlotBuilder.useTimeTickGenerator", "true");
		properties.put("GraphFrame.graphPropertiesFile", "demo1.properties");
		//
		properties.put("displayGood", "true");
		properties.put("displayQuestionable", "false");
		properties.put("displayReject", "false");
		properties.put("displayUnscreened", "true");
		// load from file
		java.io.InputStream is = null;
		if (is == null)
			is = VistaUtils.getPropertyFileAsStream("graph.properties");
		if (is == null)
			is = VistaUtils
					.getResourceAsStream("/vista/graph/graph.properties");
		try {
			properties.load(new java.io.BufferedInputStream(is));
		} catch (java.io.IOException ioe) {
			System.out.println("Properties file " + "graph.properties"
					+ " not found");
			System.out.println("using default properties");
		}
	}
}
