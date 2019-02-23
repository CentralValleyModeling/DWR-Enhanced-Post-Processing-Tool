/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */

package gov.ca.hec;

import java.awt.Component;
import java.awt.Graphics2D;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import java.util.Vector;
import javax.imageio.ImageIO;
import javax.swing.*;

import hec.gfx2d.G2dDialog;
import hec.gfx2d.G2dPanelProp;
import hec.io.DataContainer;
/**
 * Utilities to plot and interact with plot like objects from HECDssvue
 * @author psandhu
 *
 */
public class HecPlotUtils {

/**
	 * remove toolbar from panel assuming its the first component in there.
	 * @param panel
	 */
	public static  void removeToolbar(JPanel panel){
	    Component[] components = panel.getComponents();
	    panel.remove(components[-1]);
	}
	
	/**
	 * Create a new plot with the title
	 * @param title
	 * @return
	 */
	@SuppressWarnings("rawtypes")
	public static  G2dDialog newPlot(String title){
	    G2dPanelProp plotProp = new G2dPanelProp();
	    plotProp.sethasToolbar(false);
	    return new G2dDialog(null, title, false, new Vector(), plotProp);
	}
	/**
	 * Saves the plot to a png file
	 * @param p
	 * @param filename
	 * @throws IOException
	 */
	public static void saveToPNG(JPanel p, String filename) throws IOException{
	    BufferedImage bi = new BufferedImage(p.getSize().width, p.getSize().height, BufferedImage.TYPE_INT_ARGB); 
	    Graphics2D g = bi.createGraphics();
	    p.invalidate();
	    p.validate();
	    p.paint(g);
	    g.dispose();
	    ImageIO.write(bi, "png", new File(filename));
	}
	
	/**
	 * Create a new plot with the given data array and title
	 * @param data array of time series
	 * @param title
	 * @return
	 */
	public static G2dDialog plot(DataContainer[] data, String title) {
		G2dDialog plot = newPlot(title);
		for(int i=0; i < data.length; i++){
			plot.addData(data[i]);
		}
		plot.showPlot();
		return plot;
	}

}
