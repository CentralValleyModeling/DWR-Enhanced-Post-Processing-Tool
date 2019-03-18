/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package vista.graph;

/**
 * 
 * 
 * @author Nicky Sandhu
 * @version $Id: DoubleRect.java,v 1.1 2003/10/02 20:48:53 redwood Exp $
 */
public class DoubleRect {
	public double x, y, width, height;

	public DoubleRect(double x, double y, double w, double h) {
		this.x = x;
		this.y = y;
		width = w;
		height = h;
	}
}
