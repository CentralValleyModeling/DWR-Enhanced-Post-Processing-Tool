/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */

package vista.graph;

import java.awt.Graphics;

/**
 * Shows the zoom overview into the plot.
 * @author psandhu
 *
 */
@SuppressWarnings("serial")
public class ZoomOverview extends GECanvas{
	
	private Zoom zoom;

	public ZoomOverview(GraphicElement ge, Zoom zoom) {
		super(ge);
		this.zoom = zoom;
	}

	@Override
	public void paint(Graphics g) {
		zoom.getZoomRectangle();
		super.paint(g);
	}

	
}
