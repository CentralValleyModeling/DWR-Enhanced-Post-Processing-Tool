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

package vista.graph;

import java.awt.Graphics;

/**
 * Shows the zoom overview into the plot.
 *
 * @author psandhu
 */
@SuppressWarnings("serial")
public class ZoomOverview extends GECanvas
{

	private Zoom zoom;

	public ZoomOverview(GraphicElement ge, Zoom zoom)
	{
		super(ge);
		this.zoom = zoom;
	}

	@Override
	public void paint(Graphics g)
	{
		zoom.getZoomRectangle();
		super.paint(g);
	}


}
