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

/**
 * @author Nicky Sandhu
 * @version $Id: DoubleRect.java,v 1.1 2003/10/02 20:48:53 redwood Exp $
 */
public class DoubleRect
{
	public double x, y, width, height;

	public DoubleRect(double x, double y, double w, double h)
	{
		this.x = x;
		this.y = y;
		width = w;
		height = h;
	}
}
