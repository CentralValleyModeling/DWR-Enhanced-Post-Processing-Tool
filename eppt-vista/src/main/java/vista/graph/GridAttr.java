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

import java.awt.Color;

/**
 * Attributes for Grid.
 *
 * @author Nicky Sandhu (DWR).
 * @version $Id: GridAttr.java,v 1.1 2003/10/02 20:49:01 redwood Exp $
 */
public class GridAttr extends LineElementAttr
{
	/**
	 * color of line
	 */
	public Color _color = new Color(150, 150, 150);

	/**
	 * copies the fields into the given GEAttr object. Also copies in the
	 * GridAttr if the object is of that type.
	 */
	public void copyInto(GEAttr ga)
	{
		super.copyInto(ga);
		if(ga instanceof GridAttr)
		{
			GridAttr lea = (GridAttr) ga;
			lea._color = this._color;
		}
	}

}
