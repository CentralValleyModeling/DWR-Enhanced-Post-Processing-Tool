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
 * Attributes of a LegendItem
 *
 * @author Nicky Sandhu (DWR).
 * @version $Id: LegendItemAttr.java,v 1.1 2003/10/02 20:49:04 redwood Exp $
 * @see LegendItem
 */
public class LegendItemAttr extends TextLineAttr
{
	/**
	 *
	 */
	private LineElementAttr _lea = new LineElementAttr();

	/**
	 * copies the fields into the given GEAttr object. Also copies in the
	 * TextLineAttr if the object is of that type.
	 */
	public void copyInto(GEAttr ga)
	{
		super.copyInto(ga);
		if(ga instanceof LegendItemAttr)
		{
			LegendItemAttr tla = (LegendItemAttr) ga;
			tla._lea = this._lea;
		}
	}

	/**
	 *
	 */
	public LineElementAttr getLineElementAttributes()
	{
		return _lea;
	}

	/**
	 *
	 */
	public void setLineElementAttributes(LineElementAttr lea)
	{
		_lea = lea;
	}
}
