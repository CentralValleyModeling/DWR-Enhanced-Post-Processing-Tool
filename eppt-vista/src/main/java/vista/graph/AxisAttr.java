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
 * Attributes of a Axis element
 *
 * @author Nicky Sandhu (DWR).
 * @version $Id: AxisAttr.java,v 1.1 2003/10/02 20:48:48 redwood Exp $
 * @see Axis
 */
public class AxisAttr extends TickLineAttr
{
	/**
	 * plot major tick labels?
	 */
	public boolean _plotMajorTickLabels = true;
	/**
	 * plot axis label?
	 */
	public boolean _plotAxisLabel = true;
	/**
	 * The major tick label TickText attributes
	 */
	private TickTextAttr _ttxa = new TickTextAttr();

	/**
	 * copies the fields into the given GEAttr object. Also copies in the
	 * TextLineAttr if the object is of that type.
	 */
	public void copyInto(GEAttr ga)
	{
		super.copyInto(ga);
		if(ga instanceof AxisAttr)
		{
			AxisAttr tla = (AxisAttr) ga;
			tla._plotMajorTickLabels = this._plotMajorTickLabels;
			tla._plotAxisLabel = this._plotAxisLabel;
			tla._ttxa = this._ttxa;
		}
	}

	/**
	 * get the attributes of the major tick label element
	 */
	public TickTextAttr getTickTextAttributes()
	{
		return _ttxa;
	}

	/**
	 * set the attributes of the labels for the major ticks
	 */
	public void getTickTextAttributes(TickTextAttr ttxa)
	{
		_ttxa = ttxa;
	}
}
