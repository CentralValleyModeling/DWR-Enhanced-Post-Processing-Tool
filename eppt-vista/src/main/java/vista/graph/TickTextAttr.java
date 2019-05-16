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
 * Attributes of a TickText
 *
 * @author Nicky Sandhu (DWR).
 * @version $Id: TickTextAttr.java,v 1.1 2003/10/02 20:49:11 redwood Exp $
 * @see TickText
 */
public class TickTextAttr extends TextLineAttr
{
	/**
	 * Text is centered on tick value
	 */
	public boolean _centeredOnBounds = true;
	/**
	 * Marks the boundaries of the text with ticks.
	 */
	public boolean _boundariesMarked = false;

	/**
	 * copies the fields into the given GEAttr object. Also copies in the
	 * TextLineAttr if the object is of that type.
	 */
	public void copyInto(GEAttr ga)
	{
		super.copyInto(ga);
		if(ga instanceof TickTextAttr)
		{
			TickTextAttr tla = (TickTextAttr) ga;
			tla._boundariesMarked = this._boundariesMarked;
			tla._centeredOnBounds = this._centeredOnBounds;
		}
	}
}
