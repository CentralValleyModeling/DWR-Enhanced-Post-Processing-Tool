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

import java.awt.Font;

/**
 * Attributes of a Plot
 *
 * @author Nicky Sandhu (DWR).
 * @version $Id: PlotAttr.java,v 1.1 2003/10/02 20:49:06 redwood Exp $
 * @see Plot
 */
public class PlotAttr extends GEAttr
{
	/**
	 *
	 */
	public Font _font = TextLineAttr._fontTable[(int) (TextLineAttr._fontTable.length * 0.75)];
	/**
	 * original font when first created.
	 */
	public int _originalFontSize = _font.getSize();
	/**
	 * draw border around drawing area.
	 */
	public boolean _drawBorder = true;
	/**
	 *
	 */
	private TickLineAttr _tla = new TickLineAttr();
	/**
	 *
	 */
	private TickTextAttr _ttxa = new TickTextAttr();
	/**
	 *
	 */
	private TextLineAttr _txla = new TextLineAttr();

	/**
	 * copies the fields into the given GEAttr object. Also copies in the
	 * TextLineAttr if the object is of that type.
	 */
	public void copyInto(GEAttr ga)
	{
		super.copyInto(ga);
		if(ga instanceof PlotAttr)
		{
			PlotAttr tla = (PlotAttr) ga;
		}
	}

	/**
	 *
	 */
	public TickLineAttr getTickLineAttr()
	{
		return _tla;
	}

	/**
	 *
	 */
	public void setTickLineAttr(TickLineAttr attr)
	{
		_tla = attr;
	}

	/**
	 *
	 */
	public TickTextAttr getTickTextAttr()
	{
		return _ttxa;
	}

	/**
	 *
	 */
	public void setTickTextAttr(TickTextAttr attr)
	{
		_ttxa = attr;
	}

	/**
	 *
	 */
	public TextLineAttr getTextLineAttr()
	{
		return _txla;
	}

	/**
	 *
	 */
	public void setTextLineAttr(TextLineAttr attr)
	{
		_txla = attr;
	}

}
