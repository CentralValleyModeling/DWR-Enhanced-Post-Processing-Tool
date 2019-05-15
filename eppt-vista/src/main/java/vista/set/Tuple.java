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
package vista.set;

import java.text.NumberFormat;

/**
 * A tuple in which the first dimension is time which also indexes the other
 * dimensions
 *
 * @author Nicky Sandhu (DWR).
 * @version $Id: Tuple.java,v 1.1 2003/10/02 20:49:34 redwood Exp $
 * @see DataSet
 */
public class Tuple implements DataSetElement
{
	/**
	 * y value
	 */
	private double[] _yvals;
	/**
	 *
	 */
	private NumberFormat _yformatter;

	public Tuple()
	{
	}

	/**
	 *
	 */
	public Tuple(double[] vals)
	{
		this();
		_yvals = vals;
	}

	/**
	 * the dimension of the tuple
	 */
	public int getDimension()
	{
		return _yvals.length;
	}

	/**
	 *
	 */
	private void checkIndex(int i)
	{
		if(i < 0 || i >= getDimension())
		{
			throw new IndexOutOfBoundsException("Dimension is "
					+ getDimension() + ", invalid access for index " + i);
		}
	}

	/**
	 * set the i'th dimension value numbered from 0 to n-1.
	 */
	public void setX(int i, double val)
	{
		checkIndex(i);
		_yvals[i] = val;
	}

	/**
	 * get the i'th dimension value
	 */
	public double getX(int i)
	{
		return _yvals[i];
	}

	/**
	 * set the i'th dimension value
	 */
	public void setY(int i, double val)
	{
		setX(i + 1, val);
	}

	/**
	 * set the i'th dimension flag
	 */
	public void setFlag(int i, int flag)
	{
		checkIndex(i + 1);
		setFlag(flag);
	}

	/**
	 * get x
	 */
	public final double getX()
	{
		return getX(0);
	}

	/**
	 * set X
	 */
	public final void setX(double x)
	{
		setX(0, x);
	}

	/**
	 * set X
	 */
	public final void setX(double[] x)
	{
		if(x == null || x.length != getDimension())
		{
			return;
		}
		_yvals = x;
	}

	/**
	 * return x's representation as string
	 */
	public String getXString()
	{
		return getXString(0);
	}

	/**
	 * get y
	 */
	public final double getY()
	{
		return getX(1);
	}

	/**
	 * set Y
	 */
	public final void setY(double y)
	{
		setX(1, y);
	}

	/**
	 * get y
	 */
	public final double getY(int i)
	{
		return _yvals[i];
	}

	/**
	 * return y's representation as string
	 */
	public String getYString()
	{
		return getXString(1);
	}

	/**
	 * return y's representation as string
	 */
	public String getXString(int i)
	{
		checkIndex(i);
		double y = _yvals[i];
		if(y == Constants.MISSING_VALUE || y == Constants.MISSING)
		{
			return "MISSING VALUE";
		}
		else if(y == Constants.MISSING_RECORD)
		{
			return "MISSING RECORD";
		}
		else if(Double.doubleToLongBits(y) == 0x7ff8000000000000L)
		{
			return "----";
		}
		else
		{
			return SetUtils.format(y);
		}
	}

	/**
	 * returns 0
	 */
	public int getFlag()
	{
		return 0;
	}

	/**
	 * does not do anything.
	 */
	public void setFlag(int flag)
	{
	}

	/**
	 *
	 */
	public String getFlagString()
	{
		return "FLAG UNAVAILABLE";
	}

	/**
	 * get flag at the i'th dimension
	 */
	public int getFlag(int i)
	{
		checkIndex(i + 1);
		return getFlag();
	}

	/**
	 * returns flag from the i'th dimension as string
	 */
	public String getFlagString(int i)
	{
		checkIndex(i + 1);
		return getFlagString();
	}

	/**
	 * copies over the fields from the other element
	 */
	public void copyFrom(DataSetElement dse)
	{
		if(dse == null)
		{
			return;
		}
		// only two types of elements default and flagged
		Tuple e = (Tuple) dse;
		_yvals = e._yvals;
	}

	/**
	 * creates a copy of itself
	 */
	public DataSetElement createClone()
	{
		Tuple e = new Tuple();
		e.copyFrom(this);
		return e;
	}

	/**
	 * string representation
	 */
	public String toString()
	{
		StringBuffer buf = new StringBuffer(15 * getDimension());
		buf.append("( ").append(getXString(0));
		int d = getDimension();
		for(int i = 1; i < d; i++)
		{
			buf.append(", ");
			buf.append(getXString(i));
		}
		buf.append(" )");
		return buf.toString();
	}
}
