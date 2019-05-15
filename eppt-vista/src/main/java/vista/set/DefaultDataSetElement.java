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


/**
 * This class describes the interface for a element which is contained in a
 * DataSet. This would be revised to be more general.
 *
 * @author Nicky Sandhu (DWR).
 * @version $Id: DefaultDataSetElement.java,v 1.1 2003/10/02 20:49:22 redwood
 * Exp $
 * @see DataSet
 */
public class DefaultDataSetElement implements DataSetElement
{
	/**
	 * x value
	 */
	private double _x;
	/**
	 * y value
	 */
	private double _y;

	public DefaultDataSetElement()
	{
	}

	public DefaultDataSetElement(double x, double y)
	{
		this();
		setX(x);
		setY(y);
	}

	/**
	 * the dimension of the tuple
	 */
	public int getDimension()
	{
		return 2;
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
		if(i == 0)
		{
			setX(val);
		}
		else
		{
			setY(val);
		}
	}

	/**
	 * get the i'th dimension value
	 */
	public double getX(int i)
	{
		checkIndex(i);
		if(i == 0)
		{
			return getX();
		}
		else
		{
			return getY();
		}
	}

	/**
	 * get the i'th dimension value string repr.
	 */
	public String getXString(int i)
	{
		checkIndex(i);
		if(i == 0)
		{
			return getXString();
		}
		else
		{
			return getYString();
		}
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
		return _x;
	}

	/**
	 * set X
	 */
	public final void setX(double x)
	{
		_x = x;
	}

	/**
	 * return x's representation as string
	 */
	public String getXString()
	{
		return SetUtils.format(_x);
	}

	/**
	 * get y
	 */
	public final double getY()
	{
		return _y;
	}

	/**
	 * set Y
	 */
	public final void setY(double y)
	{
		_y = y;
	}

	/**
	 * return y's representation as string
	 */
	public String getYString()
	{
		return SetUtils.format(_y);
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
	 *
	 */
	public String getFlagString()
	{
		return "FLAG UNAVAILABLE";
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
		DefaultDataSetElement e = (DefaultDataSetElement) dse;
		_x = e._x;
		_y = e._y;
	}

	/**
	 * creates a copy of itself
	 */
	public DataSetElement createClone()
	{
		DefaultDataSetElement e = new DefaultDataSetElement();
		e.copyFrom(this);
		return e;
	}

	/**
	 * string representation
	 */
	public String toString()
	{
		StringBuffer buf = new StringBuffer(15 * 3);
		buf.append("( ").append(getXString()).append(", ").append(getYString())
		   .append(" )");
		return buf.toString();
	}
}
