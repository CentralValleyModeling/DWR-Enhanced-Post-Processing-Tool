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
 * This class implements the interface for the data set element in which the
 * flag value is significant and present.
 *
 * @author Nicky Sandhu (DWR).
 * @version $Id: FlaggedTimeElement.java,v 1.1 2003/10/02 20:49:23 redwood Exp $
 * @see DataSet
 */
public class FlaggedTimeElement extends TimeElement
{
	/**
	 * the flag
	 */
	private int _flag;

	/**
	 * get flag as string
	 */
	public String getFlagString()
	{
		return FlagUtils.getQualityFlagName(FlagUtils.getQualityFlag(this))
				+ " | " + FlagUtils.getLastCheckedBy(this);
	}

	/**
	 * get flag
	 */
	public final int getFlag()
	{
		return _flag;
	}

	/**
	 * set flag
	 */
	public final void setFlag(int flag)
	{
		_flag = flag;
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
		super.copyFrom(dse);
		if(dse instanceof FlaggedTimeElement)
		{
			_flag = ((FlaggedTimeElement) dse)._flag;
		}
	}

	/**
	 * creates a copy of itself
	 */
	public DataSetElement createClone()
	{
		FlaggedTimeElement e = new FlaggedTimeElement();
		e.copyFrom(this);
		return e;
	}

	/**
	 * string representation
	 */
	public String toString()
	{
		StringBuffer buf = new StringBuffer(15 * 3);
		buf.append(super.toString()).append(", ").append(getFlagString());
		return buf.toString();
	}
}
