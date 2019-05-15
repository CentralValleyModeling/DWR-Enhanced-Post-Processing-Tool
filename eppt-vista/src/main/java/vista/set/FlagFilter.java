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
 * Filters out a value based on its flag being set to the given flag in the
 * constructor
 *
 * @author Nicky Sandhu
 * @version $Id: FlagFilter.java,v 1.1 2003/10/02 20:49:23 redwood Exp $
 * @seeFilterIterator
 */
public class FlagFilter implements ElementFilter
{
	/**
	 * flag to be filtered
	 */
	private int _flag;

	/**
	 * Constructor with flag to be filtered... e.g. to filter out missing flags
	 * FlagUtils.MISSING_FLAG
	 */
	public FlagFilter(int flag)
	{
		_flag = flag;
	}

	/**
	 * true if value is acceptable
	 */
	public boolean isAcceptable(DataSetElement dse)
	{
		if(dse instanceof FlaggedDataSetElement
				|| dse instanceof FlaggedTimeElement)
		{
			return FlagUtils.getQualityFlag(dse) != _flag;
		}
		else
		{
			return true;
		}
	}
}
