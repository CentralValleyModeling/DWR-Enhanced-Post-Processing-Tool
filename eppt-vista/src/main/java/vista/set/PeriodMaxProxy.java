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

import vista.time.TimeInterval;
import vista.time.TimeWindow;

/**
 * A proxy for period averaging operations
 *
 * @author Nicky Sandhu
 * @version $Id: PeriodMaxProxy.java,v 1.1 2003/10/02 20:49:29 redwood Exp $
 */
public class PeriodMaxProxy extends PeriodOperationProxy
{
	/**
	 *
	 */
	private static String operationName = "PER-MAX";

	/**
	 *
	 */
	public PeriodMaxProxy(DataReference ref, TimeInterval ti)
	{
		super(ref, ti);
	}

	/**
	 *
	 */
	protected String getOperationName()
	{
		return operationName;
	}

	/**
	 * returns the value for this period from the values in the array
	 */
	protected double doPeriodOperation(double[] yvals, int nvals)
	{
		if(nvals <= 0)
		{
			return 0;
		}
		double max = yvals[0];
		for(int i = 1; i < nvals; i++)
		{
			max = Math.max(max, yvals[i]);
		}
		return max;
	}

	/**
	 * returns the flag for the value of this period from the values in the
	 * array.
	 */
	protected int doFlagOperation(int[] flags, int nvals)
	{
		return 0;
	}

	/**
	 *
	 */
	protected String getProxyName(DataReference ref, TimeInterval ti)
	{
		return ref.getName() + operationName;
	}

	/**
	 *
	 */
	protected String getProxyServerName(DataReference ref, TimeInterval ti)
	{
		return "";
	}

	/**
	 *
	 */
	protected String getProxyFileName(DataReference ref, TimeInterval ti)
	{
		return "";
	}

	/**
	 *
	 */
	protected Pathname getProxyPathname(DataReference ref, TimeInterval ti)
	{
		Pathname path = ref.getPathname();
		// create pathname from operation + pathname -- > should be delegated
		String[] parts = new String[Pathname.MAX_PARTS];
		for(int i = 0; i < parts.length; i++)
		{
			parts[i] = path.getPart(i);
		}
		parts[Pathname.B_PART] = parts[Pathname.B_PART] + operationName;
		parts[Pathname.E_PART] = ti.getIntervalAsString();
		parts[Pathname.F_PART] = parts[Pathname.F_PART] + "(" + operationName
				+ ")";
		return Pathname.createPathname(parts);
	}

	/**
	 *
	 */
	protected TimeWindow getProxyTimeWindow(DataReference ref, TimeInterval ti)
	{
		return ref.getTimeWindow();
	}

	/**
	 * create a clone of itself
	 */
	public DataReference createClone()
	{
		return new PeriodMaxProxy(getUnderlyingReference(),
				getPeriodTimeInterval());
	}
}
