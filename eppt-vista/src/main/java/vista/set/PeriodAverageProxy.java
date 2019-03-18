/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package vista.set;

import vista.time.TimeInterval;
import vista.time.TimeWindow;

/**
 * A proxy for period averaging operations
 * 
 * @author Nicky Sandhu
 * @version $Id: PeriodAverageProxy.java,v 1.1 2003/10/02 20:49:29 redwood Exp $
 */
public class PeriodAverageProxy extends PeriodOperationProxy {
	/**
   *
   */
	protected String getOperationName() {
		return "PER-AVG";
	}

	/**
	 * returns the value for this period from the values in the array
	 */
	protected double doPeriodOperation(double[] yvals, int nvals) {
		double sum = 0.0;
		for (int i = 0; i < nvals; i++) {
			sum += yvals[i];
		}
		return sum / nvals;
	}

	/**
	 * returns the flag for the value of this period from the values in the
	 * array.
	 */
	protected int doFlagOperation(int[] flags, int nvals) {
		return 0;
	}

	/**
   *
   */
	public PeriodAverageProxy(DataReference ref, TimeInterval ti) {
		super(ref, ti);
	}

	/**
   *
   */
	protected String getProxyName(DataReference ref, TimeInterval ti) {
		return ref.getName() + getOperationName();
	}

	/**
   *
   */
	protected String getProxyServerName(DataReference ref, TimeInterval ti) {
		return "";
	}

	/**
   *
   */
	protected String getProxyFileName(DataReference ref, TimeInterval ti) {
		return "";
	}

	/**
   *
   */
	protected Pathname getProxyPathname(DataReference ref, TimeInterval ti) {
		Pathname path = ref.getPathname();
		// create pathname from operation + pathname -- > should be delegated
		String[] parts = new String[Pathname.MAX_PARTS];
		for (int i = 0; i < parts.length; i++) {
			parts[i] = path.getPart(i);
		}
		parts[Pathname.B_PART] = parts[Pathname.B_PART] + "_PERAVG";
		// changed for Excel compatibility
		// parts[Pathname.B_PART] = parts[Pathname.B_PART] + "PER-AVG";
		parts[Pathname.E_PART] = ti.getIntervalAsString();
		parts[Pathname.F_PART] = parts[Pathname.F_PART] + "_PERAVG";
		// changed for Excel compatibility
		// parts[Pathname.F_PART] = parts[Pathname.F_PART] + "(PER-AVG)";
		return Pathname.createPathname(parts);
	}

	/**
   *
   */
	protected TimeWindow getProxyTimeWindow(DataReference ref, TimeInterval ti) {
		return ref.getTimeWindow();
	}

	/**
	 * create a clone of itself
	 */
	public DataReference createClone() {
		return new PeriodAverageProxy(getUnderlyingReference(),
				getPeriodTimeInterval());
	}
}
