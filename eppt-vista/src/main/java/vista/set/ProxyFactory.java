/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package vista.set;

import vista.time.TimeInterval;

/**
 * A factory to create a proxy for a certain math operation.
 * 
 * @author Nicky Sandhu
 * @version $Id: ProxyFactory.java,v 1.1 2003/10/02 20:49:30 redwood Exp $
 */
public class ProxyFactory {
	/**
   *
   */
	public final static int PERIOD_AVERAGE = 1;
	public final static int PERIOD_MAX = 2;
	public final static int PERIOD_MIN = 3;

	/**
	 * creates a proxy for a period operation
	 */
	public static DataReference createPeriodOperationProxy(int periodOpId,
			DataReference ref, TimeInterval ti) {
		try {
			switch (periodOpId) {
			case PERIOD_AVERAGE:
				return new PeriodAverageProxy(ref, ti);
			case PERIOD_MIN:
				return new PeriodMinProxy(ref, ti);
			case PERIOD_MAX:
				return new PeriodMaxProxy(ref, ti);
			default:
				throw new IllegalArgumentException("Period operation Id: "
						+ periodOpId + " is invalid");
			}
		} catch (IllegalArgumentException e) {
			return null;
		}
	}

	/**
   *
   */
	public static DataReference createMergingProxy(DataReference[] refs) {
		try {
			if (refs == null)
				return null;
			boolean allRTS = true;
			for (int i = 0; i < refs.length; i++) {
				if (refs[i] == null)
					continue; // will ignore in merge later
				DataSet ds = refs[i].getData();
				if (ds != null && !(ds instanceof RegularTimeSeries)) {
					allRTS = false;
				}
			}
			if (allRTS)
				return new MergingProxy(refs);
			else
				return new ITSMergingProxy(refs);
		} catch (IllegalArgumentException e) {
			return null;
		}
	}

	/**
   *
   */
	public static DataReference createPairedTimeSeriesProxy(DataReference refx,
			DataReference refy) {
		try {
			return new PairedTimeSeriesProxy(refx, refy);
		} catch (IllegalArgumentException e) {
			return null;
		}
	}

	/**
   *
   */
	public static DataReference createLRFilledTimeSeriesProxy(
			DataReference refx, DataReference refy) {
		try {
			return new LRFilledTimeSeriesProxy(refx, refy);
		} catch (IllegalArgumentException e) {
			return null;
		}
	}

	/**
   *
   */
	public static DataReference createRegressionLineProxy(DataReference refx,
			DataReference refy) {
		try {
			return new RegressionLineProxy(refx, refy);
		} catch (IllegalArgumentException e) {
			return null;
		}
	}

	/**
   *
   */
	public static DataReference createMovingAverageProxy(DataReference ref,
			int backLength, int forwardLength) {
		try {
			return new MovingAverageProxy(ref, backLength, forwardLength);
		} catch (IllegalArgumentException e) {
			return null;
		}
	}
}
