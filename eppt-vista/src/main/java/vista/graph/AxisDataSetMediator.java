/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package vista.graph;

import java.util.ArrayList;

/**
 * Mediates information flow between an axis and data sets. This will promote
 * loose coupling by centralizing the relationship between the axis and the data
 * set. The axis should only be concerned with the scaling and not with the data
 * sets which are used to come up with that scaling. And the data sets should
 * only be concerned with the data and not the axis to which they might be
 * attached
 * 
 * @author Nicky Sandhu (DWR).
 * @version $Id: AxisDataSetMediator.java,v 1.1 2003/10/02 20:48:49 redwood Exp
 *          $
 */
public class AxisDataSetMediator {
	/**
	 * Attaches itself to the particular axis.
	 */
	public AxisDataSetMediator(Axis axis) {
		_axis = axis;
		_cdms = new ArrayList<CurveDataModel>();
	}

	/**
   *
   */
	public void attach(CurveDataModel cdm) {
		_cdms.add(cdm);
	}

	/**
   *
   */
	public void detach(CurveDataModel cdm) {
		_cdms.remove(cdm);
	}

	/**
   *
   */
	public void setCurveModelMinMax(Scale sc) {
		double minx = sc.getDataMinimum();
		double maxx = sc.getDataMaximum();
		for (CurveDataModel cdm : _cdms) {
			cdm.setXViewMax(maxx);
			cdm.setXViewMin(minx);
		}
	}

	/**
	 * Detach all data sets
	 */
	public void detachAll() {
		_cdms.clear();
	}

	/**
	 * returns true if atleast one data set is available
	 */
	public boolean hasDataSets() {
		return (_cdms.size() > 0);
	}

	/**
   *
   */
	public int getNumberOfCurves() {
		return _cdms.size();
	}

	/**
	 * returns the minimum of data sets attached to the axis
	 */
	public double getMinimum() {
		double min = Float.MAX_VALUE;
		int or = _axis.getOrientation();

		for (CurveDataModel cdm : _cdms) {
			if (or == AxisAttr.HORIZONTAL) {
				min = Math.min(min, cdm.getXMin());
			} else {
				min = Math.min(min, cdm.getYMin());
			}
		}
		return min;
	}

	/**
	 * returns the maximum of data sets attached to the axis
	 */
	public double getMaximum() {
		double max = -Float.MAX_VALUE;
		int or = _axis.getOrientation();

		for (CurveDataModel cdm : _cdms) {
			if (or == AxisAttr.HORIZONTAL) {
				max = Math.max(max, cdm.getXMax());
			} else {
				max = Math.max(max, cdm.getYMax());
			}
		}
		return max;
	}

	/**
	 * The axis which needs to get the information about these data sets
	 */
	Axis _axis;
	/**
	 * The data sets containing all the data sets attached to the axis
	 */
	private ArrayList<CurveDataModel> _cdms;
	/**
   *
   */
	private static final boolean DEBUG = false;
}
