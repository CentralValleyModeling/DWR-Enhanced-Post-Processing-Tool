/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package vista.graph;

/**
 * An interface containing the data required for drawing a Curve.
 * 
 * @author Nicky Sandhu
 * @version $Id: LineDataModel.java,v 1.1 2003/10/02 20:49:04 redwood Exp $
 */
public class LineDataModel implements CurveDataModel {
	private int _or;
	private CurveDataModel _cdm;
	private double _val;
	private int _index;

	/**
   *
   */
	public LineDataModel(CurveDataModel cdm, double val, int orientation) {
		_cdm = cdm;
		_val = val;
		_or = orientation;
		reset();
	}

	/**
	 * an object associated with this model.
	 */
	public Object getReferenceObject() {
		return null;
	}

	/**
	 * an object associated with this model.
	 */
	public void setReferenceObject(Object obj) {
	}

	/**
	 * sets the maximum value for the current x axis
	 */
	public void setXViewMax(double max) {

	}

	/**
	 * gets the minimum value for the current x axis
	 */
	public void setXViewMin(double min) {
	}

	/**
	 * gets the maximum value for the x axis
	 */
	public double getXMax() {
		return _cdm.getXMax();
	}

	/**
	 * gets the maximum value for the x axis
	 */
	public double getXMin() {
		return _cdm.getXMin();
	}

	/**
	 * gets the maximum value for the x axis
	 */
	public double getYMax() {
		return _cdm.getYMax();
	}

	/**
	 * gets the maximum value for the x axis
	 */
	public double getYMin() {
		return _cdm.getYMin();
	}

	/**
	 * gets the interpolation type of the data
	 */
	public int getInterpolationType() {
		return INST_VAL;
	}

	/**
	 * resets the data iterator to beginning of curve
	 */
	public void reset() {
		_index = 0;
	}

	/**
	 * gets the next point
	 * 
	 * @param points
	 *            is an array wher points[0] contains the next x value and
	 *            points[1] contains the next y value
	 * @return an integer specifing movevment only or line drawing motion
	 */
	public int nextPoint(double[] points) {
		int type = MOVE_TO;
		if (_index == 0) {
			if (_or == AxisAttr.HORIZONTAL) {
				points[0] = _cdm.getXMin();
				points[1] = _val;
			} else {
				points[0] = _val;
				points[1] = _cdm.getYMin();
			}
		} else {
			if (_or == AxisAttr.HORIZONTAL) {
				points[0] = _cdm.getXMax();
				points[1] = _val;
			} else {
				points[0] = _val;
				points[1] = _cdm.getYMax();
			}
			type = LINE_TO;
		}
		_index++;
		return type;
	}

	/**
	 * @return true while has more points on curve
	 */
	public boolean hasMorePoints() {
		return _index < 2;
	}

	/**
	 * gets teh legend text for this curve
	 */
	public String getLegendText() {
		return "Line";
	}

	/**
	 * get the x axis position for this curve
	 */
	public int getXAxisPosition() {
		return _cdm.getXAxisPosition();
	}

	/**
	 * geth the y axis position for this curve
	 */
	public int getYAxisPosition() {
		return _cdm.getYAxisPosition();
	}

	/**
	 * get the tick generator for x axis
	 */
	public TickGenerator getXAxisTickGenerator() {
		return _cdm.getXAxisTickGenerator();
	}

	/**
	 * get the tick generator for the y axis
	 */
	public TickGenerator getYAxisTickGenerator() {
		return _cdm.getYAxisTickGenerator();
	}

	/**
	 * sets filter, I don't want to determine the exact class to not allow
	 * dependency between this package and other packages
	 */
	public void setFilter(Object filter) {
	}
}
