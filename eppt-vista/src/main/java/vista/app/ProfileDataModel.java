/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package vista.app;

import vista.graph.AxisAttr;
import vista.graph.CurveDataModel;
import vista.graph.SimpleTickGenerator;
import vista.graph.TickGenerator;
import vista.set.DataSetElement;

/**
 * 
 * 
 * @author Nicky Sandhu
 * @version $Id: ProfileDataModel.java,v 1.1 2003/10/02 20:48:39 redwood Exp $
 */
public class ProfileDataModel implements CurveDataModel {
	private DataSetElement _dse;
	private double[] _distances;
	private double _xmax, _ymax, _xmin, _ymin;
	private TickGenerator _stg;
	private String _legendText;
	private int _index;

	/**
   *
   */
	public ProfileDataModel(DataSetElement dse, double[] distances,
			double ymax, double ymin, String legendText) {
		_distances = distances;
		_dse = dse;
		_stg = new SimpleTickGenerator();
		_legendText = legendText;
		_index = 1;
		_xmax = Float.MIN_VALUE;
		_xmin = Float.MAX_VALUE;
		for (int i = 0; i < _distances.length; i++) {
			_xmax = Math.max(_xmax, _distances[i]);
			_xmin = Math.min(_xmin, _distances[i]);
		}
		_ymax = ymax;
		_ymin = ymin;
	}

	/**
   *
   */
	public Object getReferenceObject() {
		return _dse;
	}

	/**
	 * an object associated with this model.
	 */
	public void setReferenceObject(Object obj) {
		_dse = (DataSetElement) obj;
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
		return _xmax;
	}

	/**
	 * gets the maximum value for the x axis
	 */
	public double getXMin() {
		return _xmin;
	}

	/**
	 * gets the maximum value for the x axis
	 */
	public double getYMax() {
		return _ymax;
	}

	/**
	 * gets the maximum value for the x axis
	 */
	public double getYMin() {
		return _ymin;
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
		_index = 1;
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
		points[0] = _distances[_index - 1];
		points[1] = _dse.getX(_index);
		_index++;
		return LINE_TO;
	}

	/**
	 * @return true while has more points on curve
	 */
	public boolean hasMorePoints() {
		return _index < _dse.getDimension();
	}

	/**
	 * gets teh legend text for this curve
	 */
	public String getLegendText() {
		return _legendText;
	}

	/**
	 * get the x axis position for this curve
	 */
	public int getXAxisPosition() {
		return AxisAttr.BOTTOM;
	}

	/**
	 * geth the y axis position for this curve
	 */
	public int getYAxisPosition() {
		return AxisAttr.LEFT;
	}

	/**
	 * get the tick generator for x axis
	 */
	public TickGenerator getXAxisTickGenerator() {
		return _stg;
	}

	/**
	 * get the tick generator for the y axis
	 */
	public TickGenerator getYAxisTickGenerator() {
		return _stg;
	}

	/**
   *
   */
	public void setFilter(Object filter) {
	}

	/**
   *
   */
	public void dataChanged() {
		_xmax = Float.MIN_VALUE;
		_xmin = Float.MAX_VALUE;
		for (int i = 0; i < _distances.length; i++) {
			_xmax = Math.max(_xmax, _distances[i]);
			_xmin = Math.min(_xmin, _distances[i]);
		}
	}
}
