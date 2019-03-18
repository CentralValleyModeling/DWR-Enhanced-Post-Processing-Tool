/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package vista.app;

import vista.graph.PieChartModel;
import vista.graph.PieModel;
import vista.set.DataSetElement;

/**
 * 
 * 
 * @author Nicky Sandhu
 * @version $Id: DefaultPieChartModel.java,v 1.1 2003/10/02 20:48:28 redwood Exp
 *          $
 */
public class DefaultPieChartModel implements PieChartModel {
	private int _index;
	private DataSetElement _dse;
	private String[] _labels;
	private DefaultPieModel _pm;

	public DefaultPieChartModel(DataSetElement dse, String[] labels) {
		_dse = dse.createClone();
		_labels = labels;
		_pm = new DefaultPieModel();
		reset();
	}

	/**
   *
   */
	public Object getReferenceObject() {
		return _dse;
	}

	/**
   *
   */
	public void setReferenceObject(Object obj) {
		if (!(obj instanceof DataSetElement))
			return;
		_dse = (DataSetElement) obj;
	}

	/**
	 * get title of pie chart
	 */
	public String getTitle() {
		return "PIE CHART";
	}

	/**
	 * get maximum value, to scale all values to this value
	 */
	public double getSumOfValues() {
		double sum = 0.0;
		for (int i = 0; i < _dse.getDimension(); i++)
			sum += _dse.getX(i);
		return sum;
	}

	/**
	 * true if more value are to follow
	 */
	public boolean hasMorePies() {
		return _index < _dse.getDimension();
	}

	/**
	 * returns information in the pie model interface
	 */
	public PieModel nextPie() {
		_pm._value = _dse.getX(_index);
		_pm._label = _labels[_index];
		_index++;
		return _pm;
	}

	/**
	 * resets to the beginning
	 */
	public void reset() {
		_index = 0;
	}

	/**
	 * 
	 * 
	 * @author Nicky Sandhu
	 * @version $Id: DefaultPieChartModel.java,v 1.1 2003/10/02 20:48:28 redwood
	 *          Exp $
	 */
	class DefaultPieModel implements PieModel {
		double _value;
		String _label;

		public String getLabel() {
			return _label;
		}

		public double getValue() {
			return _value;
		}

		public Object getReferenceObject() {
			return null;
		}
	}
}
