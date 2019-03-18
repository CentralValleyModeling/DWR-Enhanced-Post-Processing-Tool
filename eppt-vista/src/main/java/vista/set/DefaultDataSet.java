/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package vista.set;

import org.python.core.Py;

/**
 * A default implementation of the data set interface. This class uses an array
 * of x and y doubles to store its elements.
 * 
 * @see DataSetElement
 * @see DataSetIterator
 * @author Nicky Sandhu (DWR).
 * @version $Id: DefaultDataSet.java,v 1.1 2003/10/02 20:49:22 redwood Exp $
 */
public class DefaultDataSet implements DataSet {
	/**
	 * Initializes a data set with the x and y arrays. No new memory is created,
	 * only references are held to the two arrays.
	 * 
	 * @exception IllegalArgumentException
	 *                if x and y array lengths don't match.
	 */
	public DefaultDataSet(String name, double[] x, double[] y) {
		this(name, x, y, null, null);
	}

	/**
	 * Initializes a data set with the x and y arrays and with a flag array for
	 * each value. No new memory is created, only references are held to the two
	 * arrays.
	 * 
	 * @exception IllegalArgumentException
	 *                if x and y array lengths don't match.
	 */
	public DefaultDataSet(String name, double[] x, double[] y, int[] flags) {
		this(name, x, y, flags, null);
	}

	/**
	 * Initializes a data set with the x and y arrays and with a flag array for
	 * each value. No new memory is created, only references are held to the two
	 * arrays.
	 * 
	 * @exception IllegalArgumentException
	 *                if x and y array lengths don't match.
	 */
	public DefaultDataSet(String name, double[] x, double[] y, int[] flags,
			DataSetAttr attr) {
		if (x.length != y.length)
			throw new IllegalArgumentException(
					"Error: x and y array lengths don't match");
		if (flags != null) {
			if (flags.length != x.length)
				throw new IllegalArgumentException(
						"Error: flag array of incorrect length");
			else
				_flagArray = flags;
		} else {
			_flagArray = null;
		}
		_xArray = x;
		_yArray = y;
		_name = name;
		if (attr == null) {
			attr = new DataSetAttr(DataType.PAIRED, "", "", "", "INST-VAL");
		}
		_attr = attr;
		_dsi = getIterator();
	}

	/**
	 * gets element at index i
	 */
	public DataSetElement getElementAt(int i) {
		_dsi.positionAtIndex(i);
		DataSetElement e = _dsi.getElement();
		if (e == null)
			throw new IllegalArgumentException("No element at index " + i);
		else
			return e.createClone();
	}

	/**
	 * sets element at index i
	 */
	public void putElementAt(int i, DataSetElement dse) {
		_dsi.positionAtIndex(i);
		_dsi.putElement(dse);
	}

	/**
	 * true if data set is flagged
	 */
	public boolean isFlagged() {
		return (_flagArray != null);
	}

	/**
	 * returns the number of elements in the dataset
	 */
	public int size() {
		return _xArray.length;
	}

	/**
	 * Return an iterator positioned at my first item.
	 */
	public DataSetIterator getIterator() {
		return new DefaultIterator(this);
	}

	/**
	 * sets the name to identify the data set.
	 */
	public void setName(String name) {
		_name = name;
	}

	/**
	 * returns a name for this DataSet to be used to identify it.
	 */
	public String getName() {
		return _name;
	}

	/**
	 * An object attached to this data set which contains descriptive
	 * information of the underlying data.
	 */
	public DataSetAttr getAttributes() {
		return _attr;
	}

	/**
	 * An object attached to this data set which contains descriptive
	 * information of the underlying data.
	 */
	public void setAttributes(DataSetAttr attr) {
		_attr = attr;
	}

	/**
	 * for python len() functionality
	 */
	public int __len__() {
		return size();
	}

	/**
	 * for python x[] functionality
	 */
	public DataSetElement __getitem__(int i) {
		try {
			return getElementAt(i);
		} catch (Throwable t) {
			throw Py.IndexError("index out of range: " + i);
		}
	}

	/**
	 * for python
	 */
	public void __setitem__(int i, DataSetElement e) {
		putElementAt(i, e);
	}

	/**
	 * for python
	 */
	public void __setitem__(int i, double d) {
		putElementAt(i, new DefaultDataSetElement(0, d));
	}

	/**
	 * for python
	 */
	public void __delitem__(int i) {
		double[] xarray, yarray;
		int[] flagarray;
		if (i < 0)
			i = size() + i;
		xarray = new double[_xArray.length - 1];
		yarray = new double[_yArray.length - 1];
		System.arraycopy(_xArray, 0, xarray, 0, i);
		System.arraycopy(_xArray, i + 1, xarray, i, xarray.length - i + 1);
		System.arraycopy(_yArray, 0, yarray, 0, i);
		System.arraycopy(_yArray, i + 1, yarray, i, yarray.length - i + 1);
		_xArray = xarray;
		_yArray = yarray;
		if (_flagArray != null) {
			flagarray = new int[_flagArray.length];
			System.arraycopy(_flagArray, 0, flagarray, 0, i);
			System.arraycopy(_flagArray, i + 1, flagarray, i, flagarray.length
					- i + 1);
			_flagArray = flagarray;
		}
	}

	/**
	 * for python
	 */
	public DataSetElement[] __getslice__(int i, int j) {
		int bi, ei;
		if (i < 0)
			i = size() + i;
		if (j < 0)
			j = size() + j;
		bi = Math.min(i, j);
		ei = Math.max(i, j);
		DataSetElement[] elements = new DataSetElement[ei - bi + 1];
		for (int k = 0; k < elements.length; k++) {
			elements[k] = getElementAt(k + bi);
		}
		return elements;
	}

	/**
	 * for python
	 */
	public void __setslice__(int i, int j, DataSetElement[] array) {
		int bi, ei;
		if (i < 0)
			i = size() + i;
		if (j < 0)
			j = size() + j;
		bi = Math.min(i, j);
		ei = Math.max(i, j);
		for (int k = 0; k < array.length; k++) {
			putElementAt(k + bi, array[k]);
		}
	}

	/**
	 * for python
	 */
	public void __delslice__(int i, int j) {
		int bi, ei;
		if (i < 0)
			i = size() + i;
		if (j < 0)
			j = size() + j;
		bi = Math.min(i, j);
		ei = Math.max(i, j);
		int ndel = ei - bi + 1;
		double[] xarray, yarray;
		int[] flagarray;
		xarray = new double[_xArray.length - ndel];
		yarray = new double[_yArray.length - ndel];
		System.arraycopy(_xArray, 0, xarray, 0, bi);
		System.arraycopy(_xArray, ei + 1, xarray, bi, xarray.length - ndel + 1);
		System.arraycopy(_yArray, 0, yarray, 0, bi);
		System.arraycopy(_yArray, ei + 1, yarray, bi, yarray.length - ndel + 1);
		_xArray = xarray;
		_yArray = yarray;
		if (_flagArray != null) {
			flagarray = new int[_flagArray.length];
			System.arraycopy(_flagArray, 0, flagarray, 0, bi);
			System.arraycopy(_flagArray, ei + 1, flagarray, bi,
					flagarray.length - ndel + 1);
			_flagArray = flagarray;
		}
	}

	@Override
	public void addFlags() {
		if(_flagArray != null) return;
		if (_xArray!= null){
			_flagArray = new int[_xArray.length];
		}
	}

	/**
	 * array of x and y values.
	 */
	private double[] _xArray, _yArray;
	/**
	 * array of flags
	 */
	private int[] _flagArray;
	/**
	 * name of data set
	 */
	private String _name;
	/**
	 * attributes of this data
	 */
	private DataSetAttr _attr;
	/**
   *
   */
	private DataSetIterator _dsi;

	/**
	 * Private iterator class to iterate over the elements of this data set.
	 * 
	 * @author Nicky Sandhu
	 * @version $Id: DefaultDataSet.java,v 1.1 2003/10/02 20:49:22 redwood Exp $
	 */
	private class DefaultIterator implements DataSetIterator {
		/**
		 * initializes element of correct type
		 */
		public DefaultIterator(DefaultDataSet ds) {
			if (isFlagged())
				_dse = new FlaggedDataSetElement();
			else
				_dse = new DefaultDataSetElement();
			resetIterator();
		}

		/**
		 * Resets the iterator to the beginning of data
		 */
		public void resetIterator() {
			_index = 0;
			_firstNextCall = true;
		}

		/**
		 * Returns with the next element and the iterator positioned at that
		 * element.
		 */
		public DataSetElement nextElement() {
			if (_index > 0) {
				advance();
			} else {
				if (!_firstNextCall)
					advance();
				else
					_firstNextCall = false;
			}
			return getElement();
		}

		/**
		 * returns the previous element in the array.
		 */
		public DataSetElement previousElement() {
			retreat();
			return getElement();
		}

		/**
		 * gets the element at the current location
		 */
		public DataSetElement getElement() {
			_dse.setX(_xArray[_index]);
			_dse.setY(_yArray[_index]);
			if (isFlagged())
				_dse.setFlag(_flagArray[_index]);
			return _dse;
		}

		/**
		 * puts the element at the current location
		 */
		public void putElement(DataSetElement e) {
			_xArray[_index] = e.getX();
			_yArray[_index] = e.getY();
			if (isFlagged())
				_flagArray[_index] = e.getFlag();
		}

		/**
   *
   */
		public void positionAtIndex(int i) {
			if (i < 0 || i > _yArray.length - 1) {
				_index = _yArray.length;
				throw new IndexOutOfBoundsException("Incorrect index " + i
						+ " in Default Data Set ");
			}
			_index = i;
		}

		/**
		 * Advance by one. no checks for out of bound advance here?
		 */
		public void advance() {
			_index++;
		}

		/**
		 * Retreat by one. no checks for out of bound advance here?
		 */
		public void retreat() {
			_index--;
		}

		/**
		 * 0 if no elements were skipped by getting this element from the
		 * underlying data set<br>
		 * 
		 * + n if the iterator has just skipped n elements of the underlying
		 * data set<br>
		 * 
		 * - n if the iterator has just skipped n elements in the reverse
		 * direction of the underlying data set<br>
		 * 
		 */
		public int hasSkipped() {
			return 0;
		}

		/**
		 * Gets the current index for the iterator. This keeps track of the
		 * number of advances or retreates that the iterator has made on the
		 * underlying data set. Varies f
		 */
		public int getIndex() {
			return _index;
		}

		/**
		 * Gets the current index for the iterator. This keeps track of the
		 * number of advances or retreates that the iterator has made on the
		 * underlying data set. Varies f
		 */
		public int getUnderlyingIndex() {
			return getIndex();
		}

		/**
		 * if iterator is at start of data
		 */
		public boolean atStart() {
			return (_index == 0);
		}

		/**
		 * if iterator is at end of data.
		 */
		public boolean atEnd() {
			return (_index == _xArray.length);
		}

		/**
		 * The maximum of x and y range encapsulated as a data set element.
		 */
		public DataSetElement getMaximum() {
			if (_maximum == null) {
				_maximum = new DefaultDataSetElement();
				double xmax = -Float.MAX_VALUE;
				double ymax = -Float.MAX_VALUE;
				int count = size();
				for (int i = 0; i < count; i++) {
					ymax = Math.max(ymax, _yArray[i]);
					xmax = Math.max(xmax, _xArray[i]);
				}
				_maximum.setY(ymax);
				_maximum.setX(xmax);
			}
			return _maximum;
		}

		/**
		 * The minimum of x and y range encapsulated as a data set element.
		 */
		public DataSetElement getMinimum() {
			if (_minimum == null) {
				_minimum = new DefaultDataSetElement();
				double xmin = Float.MAX_VALUE;
				double ymin = Float.MAX_VALUE;
				int count = size();
				for (int i = 0; i < count; i++) {
					xmin = Math.min(xmin, _xArray[i]);
					ymin = Math.min(ymin, _yArray[i]);
				}
				_minimum.setX(xmin);
				_minimum.setY(ymin);
			}
			return _minimum;
		}

		/**
		 * the index in the array to the current data
		 */
		private int _index;
		/**
		 * The current data set element
		 */
		private DataSetElement _dse;
		/**
		 * checks to see if this the first call to nextElement() method since
		 * the resetIterator call...
		 */
		private boolean _firstNextCall;
		/**
   *
   */
		private DataSetElement _maximum, _minimum;
	}

}
