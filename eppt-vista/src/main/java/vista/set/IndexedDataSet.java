/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package vista.set;

/**
 * An indexed data set does not need to store all its x values. X values are the
 * indicies for the y values. All that is needed in such a regular indexed array
 * is to store initial and final values of x and the step size.
 * 
 * @see DataSetElement
 * @see DataSetIterator
 * @author Nicky Sandhu (DWR).
 * @version $Id: IndexedDataSet.java,v 1.4 2001/03/05 21:20:08 eli2 Exp $
 */
public class IndexedDataSet implements DataSet {
	/**
   *
   */
	IndexedDataSet() {
	}

	/**
	 * Initializes a data set with the initial x and final x values and the x
	 * step size and y arrays. No new memory is created, only references are
	 * held to the two arrays.
	 * 
	 * @exception IllegalArgumentException
	 *                if x and y array lengths don't match.
	 */
	public IndexedDataSet(String name, double xInitial, double xFinal,
			double step, double[] y) {
		this(name, xInitial, xFinal, step, y, null, null);
	}

	/**
	 * Initializes a data set with the y arrays and with a flag array for each
	 * value. No new memory is created, only references are held to the two
	 * arrays.
	 * 
	 * @exception IllegalArgumentException
	 *                if x and y array lengths don't match.
	 */
	public IndexedDataSet(String name, double xInitial, double xFinal,
			double step, double[] y, int[] flags) {
		this(name, xInitial, xFinal, step, y, flags, null);
	}

	/**
	 * Initializes a data set with the y arrays and with a flag array for each
	 * value. No new memory is created, only references are held to the two
	 * arrays.
	 * 
	 * @exception IllegalArgumentException
	 *                if x and y array lengths don't match.
	 */
	public IndexedDataSet(String name, double xInitial, double xFinal,
			double step, double[] y, int[] flags, DataSetAttr attr) {
		if (xFinal <= xInitial)
			throw new IllegalArgumentException(
					"Error: incorrect xinitial and xfinal");

		if (step <= 0.0)
			throw new IllegalArgumentException("Error: incorrect step size "
					+ step);

		_xInitial = xInitial;
		_xFinal = xFinal;
		_step = step;
		int dataCount = (int) ((_xFinal - _xInitial) / _step + 1);

		if (dataCount != y.length)
			throw new IllegalArgumentException("Error: data length = "
					+ dataCount + " and y array length = " + y.length
					+ " don't match");

		if (flags != null) {
			if (flags.length != dataCount)
				throw new IllegalArgumentException(
						"Error: flag array of incorrect length");
			else
				_flagArray = flags;
		} else {
			_flagArray = null;
		}

		_yArray = y;
		_name = name;
		_attr = attr;
		_dsi = getIterator();
	}

	/**
	 * gets element at index i
	 */
	public DataSetElement getElementAt(int i) {
		_dsi.positionAtIndex(i);
		return _dsi.getElement();
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

	@Override
	public void addFlags() {
		if (_flagArray != null) return;
		int dataCount = (int) ((_xFinal - _xInitial) / _step + 1);
		_flagArray = new int[dataCount];
	}

	/**
	 * returns the number of elements in the dataset
	 */
	public int size() {
		return _yArray.length;
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
	 * gets the initial x value
	 */
	public double getXInitial() {
		return _xInitial;
	}

	/**
	 * gets the final x value
	 */
	public double getXFinal() {
		return _xFinal;
	}

	/**
	 * gets the step for getting next x value.
	 */
	public double getXStep() {
		return _step;
	}

	/**
   *
   */
	public double[] getYArray() {
		return _yArray;
	}

	/**
   *
   */
	public int[] getFlagArray() {
		return _flagArray;
	}

	/**
   *
   */
	private double _xInitial, _xFinal, _step;
	/**
   *
   */
	private double[] _yArray;
	/**
   *
   */
	private int[] _flagArray;
	/**
	 * name of data set
	 */
	private String _name;
	/**
	 * true if flagged elements are contained in data set
	 */
	private boolean _isFlagged;
	/**
   *
   */
	private DataSetAttr _attr;
	private DataSetIterator _dsi;

	/**
	 * Private iterator class to iterate over the elements of this data set.
	 * 
	 * @author Nicky Sandhu
	 * @version $Id: IndexedDataSet.java,v 1.4 2001/03/05 21:20:08 eli2 Exp $
	 */
	private class DefaultIterator implements DataSetIterator {
		/**
		 * initializes element of correct type
		 */
		public DefaultIterator(IndexedDataSet ds) {
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
		 * Returns with the next element and the iterator positioned at the next
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
			_dse.setX(_xInitial + _index * _step);
			_dse.setY(_yArray[_index]);
			if (isFlagged())
				_dse.setFlag(_flagArray[_index]);
			return _dse;
		}

		/**
		 * puts the element at the current location
		 */
		public void putElement(DataSetElement e) {
			_yArray[_index] = e.getY();
			if (isFlagged())
				_flagArray[_index] = e.getFlag();
		}

		/**
		 * positions iterator at index
		 */
		public void positionAtIndex(int i) {
			if (i < 0 || i > _yArray.length - 1)
				throw new IndexOutOfBoundsException("Incorrect index " + i
						+ " in Default Data Set ");
			_index = i;
		}

		/**
		 * Advance by one.
		 */
		public void advance() {
			_index++;
		}

		/**
		 * Retreat by one
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
			return _index;
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
			return (_index == _yArray.length);
		}

		/**
		 * The maximum of x and y range encapsulated as a data set element.
		 */
		public DataSetElement getMaximum() {
			if (_maximum == null) {
				_maximum = new DefaultDataSetElement();
				_maximum.setX(_xFinal);
				double ymax = -Float.MAX_VALUE;
				int count = size();
				for (int i = 0; i < count; i++) {
					ymax = Math.max(ymax, _yArray[i]);
				}
				_maximum.setY(ymax);
			}
			return _maximum;
		}

		/**
		 * The minimum of x and y range encapsulated as a data set element.
		 */
		public DataSetElement getMinimum() {
			if (_minimum == null) {
				_minimum = new DefaultDataSetElement();
				_minimum.setX(_xInitial);
				double ymin = Float.MAX_VALUE;
				int count = size();
				for (int i = 0; i < count; i++) {
					ymin = Math.min(ymin, _yArray[i]);
				}
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
