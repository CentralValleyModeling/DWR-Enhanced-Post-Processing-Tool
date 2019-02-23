/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package vista.set;

/**
 * This is a multi time series iterator. Each element returned by this iterator
 * is a time tuple that has the dimension == number of time series + 1. If a
 * certain value in a time series is no existent at the time it is set to
 * Float.NAN for the tuple that is returned by getElement()
 * 
 * @see TimeTuple
 * 
 * @author Nicky Sandhu
 * @version $Id: MultiIterator.java,v 1.1 2003/10/02 20:49:27 redwood Exp $
 */
public class MultiIterator implements DataSetIterator {
	private DataSetIterator[] _dsis;
	private double[] _y;
	private int[] _flags;
	private TimeTuple _dse;
	private long currentX;

	/**
	 * Creates an iterator to iterate over a group of time series with the given
	 * filter
	 */
	public MultiIterator(TimeSeries[] ts, ElementFilter f) {
		DataSetIterator[] iterators = new DataSetIterator[ts.length];
		if (f == null) {
			_dsis = iterators;
		} else {
			for (int i = 0; i < ts.length; i++)
				iterators[i] = new ElementFilterIterator(ts[i].getIterator(), f);
			_dsis = iterators;
		}
		_y = new double[iterators.length];
		_flags = new int[iterators.length];
		_dse = new TimeTuple(0, _y, _flags);
		resetIterator();
	}

	/**
	 * Creates an iterator to iterate over a group of time series
	 */
	public MultiIterator(TimeSeries[] ts) {
		DataSetIterator[] iterators = new DataSetIterator[ts.length];
		for (int i = 0; i < ts.length; i++)
			iterators[i] = ts[i].getIterator();
		_dsis = iterators;
		_y = new double[iterators.length];
		_flags = new int[iterators.length];
		_dse = new TimeTuple(0, _y, _flags);
		resetIterator();
	}

	/**
	 * Resets the iterator to the beginning of data
	 */
	public void resetIterator() {
		for (int i = 0; i < _dsis.length; i++) {
			_dsis[i].resetIterator();
		}
		_index = 0;
		currentX = 0;
		currentX = getMinX();
	}

	/**
	 * gets the element at the current location
	 */
	public DataSetElement getElement() {
		for (int i = 0; i < _dsis.length; i++) {
			DataSetIterator dsi = _dsis[i];
			if (dsi.atEnd()) {
				_y[i] = Float.NaN;
				_flags[i] = 0;
			} else {
				DataSetElement dse = dsi.getElement();
				if (Math.round(dse.getX()) == currentX) {
					_y[i] = dse.getY();
					_flags[i] = dse.getFlag();
				} else {
					_y[i] = Float.NaN;
					_flags[i] = 0;
				}
			}
		}
		_dse.setX(currentX);
		_dse.setY(_y); // not really necessary as dse contains the same array
		_dse.setFlags(_flags);
		return _dse;
		// experimental
	}

	/**
	 * puts the element at the current location
	 */
	public void putElement(DataSetElement e) {
		if (e.getDimension() != _dse.getDimension())
			throw new IllegalArgumentException("Dimensions mismatch");
		// long currentX = getMinX();
		for (int i = 0; i < _dsis.length; i++) {
			DataSetIterator dsi = _dsis[i];
			if (dsi.atEnd()) {
			} else {
				DataSetElement dse = dsi.getElement();
				if (Math.round(dse.getX()) == currentX) {
					dse.setY(e.getX(i + 1));
					dse.setFlag(e.getFlag(i));
					dsi.putElement(dse);
				} else {
				}
			}
		}
	}

	/**
   *
   */
	private void checkIndex(int index) {
		if (index < 0)
			throw new IndexOutOfBoundsException("Index is negative");
	}

	/**
	 * position at a certain index
	 */
	public void positionAtIndex(int index) {
		checkIndex(index);
		int currentIndex = getIndex();
		int diff = index - currentIndex;
		if (diff > 0) {
			for (int i = 0; i < diff; i++)
				advance();
		} else if (diff < 0) {
			diff = -diff;
			for (int i = 0; i < diff; i++)
				retreat();
		}
	}

	/**
	 * Advance by one.
	 */
	public void advance() {
		if (atEnd())
			return;
		// advance those at current index position excepting
		// those who are at end.
		for (int i = 0; i < _dsis.length; i++) {
			DataSetIterator dsi = _dsis[i];
			if (dsi.atEnd())
				continue;
			long ct = Math.round(dsi.getElement().getX());
			if (currentX >= ct) {
				if (!dsi.atEnd())
					dsi.advance();
			}
		}
		// set the index @ the smallest current time value
		if (!atEnd()) {
			currentX = getMinX();
		} else {
			currentX = currentX + 1; // make last x slightly larger
		}
		_index++;
	}

	/**
   *
   */
	public long getMinX() {
		double x = Float.MAX_VALUE;
		for (int i = 0; i < _dsis.length; i++) {
			DataSetIterator dsi = _dsis[i];
			if (dsi.atEnd())
				continue;
			if (Math.round(dsi.getElement().getX()) > currentX)
				x = Math.min(dsi.getElement().getX(), x);
		}
		return Math.round(x);
	}

	/**
	 * Retreat by one
	 */
	public void retreat() {
		if (atStart())
			return;
		// for all the iterators with the largest time value retreat them.
		// for the rest, reset them.
		// make sure all the iterators are not at the end
		for (int i = 0; i < _dsis.length; i++) {
			DataSetIterator dsi = _dsis[i];
			if (dsi.atEnd())
				dsi.retreat();
		}
		// retreat those equal to the current x value
		for (int i = 0; i < _dsis.length; i++) {
			DataSetIterator dsi = _dsis[i];
			if (currentX <= Math.round(dsi.getElement().getX())) {
				if (!dsi.atStart())
					dsi.retreat();
			}
		}
		// get largest value less than current position
		if (!atStart())
			currentX = getMaxX();
		else {
			currentX = 0;
			currentX = getMinX();
		}
		_index--;
	}

	/**
   *
   */
	public long getMaxX() {
		double x = -Float.MAX_VALUE;
		for (int i = 0; i < _dsis.length; i++) {
			DataSetIterator dsi = _dsis[i];
			DataSetElement dse = dsi.getElement();
			if (dse != null && dse.getX() < currentX)
				x = Math.max(dsi.getElement().getX(), x);
		}
		return Math.round(x);
	}

	/**
	 * true if the iterator has skipped a few elements of the underlying data
	 * set
	 */
	public int hasSkipped() {
		int skipped = Integer.MAX_VALUE;
		long x = getMinX();
		for (int i = 0; i < _dsis.length; i++) {
			DataSetIterator dsi = _dsis[i];
			if (!dsi.atEnd()) {
				skipped = Math.min(skipped, dsi.hasSkipped());
			}
		}
		return skipped;
	}

	/**
	 * Gets the current index for the iterator. This is the index of this
	 * iterator on the underlying iterator's index of that of the data set
	 */
	public int getIndex() {
		return _index;
	}

	/**
   *
   */
	public int getUnderlyingIndex() {
		return _index;
	}

	/**
	 * if iterator is at start of data
	 */
	public boolean atStart() {
		for (int i = 0; i < _dsis.length; i++) {
			if (!_dsis[i].atStart())
				return false;
		}
		return true;
	}

	/**
	 * if iterator is at end of data.
	 */
	public boolean atEnd() {
		for (int i = 0; i < _dsis.length; i++) {
			if (!_dsis[i].atEnd())
				return false;
		}
		return true;
	}

	/**
	 * The maximum of x and y range encapsulated as a data set element.
	 */
	public DataSetElement getMaximum() {
		double xmax = -Float.MAX_VALUE;
		double ymax = -Float.MAX_VALUE;
		int prevIndex = this.getIndex();
		DataSetIterator dsi = this;
		dsi.resetIterator();
		_maximum = dsi.getElement().createClone();
		double[] maxs = new double[_maximum.getDimension()];
		while (!dsi.atEnd()) {
			DataSetElement dse = dsi.getElement();
			for (int i = 0; i < maxs.length; i++) {
				double d = dse.getX(i);
				if (Double.doubleToLongBits(d) != 0x7ff8000000000000L)
					maxs[i] = Math.max(maxs[i], d);
			}
			dsi.advance();
		}
		this.positionAtIndex(prevIndex);
		for (int i = 0; i < maxs.length; i++)
			_maximum.setX(i, maxs[i]);
		return _maximum;
	}

	/**
	 * The minimum of x and y range encapsulated as a data set element.
	 */
	public DataSetElement getMinimum() {
		double xmin = Float.MAX_VALUE;
		double ymin = Float.MAX_VALUE;
		int prevIndex = this.getIndex();
		DataSetIterator dsi = this;
		dsi.resetIterator();
		_minimum = dsi.getElement().createClone();
		double[] mins = new double[_minimum.getDimension()];
		while (!dsi.atEnd()) {
			DataSetElement dse = dsi.getElement();
			for (int i = 0; i < mins.length; i++) {
				double d = dse.getX(i);
				if (Double.doubleToLongBits(d) != 0x7ff8000000000000L)
					mins[i] = Math.min(mins[i], d);
			}
			dsi.advance();
		}
		this.positionAtIndex(prevIndex);
		for (int i = 0; i < mins.length; i++)
			_minimum.setX(i, mins[i]);
		return _minimum;
	}

	/**
	 * The current index on the iterator
	 */
	private int _index;
	/**
	 * the maximum and minimum values
	 */
	private DataSetElement _maximum, _minimum;
	/**
   *
   */
	private static final boolean DEBUG = false;
}
