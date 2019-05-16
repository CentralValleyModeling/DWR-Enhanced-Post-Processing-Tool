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

/**
 * Filters the given iterator's values using a filter function.
 *
 * @author Nicky Sandhu
 * @version $Id: ElementFilterIterator.java,v 1.1 2003/10/02 20:49:22 redwood
 * Exp $
 */
public class ElementFilterIterator implements DataSetIterator
{
	/**
	 *
	 */
	private static final boolean DEBUG = false;
	/**
	 * The underlying iterator
	 */
	private DataSetIterator _dsi;
	/**
	 * The current element
	 */
	private DataSetElement _dse;
	/**
	 * true if some values have been skipped by this iterator
	 */
	private int _skipped;
	/**
	 * The function used to filter values.
	 */
	private ElementFilter _filter;
	/**
	 * The current index on the iterator
	 */
	private int _index;
	/**
	 * underlying iterator's index
	 */
	private int _underlyingIndex, _startUnderlyingIndex;
	/**
	 * number of values skipped previously
	 */
	private int _numberSkippedForward, _numberSkippedBackward;
	/**
	 * the first call to the nextElement() method since the resetIterator call.
	 */
	private boolean _firstNextCall;
	/**
	 *
	 */
	private boolean _isAtStart, _isAtEnd;
	/**
	 * the maximum and minimum values
	 */
	private DataSetElement _maximum, _minimum;

	/**
	 * Filters the given iterator through the given filter.
	 */
	public ElementFilterIterator(DataSetIterator iterator, ElementFilter filter)
	{
		_dsi = iterator;
		_filter = filter;
		_dse = _dsi.getElement();
		if(_dse == null)
		{
			throw new IllegalArgumentException("No data in iterator");
		}
		else
		{
			_dse = _dse.createClone();
		}
		resetIterator();
	}

	/**
	 * Resets the iterator to the beginning of data
	 */
	public void resetIterator()
	{
		_dsi.resetIterator();
		_skipped = 0;
		_index = 0;
		// get element and advance underlying iterator to next acceptable value
		_numberSkippedForward = 0;
		// while acceptable value not found keep skipping...
		while(!_dsi.atEnd() && !_filter.isAcceptable(_dsi.getElement()))
		{
			_dsi.advance();
			_numberSkippedForward++;
		}
		// check if iterator at end, i.e. no valid elements
		if(_dsi.atEnd())
		{
			_isAtEnd = true;
			return;
		}
		_skipped = _numberSkippedForward;
		_underlyingIndex = _dsi.getIndex();
		_startUnderlyingIndex = _dsi.getIndex();
		_isAtStart = true;
		// if no acceptable values found
		_isAtEnd = _dsi.atEnd();
	}

	/**
	 * gets the element at the current location
	 */
	public DataSetElement getElement()
	{
		_dse.copyFrom(_dsi.getElement());
		return _dse;
	}

	/**
	 * puts the element at the current location
	 */
	public void putElement(DataSetElement e)
	{
		_dsi.putElement(e);
	}

	/**
	 *
	 */
	private void checkIndex(int index)
	{
		if(index < 0)
		{
			throw new IndexOutOfBoundsException("Index is negative");
		}
	}

	/**
	 *
	 */
	public void positionAtIndex(int index)
	{
		checkIndex(index);
		int currentIndex = getIndex();
		int diff = index - currentIndex;
		if(diff > 0)
		{
			for(int i = 0; i < diff; i++)
			{
				advance();
			}
		}
		else if(diff < 0)
		{
			diff = -diff;
			for(int i = 0; i < diff; i++)
			{
				retreat();
			}
		}
	}

	/**
	 * Advance by one.
	 */
	public void advance()
	{
		_isAtStart = false;
		if(atEnd())
		{
			return;
		}
		_dsi.advance();
		// go to next acceptable value and make sure its acceptable
		// if not keep going till you hit a good one
		_numberSkippedForward = 0;
		while(!_dsi.atEnd() && !_filter.isAcceptable(_dsi.getElement()))
		{
			_dsi.advance();
			_numberSkippedForward++;
		}
		// if end was hit
		if(_dsi.atEnd())
		{
			_isAtEnd = true;
			_skipped = _numberSkippedForward;
			_underlyingIndex = _dsi.getIndex();
			_index++;
		}
		else
		{
			// copy current iterator position info
			_skipped = _numberSkippedForward;
			_underlyingIndex = _dsi.getIndex();
			_index++;
			_dse.copyFrom(_dsi.getElement());
		}
	}

	/**
	 * Retreate by one
	 */
	public void retreat()
	{
		_isAtEnd = false;
		if(atStart())
		{
			return;
		}
		_dsi.retreat();
		while(!_dsi.atStart() && !_filter.isAcceptable(_dsi.getElement()))
		{
			_dsi.retreat();
			_numberSkippedBackward++;
		}
		if(_dsi.atStart())
		{
			resetIterator();
			return;
		}
		// copy current iterator position info
		_underlyingIndex = _dsi.getIndex();
		_skipped = _numberSkippedBackward;
		_index--;
		if(_dsi.getIndex() == _startUnderlyingIndex)
		{
			_isAtStart = true;
		}
	}

	/**
	 * true if the iterator has skipped a few elements of the underlying data
	 * set
	 */
	public int hasSkipped()
	{
		return _skipped + _dsi.hasSkipped();
	}

	/**
	 * Gets the current index for the iterator. This is the index of this
	 * iterator on the underlying iterator's index of that of the data set
	 */
	public int getIndex()
	{
		return _index;
	}

	/**
	 *
	 */
	public int getUnderlyingIndex()
	{
		return _dsi.getUnderlyingIndex();
	}

	/**
	 * if iterator is at start of data
	 */
	public boolean atStart()
	{
		return _isAtStart;
	}

	/**
	 * if iterator is at end of data.
	 */
	public boolean atEnd()
	{
		return _isAtEnd;
	}

	/**
	 * The maximum of x and y range encapsulated as a data set element.
	 */
	public DataSetElement getMaximum()
	{
		if(_maximum == null)
		{
			calcMinMax();
		}
		return _maximum;
	}

	/**
	 * The minimum of x and y range encapsulated as a data set element.
	 */
	public DataSetElement getMinimum()
	{
		if(_minimum == null)
		{
			calcMinMax();
		}
		return _minimum;
	}

	/**
	 * this is done to save time so that one iteration over the entire set of
	 * elements can calculate max and min.
	 */
	private void calcMinMax()
	{
		double xmin = Float.MAX_VALUE;
		double ymin = Float.MAX_VALUE;
		double xmax = -Float.MAX_VALUE;
		double ymax = -Float.MAX_VALUE;
		int prevIndex = _dsi.getIndex();
		DataSetIterator dsi;
		try
		{
			dsi = new ElementFilterIterator(_dsi, _filter);
			_minimum = dsi.getElement().createClone();
			_maximum = dsi.getElement().createClone();
			while(!dsi.atEnd())
			{
				DataSetElement dse = dsi.getElement();
				double x = dse.getX();
				double y = dse.getY();
				xmin = Math.min(xmin, x);
				ymin = Math.min(ymin, y);
				xmax = Math.max(xmax, x);
				ymax = Math.max(ymax, y);
				dsi.advance();
			}
			_dsi.positionAtIndex(prevIndex);
		}
		catch(IllegalArgumentException exc)
		{
			_dsi.resetIterator();
			_minimum = _dsi.getElement().createClone();
			_maximum = _dsi.getElement().createClone();
			xmin = 0;
			xmax = 1;
			ymin = 0;
			ymax = 1;
		}
		_minimum.setX(xmin);
		_minimum.setY(ymin);
		_maximum.setX(xmax);
		_maximum.setY(ymax);
	}
}
