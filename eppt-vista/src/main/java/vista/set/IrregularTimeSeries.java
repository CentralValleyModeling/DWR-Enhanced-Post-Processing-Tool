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

import java.util.Enumeration;
import java.util.Vector;

import org.python.core.Py;
import vista.time.Time;
import vista.time.TimeFactory;
import vista.time.TimeWindow;

/**
 * A default implementation of the data set interface. This class uses an array
 * of x and y doubles to store its elements.
 *
 * @author Nicky Sandhu (DWR).
 * @version $Id: IrregularTimeSeries.java,v 1.1 2003/10/02 20:49:25 redwood Exp
 * $
 * @see DataSetElement
 * @see DataSetIterator
 */
public class IrregularTimeSeries extends TimeSeries
{
	/**
	 *
	 */
	private TimeFactory _tf = TimeFactory.getInstance();
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
	private String _name, _blockInterval = "IR-YEAR";
	/**
	 * true if flagged elements are contained in data set
	 */
	private boolean _isFlagged;
	/**
	 * attributes of this data
	 */
	private DataSetAttr _attr;
	/**
	 *
	 */
	private DataSetIterator _dsi;
	/**
	 * start and end times
	 */
	private Time _stime, _etime;

	/**
	 * creates an irregular time series of given name, an array of time values
	 * and an array of values
	 */
	public IrregularTimeSeries(String name, Time[] time, double[] y)
	{
		double[] x = new double[time.length];
		for(int i = 0; i < x.length; i++)
		{
			x[i] = time[i].getTimeInMinutes();
		}
		//
		initialize(name, x, y, null, null);
	}

	/**
	 * Constructor for irregular time series
	 *
	 * @param name  The name of the time series. Could be the pathname which then
	 *              gets parsed into its attributes.
	 * @param time  An array of time objects
	 * @param y     An array corresponding to each element of the time array
	 * @param flags An array of quality flags [optional]
	 * @param attr  The data set attributes [optional]
	 */
	public IrregularTimeSeries(String name, Time[] time, double[] y,
							   int[] flags, DataSetAttr attr)
	{
		double[] x = new double[time.length];
		for(int i = 0; i < x.length; i++)
		{
			x[i] = time[i].getTimeInMinutes();
		}
		initialize(name, x, y, flags, attr);
	}

	/**
	 * Initializes an irregular time series with the x and y arrays. No new
	 * memory is created, only references are held to the two arrays.
	 *
	 * @throws IllegalArgumentException if x and y array lengths don't match.
	 */
	public IrregularTimeSeries(String name, double[] x, double[] y)
	{
		initialize(name, x, y, null, null);
	}

	/**
	 * Initializes a data set with the x and y arrays and with a flag array for
	 * each value.
	 *
	 * @throws IllegalArgumentException if x and y array lengths don't match.
	 */
	public IrregularTimeSeries(String name, double[] x, double[] y, int[] flags)
	{
		initialize(name, x, y, flags, null);
	}

	/**
	 * Creates an irregular time series from the given time series. All missing
	 * values are removed from the resulting irregular time series. This is the
	 * same as calling the constructor with a filter of Constants.DEFAULT_FILTER
	 */
	public IrregularTimeSeries(TimeSeries ts)
	{
		this(ts, Constants.DEFAULT_FILTER);
	}

	/**
	 * creates an irregular time series from a given time series and a filter
	 * such that only acceptable values go into creating this irregular time
	 * series
	 */
	public IrregularTimeSeries(TimeSeries rts, ElementFilter filter)
	{
		DataSetIterator iter = rts.getIterator();
		if(filter != null)
		{
			iter = new ElementFilterIterator(iter, filter);
		}
		int count = rts.size();
		double[] x = new double[count];
		double[] y = new double[count];
		int[] flags = null;
		boolean isFlagged = rts.isFlagged();
		if(isFlagged)
		{
			flags = new int[count];
		}
		int i = 0;
		for(iter.resetIterator(); !iter.atEnd(); iter.advance())
		{
			DataSetElement e = iter.getElement();
			x[i] = e.getX();
			y[i] = e.getY();
			if(isFlagged)
			{
				flags[i] = e.getFlag();
			}
			i++;
		}
		if(i < count)
		{
			double[] tx = new double[i];
			System.arraycopy(x, 0, tx, 0, tx.length);
			x = tx;
			tx = new double[i];
			System.arraycopy(y, 0, tx, 0, tx.length);
			y = tx;
			if(isFlagged)
			{
				int[] tf = new int[i];
				System.arraycopy(flags, 0, tf, 0, tf.length);
				flags = tf;
			}
		}
		DataSetAttr oattr = rts.getAttributes();
		DataSetAttr attr = new DataSetAttr(oattr.getGroupName(), oattr
				.getLocationName(), oattr.getTypeName(), oattr.getSourceName(),
				DataType.IRREGULAR_TIME_SERIES, oattr.getXUnits(), oattr
				.getYUnits(), oattr.getXType(), oattr.getYType());
		initialize(rts.getName(), x, y, flags, oattr);
	}

	/**
	 * The main constructor for irregular time series.
	 *
	 * @param name  The name of this irregular time series. If this name can be
	 *              parsed into pathname parts that is done and the resulting
	 *              parts are used to form the attributes of this irregular time
	 *              series
	 * @param x     The array of x values in julian minutes since 30dec1899 2400.
	 *              This array is held by this irregular time series as opposed to
	 *              creating a copy
	 * @param y     The array of y values. This array is held by this irregular
	 *              time series as opposed to creating a copy.
	 * @param flags The array of quality flags
	 * @param attr  The attributes of this data set
	 * @throws IllegalArgumentException if x and y array lengths don't match.
	 */
	public IrregularTimeSeries(String name, double[] x, double[] y,
							   int[] flags, DataSetAttr attr)
	{
		initialize(name, x, y, flags, attr);
	}

	/**
	 *
	 */
	private void initialize(String name, double[] x, double[] y, int[] flags,
							DataSetAttr attr)
	{
		// should I do a check to make sure time is in increasing order?
		if(x.length != y.length)
		{
			throw new IllegalArgumentException(
					"Error: x and y array lengths don't match");
		}
		if(flags != null)
		{
			if(flags.length != x.length)
			{
				throw new IllegalArgumentException(
						"Error: flag array of incorrect length");
			}
			else
			{
				_flagArray = flags;
			}
		}
		else
		{
			_flagArray = null;
		}
		_xArray = x;
		// start time
		_stime = _tf.createTime(Math.round(_xArray[0]));
		// end time
		_etime = _tf.createTime(Math.round(_xArray[_xArray.length - 1]));
		_yArray = y;
		if(attr == null)
		{
			attr = new DataSetAttr(DataType.IRREGULAR_TIME_SERIES, "", "", "",
					"INST-VAL");
		}
		_attr = attr;
		setName(name == null ? "" : name);
		_dsi = getIterator();
	}

	/**
	 * sets the filter on the default filter of this irregular time series. This
	 * is useful if you want to skip values when calling array access methods on
	 * this time series.
	 */
	public void setFilter(ElementFilter f)
	{
		_dsi = new ElementFilterIterator(_dsi, f);
	}

	/**
	 * gets element at index i
	 */
	public DataSetElement getElementAt(int i)
	{
		_dsi.positionAtIndex(i);
		DataSetElement e = _dsi.getElement();
		if(e == null)
		{
			throw new IllegalArgumentException("No element at index" + i);
		}
		else
		{
			return e.createClone();
		}
	}

	/**
	 * finds the element at the given index. If the given element is outside the
	 * bounds of this time series a Py.IndexError is thrown
	 */
	public DataSetElement findElementAt(int i)
	{
		_dsi.positionAtIndex(i);
		DataSetElement e = _dsi.getElement();
		if(e == null)
		{
			return null;
		}
		else
		{
			return e.createClone();
		}
	}

	/**
	 * gets element at index i
	 */
	public TimeElement getElementAt(String tm)
	{
		Time t = _stime.create(tm);
		((DefaultIterator) _dsi).positionAtTime(t);
		DataSetElement e = findElementAt(_dsi.getIndex());
		if(e == null)
		{
			throw new IllegalArgumentException("No element at time " + tm);
		}
		else
		{
			return (TimeElement) e;
		}
	}

	/**
	 * gets element at index i
	 */
	public TimeElement findElementAt(String tm)
	{
		Time t = _stime.create(tm);
		((DefaultIterator) _dsi).positionAtTime(t);
		DataSetElement e = findElementAt(_dsi.getIndex());
		if(e == null)
		{
			return null;
		}
		else
		{
			return (TimeElement) e;
		}
	}

	/**
	 * sets element at index i
	 */
	public void putElementAt(int i, DataSetElement dse)
	{
		// if index incorrect return
		if(i < 0)
		{
			return;
		}
		// if ( i > size() ) expandTo(i);
		// expand to i
		_dsi.positionAtIndex(i);
		_dsi.putElement(dse);
	}

	/**
	 * true if data set is flagged
	 */
	public boolean isFlagged()
	{
		return (_flagArray != null);
	}

	@Override
	public void addFlags()
	{
		if(_flagArray != null)
		{
			return;
		}
		_flagArray = new int[this.size()];
	}

	/**
	 * returns the number of elements in the dataset
	 */
	public int size()
	{
		return _xArray.length;
	}

	/**
	 * Return an iterator positioned at my first item.
	 */
	public DataSetIterator getIterator()
	{
		return new DefaultIterator(this);
	}

	/**
	 * returns a name for this DataSet to be used to identify it.
	 */
	public String getName()
	{
		return _name;
	}

	/**
	 * sets the name to identify the data set.
	 */
	public void setName(String name)
	{
		Pathname path = null;
		try
		{
			path = Pathname.createPathname(name);
		}
		catch(Exception e)
		{
			path = null;
		}
		if(path != null)
		{
			DataSetAttr attr = getAttributes();
			if(attr != null)
			{
				attr.setGroupName(path.getPart(Pathname.A_PART));
				attr.setLocationName(path.getPart(Pathname.B_PART));
				attr.setTypeName(path.getPart(Pathname.C_PART));
				attr.setSourceName(path.getPart(Pathname.F_PART));
			}
			_blockInterval = path.getPart(Pathname.E_PART);
			_name = "/" + attr.getGroupName() + "/" + attr.getLocationName()
					+ "/" + attr.getTypeName() + "/" + getTimeWindow() + "/"
					+ getBlockInterval() + "/" + attr.getSourceName() + "/";
		}
		else
		{
			_name = name;
		}
	}

	/**
	 * An object attached to this data set which contains descriptive
	 * information of the underlying data.
	 */
	public DataSetAttr getAttributes()
	{
		return _attr;
	}

	/**
	 * An object attached to this data set which contains descriptive
	 * information of the underlying data.
	 */
	public void setAttributes(DataSetAttr attr)
	{
		_attr = attr;
	}

	/**
	 * returns the size of the block interval of this irregular time series.
	 * This size has more to do with the way DSS stores the data and as such
	 * should really be in vista.db.dss utiltiy functions
	 */
	public String getBlockInterval()
	{
		return _blockInterval;
	}

	/**
	 * a string representation of this time series
	 */
	public String toString()
	{
		return "Irregular Time Series: " + getName() + " from "
				+ getStartTime() + " to " + getEndTime();
	}

	/**
	 * get start time
	 */
	public Time getStartTime()
	{
		return _stime;
	}

	/**
	 * get start time
	 */
	public Time getEndTime()
	{
		return _etime;
	}

	/**
	 * gets the time window
	 */
	public TimeWindow getTimeWindow()
	{
		return _tf.createTimeWindow(getStartTime(), getEndTime());
	}

	public double[] getXArray()
	{
		return _xArray;
	}

	public double[] getYArray()
	{
		return _yArray;
	}

	public int[] getFlagArray()
	{
		return _flagArray;
	}

	/**
	 * creates a slice from the current time series.
	 */
	public TimeSeries createSlice(String st, String et)
	{
		Time stm = _stime.create(st);
		Time etm = _stime.create(et);
		TimeWindow tw = _tf.createTimeWindow(stm, etm);
		return createSlice(tw);
	}

	/**
	 *
	 */
	public TimeSeries createSlice(TimeWindow tw)
	{
		TimeWindow twi = tw.intersection(getTimeWindow());
		if(twi == null)
		{
			return null;
		}
		Time stime = twi.getStartTime();
		Time etime = twi.getEndTime();
		Vector array = new Vector();
		_dsi.resetIterator();
		while(!(_dsi.atEnd())
				&& _dsi.getElement().getX() < stime.getTimeInMinutes())
		{
			_dsi.advance();
		}
		// expand if possible to get value before start time ( expanding
		// philosophy)
		if(_dsi.getElement().getX() != stime.getTimeInMinutes()
				&& !_dsi.atStart())
		{
			_dsi.retreat();
		}
		// get values between times
		while(!(_dsi.atEnd())
				&& _dsi.getElement().getX() < etime.getTimeInMinutes())
		{
			array.addElement(_dsi.getElement().createClone());
			_dsi.advance();
		}
		// get last value if exact or next value after last if no matching found
		// and
		// there exists a next value
		if(_dsi.getElement().getX() == etime.getTimeInMinutes())
		{
			array.addElement(_dsi.getElement().createClone());
		}
		else
		{
			if(!_dsi.atEnd())
			{
				_dsi.advance();
				array.addElement(_dsi.getElement().createClone());
			}
		}
		double[] xarray = new double[array.size()];
		double[] yarray = new double[array.size()];
		int[] farray = null;
		if(isFlagged())
		{
			farray = new int[array.size()];
		}
		int index = 0;
		for(Enumeration e = array.elements(); e.hasMoreElements(); )
		{
			DataSetElement dse = (DataSetElement) e.nextElement();
			xarray[index] = dse.getX();
			yarray[index] = dse.getY();
			if(isFlagged())
			{
				farray[index] = dse.getFlag();
			}
			index++;
		}
		return new IrregularTimeSeries(getName(), xarray, yarray, farray,
				getAttributes().createClone());
	}

	/**
	 * for python
	 */
	public IrregularTimeSeries __getslice__(String st, String et)
	{
		return (IrregularTimeSeries) createSlice(st, et);
	}

	/**
	 * for python
	 */
	public DataSetElement[] __getslice__(int i, int j)
	{
		int bi, ei;
		if(i < 0)
		{
			i = size() + i;
		}
		if(j < 0)
		{
			j = size() + j;
		}
		bi = Math.min(i, j);
		ei = Math.max(i, j);
		ei = Math.min(ei, size() - 1);
		DataSetElement[] elements = new DataSetElement[ei - bi + 1];
		for(int k = 0; k < elements.length; k++)
		{
			elements[k] = getElementAt(k + bi);
		}
		return elements;
	}

	/**
	 * for python
	 */
	public void __setslice__(int i, int j, double[] array)
	{
		int bi, ei;
		if(i < 0)
		{
			i = size() + i;
		}
		if(j < 0)
		{
			j = size() + j;
		}
		bi = Math.min(i, j);
		ei = Math.max(i, j);
		DataSetElement ndse = _dsi.getElement().createClone();
		for(int k = 0; k < array.length; k++)
		{
			ndse.setY(array[k]);
			putElementAt(k + bi, ndse);
		}
	}

	/**
	 * for python len() functionality
	 */
	public int __len__()
	{
		return size();
	}

	/**
	 * for python x[] functionality
	 */
	public DataSetElement __getitem__(int i)
	{
		try
		{
			return findElementAt(i);
		}
		catch(Throwable t)
		{
			throw Py.IndexError("index out of range: " + i);
		}
	}

	/**
	 * for python x[] functionality
	 */
	public DataSetElement __finditem__(int i)
	{
		return findElementAt(i);
	}

	/**
	 * for python x[] functionality
	 */
	public DataSetElement __getitem__(String tm)
	{
		try
		{
			((DefaultIterator) _dsi).positionAtTime(_stime.create(tm));
			return __getitem__(_dsi.getIndex());
		}
		catch(IndexOutOfBoundsException e)
		{
			throw Py.IndexError("index out of range: " + e.getMessage());
		}
		catch(IllegalArgumentException e)
		{
			throw Py.KeyError("incorrect time format: " + e.getMessage());
		}
	}

	/**
	 * for python
	 */
	public void __setitem__(int i, TimeElement dse)
	{
		putElementAt(i, dse);
	}

	/**
	 * for python
	 */
	public void __setitem__(int i, double d)
	{
		putElementAt(i, new TimeElement(0, d));
	}

	/**
	 * for python
	 */
	public void __setitem__(String tm, TimeElement dse)
	{
		((DefaultIterator) _dsi).positionAtTime(_stime.create(tm));
		putElementAt(_dsi.getIndex(), dse);
	}

	/**
	 * for python
	 */
	public void __setitem__(String tm, double d)
	{
		((DefaultIterator) _dsi).positionAtTime(_stime.create(tm));
		putElementAt(_dsi.getIndex(), new TimeElement(0, d));
	}

	/**
	 *
	 */
	public IrregularTimeSeries __pow__(double power)
	{
		return (IrregularTimeSeries) TimeSeriesMath.pow(this, power);
	}

	/**
	 *
	 */
	public IrregularTimeSeries __add__(RegularTimeSeries tids)
	{
		return (IrregularTimeSeries) TimeSeriesMath.doBinaryOperation(this,
				tids, TimeSeriesMath.ADD);
	}

	/**
	 *
	 */
	public IrregularTimeSeries __sub__(RegularTimeSeries tids)
	{
		return (IrregularTimeSeries) TimeSeriesMath.doBinaryOperation(this,
				tids, TimeSeriesMath.SUB);
	}

	/**
	 *
	 */
	public IrregularTimeSeries __mul__(RegularTimeSeries tids)
	{
		return (IrregularTimeSeries) TimeSeriesMath.doBinaryOperation(this,
				tids, TimeSeriesMath.MUL);
	}

	/**
	 *
	 */
	public IrregularTimeSeries __div__(RegularTimeSeries tids)
	{
		return (IrregularTimeSeries) TimeSeriesMath.doBinaryOperation(this,
				tids, TimeSeriesMath.DIV);
	}

	/**
	 *
	 */
	public IrregularTimeSeries __add__(double d)
	{
		return (IrregularTimeSeries) TimeSeriesMath.doBinaryOperation(this, d,
				TimeSeriesMath.ADD, false);
	}

	/**
	 *
	 */
	public IrregularTimeSeries __sub__(double d)
	{
		return (IrregularTimeSeries) TimeSeriesMath.doBinaryOperation(this, d,
				TimeSeriesMath.SUB, false);
	}

	/**
	 *
	 */
	public IrregularTimeSeries __mul__(double d)
	{
		return (IrregularTimeSeries) TimeSeriesMath.doBinaryOperation(this, d,
				TimeSeriesMath.MUL, false);
	}

	/**
	 *
	 */
	public IrregularTimeSeries __div__(double d)
	{
		return (IrregularTimeSeries) TimeSeriesMath.doBinaryOperation(this, d,
				TimeSeriesMath.DIV, false);
	}

	/**
	 *
	 */
	public IrregularTimeSeries __radd__(double d)
	{
		return __add__(d);
	}

	/**
	 *
	 */
	public IrregularTimeSeries __rsub__(double d)
	{
		return (IrregularTimeSeries) TimeSeriesMath.doBinaryOperation(this, d,
				TimeSeriesMath.SUB, true);
	}

	/**
	 *
	 */
	public IrregularTimeSeries __rmul__(double d)
	{
		return __mul__(d);
	}

	/**
	 *
	 */
	public IrregularTimeSeries __rdiv__(double d)
	{
		return (IrregularTimeSeries) TimeSeriesMath.doBinaryOperation(this, d,
				TimeSeriesMath.DIV, true);
	}

	/**
	 * Private iterator class to iterate over the elements of this data set.
	 *
	 * @author Nicky Sandhu
	 * @version $Id: IrregularTimeSeries.java,v 1.1 2003/10/02 20:49:25 redwood
	 * Exp $
	 */
	private class DefaultIterator implements TimeSeriesIterator
	{
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

		/**
		 * initializes element of correct type
		 */
		public DefaultIterator(IrregularTimeSeries ds)
		{
			if(isFlagged())
			{
				_dse = new FlaggedTimeElement();
			}
			else
			{
				_dse = new TimeElement();
			}
			resetIterator();
		}

		/**
		 * Resets the iterator to the beginning of data
		 */
		public void resetIterator()
		{
			_index = 0;
			_firstNextCall = true;
		}

		/**
		 * Returns with the next element and the iterator positioned at that
		 * element.
		 */
		public DataSetElement nextElement()
		{
			if(_index > 0)
			{
				advance();
			}
			else
			{
				if(!_firstNextCall)
				{
					advance();
				}
				else
				{
					_firstNextCall = false;
				}
			}
			return getElement();
		}

		/**
		 * returns the previous element in the array.
		 */
		public DataSetElement previousElement()
		{
			retreat();
			return getElement();
		}

		/**
		 * gets the element at the current location
		 */
		public DataSetElement getElement()
		{
			if(_index < 0 || _index >= _yArray.length)
			{
				return null;
			}
			_dse.setX(_xArray[_index]);
			_dse.setY(_yArray[_index]);
			if(isFlagged())
			{
				_dse.setFlag(_flagArray[_index]);
			}
			return _dse;
		}

		/**
		 * puts the element at the current location
		 */
		public void putElement(DataSetElement e)
		{
			_xArray[_index] = e.getX();
			_yArray[_index] = e.getY();
			if(isFlagged())
			{
				_flagArray[_index] = e.getFlag();
			}
		}

		/**
		 *
		 */
		public void positionAtIndex(int i)
		{
			if(i < 0 || i > _yArray.length - 1)
			{
				throw new IndexOutOfBoundsException("Incorrect index " + i
						+ " in Default Data Set ");
			}
			_index = i;
		}

		/**
		 * positions iterator at index
		 */
		public void positionAtTime(Time tm)
		{
			int ec = tm.compare(_etime);
			int sc = tm.compare(_stime);
			// check end time
			if(ec > 0)
			{
				throw new IndexOutOfBoundsException("Time : " + tm
						+ " is greater than end time of " + _etime);
			}
			else if(ec == 0)
			{
				_index = _xArray.length - 1;
				return;
			}
			// start time
			if(sc < 0)
			{
				throw new IndexOutOfBoundsException("Time : " + tm
						+ " is less than start time of " + _stime);
			}
			else if(sc == 0)
			{
				_index = 0;
				return;
			}
			//
			long k = tm.getTimeInMinutes();
			DataSetElement currentElement = getElement();
			double currentTimeInMins = currentElement.getX();
			int i = _index;
			if(currentTimeInMins < k)
			{
				while(((long) _xArray[i]) < k)
				{
					i++;
				}
				if((long) _xArray[i] != k)
				{
					i--;
				}
			}
			else
			{
				while(_xArray[i] > k)
				{
					i--;
				}
			}
			_index = i;
		}

		/**
		 * Advance by one. no checks for out of bound advance here?
		 */
		public void advance()
		{
			_index++;
		}

		/**
		 * Retreat by one. no checks for out of bound advance here?
		 */
		public void retreat()
		{
			_index--;
		}

		/**
		 * 0 if no elements were skipped by getting this element from the
		 * underlying data set<br>
		 * <p>
		 * + n if the iterator has just skipped n elements of the underlying
		 * data set<br>
		 * <p>
		 * - n if the iterator has just skipped n elements in the reverse
		 * direction of the underlying data set<br>
		 */
		public int hasSkipped()
		{
			return 0;
		}

		/**
		 * Gets the current index for the iterator. This keeps track of the
		 * number of advances or retreates that the iterator has made on the
		 * underlying data set. Varies f
		 */
		public int getIndex()
		{
			return _index;
		}

		/**
		 * Gets the current index for the iterator. This keeps track of the
		 * number of advances or retreates that the iterator has made on the
		 * underlying data set. Varies f
		 */
		public int getUnderlyingIndex()
		{
			return _index;
		}

		/**
		 * if iterator is at start of data
		 */
		public boolean atStart()
		{
			return (_index == 0);
		}

		/**
		 * if iterator is at end of data.
		 */
		public boolean atEnd()
		{
			return (_index == _xArray.length);
		}

		/**
		 * The maximum of x and y range encapsulated as a data set element.
		 */
		public DataSetElement getMaximum()
		{
			if(_maximum == null)
			{
				_maximum = new TimeElement();
				double xmax = -Float.MAX_VALUE;
				double ymax = -Float.MAX_VALUE;
				int count = size();
				for(int i = 0; i < count; i++)
				{
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
		public DataSetElement getMinimum()
		{
			if(_minimum == null)
			{
				_minimum = new TimeElement();
				double xmin = Float.MAX_VALUE;
				double ymin = Float.MAX_VALUE;
				int count = size();
				for(int i = 0; i < count; i++)
				{
					xmin = Math.min(xmin, _xArray[i]);
					ymin = Math.min(ymin, _yArray[i]);
				}
				_minimum.setX(xmin);
				_minimum.setY(ymin);
			}
			return _minimum;
		}
	}

}
