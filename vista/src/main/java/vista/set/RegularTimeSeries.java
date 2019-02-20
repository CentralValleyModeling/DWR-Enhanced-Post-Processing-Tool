/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package vista.set;

import org.python.core.Py;
import vista.time.Time;
import vista.time.TimeFactory;
import vista.time.TimeInterval;
import vista.time.TimeWindow;

/**
 * An indexed data set does not need to store all its x values. X values are the
 * indicies for the y values. All that is needed in such a regular indexed array
 * is to store initial and final values of x and the step size.
 *
 * @author Nicky Sandhu (DWR).
 * @version $Id: RegularTimeSeries.java,v 1.1 2003/10/02 20:49:31 redwood Exp $
 * @see DataSetElement
 * @see DataSetIterator
 */
public class RegularTimeSeries extends TimeSeries
{
	public static final int EQ_OP = 1000, NE_OP = 1001, LE_OP = 1002,
			LT_OP = 1003, GE_OP = 1004, GT_OP = 1005;
	private Time _tp = TimeFactory.getInstance().getTimeInstance();
	private TimeFactory _tf = TimeFactory.getInstance();
	private TimeInterval _ti;
	private double _xInitial, _step;
	private Time _stime, _etime;
	private double[] _yArray;
	private int[] _flagArray;
	private String _name;
	private boolean _isFlagged;
	private DataSetAttr _attr;
	private DataSetIterator _dsi;

	/**
	 * @example new RegularTimeSeries("time data", "01JAN1990 0100", "1MON",
	 * yarray);
	 */
	public RegularTimeSeries(String name, String startTime, String interval,
							 double[] y)
	{
		this(name, startTime, interval, y, null, null);
	}

	/**
	 * @example new RegularTimeSeries("time data", "01JAN1990 0100", "1MON",
	 * yarray);
	 */
	public RegularTimeSeries(String name, String startTime, String interval,
							 double[] y, DataSetAttr attr)
	{
		this(name, startTime, interval, y, null, attr);
	}

	/**
	 * @example new RegularTimeSeries("time data", "01JAN1990 0100", "1MON",
	 * yarray);
	 */
	public RegularTimeSeries(String name, String startTime, String interval,
							 double[] y, int[] flags, DataSetAttr attr)
	{
		TimeInterval ti = _tf.createTimeInterval(interval);
		initialize(name, _tf.getTimeInstance().create(startTime), ti, y, flags,
				attr);
	}

	/**
	 *
	 */
	public RegularTimeSeries(String name, Time startTime, TimeInterval ti,
							 double[] y, int[] flags, DataSetAttr attr)
	{
		initialize(name, startTime, ti, y, flags, attr);
	}

	/**
	 *
	 */
	private void initialize(String name, Time startTime, TimeInterval ti,
							double[] y, int[] flags, DataSetAttr attr)
	{

		double xInitial = startTime.getTimeInMinutes();
		if(y == null)
		{
			throw new IllegalArgumentException("y is null");
		}
		Time etime = _tf.createTime(startTime.getTimeInMinutes());
		etime.incrementBy(ti, y.length - 1);
		double xFinal = etime.getTimeInMinutes();

		if(xInitial > xFinal)
		{
			throw new IllegalArgumentException("Start time " + startTime
					+ " cannot be greater than end time " + etime);
		}

		_ti = ti;
		_stime = _tp.create(Math.round(xInitial));
		_etime = _tp.create(Math.round(xFinal));
		_xInitial = xInitial;
		_step = ti.getIntervalInMinutes(_stime);
		int dataCount = (int) (_stime.getExactNumberOfIntervalsTo(_etime, _ti) + 1);
		if(dataCount != y.length)
		{
			throw new IllegalArgumentException("Error: data length : "
					+ dataCount + " and y array length: " + y.length
					+ " don't match");
		}

		if(flags != null)
		{
			if(flags.length != dataCount)
			{
				throw new IllegalArgumentException("Error: flag array of"
						+ " incorrect length");
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
		_yArray = y;
		//
		if(attr == null)
		{
			attr = new DataSetAttr(DataType.REGULAR_TIME_SERIES, "", "", "",
					"INST-VAL");
		}
		_attr = attr;
		//
		setName(name == null ? "" : name);
		//
		_dsi = getIterator();
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
			throw new IndexOutOfBoundsException("No element at index " + i);
		}
		else
		{
			return e.createClone();
		}
	}

	/**
	 * gets element at index i
	 */
	public DataSetElement findElementAt(int i)
	{
		try
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
		catch(IndexOutOfBoundsException e)
		{
			return null;
		}
	}

	/**
	 * gets element at index i
	 */
	public TimeElement getElementAt(String tm)
	{
		TimeElement e = findElementAt(tm);
		if(e == null)
		{
			throw new IllegalArgumentException("No element at time " + tm);
		}
		else
		{
			return e;
		}
	}

	/**
	 * gets element at index i
	 */
	public TimeElement findElementAt(String tm)
	{
		DataSetElement e = null;
		Time t = _stime.create(tm);
		if(_dsi instanceof DefaultIterator)
		{
			((DefaultIterator) _dsi).positionAtTime(_stime.create(tm));
		}
		else
		{
			positionAtTime(_stime.create(tm));
		}
		e = findElementAt(_dsi.getIndex());
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
		return _yArray.length;
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
			_name = "/" + attr.getGroupName() + "/" + attr.getLocationName()
					+ "/" + attr.getTypeName() + "/" + getTimeWindow() + "/"
					+ getTimeInterval() + "/" + attr.getSourceName() + "/";
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
	 * for python len() functionality
	 */
	public int __len__()
	{
		return size();
	}

	/**
	 * for python abs functionality
	 */
	public RegularTimeSeries __abs__()
	{
		RegularTimeSeries rts = (RegularTimeSeries) this.createSlice(this
				.getTimeWindow());
		for(DataSetIterator iter = rts.getIterator(); !iter.atEnd(); iter
				.advance())
		{
			DataSetElement e = iter.getElement();
			double y = e.getY();
			e.setY(Math.abs(y));
			iter.putElement(e);
		}
		return rts;
	}

	/**
	 * for python x[] functionality
	 */
	public DataSetElement __getitem__(int i)
	{
		try
		{
			return getElementAt(i);
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
			if(_dsi instanceof DefaultIterator)
			{
				((DefaultIterator) _dsi).positionAtTime(_stime.create(tm));
			}
			else
			{
				positionAtTime(_stime.create(tm));
			}
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
		try
		{
			_dsi.positionAtIndex(i);
		}
		catch(IndexOutOfBoundsException e)
		{
			throw Py.IndexError("index out of range: " + e.getMessage());
		}
		DataSetElement e = _dsi.getElement();
		e.setY(dse.getY());
		e.setFlag(dse.getFlag());
		_dsi.putElement(e);
	}

	/**
	 * for python
	 */
	public void __setitem__(int i, double d)
	{
		try
		{
			_dsi.positionAtIndex(i);
		}
		catch(IndexOutOfBoundsException e)
		{
			throw Py.KeyError("index out of range: " + e.getMessage());
		}
		DataSetElement e = _dsi.getElement();
		e.setY(d);
		_dsi.putElement(e);
	}

	/**
	 * for python
	 */
	public void __setitem__(String tm, TimeElement dse)
	{
		try
		{
			if(_dsi instanceof DefaultIterator)
			{
				((DefaultIterator) _dsi).positionAtTime(_stime.create(tm));
			}
			else
			{
				positionAtTime(_stime.create(tm));
			}
		}
		catch(IndexOutOfBoundsException e)
		{
			throw Py.IndexError("index out of range: " + e.getMessage());
		}
		catch(IllegalArgumentException e)
		{
			throw Py.KeyError("incorrect time format: " + e.getMessage());
		}
		__setitem__(_dsi.getIndex(), dse);
	}

	/**
	 * for python
	 */
	public void __setitem__(String tm, double d)
	{
		try
		{
			if(_dsi instanceof DefaultIterator)
			{
				((DefaultIterator) _dsi).positionAtTime(_stime.create(tm));
			}
			else
			{
				positionAtTime(_stime.create(tm));
			}
		}
		catch(IndexOutOfBoundsException e)
		{
			throw Py.IndexError("index out of range: " + e.getMessage());
		}
		catch(IllegalArgumentException e)
		{
			throw Py.KeyError("incorrect time format: " + e.getMessage());
		}
		__setitem__(_dsi.getIndex(), d);
	}

	/**
	 * positions iterator at index
	 */
	public void positionAtTime(Time tm)
	{
		if(tm.compare(_etime) > 0)
		{
			while(!_dsi.atEnd())
			{
				_dsi.advance();
			}
			_dsi.retreat();
			throw new IndexOutOfBoundsException("Time : " + tm
					+ " is greater than end time of " + _etime);
		}
		if(tm.compare(_stime) < 0)
		{
			while(!_dsi.atStart())
			{
				_dsi.retreat();
			}
			throw new IndexOutOfBoundsException("Time : " + tm
					+ " is less than start time of " + _stime);
		}
		int i = (int) _stime.getNumberOfIntervalsTo(tm, _ti);
		_dsi.positionAtIndex(i);
		if(_dsi.atEnd())
		{
			_dsi.retreat();
		}
		if(_dsi.getElement().getX() > tm.getTimeInMinutes())
		{
			while(_dsi.getElement().getX() > tm.getTimeInMinutes())
			{
				if(_dsi.atStart())
				{
					break;
				}
				_dsi.retreat();
			}
		}
		else
		{
			while(_dsi.getElement().getX() < tm.getTimeInMinutes())
			{
				_dsi.advance();
				if(_dsi.atEnd())
				{
					break;
				}
			}
		}
		if(_dsi.getElement() == null
				|| _dsi.getElement().getX() != tm.getTimeInMinutes())
		{
			_dsi.positionAtIndex(-1);
		}
	}

	/**
	 * gets the start time
	 */
	public Time getStartTime()
	{
		return _stime;
	}

	/**
	 * gets the end time
	 */
	public Time getEndTime()
	{
		return _etime;
	}

	/**
	 * the time window == start time to end time
	 */
	public TimeWindow getTimeWindow()
	{
		return _tf.createTimeWindow(_stime, _etime);
	}

	/**
	 * the regular sampling time interval
	 */
	public TimeInterval getTimeInterval()
	{
		return _ti;
	}

	/**
	 * the y array of data
	 */
	public double[] getYArray()
	{
		return _yArray;
	}

	/**
	 * the flag array
	 */
	public int[] getFlagArray()
	{
		return _flagArray;
	}

	/**
	 * to string
	 */
	public String toString()
	{
		return "Regular Time Series: " + getName() + " from " + getStartTime()
				+ " to " + getEndTime();
	}

	/**
	 * create slice
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
		twi = TimeFactory.createRoundedTimeWindow(twi, getTimeInterval());
		Time stime = twi.getStartTime();
		try
		{
			positionAtTime(stime);
		}
		catch(IndexOutOfBoundsException e)
		{ // problem with time window
			e.printStackTrace();
			stime = TimeFactory.getInstance().createTime(
					_dsi.getElement().getXString());
			System.err.println(e.getMessage());
		}
		//
		Time etime = twi.getEndTime();
		int nvals = (int) stime.getExactNumberOfIntervalsTo(etime,
				getTimeInterval());
		nvals++; // include end time as well.
		double[] yarray = new double[nvals];
		int[] flags = null;
		if(isFlagged())
		{
			flags = new int[nvals];
		}
		//
		for(int i = 0; i < nvals; i++)
		{
			if(_dsi.atEnd())
			{
				break;
			}
			yarray[i] = _dsi.getElement().getY();
			if(isFlagged())
			{
				flags[i] = _dsi.getElement().getFlag();
			}
			_dsi.advance();
		}
		// create new attribute object
		DataSetAttr attr = getAttributes().createClone();
		//
		return new RegularTimeSeries(getName(), twi.getStartTime(),
				getTimeInterval(), yarray, flags, attr);
	}

	/**
	 * for python
	 */
	public RegularTimeSeries __getslice__(String st, String et)
	{
		return (RegularTimeSeries) createSlice(st, et);
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
	 *
	 */
	public RegularTimeSeries __pow__(double power)
	{
		return (RegularTimeSeries) TimeSeriesMath.pow(this, power);
	}

	/**
	 *
	 */
	public RegularTimeSeries __add__(RegularTimeSeries tids)
	{
		return TimeSeriesMath.doBinaryOperation(this, tids,
				TimeSeriesMath.ADD);
	}

	/**
	 *
	 */
	public RegularTimeSeries __sub__(RegularTimeSeries tids)
	{
		return TimeSeriesMath.doBinaryOperation(this, tids,
				TimeSeriesMath.SUB);
	}

	/**
	 *
	 */
	public RegularTimeSeries __mul__(RegularTimeSeries tids)
	{
		return TimeSeriesMath.doBinaryOperation(this, tids,
				TimeSeriesMath.MUL);
	}

	/**
	 *
	 */
	public RegularTimeSeries __div__(RegularTimeSeries tids)
	{
		return TimeSeriesMath.doBinaryOperation(this, tids,
				TimeSeriesMath.DIV);
	}

	/**
	 *
	 */
	public IrregularTimeSeries __sub__(IrregularTimeSeries tids)
	{
		IrregularTimeSeries its = new IrregularTimeSeries(this);
		return (IrregularTimeSeries) TimeSeriesMath.doBinaryOperation(its,
				tids, TimeSeriesMath.SUB);
	}

	/**
	 *
	 */
	public IrregularTimeSeries __add__(IrregularTimeSeries tids)
	{
		IrregularTimeSeries its = new IrregularTimeSeries(this);
		return (IrregularTimeSeries) TimeSeriesMath.doBinaryOperation(its,
				tids, TimeSeriesMath.ADD);
	}

	/**
	 *
	 */
	public IrregularTimeSeries __mul__(IrregularTimeSeries tids)
	{
		IrregularTimeSeries its = new IrregularTimeSeries(this);
		return (IrregularTimeSeries) TimeSeriesMath.doBinaryOperation(its,
				tids, TimeSeriesMath.MUL);
	}

	/**
	 *
	 */
	public IrregularTimeSeries __div__(IrregularTimeSeries tids)
	{
		IrregularTimeSeries its = new IrregularTimeSeries(this);
		return (IrregularTimeSeries) TimeSeriesMath.doBinaryOperation(its,
				tids, TimeSeriesMath.DIV);
	}

	/**
	 *
	 */
	public IrregularTimeSeries __rsub__(IrregularTimeSeries tids)
	{
		return __sub__(tids);
	}

	/**
	 *
	 */
	public IrregularTimeSeries __radd__(IrregularTimeSeries tids)
	{
		return __add__(tids);
	}

	/**
	 *
	 */
	public IrregularTimeSeries __rmul__(IrregularTimeSeries tids)
	{
		return __mul__(tids);
	}

	/**
	 *
	 */
	public IrregularTimeSeries __rdiv__(IrregularTimeSeries tids)
	{
		return __div__(tids);
	}

	/**
	 *
	 */
	public RegularTimeSeries __add__(double d)
	{
		return (RegularTimeSeries) TimeSeriesMath.doBinaryOperation(this, d,
				TimeSeriesMath.ADD, false);
	}

	/**
	 *
	 */
	public RegularTimeSeries __sub__(double d)
	{
		return (RegularTimeSeries) TimeSeriesMath.doBinaryOperation(this, d,
				TimeSeriesMath.SUB, false);
	}

	/**
	 *
	 */
	public RegularTimeSeries __mul__(double d)
	{
		return (RegularTimeSeries) TimeSeriesMath.doBinaryOperation(this, d,
				TimeSeriesMath.MUL, false);
	}

	/**
	 *
	 */
	public RegularTimeSeries __div__(double d)
	{
		return (RegularTimeSeries) TimeSeriesMath.doBinaryOperation(this, d,
				TimeSeriesMath.DIV, false);
	}

	/**
	 *
	 */
	public RegularTimeSeries __radd__(double d)
	{
		return __add__(d);
	}

	/**
	 *
	 */
	public RegularTimeSeries __rsub__(double d)
	{
		return (RegularTimeSeries) TimeSeriesMath.doBinaryOperation(this, d,
				TimeSeriesMath.SUB, true);
	}

	/**
	 *
	 */
	public RegularTimeSeries __rmul__(double d)
	{
		return __mul__(d);
	}

	/**
	 *
	 */
	public RegularTimeSeries __rdiv__(double d)
	{
		return (RegularTimeSeries) TimeSeriesMath.doBinaryOperation(this, d,
				TimeSeriesMath.DIV, true);
	}

	/**
	 *
	 */
	public RegularTimeSeries __add__(double[] d)
	{
		return (RegularTimeSeries) TimeSeriesMath.doBinaryOperation(this, d,
				TimeSeriesMath.ADD, false);
	}

	/**
	 *
	 */
	public RegularTimeSeries __sub__(double[] d)
	{
		return (RegularTimeSeries) TimeSeriesMath.doBinaryOperation(this, d,
				TimeSeriesMath.SUB, false);
	}

	/**
	 *
	 */
	public RegularTimeSeries __mul__(double[] d)
	{
		return (RegularTimeSeries) TimeSeriesMath.doBinaryOperation(this, d,
				TimeSeriesMath.MUL, false);
	}

	/**
	 *
	 */
	public RegularTimeSeries __div__(double[] d)
	{
		return (RegularTimeSeries) TimeSeriesMath.doBinaryOperation(this, d,
				TimeSeriesMath.DIV, false);
	}

	/**
	 *
	 */
	public RegularTimeSeries __radd__(double[] d)
	{
		return __add__(d);
	}

	/**
	 *
	 */
	public RegularTimeSeries __rsub__(double[] d)
	{
		return (RegularTimeSeries) TimeSeriesMath.doBinaryOperation(this, d,
				TimeSeriesMath.SUB, true);
	}

	/**
	 *
	 */
	public RegularTimeSeries __rmul__(double[] d)
	{
		return __mul__(d);
	}

	/**
	 *
	 */
	public RegularTimeSeries __rdiv__(double[] d)
	{
		return (RegularTimeSeries) TimeSeriesMath.doBinaryOperation(this, d,
				TimeSeriesMath.DIV, true);
	}

	/**
	 *
	 */
	public RegularTimeSeries __lshift__(int s)
	{
		return TimeSeriesMath.createShifted(this, -s);
	}

	/**
	 *
	 */
	public RegularTimeSeries __rshift__(int s)
	{
		return TimeSeriesMath.createShifted(this, s);
	}

	/**
	 *
	 */
	public RegularTimeSeries compare(Object obj, int opId)
	{
		if(!(obj instanceof RegularTimeSeries))
		{
			throw new RuntimeException(
					"Invalid comparision type for regular time series");
		}
		RegularTimeSeries rts = (RegularTimeSeries) obj;
		RegularTimeSeries xrts = (RegularTimeSeries) rts.createSlice(this
				.getTimeWindow());
		DataSetIterator xdsi = xrts.getIterator();
		DataSetIterator dsi = this.getIterator();
		while(!xdsi.atEnd())
		{
			DataSetElement xdse = xdsi.getElement();
			DataSetElement dse = dsi.getElement();
			if(xdse == null || dse == null)
			{
				xdse.setY(Constants.MISSING_VALUE);
			}
			else
			{
				double xy = xdse.getY();
				double y = dse.getY();
				if(opId == EQ_OP)
				{
					xdse.setY(y == xy ? 1 : 0);
				}
				else if(opId == NE_OP)
				{
					xdse.setY(y != xy ? 1 : 0);
				}
				else if(opId == LE_OP)
				{
					xdse.setY(y <= xy ? 1 : 0);
				}
				else if(opId == LT_OP)
				{
					xdse.setY(y < xy ? 1 : 0);
				}
				else if(opId == GE_OP)
				{
					xdse.setY(y >= xy ? 1 : 0);
				}
				else if(opId == GT_OP)
				{
					xdse.setY(y > xy ? 1 : 0);
				}
				else
				{
					throw new RuntimeException("Unknown relational operator: "
							+ opId);
				}
			}
			//
			xdsi.putElement(xdse);
			dsi.advance();
			xdsi.advance();
		}
		return xrts;
	}

	/**
	 *
	 */
	public RegularTimeSeries compare(double x, int opId)
	{
		DataSetIterator dsi = this.getIterator();
		RegularTimeSeries xrts = (RegularTimeSeries) this.createSlice(this
				.getTimeWindow());
		DataSetIterator xdsi = xrts.getIterator();
		DataSetElement dse = null;
		while(!dsi.atEnd())
		{
			dse = dsi.getElement();
			double xy = x;
			double y = dse.getY();
			if(opId == EQ_OP)
			{
				dse.setY(y == xy ? 1 : 0);
			}
			else if(opId == NE_OP)
			{
				dse.setY(y != xy ? 1 : 0);
			}
			else if(opId == LE_OP)
			{
				dse.setY(y <= xy ? 1 : 0);
			}
			else if(opId == LT_OP)
			{
				dse.setY(y < xy ? 1 : 0);
			}
			else if(opId == GE_OP)
			{
				dse.setY(y >= xy ? 1 : 0);
			}
			else if(opId == GT_OP)
			{
				dse.setY(y > xy ? 1 : 0);
			}
			else
			{
				throw new RuntimeException("Unknown relational operator: "
						+ opId);
			}
			//
			xdsi.putElement(dse);
			dsi.advance();
			xdsi.advance();
		}
		return xrts;
	}

	/**
	 *
	 */
	public Object __eq__(Object obj)
	{
		if(obj == null)
		{
			return Boolean.FALSE;
		}
		return this.compare(obj, EQ_OP);
	}

	/**
	 *
	 */
	public Object __ne__(Object obj)
	{
		if(obj == null)
		{
			return Boolean.TRUE;
		}
		return this.compare(obj, NE_OP);
	}

	/**
	 *
	 */
	public RegularTimeSeries __lt__(Object obj)
	{
		return this.compare(obj, LT_OP);
	}

	/**
	 *
	 */
	public RegularTimeSeries __le__(Object obj)
	{
		return this.compare(obj, LE_OP);
	}

	/**
	 *
	 */
	public RegularTimeSeries __gt__(Object obj)
	{
		return this.compare(obj, GT_OP);
	}

	/**
	 *
	 */
	public RegularTimeSeries __ge__(Object obj)
	{
		return this.compare(obj, GE_OP);
	}

	/**
	 *
	 */
	public RegularTimeSeries __eq__(double x)
	{
		return this.compare(x, EQ_OP);
	}

	/**
	 *
	 */
	public RegularTimeSeries __ne__(double x)
	{
		return this.compare(x, NE_OP);
	}

	/**
	 *
	 */
	public RegularTimeSeries __lt__(double x)
	{
		return this.compare(x, LT_OP);
	}

	/**
	 *
	 */
	public RegularTimeSeries __le__(double x)
	{
		return this.compare(x, LE_OP);
	}

	/**
	 *
	 */
	public RegularTimeSeries __gt__(double x)
	{
		return this.compare(x, GT_OP);
	}

	/**
	 *
	 */
	public RegularTimeSeries __ge__(double x)
	{
		return this.compare(x, GE_OP);
	}

	/**
	 *
	 */
	void leftShift(int s)
	{
		_stime.incrementBy(_ti, -s);
		_etime.incrementBy(_ti, -s);
		_dsi = getIterator();
	}

	/**
	 *
	 */
	void rightShift(int s)
	{
		_stime.incrementBy(_ti, s);
		_etime.incrementBy(_ti, s);
		_dsi = getIterator();
	}

	/**
	 * Private iterator class to iterate over the elements of this data set.
	 *
	 * @author Nicky Sandhu
	 * @version $Id: RegularTimeSeries.java,v 1.1 2003/10/02 20:49:31 redwood
	 * Exp $
	 */
	private class DefaultIterator implements TimeSeriesIterator
	{
		/**
		 * the index in the array to the current data, the index the last time
		 * getElement was accessed...
		 */
		private int _index, _lastIndexGot;
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
		private Time _nstime;
		private boolean _isTimeDependent;

		/**
		 * initializes element of correct type
		 */
		public DefaultIterator(RegularTimeSeries ds)
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
			_isTimeDependent = _ti.isTimeContextDependent();
		}

		/**
		 * Resets the iterator to the beginning of data
		 */
		public void resetIterator()
		{
			_nstime = _stime.create(_stime);
			_lastIndexGot = 0;
			_index = 0;
			_firstNextCall = true;
		}

		/**
		 * Returns with the next element and the iterator positioned at the next
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
			if(_isTimeDependent)
			{
				_nstime.incrementBy(_ti, _index - _lastIndexGot);
				_dse.setX(_nstime.getTimeInMinutes());
			}
			else
			{
				_dse.setX(_xInitial + _index * _step);
			}
			_lastIndexGot = _index;
			_dse.setY(_yArray[_index]);
			if(_flagArray != null)
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
			if(atEnd())
			{
				return;
			}
			_yArray[_index] = e.getY();
			if(isFlagged())
			{
				_flagArray[_index] = e.getFlag();
			}
		}

		/**
		 * positions iterator at index
		 */
		public void positionAtIndex(int i)
		{
			if(i < 0 || i > _yArray.length - 1)
			{
				_index = _yArray.length;
				throw new IndexOutOfBoundsException("Index " + i
						+ " out of bounds " + 0 + " to " + (_yArray.length - 1));
			}
			_index = i;
		}

		/**
		 * positions iterator at index
		 */
		public void positionAtTime(Time tm)
		{
			if(tm.compare(_etime) > 0)
			{
				throw new IndexOutOfBoundsException("Time : " + tm
						+ " is greater than end time of " + _etime);
			}
			if(tm.compare(_stime) < 0)
			{
				throw new IndexOutOfBoundsException("Time : " + tm
						+ " is less than start time of " + _stime);
			}
			int i = (int) _stime.getNumberOfIntervalsTo(tm, _ti);
			_index = i;
		}

		/**
		 * Advance by one.
		 */
		public void advance()
		{
			// if ( atStart() ) return;
			_index++;
		}

		/**
		 * Retreat by one
		 */
		public void retreat()
		{
			// if ( atEnd() ) return;
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
			return (_index == _yArray.length);
		}

		/**
		 * The maximum of x and y range encapsulated as a data set element.
		 */
		public DataSetElement getMaximum()
		{
			if(_maximum == null)
			{
				_maximum = new TimeElement();
				_maximum.setX(_etime.getTimeInMinutes());
				double ymax = -Float.MAX_VALUE;
				int count = size();
				for(int i = 0; i < count; i++)
				{
					ymax = Math.max(ymax, _yArray[i]);
				}
				_maximum.setY(ymax);
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
				_minimum.setX(_xInitial);
				double ymin = Float.MAX_VALUE;
				int count = size();
				for(int i = 0; i < count; i++)
				{
					ymin = Math.min(ymin, _yArray[i]);
				}
				_minimum.setY(ymin);
			}
			return _minimum;
		}
	}
}
