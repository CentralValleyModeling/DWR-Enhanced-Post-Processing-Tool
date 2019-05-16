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

import vista.time.Time;
import vista.time.TimeFactory;
import vista.time.TimeInterval;
import vista.time.TimeWindow;

/**
 * A proxy for period operations
 *
 * @author Nicky Sandhu
 * @version $Id: PeriodOperationProxy.java,v 1.1 2003/10/02 20:49:30 redwood Exp
 * $
 */
public abstract class PeriodOperationProxy extends DataReference
{
	/**
	 * data set
	 */
	private transient DataSet _dataSet;
	/**
	 * underlying reference
	 */
	private DataReference _ref;
	/**
	 *
	 */
	private float _operationViableThreshold = 0.25f;
	/**
	 *
	 */
	private ElementFilter _filter = Constants.DEFAULT_FLAG_FILTER;

	/**
	 *
	 */
	public PeriodOperationProxy(DataReference ref, TimeInterval ti)
	{
		//
		if(ref.getTimeInterval() == null)
		{
			throw new IllegalArgumentException("Cannot do period operation on "
					+ ref.getPathname() + " ? empty time interval");
		}
		if(ref.getTimeWindow() == null)
		{
			throw new IllegalArgumentException("Cannot do period operation on "
					+ ref.getPathname() + " ? emtpy time window ");
		}

		if(ref.getTimeInterval().compare(ti) > 0)
		{
			throw new IllegalArgumentException("Cannot do period operation on "
					+ ref.getPathname() + " ? time interval is greater ");
		}

		super.setName(getProxyName(ref, ti));
		super.setServername(getProxyServerName(ref, ti));
		super.setFilename(getProxyFileName(ref, ti));
		super.setPathname(getProxyPathname(ref, ti));
		super.setTimeInterval(getProxyTimeInterval(ref, ti));
		super.setTimeWindow(getProxyTimeWindow(ref, ti));
		_ref = ref;
	}

	/**
	 * returns the value for this period from the values in the array
	 */
	protected abstract double doPeriodOperation(double[] yvals, int nvals);

	/**
	 * returns the flag for the value of this period from the values in the
	 * array.
	 */
	protected abstract int doFlagOperation(int[] flags, int nvals);

	/**
	 *
	 */
	protected abstract String getProxyName(DataReference ref, TimeInterval ti);

	/**
	 *
	 */
	protected abstract String getProxyServerName(DataReference ref,
												 TimeInterval ti);

	/**
	 *
	 */
	protected abstract String getProxyFileName(DataReference ref,
											   TimeInterval ti);

	/**
	 *
	 */
	protected abstract Pathname getProxyPathname(DataReference ref,
												 TimeInterval ti);

	/**
	 *
	 */
	protected abstract TimeWindow getProxyTimeWindow(DataReference ref,
													 TimeInterval ti);

	/**
	 *
	 */
	protected abstract String getOperationName();

	/**
	 *
	 */
	protected TimeInterval getProxyTimeInterval(DataReference ref,
												TimeInterval ti)
	{
		return ti;
	}

	/**
	 * create a clone of itself
	 */
	public abstract DataReference createClone();

	/**
	 * returns true if period operation can be performed
	 */
	public void setViableThreshold(float threshold)
	{
		_operationViableThreshold = threshold;
	}

	/**
	 * @param filter The filter used to decide acceptable values for operation
	 */
	public void setFilter(ElementFilter filter)
	{
		_filter = filter;
	}

	/**
	 *
	 */
	public void reloadData()
	{
		_dataSet = null;
		_ref.reloadData();
	}

	/**
	 * returns data after initialation and operation...
	 */
	public DataSet getData() throws DataRetrievalException
	{
		if(_dataSet == null)
		{
			DataSet d = _ref.getData();
			TimeInterval dsti = _ref.getTimeInterval();
			_dataSet = doPeriodOperation(d, dsti, getTimeInterval());
		}
		return _dataSet;
	}

	/**
	 *
	 */
	private DataSet doPeriodOperation(DataSet ds, TimeInterval dsti,
									  TimeInterval ti)
	{
		// create arrays
		TimeFactory tf = TimeFactory.getInstance();
		Time timeproto = tf.getTimeInstance();
		int maxvals = getMaximumNumberOfValues(dsti, ti);
		double[] yvals = new double[maxvals];
		int[] fvals = null;
		if(!(ds instanceof RegularTimeSeries))
		{
			throw new IllegalArgumentException(
					"Can only do peiod operation on regular time series");
		}
		boolean flagged = ds.isFlagged();
		if(flagged)
		{
			fvals = new int[maxvals];
		}
		double[] yArray = new double[ds.size()];
		int[] flagArray = null;
		if(flagged)
		{
			flagArray = new int[ds.size()];
		}
		// initialize iterators and set start time
		DataSetIterator dsi = ds.getIterator();
		DataSetElement dse = dsi.getElement();
		Time nstime = timeproto.create(timeproto.create(Math.round(dse.getX()))
												.ceiling(ti));
		int yIndex = -1;
		int vIndex = 0;
		Time stime = timeproto.create(Math.round(dse.getX()));
		int nvals = 0;
		// do loop over values
		while(!dsi.atEnd())
		{
			vIndex = 0;
			dse = dsi.getElement();
			stime.incrementBy(dsti, nvals);
			nvals = getNumberOfValues(stime, dsti, ti) + 1;
			// collect values for the given intervals
			for(int i = 0; i < nvals; i++)
			{
				dse = dsi.getElement();
				if(_filter.isAcceptable(dse))
				{
					yvals[vIndex] = dse.getY();
					if(flagged)
					{
						fvals[vIndex] = dse.getFlag();
					}
					vIndex++;
				}
				dsi.advance();
				if(dsi.atEnd())
				{
					break;
				}
			}
			// do required operations on values and store in new array.
			yIndex++;
			if(vIndex > (_operationViableThreshold * maxvals))
			{
				yArray[yIndex] = doPeriodOperation(yvals, vIndex);
				if(flagged)
				{
					flagArray[yIndex] = doFlagOperation(fvals, vIndex);
				}
			}
			else
			{
				yArray[yIndex] = vista.set.Constants.MISSING_VALUE;
				// ??? if values missing set flag to bad value/missing
				// value/questionable value
				// if (ds.isFlagged()) flagArray[yIndex] =
				// doFlagOperation(fvals, vIndex);
			}
		}
		// resize arrays
		double[] tmpArray = new double[yIndex + 1];
		System.arraycopy(yArray, 0, tmpArray, 0, tmpArray.length);
		yArray = tmpArray;
		if(flagged)
		{
			int[] tmp = new int[yIndex + 1];
			System.arraycopy(flagArray, 0, tmp, 0, tmp.length);
			flagArray = tmp;
		}
		// create end time
		Time netime = nstime.create(nstime);
		netime.incrementBy(ti, yIndex);
		// create data set
		String dsname = ds.getName() + getOperationName()
				+ ti.getIntervalAsString();
		DataSetAttr attr = ds.getAttributes();
		attr = new DataSetAttr(attr.getType(), attr.getXUnits(), attr
				.getYUnits(), attr.getXType(), "PER-AVER");
		return new RegularTimeSeries(dsname, nstime, ti, yArray, flagArray,
				attr);
	}

	/**
	 *
	 */
	private int getNumberOfValues(Time tm, TimeInterval oti, TimeInterval nti)
	{
		Time ntm = tm.ceiling(nti);
		return (int) tm.getExactNumberOfIntervalsTo(ntm, oti);
	}

	/**
	 *
	 */
	private int getMaximumNumberOfValues(TimeInterval oti, TimeInterval nti)
	{
		Time timeproto = TimeFactory.getInstance().getTimeInstance();
		Time atime = timeproto.create(0);
		Time etime = timeproto.create(0);
		etime.incrementBy(nti, 1);
		return (int) atime.getExactNumberOfIntervalsTo(etime, oti);
	}

	/**
	 *
	 */
	protected DataReference getUnderlyingReference()
	{
		return _ref;
	}

	/**
	 *
	 */
	protected TimeInterval getPeriodTimeInterval()
	{
		return getTimeInterval();
	}

	/**
	 *
	 */
	protected void setTimeWindow(TimeWindow tw)
	{
		super.setTimeWindow(tw);
		_ref = DataReference.create(_ref, super.getTimeWindow());
	}
}
