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
 * @author Nicky Sandhu
 * @version $Id: LRFilledTimeSeriesProxy.java,v 1.1 2003/10/02 20:49:25 redwood
 * Exp $
 */
public class LRFilledTimeSeriesProxy extends DataReference
{
	private static final boolean DEBUG = false;
	/**
	 *
	 */
	private transient DataSet _dataSet;
	private DataReference _refx, _refy;
	private ElementFilter _filter = Constants.DEFAULT_FILTER;

	/**
	 *
	 */
	public LRFilledTimeSeriesProxy(DataReference refx, DataReference refy)
	{
		// check nput
		checkInput(refx, refy);
		// set server, file, pathname, time window, time interval
		// choose max interval
		setTimeInterval(getProxyTimeInterval(refx, refy));
		setName(getProxyName(refx, refy));
		setServername(getProxyServerName(refx, refy));
		setFilename(getProxyFileName(refx, refy));
		setPathname(getProxyPathname(refx, refy));
		setTimeWindow(getProxyTimeWindow(refx, refy));
		// create references with correct time window and time interval
		setReferences(refx, refy);
	}

	/**
	 *
	 */
	protected String getProxyName(DataReference refx, DataReference refy)
	{
		return refx.getName() + "," + refy.getName() + " | "
				+ getOperationName();
	}

	/**
	 *
	 */
	protected String getProxyServerName(DataReference refx, DataReference refy)
	{
		return "";
	}

	/**
	 *
	 */
	protected String getProxyFileName(DataReference refx, DataReference refy)
	{
		return "";
	}

	/**
	 *
	 */
	protected Pathname getProxyPathname(DataReference refx, DataReference refy)
	{
		String[] parts = new String[Pathname.MAX_PARTS];
		Pathname pathx = refx.getPathname();
		Pathname pathy = refy.getPathname();
		//
		for(int i = 0; i < parts.length; i++)
		{
			if(pathx.getPart(i).equals(pathy.getPart(i)))
			{
				parts[i] = pathx.getPart(i);
			}
			else
			{
				parts[i] = pathx.getPart(i) + "," + pathy.getPart(i);
			}
		}
		parts[Pathname.B_PART] = pathx.getPart(Pathname.B_PART) + "("
				+ getOperationName() + ")" + " by "
				+ pathy.getPart(Pathname.B_PART);
		parts[Pathname.F_PART] = parts[Pathname.F_PART] + "("
				+ getOperationName() + ")";
		return Pathname.createPathname(parts);
	}

	/**
	 * returns a union of all the references time window.
	 */
	protected TimeWindow getProxyTimeWindow(DataReference refx,
											DataReference refy)
	{
		return refx.getTimeWindow().intersection(refy.getTimeWindow());
	}

	/**
	 *
	 */
	protected TimeInterval getProxyTimeInterval(DataReference refx,
												DataReference refy)
	{
		TimeInterval ti = refx.getTimeInterval();
		if(ti.compare(refy.getTimeInterval()) < 0)
		{
			ti = refy.getTimeInterval();
		}
		if(DEBUG)
		{
			System.out.println("Proxy time interval: " + ti);
		}
		return ti.create(ti);
	}

	/**
	 *
	 */
	protected String getOperationName()
	{
		return "LR-FILLED";
	}

	/**
	 *
	 */
	private void setReferences(DataReference refx, DataReference refy)
	{
		TimeWindow tw = getTimeWindow();
		// uniformitized time window
		if(refx.getTimeWindow().equals(tw))
		{
			_refx = refx;
		}
		else
		{
			_refx = DataReference.create(refx, tw);
		}
		if(refy.getTimeWindow().equals(tw))
		{
			_refy = refy;
		}
		else
		{
			_refy = DataReference.create(refy, tw);
		}
		// uniformitize time interval
		TimeInterval ti = getTimeInterval();
		if(ti.compare(_refx.getTimeInterval()) != 0)
		{
			_refx = ProxyFactory.createPeriodOperationProxy(
					ProxyFactory.PERIOD_AVERAGE, _refx, ti);
		}
		if(ti.compare(_refy.getTimeInterval()) != 0)
		{
			_refy = ProxyFactory.createPeriodOperationProxy(
					ProxyFactory.PERIOD_AVERAGE, _refy, ti);
		}
	}

	/**
	 *
	 */
	protected void setTimeWindow(TimeWindow tw)
	{
		super.setTimeWindow(tw);
		if(_refx != null)
		{
			_refx = DataReference.create(_refx, getTimeWindow());
		}
		if(_refy != null)
		{
			_refy = DataReference.create(_refy, getTimeWindow());
		}
	}

	/**
	 * creates a clone of itself and returns the reference to it. This is used
	 * in creating a clone of itself
	 */
	public DataReference createClone()
	{
		DataReference ref = new LRFilledTimeSeriesProxy(this._refx, this._refy);
		return ref;
	}

	/**
	 *
	 */
	public void reloadData()
	{
		_dataSet = null;
		_refx.reloadData();
		_refy.reloadData();
	}

	/**
	 *
	 */
	public DataSet getData() throws DataRetrievalException
	{
		if(_dataSet == null)
		{
			DataReference refxy = ProxyFactory.createPairedTimeSeriesProxy(
					_refx, _refy);
			DataSet dsx = _refx.getData();
			DataSet dsy = _refy.getData();
			RegressionLine rl = Utils.linearLSRegression(refxy.getData());
			double a = rl.getIntercept();
			double b = rl.getSlope();
			DataSetIterator dsix = dsx.getIterator(), dsiy = dsy.getIterator();
			double[] yArray = new double[dsy.size()];
			int index = 0;
			double xi = dsiy.getElement().getX();
			while(!(dsix.atEnd() && dsiy.atEnd()))
			{
				DataSetElement dsex = dsix.getElement();
				DataSetElement dsey = dsiy.getElement();
				//
				if(_filter.isAcceptable(dsey))
				{
					yArray[index] = dsey.getY();
				}
				else if(!(_filter.isAcceptable(dsey))
						&& _filter.isAcceptable(dsex))
				{
					yArray[index] = dsex.getY() * b + a;
				}
				else
				{
					yArray[index] = dsey.getY();
				}
				//
				dsix.advance();
				dsiy.advance();
				index++;
			}
			if(dsiy.atEnd())
			{
				dsiy.retreat(); // get to the last element..
			}
			double xf = dsiy.getElement().getX();
			if(DEBUG)
			{
				System.out.println(xi + "," + xf);
			}
			Time stime = TimeFactory.getInstance().createTime(Math.round(xi));
			_dataSet = new RegularTimeSeries("LR-Filled: (" + dsy.getName()
					+ " with" + dsx.getName() + ")", stime, getTimeInterval(),
					yArray, null, dsy.getAttributes());
		}
		return _dataSet;
	}

	/**
	 *
	 */
	protected void checkInput(DataReference refx, DataReference refy)
	{
		// check for non-null
		if(refx == null || refy == null)
		{
			throw new IllegalArgumentException("References for proxy are null");
		}
		// check for being time series
		if(refx.getPathname().getPart(Pathname.E_PART).indexOf("IR-") >= 0)
		{
			throw new IllegalArgumentException("Oops... " + refx
					+ " is irregular.");
		}
		if(refy.getPathname().getPart(Pathname.E_PART).indexOf("IR-") >= 0)
		{
			throw new IllegalArgumentException("Oops... " + refy
					+ " is irregular.");
		}
		// check for non-null common time window
		if(refx.getTimeWindow() == null)
		{
			throw new IllegalArgumentException("Hey! no time window on " + refx);
		}
		if(refx.getTimeInterval() == null)
		{
			throw new IllegalArgumentException("Hey! no time interval on "
					+ refx);
		}
		if(refy.getTimeWindow() == null)
		{
			throw new IllegalArgumentException("Hey! no time window on " + refy);
		}
		if(refy.getTimeInterval() == null)
		{
			throw new IllegalArgumentException("Hey! no time interval on "
					+ refy);
		}
		// check for compatible interval or ability to do so
		if(!(refx.getTimeWindow().intersects(refy.getTimeWindow())))
		{
			throw new IllegalArgumentException(refx + " & " + refy
					+ " have no common time window");
		}
	}

	/**
	 *
	 */
	public String toString()
	{
		StringBuffer buf = new StringBuffer(300);
		buf.append("Linear Regression Filled TS Proxy: ").append("\n");
		buf.append(getServername()).append("::").append(getPathname()).append(
				"::");
		buf.append(getTimeWindow()).append("::").append(getTimeInterval());
		buf.append("Filling reference: ").append(_refy.toString());
		buf.append(" with ").append(_refx.toString());
		return buf.toString();
	}
}
