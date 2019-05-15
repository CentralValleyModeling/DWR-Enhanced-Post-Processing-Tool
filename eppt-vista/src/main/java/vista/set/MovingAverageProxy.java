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
 * A proxy representing a moving average operation on the given reference.
 * 
 * @author Nicky Sandhu
 * @version $Id: MovingAverageProxy.java,v 1.1 2003/10/02 20:49:27 redwood Exp $
 */
public class MovingAverageProxy extends UnaryOperationProxy {
	/**
	 * Creates a proxy which is the moving average of the data contained in the
	 * given reference averaged using backLength previous points, the current
	 * point and forwardLength next points.
	 */
	public MovingAverageProxy(DataReference ref, int backLength,
			int forwardLength) {
		super(ref);
		if (backLength < 0)
			throw new IllegalArgumentException("Illegal back length = "
					+ backLength);
		if (forwardLength < 0)
			throw new IllegalArgumentException("Illegal forward length = "
					+ forwardLength);
		_backLength = backLength;
		_forwardLength = forwardLength;
		if (ref.getData().isFlagged()){
			setFilter(Constants.DEFAULT_FLAG_FILTER);
		} else {
			setFilter(Constants.DEFAULT_FILTER);
		}
		super.checkInput(ref);
		super.initializeAll(ref);
	}

	/**
	 * creates a copy of self
	 */
	public DataReference createClone() {
		return new MovingAverageProxy(getProxyReference(), _backLength,
				_forwardLength);
	}

	/**
	 * returns the name of the proxy
	 */
	protected String getProxyName(DataReference ref) {
		return ref.getName() + getOperationName();
	}

	/**
	 * returns the proxy server name if any
	 */
	protected String getProxyServerName(DataReference ref) {
		return "";
	}

	/**
	 * constructs the filename for the proxy if any
	 */
	protected String getProxyFileName(DataReference ref) {
		return "";
	}

	/**
	 * constructs the pathname for the proxy
	 */
	protected Pathname getProxyPathname(DataReference ref) {
		Pathname path = ref.getPathname();
		String[] parts = new String[Pathname.MAX_PARTS];
		for (int i = 0; i < Pathname.MAX_PARTS; i++) {
			parts[i] = path.getPart(i);
			if (i == Pathname.B_PART)
				parts[i] += "(" + getOperationName() + ")";
			if (i == Pathname.F_PART)
				parts[i] = "(MATH-COMP)";
		}
		return Pathname.createPathname(parts);
	}

	/**
	 * constructs the time window for the proxy
	 */
	protected TimeWindow getProxyTimeWindow(DataReference ref) {
		TimeWindow tw = ref.getTimeWindow();
		return tw.create();
	}

	/**
	 * gets the string representation for the operation name.
	 */
	protected String getOperationName() {
		StringBuffer buf = new StringBuffer(50);
		buf.append("MA[-").append(_backLength).append("],[+").append(
				_forwardLength).append("]");
		return buf.toString();
	}

	/**
	 * constructs the time interval for the proxy
	 */
	protected TimeInterval getProxyTimeInterval(DataReference ref) {
		TimeInterval ti = ref.getTimeInterval();
		return ti.create(ti);
	}

	/**
	 * generates a data set as a function of a data set.
	 */
	protected DataSet doOperation(DataSet ds) {
		DataSetElement dse = null;
		DataSetIterator dsi = ds.getIterator();
		int index = 0, valIndex = -_forwardLength;
		int dsl = ds.size();
		int tl = _backLength + _forwardLength + 1;
		double[] yArray = new double[dsl];
		double[] vals = new double[tl];
		int sincebad = 0;
		int dropindex = 0;
		double total = 0.;
		double newval = 0.;
		double oldval = 0.;
		for (index = 0, valIndex = -_forwardLength, dsi.resetIterator(); !dsi
				.atEnd(); dsi.advance(), index++, valIndex++) {
			if (index >= dsl - _forwardLength)
				yArray[index] = Constants.MISSING_VALUE;
			// get current value at iterator
			dse = dsi.getElement();
			// first and last points cannot be moving averaged
			if (_filter.isAcceptable(dse)) {
				newval = dse.getY();
				oldval = vals[index % tl];
				vals[index % tl] = newval;
				total = total + newval;
				sincebad = sincebad + 1;
			} else {
				vals[index % tl] = 0.;
				total = 0.;
				sincebad = 0;
			}
			if (sincebad > tl) {
				total = total - oldval;
				yArray[valIndex] = total / tl;
			} else {
				if (valIndex >= 0) {
					if (sincebad < tl) {
						yArray[valIndex] = Constants.MISSING_VALUE;
					} else {
						yArray[valIndex] = total / tl;
					}
				}
			}
		}
		// get beginning time
		double xi = 0.;
		dsi.resetIterator();
		xi = dsi.getElement().getX();
		// create and return data set.
		DataSetAttr attr = ds.getAttributes();
		Time stime = TimeFactory.getInstance().createTime(Math.round(xi));
		return new RegularTimeSeries(ds.getName() + getOperationName(), stime,
				getTimeInterval(), yArray, null, attr);
	}

	/**
	 * returns the sum of the
	 */
	private double doAverage(double[] vals, int[] valFlags) {
		double sum = 0.0;
		int count = 0;
		DataSetElement dse = new FlaggedDataSetElement();
		for (int i = 0; i < vals.length; i++) {
			dse.setY(vals[i]);
			dse.setFlag(valFlags[i]);
			if (_filter.isAcceptable(dse)) {
				sum += vals[i];
				count++;
			}
		}
		if (count > (_operationViableThreshold * vals.length))
			sum = sum / count;
		else
			sum = Constants.MISSING_VALUE;
		return sum;
	}

	/**
	 * returns true if period operation can be performed
	 */
	public void setViableThreshold(float threshold) {
		_operationViableThreshold = threshold;
	}

	/**
	 *@param filter
	 *            The filter used to decide acceptable values for operation
	 */
	public void setFilter(ElementFilter filter) {
		_filter = filter;
	}

	/**
   *
   */
	public String toString() {
		StringBuffer buf = new StringBuffer(300);
		buf.append("Moving Average Proxy: ").append("\n");
		buf.append(getServername()).append("::").append(getPathname()).append(
				"::");
		buf.append(getTimeWindow()).append("::").append(getTimeInterval());
		return buf.toString();
	}

	/**
   *
   */
	private float _operationViableThreshold = 0.25f;
	/**
   *
   */
	private ElementFilter _filter = Constants.DEFAULT_FILTER;
	/**
	 * lengths of averaging.
	 */
	private int _backLength, _forwardLength;
}
