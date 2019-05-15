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

import vista.time.TimeInterval;
import vista.time.TimeWindow;

/**
 * A proxy which results from operations on a single reference.
 * 
 * @author Nicky Sandhu
 * @version $Id: UnaryOperationProxy.java,v 1.1 2003/10/02 20:49:34 redwood Exp
 *          $
 */
public abstract class UnaryOperationProxy extends DataReference {
	/**
	 * returns the name of the proxy
	 */
	protected abstract String getProxyName(DataReference ref);

	/**
	 * returns the proxy server name if any
	 */
	protected abstract String getProxyServerName(DataReference ref);

	/**
	 * constructs the filename for the proxy if any
	 */
	protected abstract String getProxyFileName(DataReference ref);

	/**
	 * constructs the pathname for the proxy
	 */
	protected abstract Pathname getProxyPathname(DataReference ref);

	/**
	 * constructs the time window for the proxy
	 */
	protected abstract TimeWindow getProxyTimeWindow(DataReference ref);

	/**
	 * gets the string representation for the operation name.
	 */
	protected abstract String getOperationName();

	/**
	 * constructs the time interval for the proxy
	 */
	protected abstract TimeInterval getProxyTimeInterval(DataReference ref);

	/**
	 * sets the proxy reference
	 */
	protected void setProxyReference(DataReference ref) {
		TimeWindow tw = getTimeWindow();
		if (tw.equals(ref.getTimeWindow()))
			_ref = ref;
		else
			_ref = DataReference.create(ref, tw);

	}

	/**
	 * checks input reference for satisfying pre-conditions
	 */
	protected void checkInput(DataReference ref) {
		if (ref == null)
			throw new IllegalArgumentException("? Reference is null");
		TimeWindow tw = ref.getTimeWindow();
		if (tw == null)
			throw new IllegalArgumentException("? Time Window is null");
		if (ref.getTimeInterval() == null)
			throw new IllegalArgumentException("? Time Interval is null");
	}

	/**
	 * intializes a proxy from given names
	 */
	public void initializeAll(DataReference ref) {
		// choose time interval
		super.setTimeInterval(getProxyTimeInterval(ref));
		super.setName(getProxyName(ref));
		super.setServername(getProxyServerName(ref));
		super.setFilename(getProxyFileName(ref));
		super.setPathname(getProxyPathname(ref));
		super.setTimeWindow(getProxyTimeWindow(ref));
		// set reference
		setProxyReference(ref);
	}

	/**
   *
   */
	protected UnaryOperationProxy() {
	}

	/**
   *
   */
	public UnaryOperationProxy(DataReference ref) {
		checkInput(ref);
		initializeAll(ref);
	}

	/**
	 * sets the time window of the proxy and its referencing proxies as well.
	 */
	protected void setTimeWindow(TimeWindow tw) {
		super.setTimeWindow(tw);
		_ref = DataReference.create(_ref, getTimeWindow());
	}

	/**
   *
   */
	public void reloadData() {
		_dataSet = null;
		_ref.reloadData();
	}

	/**
	 * returns data after initialation and operation...
	 */
	public DataSet getData() throws DataRetrievalException {
		if (_dataSet == null) {
			DataSet ds = _ref.getData();
			_dataSet = doOperation(ds);
		}
		return _dataSet;
	}

	/**
	 * generates a data set as a function of a data set.
	 */
	protected abstract DataSet doOperation(DataSet ds);

	/**
   *
   */
	protected DataReference getProxyReference() {
		return _ref;
	}

	/**
	 * the data set after it is initialized
	 */
	private transient DataSet _dataSet;
	/**
	 * reference from which generated
	 */
	private DataReference _ref;
}
