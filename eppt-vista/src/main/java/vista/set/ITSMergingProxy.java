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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;

import vista.time.Time;
import vista.time.TimeInterval;
import vista.time.TimeWindow;

/**
 * This proxy is for merging data references. The time window is the union of
 * the time windows of the given references
 * 
 * @author Nicky Sandhu
 * @version $Id: ITSMergingProxy.java,v 1.1 2003/10/02 20:49:25 redwood Exp $
 */
public class ITSMergingProxy extends DataReference {
	/**
   *
   */
	protected String getProxyName(DataReference[] refs) {
		return refs[0].getName() + " | " + getOperationName();
	}

	/**
   *
   */
	protected String getProxyServerName(DataReference[] refs) {
		return refs[0].getServername();
	}

	/**
   *
   */
	protected String getProxyFileName(DataReference[] refs) {
		return refs[0].getFilename();
	}

	/**
   *
   */
	protected Pathname getProxyPathname(DataReference[] refs) {
		String[] parts = new String[Pathname.MAX_PARTS];
		for (int j = 0; j < refs.length; j++) {
			Pathname path = refs[j].getPathname();
			for (int i = 0; i < parts.length; i++) {
				String part = path.getPart(i);
				if (parts[i] == null)
					parts[i] = part;
				else if (parts[i].indexOf(part) < 0)
					parts[i] += " & " + part;
			}
		}
		parts[Pathname.B_PART] = parts[Pathname.B_PART] + "("
				+ getOperationName() + ")";
		String maxIntervalField = null;
		TimeInterval ti = getTimeInterval();
		for (int i = 8; i > 0; i--) {
			if (ti.getNumberOfIntervals(i) >= 1)
				maxIntervalField = ti.getFieldName(i);
			if (maxIntervalField != null)
				break;
		}
		parts[Pathname.E_PART] = "IR-" + maxIntervalField;
		parts[Pathname.F_PART] = parts[Pathname.F_PART] + "("
				+ getOperationName() + ")";
		return Pathname.createPathname(parts);
	}

	/**
	 * returns a union of all the references time window.
	 */
	protected TimeWindow getProxyTimeWindow(DataReference[] refs) {
		Time stime = refs[0].getTimeWindow().getStartTime(), etime = refs[0]
				.getTimeWindow().getEndTime();
		for (int i = 1; i < refs.length; i++) {
			Time stm = refs[i].getTimeWindow().getStartTime();
			Time etm = refs[i].getTimeWindow().getEndTime();
			if (stime.compare(stm) > 0)
				stime = stm;
			if (etime.compare(etm) < 0)
				etime = etm;
		}
		if (DEBUG)
			System.out.println("Proxy time window : " + stime + " - " + etime);
		return refs[0].getTimeWindow().create(stime, etime);
	}

	/**
   *
   */
	protected TimeInterval getProxyTimeInterval(DataReference[] refs) {
		TimeInterval ti = refs[0].getTimeInterval();
		for (int i = 1; i < refs.length; i++) {
			if (ti.compare(refs[i].getTimeInterval()) < 0) {
				ti = refs[i].getTimeInterval();
			}
		}
		if (DEBUG)
			System.out.println("Proxy time interval: " + ti);
		return ti.create(ti);
	}

	/**
   *
   */
	protected String getOperationName() {
		return "MERGER";
	}

	/**
   *
   */
	protected void checkInput(DataReference[] refs) {
		// check for null input
		if (refs == null)
			throw new IllegalArgumentException("References for proxy are null");
		// check for empty input
		if (refs.length == 0)
			throw new IllegalArgumentException("What? no references for proxy!");
		// check for empty input
		for (int i = 0; i < refs.length; i++) {
			if (refs[i] == null)
				continue; // will discard later...
			// throw new
			// IllegalArgumentException("Hmmm...one of the references for proxy is null");
			// if (refs[i].getPathname().getPart(Pathname.E_PART).indexOf("IR-")
			// >= 0)
			// throw new
			// IllegalArgumentException("Hmmm...one of the references for proxy is irregular");
			if (refs[i].getTimeWindow() == null)
				throw new IllegalArgumentException("Hey! no time window on "
						+ refs[i]);
			if (refs[i].getTimeInterval() == null)
				throw new IllegalArgumentException("Hey! no time interval on "
						+ refs[i]);
		}
		// check on location proximity ?
	}

	/**
   *
   */
	public ITSMergingProxy(DataReference[] refs) {
		checkInput(refs);
		// filter out null references
		_refs = new DataReference[refs.length];
		int k = 0;
		for (int i = 0; i < refs.length; i++) {
			if (refs[i] == null)
				continue;
			_refs[k] = refs[i];
			k++;
		}
		if (k == 0)
			throw new IllegalArgumentException("All data references are null");
		refs = new DataReference[k];
		for (int i = 0; i < k; i++) {
			refs[i] = _refs[i];
		}
		// choose max interval
		super.setTimeInterval(getProxyTimeInterval(refs));
		// set state variables
		super.setName(getProxyName(refs));
		super.setServername(getProxyServerName(refs));
		super.setFilename(getProxyFileName(refs));
		super.setPathname(getProxyPathname(refs));
		super.setTimeWindow(getProxyTimeWindow(refs));
		// handle to references being merged.
		_refs = refs;
		// choose priority ordering from greatest to least
		sort(_refs);
	}

	/**
   *
   */
	protected void setTimeWindow(TimeWindow tw) {
		super.setTimeWindow(tw);
		int index = 0;
		for (int i = 0; i < _refs.length; i++) {
			DataReference ref = DataReference.create(_refs[i], getTimeWindow());
			if (ref == null)
				continue;
			_refs[index] = ref;
			index++;
		}
		DataReference[] refs = new DataReference[index];
		System.arraycopy(_refs, 0, refs, 0, index);
		_refs = refs;
	}

	/**
	 * creates a clone of itself and returns the reference to it. This is used
	 * in creating a clone of itself
	 */
	public DataReference createClone() {
		DataReference ref = new ITSMergingProxy(this._refs);
		return ref;
	}

	private DataReference[] _refs;

	/**
   *
   */
	public void setFilter(ElementFilter f) {
		_filter = f;
	}

	/**
   *
   */
	private ElementFilter _filter = Constants.DEFAULT_FILTER;

	/**
   *
   */
	public void reloadData() {
		_dataSet = null;
		if (_refs != null) {
			for (int i = 0; i < _refs.length; i++) {
				if (_refs[i] != null)
					_refs[i].reloadData();
			}
		}
	}

	/**
   *
   */
	public DataSet getData() throws DataRetrievalException {
		if (_dataSet == null) {
			// merge the references
			_dataSet = getMergedDataSet(_refs);
		}
		return _dataSet;
	}

	private transient DataSet _dataSet = null;

	/**
   *
   */
	private DataSet getMergedDataSet(DataReference[] refs)
			throws DataRetrievalException {
		TimeSeries[] tsArray = new TimeSeries[refs.length];
		for (int i = 0; i < tsArray.length; i++) {
			DataSet ds = refs[i].getData();
			if (ds != null && ds instanceof TimeSeries)
				tsArray[i] = (TimeSeries) ds;
		}
		// get first non-null
		int k = 0;
		while (tsArray[k] == null) {
			k++;
			if (k == tsArray.length)
				return null; // all null sets
		}
		TimeSeries ts = tsArray[k];
		// loop over from the beginning to merge two sets at a time.
		for (int i = 0; i < tsArray.length; i++) {
			TimeSeries tso = tsArray[i];
			if (tso != null) {
				ts = TimeSeriesMath.merge(ts, tso);
			}
		}
		return ts;
	}

	/**
   *
   */
	protected void sort(DataReference[] refs) {
		ArrayList<DataReference> a = new ArrayList<DataReference>(Arrays.asList(refs));
		Collections.sort(a, new MergingOrder());
		DataReference[] nrefs = new DataReference[a.size()];
		nrefs=a.toArray(nrefs);
		System.arraycopy(nrefs, 0, refs, 0, refs.length);
	}

	/**
   *
   */
	private class MergingOrder implements Comparator<DataReference> {
		/**
		 * return true if obj1 comes before obj2 in sort order
		 */
		@Override
		public int compare(DataReference ref1, DataReference ref2) {
			return ref1.getTimeInterval().compare(ref2.getTimeInterval());
		}
	}

	/**
   *
   */
	private static final boolean DEBUG = false;

	/**
   *
   */
	public String toString() {
		StringBuffer buf = new StringBuffer(100 + 150 * _refs.length);
		buf.append("Time Series Merging Proxy: ").append("\n");
		for (int i = 0; i < _refs.length; i++) {
			buf.append("Reference #:").append(i).append(" -> ")
					.append(_refs[i]).append("\n");
		}
		return buf.toString();
	}
}
