/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package vista.db.dss;

import java.lang.ref.SoftReference;

import vista.set.DataReference;
import vista.set.DataRetrievalException;
import vista.set.DataSet;
import vista.set.Pathname;
import vista.set.RegularTimeSeries;
import vista.set.TimeSeries;
import vista.time.TimeInterval;
import vista.time.TimeWindow;

/**
 * An implementation of a data reference for dss data sets. This class specifies
 * the creation mechanism for this kind of data reference and the retrieval
 * mechanism for the data.
 * 
 * @author Nicky Sandhu
 * @version $Id: DSSDataReference.java,v 1.1 2003/10/02 20:48:44 redwood Exp $
 */
class DSSDataReference extends DataReference {
	protected DSSDataReference(String server, String file, Pathname path,
			DataSet ds) {
		if (file == null)
			throw new NullPointerException("File is null");
		if (path == null)
			throw new NullPointerException("Path is null");
		TimeInterval ti = null;
		TimeWindow tw = null;
		if (ds instanceof TimeSeries) {
			TimeSeries ts = (TimeSeries) ds;
			if (ts instanceof RegularTimeSeries) {
				ti = ((RegularTimeSeries) ts).getTimeInterval();
				path.setPart(Pathname.E_PART, ti.toString());
			}
			tw = ts.getTimeWindow();
			// path.setPart(Pathname.D_PART,tw.getStartTime().toString());
			path.setPart(Pathname.D_PART, DSSUtil.getBlockStart(ts));
		} else {
			path.setPart(Pathname.D_PART, "");
			path.setPart(Pathname.E_PART, "");
		}
		setServername(server);
		setFilename(file);
		setPathname(path);
		setTimeInterval(ti);
		setTimeWindow(tw);
		setData(ds);
	}

	/**
	 * create a reference using the given reference as the prototype.
	 */
	protected DSSDataReference(String server, String file, Pathname path) {
		if (file == null)
			throw new NullPointerException("File is null");
		if (path == null)
			throw new NullPointerException("Path is null");
		TimeInterval ti = DSSUtil.createTimeInterval(path);
		TimeWindow tw = DSSUtil.createTimeWindow(path);
		String dpart = path.getPart(Pathname.D_PART);
		if (dpart.indexOf("-") > 0)
			path.setPart(Pathname.D_PART, dpart
					.substring(0, dpart.indexOf("-")).trim());
		setServername(server);
		setFilename(file);
		setPathname(path);
		setTimeInterval(ti);
		setTimeWindow(tw);
		// setName( server+SEPARATOR+file+SEPARATOR+path+SEPARATOR+tw );
	}

	/**
	 * constructor for use only in session builder
	 */
	protected DSSDataReference() {
	}

	/**
	 * creates a clone of itself and returns the reference to it. This is used
	 * in creating a clone of itself
	 */
	public DataReference createClone() {
		DSSDataReference ref = new DSSDataReference();
		ref.setServername(this.getServername());
		ref.setFilename(this.getFilename());
		ref.setPathname(this.getPathname());
		ref.setTimeInterval(this.getTimeInterval());
		ref.setTimeWindow(this.getTimeWindow());
		ref.setLocation(this.getLocation());
		return ref;
	}

	/**
   *
   */
	public void reloadData() {
		if (dataset != null) {
			dataset.clear();
			dataset = null;
		}
		getData();
	}

	/**
	 * Retrieves dss data as a data set.
	 */
	public DataSet getData() {
		if (dataset == null || dataset.get() == null) {
			DataSet data = null;
			try {
				DSSRemoteClient client = DSSUtil.createRemoteClient(
						getServername(), -1);
				boolean retrieveFlags = DSSUtil.areFlagsRetrieved();
				data = client.getData(this, retrieveFlags);
				if (data instanceof TimeSeries)
					setTimeWindow(((TimeSeries) data).getTimeWindow());
			} catch (Exception re) {
				re.printStackTrace();
				throw new DataRetrievalException(re.getMessage());
			}
			dataset = new SoftReference<DataSet>(data);
		}
		return dataset.get();
	}

	/**
	 * overrides getTimeWindow
	 */
	public TimeWindow getTimeWindow() {
		return _dtw;
	}

	/**
	 * gets the time window for this reference
	 */
	protected void setTimeWindow(TimeWindow tw) {
		_dtw = tw;
		DataSet data = null;
		if (dataset != null) {
			data = dataset.get();
		}
		if (data instanceof TimeSeries) {
			TimeSeries ts = (TimeSeries) data;
			if (!ts.getTimeWindow().isSameAs(tw)) {
				setData(ts.createSlice(tw));
			}
		} else {
		}
	}

	private void setData(DataSet data) {
		if (dataset != null) {
			dataset.clear();
		}
		dataset = new SoftReference<DataSet>(data);
	}

	/**
	 * gets the name for this reference along with associated pathname.
	 */
	public String getName() {
		if (super.getName() != null)
			return super.getName();
		StringBuffer buf = new StringBuffer(1000).append(getServername())
				.append(SEPARATOR).append(getFilename()).append(SEPARATOR)
				.append(getPathname()).append(SEPARATOR)
				.append(getTimeWindow());
		return buf.toString();
	}

	/**
	 * string representation of this data reference.
	 */
	public String toString() {
		String p = getPathname().toString();
		StringBuffer buf = new StringBuffer(p.length() + getName().length()
				+ 20);
		return buf.append(getName()).toString();
	}

	/**
	 * The data set contained by this reference. Don't save data on serializing
	 */
	private transient SoftReference<DataSet> dataset;
	/**
	 * separator for name
	 */
	static final String SEPARATOR = "::";
	private TimeWindow _dtw;
}
