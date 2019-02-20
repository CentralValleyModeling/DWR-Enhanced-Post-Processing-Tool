/*
    Copyright (C) 1996, 1997, 1998 State of California, Department of 
    Water Resources.

    VISTA : A VISualization Tool and Analyzer. 
	Version 1.0beta
	by Nicky Sandhu
    California Dept. of Water Resources
    Division of Planning, Delta Modeling Section
    1416 Ninth Street
    Sacramento, CA 95814
    (916)-653-7552
    nsandhu@water.ca.gov

    Send bug reports to nsandhu@water.ca.gov

    This program is licensed to you under the terms of the GNU General
    Public License, version 2, as published by the Free Software
    Foundation.

    You should have received a copy of the GNU General Public License
    along with this program; if not, contact Dr. Francis Chung, below,
    or the Free Software Foundation, 675 Mass Ave, Cambridge, MA
    02139, USA.

    THIS SOFTWARE AND DOCUMENTATION ARE PROVIDED BY THE CALIFORNIA
    DEPARTMENT OF WATER RESOURCES AND CONTRIBUTORS "AS IS" AND ANY
    EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
    IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
    PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE CALIFORNIA
    DEPARTMENT OF WATER RESOURCES OR ITS CONTRIBUTORS BE LIABLE FOR
    ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
    CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT
    OR SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA OR PROFITS; OR
    BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
    LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
    (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE
    USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
    DAMAGE.

    For more information about VISTA, contact:

    Dr. Francis Chung
    California Dept. of Water Resources
    Division of Planning, Delta Modeling Section
    1416 Ninth Street
    Sacramento, CA  95814
    916-653-5601
    chung@water.ca.gov

    or see our home page: http://wwwdelmod.water.ca.gov/

    Send bug reports to nsandhu@water.ca.gov or call (916)-653-7552

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
