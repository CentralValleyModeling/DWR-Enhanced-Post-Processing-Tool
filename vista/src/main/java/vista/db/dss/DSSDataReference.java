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
