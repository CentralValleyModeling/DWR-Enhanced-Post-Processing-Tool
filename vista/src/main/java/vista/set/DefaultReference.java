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

import vista.time.TimeFactory;
import vista.time.TimeWindow;

/**
 * 
 * @author Nicky Sandhu
 * @version $Id: DefaultReference.java,v 1.5 2000/03/27 18:38:42 nsandhu Exp $
 */
public class DefaultReference extends DataReference {
	/**
   *
   */
	public DefaultReference(DataSet ds) {
		String pathname = null;
		DataSetAttr attr = ds.getAttributes();
		if (ds instanceof RegularTimeSeries) {
			RegularTimeSeries rts = (RegularTimeSeries) ds;
			setName(ds.getName());
			pathname = "/" + attr.getGroupName() + "/" + attr.getLocationName()
					+ "/" + attr.getTypeName() + "/"
					+ rts.getTimeWindow().getStartTime().toString() + "/"
					+ rts.getTimeInterval().toString() + "/"
					+ attr.getSourceName() + "/";
		} else if (ds instanceof IrregularTimeSeries) {
			setName(ds.getName());
			IrregularTimeSeries its = (IrregularTimeSeries) ds;
			pathname = "/" + attr.getGroupName() + "/" + attr.getLocationName()
					+ "/" + attr.getTypeName() + "/"
					+ its.getTimeWindow().getStartTime().toString()
					+ "/IR-YEAR/" + attr.getSourceName() + "/";
		} else if (ds instanceof DefaultDataSet) {
			setName(ds.getName());
			pathname = "/" + attr.getGroupName() + "/" + attr.getLocationName()
					+ "/" + attr.getTypeName() + "///" + attr.getSourceName()
					+ "/";
		} else {
			throw new RuntimeException("Unrecognized type of data set: " + ds);
		}
		init("local", "data.dss", pathname, ds);
	}

	/**
   *
   */
	public DefaultReference(String server, String file, String path, DataSet ds) {
		init(server, file, path, ds);
	}

	/**
   *
   */
	private void init(String server, String file, String path, DataSet ds) {
		setServername(server);
		setFilename(file);
		Pathname pathname = Pathname.createPathname(path);
		setPathname(pathname);
		int dataType = 0;
		String xUnits = "TIME";
		String yUnits = "";
		String xType = "";
		String yType = "";
		if (ds instanceof RegularTimeSeries) {
			RegularTimeSeries rts = (RegularTimeSeries) ds;
			// always set the time interval before setting the timewindow..
			setTimeInterval(rts.getTimeInterval());
			super.setTimeWindow(rts.getTimeWindow());
			dataType = DataType.REGULAR_TIME_SERIES;
			xUnits = ds.getAttributes().getXUnits();
			yUnits = ds.getAttributes().getYUnits();
			xType = ds.getAttributes().getXType();
			yType = ds.getAttributes().getYType();
		} else if (ds instanceof IrregularTimeSeries) {
			IrregularTimeSeries its = (IrregularTimeSeries) ds;
			// always set the time interval before setting the timewindow..
			setTimeInterval(TimeFactory.getInstance().createTimeInterval(
					"1YEAR"));
			super.setTimeWindow(its.getTimeWindow());
			dataType = DataType.IRREGULAR_TIME_SERIES;
			xUnits = ds.getAttributes().getXUnits();
			yUnits = ds.getAttributes().getYUnits();
			xType = ds.getAttributes().getXType();
			yType = ds.getAttributes().getYType();
		} else {
			dataType = DataType.PAIRED;
			xUnits = ds.getAttributes().getXUnits();
			yUnits = ds.getAttributes().getYUnits();
			xType = ds.getAttributes().getXType();
			yType = ds.getAttributes().getYType();
		}
		DataSetAttr attr = new DataSetAttr(pathname.getPart(Pathname.A_PART),
				pathname.getPart(Pathname.B_PART), pathname
						.getPart(Pathname.C_PART), pathname
						.getPart(Pathname.F_PART), dataType, xUnits, yUnits,
				xType, yType);
		_dataSet = ds;
	}

	/**
	 * reloads data
	 */
	public void reloadData() {
	}

	/**
	 * Retrieves data from the data base if data is null. If retrieval fails it
	 * throws a RuntimeException.
	 * 
	 * @return reference to the initialized data set.
	 */
	public DataSet getData() {
		return _dataSet;
	}

	/**
	 * create a clone of itself
	 */
	protected DataReference createClone() {
		return new DefaultReference(getServername(), getFilename(),
				getPathname().toString(), _dataSet);
	}

	/**
	 * checks equivalency of two data references.
	 */
	public boolean isSameAs(DataReference ref) {
		return (ref != null)
				&& ((ref.getTimeWindow() == null && getTimeWindow() == null) || (ref
						.getTimeWindow() != null && ref.getTimeWindow().equals(
						getTimeWindow())))
				&& ((ref.getTimeInterval() == null && getTimeInterval() == null) || (ref
						.getTimeInterval() != null && ref.getTimeInterval()
						.equals(getTimeInterval())))
				&& (ref.getFilename().equals(getFilename()))
				&& (ref.getServername().equals(getServername()))
				&& (ref.getPathname().equals(getPathname()));
	}

	/**
	 * gets the time window for this reference
	 */
	protected void setTimeWindow(TimeWindow tw) {
		super.setTimeWindow(tw);
		if (tw == null)
			return;
		if (!(_dataSet instanceof TimeSeries))
			return;
		if (_dataSet instanceof RegularTimeSeries)
			_dataSet = ((RegularTimeSeries) _dataSet)
					.createSlice(getTimeWindow());
		else if (_dataSet instanceof IrregularTimeSeries)
			_dataSet = ((IrregularTimeSeries) _dataSet)
					.createSlice(getTimeWindow());
	}

	/**
   *
   */
	private DataSet _dataSet;
}
