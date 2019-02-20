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

import vista.time.Time;
import vista.time.TimeFactory;
import vista.time.TimeInterval;
import vista.time.TimeWindow;

/**
 * 
 * 
 * @author Nicky Sandhu
 * @version $Id: LRFilledTimeSeriesProxy.java,v 1.1 2003/10/02 20:49:25 redwood
 *          Exp $
 */
public class LRFilledTimeSeriesProxy extends DataReference {
	/**
   *
   */
	protected String getProxyName(DataReference refx, DataReference refy) {
		return refx.getName() + "," + refy.getName() + " | "
				+ getOperationName();
	}

	/**
   *
   */
	protected String getProxyServerName(DataReference refx, DataReference refy) {
		return "";
	}

	/**
   *
   */
	protected String getProxyFileName(DataReference refx, DataReference refy) {
		return "";
	}

	/**
   *
   */
	protected Pathname getProxyPathname(DataReference refx, DataReference refy) {
		String[] parts = new String[Pathname.MAX_PARTS];
		Pathname pathx = refx.getPathname();
		Pathname pathy = refy.getPathname();
		//  
		for (int i = 0; i < parts.length; i++) {
			if (pathx.getPart(i).equals(pathy.getPart(i)))
				parts[i] = pathx.getPart(i);
			else
				parts[i] = pathx.getPart(i) + "," + pathy.getPart(i);
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
			DataReference refy) {
		return refx.getTimeWindow().intersection(refy.getTimeWindow());
	}

	/**
   *
   */
	protected TimeInterval getProxyTimeInterval(DataReference refx,
			DataReference refy) {
		TimeInterval ti = refx.getTimeInterval();
		if (ti.compare(refy.getTimeInterval()) < 0) {
			ti = refy.getTimeInterval();
		}
		if (DEBUG)
			System.out.println("Proxy time interval: " + ti);
		return ti.create(ti);
	}

	/**
   *
   */
	protected String getOperationName() {
		return "LR-FILLED";
	}

	/**
   *
   */
	private void setReferences(DataReference refx, DataReference refy) {
		TimeWindow tw = getTimeWindow();
		// uniformitized time window
		if (refx.getTimeWindow().equals(tw))
			_refx = refx;
		else
			_refx = DataReference.create(refx, tw);
		if (refy.getTimeWindow().equals(tw))
			_refy = refy;
		else
			_refy = DataReference.create(refy, tw);
		// uniformitize time interval
		TimeInterval ti = getTimeInterval();
		if (ti.compare(_refx.getTimeInterval()) != 0)
			_refx = ProxyFactory.createPeriodOperationProxy(
					ProxyFactory.PERIOD_AVERAGE, _refx, ti);
		if (ti.compare(_refy.getTimeInterval()) != 0)
			_refy = ProxyFactory.createPeriodOperationProxy(
					ProxyFactory.PERIOD_AVERAGE, _refy, ti);
	}

	/**
   *
   */
	public LRFilledTimeSeriesProxy(DataReference refx, DataReference refy) {
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
	protected void setTimeWindow(TimeWindow tw) {
		super.setTimeWindow(tw);
		if (_refx != null)
			_refx = DataReference.create(_refx, getTimeWindow());
		if (_refy != null)
			_refy = DataReference.create(_refy, getTimeWindow());
	}

	/**
	 * creates a clone of itself and returns the reference to it. This is used
	 * in creating a clone of itself
	 */
	public DataReference createClone() {
		DataReference ref = new LRFilledTimeSeriesProxy(this._refx, this._refy);
		return ref;
	}

	/**
   *
   */
	public void reloadData() {
		_dataSet = null;
		_refx.reloadData();
		_refy.reloadData();
	}

	/**
   *
   */
	public DataSet getData() throws DataRetrievalException {
		if (_dataSet == null) {
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
			while (!(dsix.atEnd() && dsiy.atEnd())) {
				DataSetElement dsex = dsix.getElement();
				DataSetElement dsey = dsiy.getElement();
				//
				if (_filter.isAcceptable(dsey))
					yArray[index] = dsey.getY();
				else if (!(_filter.isAcceptable(dsey))
						&& _filter.isAcceptable(dsex))
					yArray[index] = dsex.getY() * b + a;
				else
					yArray[index] = dsey.getY();
				//
				dsix.advance();
				dsiy.advance();
				index++;
			}
			if (dsiy.atEnd())
				dsiy.retreat(); // get to the last element..
			double xf = dsiy.getElement().getX();
			if (DEBUG)
				System.out.println(xi + "," + xf);
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
	protected void checkInput(DataReference refx, DataReference refy) {
		// check for non-null
		if (refx == null || refy == null)
			throw new IllegalArgumentException("References for proxy are null");
		// check for being time series
		if (refx.getPathname().getPart(Pathname.E_PART).indexOf("IR-") >= 0)
			throw new IllegalArgumentException("Oops... " + refx
					+ " is irregular.");
		if (refy.getPathname().getPart(Pathname.E_PART).indexOf("IR-") >= 0)
			throw new IllegalArgumentException("Oops... " + refy
					+ " is irregular.");
		// check for non-null common time window
		if (refx.getTimeWindow() == null)
			throw new IllegalArgumentException("Hey! no time window on " + refx);
		if (refx.getTimeInterval() == null)
			throw new IllegalArgumentException("Hey! no time interval on "
					+ refx);
		if (refy.getTimeWindow() == null)
			throw new IllegalArgumentException("Hey! no time window on " + refy);
		if (refy.getTimeInterval() == null)
			throw new IllegalArgumentException("Hey! no time interval on "
					+ refy);
		// check for compatible interval or ability to do so
		if (!(refx.getTimeWindow().intersects(refy.getTimeWindow()))) {
			throw new IllegalArgumentException(refx + " & " + refy
					+ " have no common time window");
		}
	}

	/**
   *
   */
	public String toString() {
		StringBuffer buf = new StringBuffer(300);
		buf.append("Linear Regression Filled TS Proxy: ").append("\n");
		buf.append(getServername()).append("::").append(getPathname()).append(
				"::");
		buf.append(getTimeWindow()).append("::").append(getTimeInterval());
		buf.append("Filling reference: ").append(_refy.toString());
		buf.append(" with ").append(_refx.toString());
		return buf.toString();
	}

	/**
   *
   */
	private transient DataSet _dataSet;
	private DataReference _refx, _refy;
	private static final boolean DEBUG = false;
	private ElementFilter _filter = Constants.DEFAULT_FILTER;
}
