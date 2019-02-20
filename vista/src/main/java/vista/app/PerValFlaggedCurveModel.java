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
package vista.app;

import vista.graph.AxisAttr;
import vista.graph.CurveDataModel;
import vista.graph.SimpleTickGenerator;
import vista.graph.TickGenerator;
import vista.graph.TimeTickGenerator;
import vista.set.DataSetElement;
import vista.set.DataSetIterator;
import vista.set.ElementFilter;
import vista.set.ElementFilterIterator;
import vista.set.FlagUtils;
import vista.set.RegularTimeSeries;
import vista.time.Time;
import vista.time.TimeFactory;
import vista.time.TimeInterval;

/**
 * An interface containing the data required for drawing a Curve.
 * 
 * @author Nicky Sandhu
 * @version $Id: PerValFlaggedCurveModel.java,v 1.1 2003/10/02 20:48:38 redwood
 *          Exp $
 */
public class PerValFlaggedCurveModel implements CurveDataModel {
	private Object _obj;
	private RegularTimeSeries _ds;
	private DataSetIterator _dsi;
	private double _xmax, _xmin, _ymax, _ymin;
	private int _xap, _yap;
	private String _legendName;
	private int _interType = -1;
	private TimeInterval _ti;
	private long _interval;
	private boolean _contextDependentTI, _atIntervalStart, _lastQR;
	private ElementFilter _qF = FlagUtils.QUESTIONABLE_FILTER;
	private ElementFilter _rF = FlagUtils.REJECT_FILTER;

	/**
   *
   */
	public PerValFlaggedCurveModel(RegularTimeSeries ds, ElementFilter f,
			int xAxisPosition, int yAxisPosition, String legend) {
		if (ds == null)
			throw new IllegalArgumentException("Null data set");
		_ds = ds;
		if (f == null) {
			_dsi = ds.getIterator();
		} else {
			// _dsi = new ElementFilterCachedIterator(ds.getIterator(), f);
			_dsi = new ElementFilterIterator(ds.getIterator(), f);
		}
		DataSetElement dse = _dsi.getMaximum();
		_xmax = dse.getX();
		_ymax = dse.getY();
		dse = _dsi.getMinimum();
		_xmin = dse.getX();
		_ymin = dse.getY();
		_xap = xAxisPosition;
		_yap = yAxisPosition;
		_legendName = legend;
		_ti = ds.getTimeInterval();
		_contextDependentTI = _ti.isTimeContextDependent();
		if (!_contextDependentTI)
			_interval = _ti.getIntervalInMinutes(null);
		_atIntervalStart = true;
		_lastQR = false;
	}

	/**
   *
   */
	public PerValFlaggedCurveModel(RegularTimeSeries ds, ElementFilter f,
			int xap, int yap) {
		this(ds, f, xap, yap, ds.getName());
	}

	/**
   *
   */
	public PerValFlaggedCurveModel(RegularTimeSeries ds, ElementFilter f) {
		this(ds, f, AxisAttr.BOTTOM, AxisAttr.LEFT, ds.getName());
	}

	/**
	 * sets the maximum value for the current x axis
	 */
	public void setXViewMax(double max) {

	}

	/**
	 * gets the minimum value for the current x axis
	 */
	public void setXViewMin(double min) {

	}

	/**
	 * gets the maximum value for the x axis
	 */
	public double getXMax() {
		return _xmax;
	}

	/**
	 * gets the maximum value for the x axis
	 */
	public double getXMin() {
		return _xmin;
	}

	/**
	 * gets the maximum value for the x axis
	 */
	public double getYMax() {
		return _ymax;
	}

	/**
	 * gets the maximum value for the x axis
	 */
	public double getYMin() {
		return _ymin;
	}

	/**
	 * an object associated with this model.
	 */
	public Object getReferenceObject() {
		return _obj;
	}

	/**
	 * an object associated with this model.
	 */
	public void setReferenceObject(Object obj) {
		_obj = obj;
	}

	/**
	 * gets the interpolation type of the data
	 */
	public int getInterpolationType() {
		return PER_VAL;
	}

	/**
	 * resets the data iterator to beginning of curve
	 */
	public void reset() {
		_dsi.resetIterator();
		_atIntervalStart = true;
	}

	/**
	 * gets the next point
	 * 
	 * @param points
	 *            is an array wher points[0] contains the next x value and
	 *            points[1] contains the next y value
	 * @return an integer specifing movevment only or line drawing motion
	 */
	public int nextPoint(double[] points) {
		int lineType = LINE_TO;
		DataSetElement dse = _dsi.getElement();
		if (!_qF.isAcceptable(dse)) {
			points[0] = dse.getX();
			points[1] = dse.getY();
			lineType = QUESTIONABLE_AT;
			_atIntervalStart = true;
			_lastQR = true;
			_dsi.advance();
		} else if (!_rF.isAcceptable(dse)) {
			points[0] = dse.getX();
			points[1] = dse.getY();
			lineType = REJECT_AT;
			_atIntervalStart = true;
			_lastQR = true;
			_dsi.advance();
		} else if (_atIntervalStart) {
			double val = getBeginningIntervalValue(dse.getX());
			points[0] = val;
			points[1] = dse.getY();
			if (_dsi.atStart() || _dsi.hasSkipped() > 0 || _lastQR) {
				lineType = MOVE_TO;
			} else {
				lineType = LINE_TO;
			}
			_lastQR = false;
			_atIntervalStart = false;
		} else {
			points[0] = dse.getX();
			points[1] = dse.getY();
			lineType = LINE_TO;
			_atIntervalStart = true;
			_dsi.advance();
		}
		return lineType;
	}

	/**
   *
   */
	public double getBeginningIntervalValue(double x) {
		if (_contextDependentTI) {
			Time tm = TimeFactory.getInstance().createTime(Math.round(x));
			tm.incrementBy(_ti, -1);
			return tm.getTimeInMinutes();
		} else {
			return (x - _interval);
		}
	}

	/**
	 * @return true while has more points on curve
	 */
	public boolean hasMorePoints() {
		return !_dsi.atEnd() || !_atIntervalStart;
	}

	/**
	 * gets teh legend text for this curve
	 */
	public String getLegendText() {
		return _legendName;
	}

	/**
	 * get the x axis position for this curve
	 */
	public int getXAxisPosition() {
		return _xap;
	}

	/**
	 * geth the y axis position for this curve
	 */
	public int getYAxisPosition() {
		return _yap;
	}

	/**
	 * get the tick generator for x axis
	 */
	public TickGenerator getXAxisTickGenerator() {
		return new TimeTickGenerator();
	}

	/**
	 * get the tick generator for the y axis
	 */
	public TickGenerator getYAxisTickGenerator() {
		return new SimpleTickGenerator();
	}

	/**
	 * set filter on this model
	 */
	public void setFilter(Object f) {
		if (f == null)
			_dsi = _ds.getIterator();
		else {
			if (f instanceof ElementFilter)
				_dsi = new ElementFilterIterator(_ds.getIterator(),
						(ElementFilter) f);
		}
	}
}
