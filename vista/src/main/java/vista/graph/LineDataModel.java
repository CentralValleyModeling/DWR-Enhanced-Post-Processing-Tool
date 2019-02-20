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
package vista.graph;

/**
 * An interface containing the data required for drawing a Curve.
 * 
 * @author Nicky Sandhu
 * @version $Id: LineDataModel.java,v 1.1 2003/10/02 20:49:04 redwood Exp $
 */
public class LineDataModel implements CurveDataModel {
	private int _or;
	private CurveDataModel _cdm;
	private double _val;
	private int _index;

	/**
   *
   */
	public LineDataModel(CurveDataModel cdm, double val, int orientation) {
		_cdm = cdm;
		_val = val;
		_or = orientation;
		reset();
	}

	/**
	 * an object associated with this model.
	 */
	public Object getReferenceObject() {
		return null;
	}

	/**
	 * an object associated with this model.
	 */
	public void setReferenceObject(Object obj) {
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
		return _cdm.getXMax();
	}

	/**
	 * gets the maximum value for the x axis
	 */
	public double getXMin() {
		return _cdm.getXMin();
	}

	/**
	 * gets the maximum value for the x axis
	 */
	public double getYMax() {
		return _cdm.getYMax();
	}

	/**
	 * gets the maximum value for the x axis
	 */
	public double getYMin() {
		return _cdm.getYMin();
	}

	/**
	 * gets the interpolation type of the data
	 */
	public int getInterpolationType() {
		return INST_VAL;
	}

	/**
	 * resets the data iterator to beginning of curve
	 */
	public void reset() {
		_index = 0;
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
		int type = MOVE_TO;
		if (_index == 0) {
			if (_or == AxisAttr.HORIZONTAL) {
				points[0] = _cdm.getXMin();
				points[1] = _val;
			} else {
				points[0] = _val;
				points[1] = _cdm.getYMin();
			}
		} else {
			if (_or == AxisAttr.HORIZONTAL) {
				points[0] = _cdm.getXMax();
				points[1] = _val;
			} else {
				points[0] = _val;
				points[1] = _cdm.getYMax();
			}
			type = LINE_TO;
		}
		_index++;
		return type;
	}

	/**
	 * @return true while has more points on curve
	 */
	public boolean hasMorePoints() {
		return _index < 2;
	}

	/**
	 * gets teh legend text for this curve
	 */
	public String getLegendText() {
		return "Line";
	}

	/**
	 * get the x axis position for this curve
	 */
	public int getXAxisPosition() {
		return _cdm.getXAxisPosition();
	}

	/**
	 * geth the y axis position for this curve
	 */
	public int getYAxisPosition() {
		return _cdm.getYAxisPosition();
	}

	/**
	 * get the tick generator for x axis
	 */
	public TickGenerator getXAxisTickGenerator() {
		return _cdm.getXAxisTickGenerator();
	}

	/**
	 * get the tick generator for the y axis
	 */
	public TickGenerator getYAxisTickGenerator() {
		return _cdm.getYAxisTickGenerator();
	}

	/**
	 * sets filter, I don't want to determine the exact class to not allow
	 * dependency between this package and other packages
	 */
	public void setFilter(Object filter) {
	}
}
