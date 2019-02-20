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

import java.util.ArrayList;

/**
 * Mediates information flow between an axis and data sets. This will promote
 * loose coupling by centralizing the relationship between the axis and the data
 * set. The axis should only be concerned with the scaling and not with the data
 * sets which are used to come up with that scaling. And the data sets should
 * only be concerned with the data and not the axis to which they might be
 * attached
 * 
 * @author Nicky Sandhu (DWR).
 * @version $Id: AxisDataSetMediator.java,v 1.1 2003/10/02 20:48:49 redwood Exp
 *          $
 */
public class AxisDataSetMediator {
	/**
	 * Attaches itself to the particular axis.
	 */
	public AxisDataSetMediator(Axis axis) {
		_axis = axis;
		_cdms = new ArrayList<CurveDataModel>();
	}

	/**
   *
   */
	public void attach(CurveDataModel cdm) {
		_cdms.add(cdm);
	}

	/**
   *
   */
	public void detach(CurveDataModel cdm) {
		_cdms.remove(cdm);
	}

	/**
   *
   */
	public void setCurveModelMinMax(Scale sc) {
		double minx = sc.getDataMinimum();
		double maxx = sc.getDataMaximum();
		for (CurveDataModel cdm : _cdms) {
			cdm.setXViewMax(maxx);
			cdm.setXViewMin(minx);
		}
	}

	/**
	 * Detach all data sets
	 */
	public void detachAll() {
		_cdms.clear();
	}

	/**
	 * returns true if atleast one data set is available
	 */
	public boolean hasDataSets() {
		return (_cdms.size() > 0);
	}

	/**
   *
   */
	public int getNumberOfCurves() {
		return _cdms.size();
	}

	/**
	 * returns the minimum of data sets attached to the axis
	 */
	public double getMinimum() {
		double min = Float.MAX_VALUE;
		int or = _axis.getOrientation();

		for (CurveDataModel cdm : _cdms) {
			if (or == AxisAttr.HORIZONTAL) {
				min = Math.min(min, cdm.getXMin());
			} else {
				min = Math.min(min, cdm.getYMin());
			}
		}
		return min;
	}

	/**
	 * returns the maximum of data sets attached to the axis
	 */
	public double getMaximum() {
		double max = -Float.MAX_VALUE;
		int or = _axis.getOrientation();

		for (CurveDataModel cdm : _cdms) {
			if (or == AxisAttr.HORIZONTAL) {
				max = Math.max(max, cdm.getXMax());
			} else {
				max = Math.max(max, cdm.getYMax());
			}
		}
		return max;
	}

	/**
	 * The axis which needs to get the information about these data sets
	 */
	Axis _axis;
	/**
	 * The data sets containing all the data sets attached to the axis
	 */
	private ArrayList<CurveDataModel> _cdms;
	/**
   *
   */
	private static final boolean DEBUG = false;
}
