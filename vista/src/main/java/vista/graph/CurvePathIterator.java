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

import java.awt.geom.AffineTransform;
import java.awt.geom.PathIterator;

/**
 * A PathIterator for the Curve to outline its shape. This implementation is to
 * avoid using GeneralPath as that would be very expensive in terms of memory
 * and speed.
 * 
 * @author Nicky Sandhu
 * @version $Id: CurvePathIterator.java,v 1.1 2003/10/02 20:48:53 redwood Exp $
 */
public class CurvePathIterator implements PathIterator {
	/**
	 * initializes with a curve data model and resets itself
	 */
	public CurvePathIterator(CurveDataModel cdm, AffineTransform at) {
		_cdm = cdm;
		_cdm.reset();
		_type = _cdm.nextPoint(_points);
		atStart = true;
		_at = at;
	}

	/**
	 * winding rule set to WIND_EVEN_ODD
	 */
	public int getWindingRule() {
		return PathIterator.WIND_EVEN_ODD;
	}

	/**
   *
   */
	public boolean isDone() {
		return (!_cdm.hasMorePoints());
	}

	/**
   *
   */
	public void next() {
		_type = _cdm.nextPoint(_points);
	}

	/**
   *
   */
	public int currentSegment(float[] coords) {
		double m00 = _at.getScaleX();
		double m02 = _at.getTranslateX();
		double m10 = _at.getScaleY();
		double m12 = _at.getTranslateY();
		coords[0] = (float) Math.round(_points[0] * m00 + m02);
		coords[1] = (float) Math.round(_points[1] * m10 + m12);
		if (atStart) {
			atStart = false;
			return SEG_MOVETO;
		} else {
			if (_type == CurveDataModel.MOVE_TO) {
				return SEG_MOVETO;
			} else {
				return SEG_LINETO;
			}
		}
	}

	/**
   *
   */
	public int currentSegment(double[] coords) {
		if (atStart) {
			coords[0] = _points[0];
			coords[1] = _points[1];
			atStart = false;
			return SEG_MOVETO;
		} else {
			coords[0] = _points[0];
			coords[1] = _points[1];
			if (_type == CurveDataModel.MOVE_TO) {
				return SEG_MOVETO;
			} else {
				return SEG_LINETO;
			}
		}
	}

	/**
   *
   */
	private CurveDataModel _cdm;
	private double[] _points = { 0.0, 0.0 };
	private int _type;
	private boolean atStart;
	private AffineTransform _at;
}
