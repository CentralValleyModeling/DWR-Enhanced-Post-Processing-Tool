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

/**
 * 
 * 
 * @author Nicky Sandhu
 * @version $Id: RegressionLine.java,v 1.1 2003/10/02 20:49:30 redwood Exp $
 */
public class RegressionLine {
	/**
   *
   */
	public RegressionLine(double a, double b, double siga, double sigb,
			double chiSq, double rab, double covab) {
		_a = a;
		_b = b;
		_siga = siga;
		_sigb = sigb;
		_chiSq = chiSq;
		_rab = rab;
		_covab = covab;
	}

	/**
   *
   */
	public double getSlope() {
		return _b;
	}

	/**
   *
   */
	public double getIntercept() {
		return _a;
	}

	/**
   *
   */
	public double getCovariance() {
		return _covab;
	}

	/**
   *
   */
	public double getCorelation() {
		return _rab;
	}

	/**
   *
   */
	public double getVarianceA() {
		return _siga;
	}

	/**
   *
   */
	public double getVarianceB() {
		return _sigb;
	}

	/**
   *
   */
	public double getChiSquare() {
		return _chiSq;
	}

	/**
   *
   */
	public DataSet generateDataSet(DataSet ds) {
		DataSetIterator dsi = ds.getIterator();
		double[] ym = new double[ds.size()];
		double[] xm = new double[ds.size()];
		int index = 0;
		while (true) {
			DataSetElement dse = dsi.getElement();
			double x = dse.getX(), y = dse.getY();
			//
			xm[index] = x;
			ym[index] = _a + _b * x;
			if (dsi.atEnd())
				break;
			dsi.advance();
			index++;
		}
		return new DefaultDataSet(ds.getName() + "(Regression Model)", xm, ym,
				null, ds.getAttributes());
	}

	/**
   *
   */
	private double _a, _b, _siga, _sigb, _chiSq, _rab, _covab;
} // endo of class RegressionLine
