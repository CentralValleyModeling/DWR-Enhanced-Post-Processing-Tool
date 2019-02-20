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
 * Attributes for the Curve Element
 * 
 * @see Curve
 * @author Nicky Sandhu (DWR).
 * @version $Id: CurveAttr.java,v 1.1 2003/10/02 20:48:51 redwood Exp $
 */
public class CurveAttr extends GEAttr {
	/**
	 * thickness of line in pixels
	 */
	public float _thickness = 1;
	/**
	 * style of line such as plain, dashed, dotted, etcetra.
	 */
	public float[] _dashArray = new float[] { 1 };
	/**
	 * Name of this curve.
	 */
	public String _curveName;
	/**
	 * indicates if symbol is to be drawn for given data points
	 */
	public boolean _drawSymbol = false;
	/**
	 * indicates if data points should be connected.
	 */
	public boolean _drawLines = true;
	/**
	 * number of data points to be skipped before drawing a symbol
	 */
	public int _dataPerSymbol = 5;
	/**
	 * attribute of the symbols for this curve
	 */
	public Symbol _symbol = new Symbol(new SymbolAttr());

	/**
	 * sets DrawSymbol
	 */
	public void setDrawSymbol(boolean drawSymbol) {
		_drawSymbol = drawSymbol;
	}

	/**
	 * gets DrawSymbol
	 */
	public boolean getDrawSymbol() {
		return _drawSymbol;
	}

	/**
	 * sets DrawLines
	 */
	public void setDrawLines(boolean drawLines) {
		_drawLines = drawLines;
	}

	/**
	 * gets DrawLines
	 */
	public boolean getDrawLines() {
		return _drawLines;
	}

	/**
	 * copies the fields into the given GEAttr object. Also copies in the
	 * CurveAttr if the object is of that type.
	 */
	public void copyInto(GEAttr ga) {
		super.copyInto(ga);
		if (ga instanceof CurveAttr) {
			CurveAttr ca = (CurveAttr) ga;
			ca._drawSymbol = this._drawSymbol;
			ca._drawLines = this._drawLines;
		}
	}

}
