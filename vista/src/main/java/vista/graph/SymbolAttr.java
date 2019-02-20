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

import java.awt.Polygon;

/**
 * Attributes for a symbol
 * 
 * @author Nicky Sandhu (DWR).
 * @version $Id: SymbolAttr.java,v 1.1 2003/10/02 20:49:09 redwood Exp $
 */
public class SymbolAttr extends LineElementAttr {
	/**
	 * indicates if symbol polygon is filled
	 */
	public boolean _isFilled = false;

	/**
	 * copies the fields into the given GEAttr object. Also copies in the
	 * SymbolAttr if the object is of that type.
	 */
	public void copyInto(GEAttr ga) {
		super.copyInto(ga);
		if (ga instanceof SymbolAttr) {
			SymbolAttr lea = (SymbolAttr) ga;
			lea._isFilled = this._isFilled;
		}
	}

	/**
	 * sets IsFilled
	 */
	public void setIsFilled(boolean isFilled) {
		_isFilled = isFilled;
	}

	/**
	 * gets IsFilled
	 */
	public boolean getIsFilled() {
		return _isFilled;
	}

	/**
	 * The symbol is defined by a polygon. The polygon is defined by a series of
	 * points (x,y). These points are connected in succession to form the
	 * outline of the symbol. These x and y points are such that 0 <= x,y < 25.
	 * In other words an area of 25 X 25 is used for the initial definition.
	 * This would be the size of the bounding box for this polygon.
	 * 
	 * @param xc
	 *            Array of x co-ordinates 0 <= x < 25
	 * @param yc
	 *            Array of y co-ordinates 0 <= y < 25
	 */
	public void setSymbol(int[] xc, int[] yc, int np) {
		_symbolPolygon.xpoints = xc;
		_symbolPolygon.ypoints = yc;
		_symbolPolygon.npoints = np;
	}

	/**
   *
   */
	public void setSymbol(Polygon p) {
		_symbolPolygon = p;
	}

	/**
	 * gets the symbol shape.
	 * 
	 * @return a Polygon object representing the shape
	 */
	public Polygon getSymbol() {
		return _symbolPolygon;
	}

	/**
	 * Polygon for symbol shape.
	 */
	private Polygon _symbolPolygon = new Polygon();
}
