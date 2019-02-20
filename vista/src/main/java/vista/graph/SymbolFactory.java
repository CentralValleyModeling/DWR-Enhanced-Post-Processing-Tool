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

import java.awt.Color;
import java.awt.Polygon;
import java.awt.Rectangle;

/**
 * 
 * 
 * @author Nicky Sandhu
 * @version $Id: SymbolFactory.java,v 1.1 2003/10/02 20:49:09 redwood Exp $
 */
public class SymbolFactory {
	/**
   *
   */
	public static Symbol createCircle(boolean fill, Color color, int size) {
		SymbolAttr sa = new SymbolAttr();
		sa._isFilled = fill;
		sa._foregroundColor = color;
		Symbol s = new CircleSymbol(sa);
		s.setBounds(new Rectangle(0, 0, size, size));
		return s;
	}

	/**
	 * polygon based drawing
	 */
	public static Symbol createTriangle(boolean fill, Color color, int size) {
		SymbolAttr sa = new SymbolAttr();
		Symbol s = new Symbol(sa);
		sa._isFilled = fill;
		sa._foregroundColor = color;
		sa.setSymbol(createTriangleShape(size));
		return s;
	}

	/**
	 * polygon based drawing
	 */
	public static Symbol createUprightTriangle(boolean fill, Color color,
			int size) {
		SymbolAttr sa = new SymbolAttr();
		Symbol s = new Symbol(sa);
		sa._isFilled = fill;
		sa._foregroundColor = color;
		sa.setSymbol(createUprightTriangleShape(size));
		return s;
	}

	/**
	 * square symbol
	 */
	public static Symbol createSquare(boolean fill, Color color, int size) {
		SymbolAttr sa = new SymbolAttr();
		Symbol s = new Symbol(sa);
		sa._isFilled = fill;
		sa._foregroundColor = color;
		sa.setSymbol(createSquareShape(size));
		return s;
	}

	/**
	 * cross hair symbol
	 */
	public static Symbol createCross(boolean fill, Color color, int size) {
		SymbolAttr sa = new SymbolAttr();
		Symbol s = new Symbol(sa);
		sa._isFilled = fill;
		sa._foregroundColor = color;
		sa.setSymbol(createCrossShape(size));
		return s;
	}

	/**
	 * cross hair symbol
	 */
	public static Symbol createButterfly(boolean fill, Color color, int size) {
		SymbolAttr sa = new SymbolAttr();
		Symbol s = new Symbol(sa);
		sa._isFilled = fill;
		sa._foregroundColor = color;
		sa.setSymbol(createButterflyShape(size));
		return s;
	}

	/**
	 * cross hair symbol
	 */
	public static Symbol createHourGlass(boolean fill, Color color, int size) {
		SymbolAttr sa = new SymbolAttr();
		Symbol s = new Symbol(sa);
		sa._isFilled = fill;
		sa._foregroundColor = color;
		sa.setSymbol(createHourGlassShape(size));
		return s;
	}

	/**
	 * creates triangle shape
	 */
	static Polygon createUprightTriangleShape(int size) {
		return new Polygon(new int[] { -size, size, 0, -size }, new int[] {
				size, size, -size, size }, 4);
	}

	/**
	 * creates triangle shape
	 */
	static Polygon createTriangleShape(int size) {
		return new Polygon(new int[] { -size, size, 0, -size }, new int[] {
				-size, -size, size, -size }, 4);
	}

	/**
	 * creates triangle shape
	 */
	static Polygon createSlashShape(int size) {
		return new Polygon(new int[] { -size, size, 0 }, new int[] { -size,
				size, 0 }, 3);
	}

	/**
	 * creates triangle shape
	 */
	static Polygon createXShape(int size) {
		return new Polygon(new int[] { -size, size, 0, size, -size, 0 },
				new int[] { -size, size, 0, -size, size, 0 }, 6);
	}

	/**
	 * creates butterfly shape
	 */
	static Polygon createButterflyShape(int size) {
		return new Polygon(new int[] { -size, size, size, -size }, new int[] {
				-size, size, -size, size }, 4);
	}

	/**
	 * creates butterfly shape
	 */
	static Polygon createHourGlassShape(int size) {
		return new Polygon(new int[] { -size, size, -size, size }, new int[] {
				-size, -size, size, size }, 4);
	}

	/**
	 * creates triangle shape
	 */
	static Polygon createSquareShape(int size) {
		return new Polygon(new int[] { -size, size, size, -size, -size },
				new int[] { -size, -size, size, size, -size }, 5);
	}

	/**
	 * creates triangle shape
	 */
	static Polygon createCrossShape(int size) {
		return new Polygon(new int[] { -size, size, 0, 0, 0, 0 }, new int[] {
				0, 0, 0, -size, size, 0 }, 6);
	}
}
