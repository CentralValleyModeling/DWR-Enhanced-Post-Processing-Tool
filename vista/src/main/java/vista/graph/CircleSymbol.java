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

import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Rectangle;

/**
 * A circle symbol
 * 
 * @author Nicky Sandhu
 * @version $Id: CircleSymbol.java,v 1.1 2003/10/02 20:48:50 redwood Exp $
 */
public class CircleSymbol extends Symbol {
	/**
	 * for debugging
	 */
	public static final boolean DEBUG = false;

	/**
	 * Constructor.
	 */
	public CircleSymbol(SymbolAttr attributes) {
		super(attributes);
		setName("CircleSymbol");
	}

	/**
	 * true if x,y hits the drawing. More precise than contains(x,y)
	 */
	public boolean hitsDrawing(int x, int y) {
		Rectangle r = getInsetedBounds();
		int wd = r.width / 4;
		int ht = r.height / 4;
		int radius = Math.min(wd, ht);
		int xc = r.x - radius / 2;
		int yc = r.y - radius / 2;
		return Math.sqrt((x - r.x) * (x - r.x) + (y - r.y) * (y - r.y)) <= radius / 2;
	}

	/**
	 * draws polygon representing symbol. Currently for efficiency no scaling is
	 * done. However a subclass will do so. In this way it would be the users
	 * choice to use the most suitable class.
	 */
	public void Draw() {
		SymbolAttr attr = (SymbolAttr) getAttributes();
		Graphics gc = getGraphics();

		Rectangle r = getInsetedBounds();
		int wd = r.width / 4;
		int ht = r.height / 4;
		int radius = Math.min(wd, ht);
		int x = r.x - radius / 2;
		int y = r.y - radius / 2;
		if (attr._isFilled)
			gc.fillArc(x, y, radius, radius, 0, 360);
		else
			gc.drawArc(x, y, radius, radius, 0, 360);
	}

	/**
	 * calculates the preferred size of this element
	 * 
	 * @return The preferred size
	 */
	public Dimension getPreferredSize() {
		return new Dimension(25, 25);
	}

	/**
	 * calculates the minimum size of this element
	 * 
	 * @return The minimum size
	 */
	public Dimension getMinimumSize() {
		return getPreferredSize();
	}

	/**
   *
   */
	public String getPrefixTag(String prefixTag) {
		return prefixTag + getName() + ".";
	}
}
