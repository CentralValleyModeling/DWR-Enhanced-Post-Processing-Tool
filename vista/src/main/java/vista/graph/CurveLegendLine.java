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
 * A line which represents the curve characterstics.
 * 
 * @author Nicky Sandhu
 * @version $Id: CurveLegendLine.java,v 1.1 2003/10/02 20:48:52 redwood Exp $
 */
public class CurveLegendLine extends GraphicElement {
	/**
	 * A line with the same attributes as the curve
	 */
	public CurveLegendLine(Curve curve) {
		super(curve.getAttributes());
		_curve = curve;
	}

	/**
   *
   */
	public Dimension getPreferredSize() {
		return new Dimension(50, 20);
	}

	/**
   *
   */
	public Dimension getMinimumSize() {
		return getPreferredSize();
	}

	/**
   *
   */
	public void Draw() {
		Graphics gc = getGraphics();
		CurveAttr attr = (CurveAttr) getAttributes();
		Rectangle r = getBounds();
		int yPos = r.y + r.height / 2;
		if (DEBUG)
			System.out.println("Rectangle for legend item: " + r);
		if (attr._thickness > 1)
			GraphUtils.drawThickLine(gc, r.x, yPos, r.x + r.width, yPos,
					attr._thickness);
		else
			gc.drawLine(r.x, yPos, r.x + r.width, yPos);
		if (attr._drawSymbol) {
			Symbol sym = _curve.getSymbol();
			if (sym != null) {
				Rectangle symbolBounds = sym.getBounds();
				if (DEBUG)
					System.out.println("Symbol for legend item: "
							+ symbolBounds);
				symbolBounds.x = r.x + r.width / 2;
				symbolBounds.y = yPos;
				sym.draw(gc, symbolBounds);
				symbolBounds.x = r.x + r.width / 4;
				symbolBounds.y = yPos;
				sym.draw(gc, symbolBounds);
				symbolBounds.x = r.x + (r.width * 3) / 4;
				symbolBounds.y = yPos;
				sym.draw(gc, symbolBounds);
			}
		}
	}

	/**
	 * The curve which is being represented
	 */
	protected Curve _curve;
	/**
   *
   */
	private static final boolean DEBUG = false;
}
