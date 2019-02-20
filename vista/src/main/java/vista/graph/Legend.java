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
import java.awt.Font;
import java.awt.Rectangle;
import java.util.Enumeration;
import java.util.Properties;

/**
 * This is an aggregate of LegendItem(s). The layout is handled by the
 * GELineLayout
 * 
 * @see LegendItem
 * @see GELineLayout
 * @author Nicky Sandhu
 * @version $Id: Legend.java,v 1.1 2003/10/02 20:49:03 redwood Exp $
 */
public class Legend extends GEContainer implements ScaledElement {
	boolean _ignoreBounds = false;

	/**
	 * constructor
	 */
	public Legend() {
		this(new LegendAttr());
	}

	/**
	 * constructor
	 */
	public Legend(LegendAttr attributes) {
		super(attributes);
		setLayout(new GELineLayout(GELineLayout.VERTICAL,
				GELineLayout.CENTERED_BETWEEN_BOUNDS));
	}

	/**
	 * adds a graphic element to the legend. Only LegendItems are acceptable
	 * additions to this container.
	 * 
	 * @see LegendItem
	 */
	public void add(GraphicElement ge) {
		if (ge instanceof LegendItem) {
			LegendItem li = (LegendItem) ge;
			li.setParent(this);
			LegendItemAttr lia = (LegendItemAttr) li.getAttributes();
			lia._font = ((LegendItemAttr) getAttributes())._font;
			super.add(ge);
			li.setName(li.getLegendName().trim());
		} else {
			super.add(ge);
		}
	}

	/**
	 * draws a rectangle around legend and then draws its components
	 */
	public void Draw() {
		LegendAttr attr = (LegendAttr) getAttributes();
		super.Draw();

		if (attr._boundariesMarked) {
			Rectangle drawingArea = getDrawBounds();
			getGraphics().drawRect(drawingArea.x, drawingArea.y,
					drawingArea.width, drawingArea.height);
		}
	}

	/**
    *
    */
	public void setFontSize(int size) {
		Font font = null;
		for (Enumeration iterator = getIterator(); iterator.hasMoreElements();) {
			GraphicElement ge = (GraphicElement) iterator.nextElement();
			if (ge instanceof LegendItem) {
				LegendItemAttr lia = (LegendItemAttr) ge.getAttributes();
				lia._resizeOnTheFly = false;
				if (font == null) {
					font = lia._font;
					font = new Font(font.getName(), font.getStyle(), size);
				}
				lia._font = font;
			}
		}
	}

	/**
	 * this layout can ignore its size when laying out its children and can lay
	 * them out depending upon their preferred sizes.
	 */
	public void doLayout() {
		if (_ignoreBounds) {
			Rectangle r = getBounds();
			Dimension d = getPreferredSize();
			r.width = d.width;
			r.height = d.height / 2; // the font's return too much space in
										// preferred
			super.doLayout();
		} else {
			super.doLayout();
		}
	}

	/**
	 * Sets font for all the components for which it can be set.
	 */
	public void setFont(Font font) {
		for (Enumeration iterator = getIterator(); iterator.hasMoreElements();) {
			GraphicElement ge = (GraphicElement) iterator.nextElement();
			if (ge instanceof LegendItem) {
				LegendItemAttr lia = (LegendItemAttr) ge.getAttributes();
				lia._font = font;
			}
		}
	}

	/**
	 * Returns its properties in a Properties object. These are saved on disk.
	 * 
	 * @param prefixTag
	 *            A tag to assign the context for these properties e.g. if these
	 *            properties belong to Axis class then prefixTag will be "Axis."
	 */
	public void toProperties(Properties p, String prefixTag) {
		super.toProperties(p, getPrefixTag(prefixTag));
	}

	/**
	 * initializes attributes and state from Properties object.
	 */
	public void fromProperties(Properties p, String prefixTag) {
		super.fromProperties(p, getPrefixTag(prefixTag));
	}

	/**
   *
   */
	public String getPrefixTag(String prefixTag) {
		return prefixTag + getName() + "Legend.";
	}
}
