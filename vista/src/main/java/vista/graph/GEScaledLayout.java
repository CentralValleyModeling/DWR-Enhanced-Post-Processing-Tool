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
import java.awt.Rectangle;
import java.util.Enumeration;
import java.util.Vector;

/**
 * This interface provides methods for managing the layouts of BoundedComposite
 * objects which contain other Bounded objects ( which may themselves be
 * Composites). This would be the "Strategy" Pattern. This strategy object
 * encapsulates the know how of laying out composites.
 * <p>
 * 
 * It is the responsibility of classses implementing this interface to query
 * Bounded elements for their preferred sizes and lay them out accordingly. The
 * GEContainer classes use this interface for laying out their children.
 * 
 * @see Bounded
 * @see BoundedComposite
 * @see GEContainer
 * @author Nicky Sandhu (DWR).
 * @version $Id: GEScaledLayout.java,v 1.1 2003/10/02 20:48:58 redwood Exp $
 */
public class GEScaledLayout implements GELayoutManager {
	private Scale _xScale, _yScale;
	private Vector _elements;

	/**
	 * lays out and sizes elements based on scale
	 */
	public GEScaledLayout(Scale xScale, Scale yScale) {
		_xScale = xScale;
		_yScale = yScale;
		_elements = new Vector();
	}

	/**
   *
   */
	public void setXScale(Scale s) {
		_xScale = s;
	}

	/**
   *
   */
	public void setYScale(Scale s) {
		_yScale = s;
	}

	/**
   *
   */
	public Scale getXScale() {
		return _xScale;
	}

	/**
   *
   */
	public Scale getYScale() {
		return _yScale;
	}

	/**
	 * Adds the specified element with the specified name to the layout.
	 * 
	 * @param name
	 *            the element name
	 * @param element
	 *            the element to be added
	 */
	public void addLayoutElement(Object obj, Bounded element) {
		if (obj instanceof DoubleRect) {
			DoubleRect rect = (DoubleRect) obj;
			_elements.addElement(new AnchoredElement(element, rect));
		} else {
			throw new IllegalArgumentException("Cannot add element " + element
					+ "to scaled Layout");
		}
	}

	/**
	 * Removes the specified element from the layout.
	 * 
	 * @param element
	 *            the element ot be removed
	 */
	public void removeLayoutElement(Bounded element) {
		for (Enumeration e = _elements.elements(); e.hasMoreElements();) {
			AnchoredElement ae = (AnchoredElement) e.nextElement();
			if (ae.element.equals(element)) {
				_elements.removeElement(ae);
				break;
			}
		}
	}

	/**
	 * Calculates the preferred size dimensions for the specified panel given
	 * the graphicElements in the specified parent BoundedComposite.
	 * 
	 * @param parent
	 *            the Bounded to be laid out
	 * 
	 * @see #minimumLayoutSize
	 */
	public Dimension preferredLayoutSize(BoundedComposite parent) {
		return new Dimension(100, 100);
	}

	/**
	 * Calculates the minimum size dimensions for the specified panel given the
	 * graphicElements in the specified parent BoundedComposite.
	 * 
	 * @param parent
	 *            the Bounded to be laid out
	 * @see #preferredLayoutSize
	 */
	public Dimension minimumLayoutSize(BoundedComposite parent) {
		return new Dimension(100, 100);
	}

	/**
	 * Lays out the BoundedComposite in the specified panel.
	 * 
	 * @param parent
	 *            the Bounded which needs to be laid out
	 */
	public void layoutContainer(BoundedComposite parent) {
		// set scale to new bounds
		Rectangle bounds = parent.getBounds();
		_xScale.setUCRange(bounds.x, bounds.x + bounds.width);
		_yScale.setUCRange(bounds.y + bounds.height, bounds.y);
		// set the bounds of all the anchored elements
		for (Enumeration e = _elements.elements(); e.hasMoreElements();) {
			AnchoredElement ae = (AnchoredElement) e.nextElement();
			Bounded b = ae.element;
			Rectangle r = b.getBounds();
			DoubleRect dp = ae.rect;
			r.x = _xScale.scaleToUC(dp.x);
			r.y = _yScale.scaleToUC(dp.y);
			if (b instanceof ScaledElement) {
				r.width = _xScale.scaleToUC(dp.width) - _xScale.scaleToUC(0);
				r.height = _yScale.scaleToUC(dp.height) - _yScale.scaleToUC(0);
			}
			b.setBounds(r);
		}
	}
}
