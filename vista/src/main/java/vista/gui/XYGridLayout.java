/*
    Copyright (C) 1996-2000 State of California, Department of 
    Water Resources.

    VISTA : A VISualization Tool and Analyzer. 
	Version 1.0
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
package vista.gui;

import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.Insets;
import java.awt.LayoutManager;
import java.awt.LayoutManager2;
import java.awt.Rectangle;
import java.util.Hashtable;

/**
 * Defines the interface for classes that know how to layout Containers.
 * 
 * @see Container
 * 
 * @version 1.14, 11/23/96
 * @author Sami Shaio
 * @author Arthur van Hoff
 */
public class XYGridLayout implements LayoutManager2 {
	public int X_GRID_SIZE = 10;
	public int Y_GRID_SIZE = 10;
	private Hashtable _compTable;

	public XYGridLayout(int xsize, int ysize) {
		X_GRID_SIZE = xsize;
		Y_GRID_SIZE = ysize;
		_compTable = new Hashtable();
	}

	/**
	 * Adds the specified component with the specified name to the layout.
	 * 
	 * @param name
	 *            the component name
	 * @param comp
	 *            the component to be added
	 */
	public void addLayoutComponent(String name, Component comp) {
		throw new RuntimeException(
				"I don't use this method for adding components");
	}

	/**
   *
   */
	public void addLayoutComponent(Rectangle rect, Component comp) {
		if (rect.x + rect.width > X_GRID_SIZE)
			throw new RuntimeException("Invalid additon for component " + comp
					+ " @ " + rect);
		if (rect.y + rect.height > Y_GRID_SIZE)
			throw new RuntimeException("Invalid additon for component " + comp
					+ " @ " + rect);
		_compTable.put(comp, rect);
	}

	/**
   *
   */
	public Rectangle getBoundsForComponent(Component comp) {
		return (Rectangle) _compTable.get(comp);
	}

	/**
	 * Removes the specified component from the layout.
	 * 
	 * @param comp
	 *            the component ot be removed
	 */
	public void removeLayoutComponent(Component comp) {
	}

	/**
	 * Calculates the preferred size dimensions for the specified panel given
	 * the components in the specified parent container.
	 * 
	 * @param parent
	 *            the component to be laid out
	 * 
	 * @see #minimumLayoutSize
	 */
	public Dimension preferredLayoutSize(Container parent) {
		Insets insets = parent.getInsets();
		int ncomponents = parent.getComponentCount();
		int width = 0;
		int height = 0;
		for (int i = 0; i < ncomponents; i++) {
			Component comp = parent.getComponent(i);
			Dimension d = comp.getPreferredSize();
			Rectangle rect = getBoundsForComponent(parent.getComponent(i));
			width = (int) Math.max(width, Math.round(1.0 * Y_GRID_SIZE
					/ rect.width * d.width));
			height = (int) Math.max(height, Math.round(1.0 * X_GRID_SIZE
					/ rect.height * d.height));
		}
		return new Dimension(width + insets.left + insets.right, height
				+ insets.top + insets.bottom);
	}

	/**
	 * Calculates the minimum size dimensions for the specified panel given the
	 * components in the specified parent container.
	 * 
	 * @param parent
	 *            the component to be laid out
	 * @see #preferredLayoutSize
	 */
	public Dimension minimumLayoutSize(Container parent) {
		Insets insets = parent.getInsets();
		int ncomponents = parent.getComponentCount();
		int width = 0;
		int height = 0;
		for (int i = 0; i < ncomponents; i++) {
			Component comp = parent.getComponent(i);
			Dimension d = comp.getMinimumSize();
			Rectangle rect = getBoundsForComponent(parent.getComponent(i));
			width = (int) Math.max(width, Math.round(1.0 * Y_GRID_SIZE
					/ rect.width * d.width));
			height = (int) Math.max(height, Math.round(1.0 * X_GRID_SIZE
					/ rect.height * d.height));
		}
		return new Dimension(width + insets.left + insets.right, height
				+ insets.top + insets.bottom);
	}

	/**
	 * Lays out the container in the specified panel.
	 * 
	 * @param parent
	 *            the component which needs to be laid out
	 */
	public void layoutContainer(Container parent) {
		Insets insets = parent.getInsets();
		int ncomponents = parent.getComponentCount();
		if (ncomponents == 0)
			return;
		int pw = parent.getBounds().width - (insets.left + insets.right);
		int ph = parent.getBounds().height - (insets.top + insets.bottom);
		for (int i = 0; i < ncomponents; i++) {
			Rectangle rect = getBoundsForComponent(parent.getComponent(i));
			int x = (int) Math.round((1.0 * rect.x) / X_GRID_SIZE * pw);
			int y = (int) Math.round((1.0 * rect.y) / Y_GRID_SIZE * ph);
			int w = (int) Math.round((1.0 * rect.width) / X_GRID_SIZE * pw);
			int h = (int) Math.round((1.0 * rect.height) / Y_GRID_SIZE * ph);
			parent.getComponent(i).setBounds(x, y, w, h);
		}
	}

	/**
	 * Adds the specified component to the layout, using the specified
	 * constraint object.
	 * 
	 * @param comp
	 *            the component to be added
	 * @param constraints
	 *            where/how the component is added to the layout.
	 */
	public void addLayoutComponent(Component comp, Object constraints) {
		addLayoutComponent((Rectangle) constraints, comp);
	}

	/**
	 * Returns the maximum size of this component.
	 * 
	 * @see java.awt.Component#getMinimumSize()
	 * @see java.awt.Component#getPreferredSize()
	 * @see LayoutManager
	 */
	public Dimension maximumLayoutSize(Container target) {
		return preferredLayoutSize(target);
	}

	/**
	 * Returns the alignment along the x axis. This specifies how the component
	 * would like to be aligned relative to other components. The value should
	 * be a number between 0 and 1 where 0 represents alignment along the
	 * origin, 1 is aligned the furthest away from the origin, 0.5 is centered,
	 * etc.
	 */
	public float getLayoutAlignmentX(Container target) {
		return 0.5f;
	}

	/**
	 * Returns the alignment along the y axis. This specifies how the component
	 * would like to be aligned relative to other components. The value should
	 * be a number between 0 and 1 where 0 represents alignment along the
	 * origin, 1 is aligned the furthest away from the origin, 0.5 is centered,
	 * etc.
	 */
	public float getLayoutAlignmentY(Container target) {
		return 0.5f;
	}

	/**
	 * Invalidates the layout, indicating that if the layout manager has cached
	 * information it should be discarded.
	 */
	public void invalidateLayout(Container target) {
	}

}
