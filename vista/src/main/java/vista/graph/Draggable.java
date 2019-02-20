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
 * This class is useful when a graphic element is being dragged. In such a case
 * the GECanvas will not update in the double buffered mode. However if the
 * element being dragged is replaced by an instance of this class with that
 * element passed in to the construtor the animatenext method of this class will
 * simulate the dragging of that element.
 * 
 * @see Drawable
 * @see Bounded
 * @see GEContainer
 * @see GELayoutManager
 * @see java.awt.Graphics
 * @author Nicky Sandhu
 * @version $Id: Draggable.java,v 1.2 1999/07/08 22:09:13 nsandhu Exp $
 */
public class Draggable extends GraphicElement {
	public Draggable(GraphicElement dragee) {
		_dragee = dragee;
		setParent(dragee.getParent());
		setAttributes(dragee.getAttributes());
		setInsets(dragee.getInsets());
	}

	/**
	 * draws with the same graphics context and rectangle size.
	 */
	protected void Draw() {
		// do nothing
	}

	/**
	 * The update method is called when the animation frame needs to be updated.
	 * Each element is responsible for drawing itself within the bounds given.
	 */
	public void update(AnimationObservable o, Object arg) {
		// do nothing
	}

	/**
    *
    */
	public void setBounds(Rectangle r) {
		super.setBounds(r);
		_dragee.setBounds(r);
	}

	/**
    *
    */
	public void setGraphics(Graphics g) {
		super.setGraphics(g);
		_dragee.setGraphics(g);
	}

	/**
	 * Animates and displays next frame.
	 */
	public void animateNext() {
		if (_dragee != null)
			_dragee.draw();
	}

	/**
	 * returns the graphic element that is being dragged
	 */
	public GraphicElement getDragee() {
		return _dragee;
	}

	/**
	 * same preferred size as dragee
	 */
	public Dimension getPreferredSize() {
		return _dragee.getPreferredSize();
	}

	/**
	 * same preferred size as dragee
	 */
	public Dimension getMinimumSize() {
		return _dragee.getMinimumSize();
	}

	private GraphicElement _dragee;
}
