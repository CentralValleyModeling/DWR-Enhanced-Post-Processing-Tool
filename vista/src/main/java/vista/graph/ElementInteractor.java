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

import java.awt.event.ComponentEvent;
import java.awt.event.ComponentListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;

/**
 * This class handles interaction of mouse and mouse motion with Element class
 * and its components.
 * 
 * @author Nicky Sandhu
 * @version $Id: ElementInteractor.java,v 1.1 2003/10/02 20:48:54 redwood Exp $
 */
public class ElementInteractor implements MouseListener, MouseMotionListener,
		ComponentListener, KeyListener {
	/**
	 * for debuggin' purposes
	 */
	public static final boolean DEBUG = false;

	/**
	 * Invoked when the mouse has been clicked on a component.
	 */
	public void mouseClicked(MouseEvent e) {
		if (DEBUG)
			System.out.println("Mouse Clicked at ( " + e.getX() + ", "
					+ e.getY() + " )");
	}

	/**
	 * Invoked when a mouse button has been pressed on a component.
	 */
	public void mousePressed(MouseEvent e) {
		if (DEBUG)
			System.out.println("Mouse Pressed at ( " + e.getX() + ", "
					+ e.getY() + " )");
	}

	/**
	 * Invoked when a mouse button has been released on a component.
	 */
	public void mouseReleased(MouseEvent e) {
		if (DEBUG)
			System.out.println("Mouse Released at ( " + e.getX() + ", "
					+ e.getY() + " )");
	}

	/**
	 * Invoked when the mouse enters a component.
	 */
	public void mouseEntered(MouseEvent e) {
		if (DEBUG)
			System.out.println("Mouse Entered at ( " + e.getX() + ", "
					+ e.getY() + " )");
	}

	/**
	 * Invoked when the mouse exits a component.
	 */
	public void mouseExited(MouseEvent e) {
		if (DEBUG)
			System.out.println("Mouse Exited at ( " + e.getX() + ", "
					+ e.getY() + " )");
	}

	/**
	 * Invoked when a mouse button is pressed on a component and then dragged.
	 * Mouse drag events will continue to be delivered to the component where
	 * the first originated until the mouse button is released (regardless of
	 * whether the mouse position is within the bounds of the component).
	 */
	public void mouseDragged(MouseEvent e) {
		if (DEBUG)
			System.out.println("Mouse Dragged at ( " + e.getX() + ", "
					+ e.getY() + " )");
	}

	/**
	 * Invoked when the mouse button has been moved on a component (with no
	 * buttons no down).
	 */
	public void mouseMoved(MouseEvent e) {
		if (DEBUG)
			System.out.println("Mouse Moved at ( " + e.getX() + ", " + e.getY()
					+ " )");
	}

	/**
	 * Invoked when component has been resized.
	 */
	public void componentResized(ComponentEvent e) {
		if (DEBUG)
			System.out.println("Component Event: " + e.paramString());
	}

	/**
	 * Invoked when component has been moved.
	 */
	public void componentMoved(ComponentEvent e) {
		if (DEBUG)
			System.out.println("Component Event: " + e.toString());
	}

	/**
	 * Invoked when component has been shown.
	 */
	public void componentShown(ComponentEvent e) {
		if (DEBUG)
			System.out.println("Component Event: " + e.toString());
	}

	/**
	 * Invoked when component has been hidden.
	 */
	public void componentHidden(ComponentEvent e) {
		if (DEBUG)
			System.out.println("Component Event: " + e.toString());
	}

	/**
	 * Invoked when a key has been typed. This event occurs when a key press is
	 * followed by a key release.
	 */
	public void keyTyped(KeyEvent e) {
		if (DEBUG)
			System.out.println("Key Event: " + e.toString());
	}

	/**
	 * Invoked when a key has been pressed.
	 */
	public void keyPressed(KeyEvent e) {
		if (DEBUG)
			System.out.println("Key Event: " + e.toString());
	}

	/**
	 * Invoked when a key has been released.
	 */
	public void keyReleased(KeyEvent e) {
		if (DEBUG)
			System.out.println("Key Event: " + e.toString());
	}
}
