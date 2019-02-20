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
package vista.app;

import java.awt.Graphics;
import java.awt.Image;
import java.awt.Rectangle;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;

import vista.graph.CoordinateDisplayInteractor;
import vista.graph.Curve;
import vista.graph.GECanvas;
import vista.graph.RangeActor;
import vista.graph.RangeSelected;
import vista.graph.Scale;

/**
 * Selects a range on the x axis of the curve in the graph canvas.
 * 
 * @author Nicky Sandhu
 * @version $Id: RangeSelector.java,v 1.1 2003/10/02 20:48:39 redwood Exp $
 */
public class XRangeSelector implements RangeSelected{
	private Curve _curve;
	private RangeListener rl;
	private GECanvas _gC;
	private double _rmin, _rmax;
	private RangeActor fe;

	/**
	 * selects a range on the x axis of the curve in the graph canvas.
	 */
	public XRangeSelector(RangeActor fe, GECanvas gC, Curve curve) {
		_curve = curve;
		_gC = gC;
		this.fe = fe;
		selectRange();
	}

	/**
    *
    */
	void doneSelecting() {
		fe.selectedRange(rl.getRangeMin(), rl.getRangeMax(), Integer.MIN_VALUE, Integer.MAX_VALUE);
	}

	/**
	 * gets the minimum of range
	 */
	public double getXRangeMin() {
		Scale sc = _curve.getXAxis().getScale();
		_rmin = sc.scaleToDC(rl.getRangeMin());
		return _rmin;
	}

	/**
	 * gets maximum of range
	 */
	public double getXRangeMax() {
		Scale sc = _curve.getXAxis().getScale();
		_rmax = sc.scaleToDC(rl.getRangeMax());
		return _rmax;
	}

	/**
	 * gets the minimum of range on y axis
	 */
	public double getYRangeMin(){
		return _curve.getYAxis().getScale().getDataMinimum();
	}

	/**
	 * gets the maximum of range on y axis
	 */
	public double getYRangeMax(){
		return _curve.getYAxis().getScale().getDataMaximum();
	}

	/**
    *
    */
	private void selectRange() {
		_gC.addMouseListener(rl = new RangeListener(_gC, _curve));
		_gC.addMouseMotionListener(rl);
	}

	/**
    *
    */
	private class RangeListener implements MouseListener, MouseMotionListener {
		private boolean DEBUG = false;
		private boolean click1 = false, click2 = false;
		private Curve _curve;
		private GECanvas _gC;
		private int x1, x2;
		private Image _gCImage;
		private CoordinateDisplayInteractor _cdi;

		/**
      *
      */
		public RangeListener(GECanvas gC, Curve c) {
			_curve = c;
			_gC = gC;
			_gC.addMouseMotionListener(_cdi = new CoordinateDisplayInteractor(
					_gC));
			_gC.setDoubleBuffered(true);
			Rectangle r = _gC.getBounds();
			_gCImage = _gC.createImage(r.width, r.height);
		}

		/**
      *
      */
		public int getRangeMax() {
			return Math.max(x1, x2);
		}

		/**
      *
      */
		public int getRangeMin() {
			return Math.min(x1, x2);
		}

		/**
		 * Invoked when the mouse has been clicked on a component.
		 */
		public void mouseClicked(MouseEvent e) {
			if (DEBUG)
				System.out.println("Mouse Clicked at ( " + e.getX() + ", "
						+ e.getY() + " )");
			Rectangle r = _curve.getBounds();
			if (!click1) {
				click1 = true;
				x1 = Math.min(Math.max(e.getX(), r.x), r.x + r.width);
				if (DEBUG)
					System.out.println("x1 = " + x1);
			} else if (!click2) {
				click2 = true;
				x2 = Math.min(Math.max(e.getX(), r.x), r.x + r.width);
				if (DEBUG)
					System.out.println("x2 = " + x2);
				_gC.removeMouseMotionListener(this);
				_gC.addMouseListener(this);
				_gC.removeMouseMotionListener(_cdi);
				_cdi.doneDisplaying();
				XRangeSelector.this.doneSelecting();
			}
		}

		/**
		 * Invoked when the mouse button has been moved on a component (with no
		 * buttons no down).
		 */
		public void mouseMoved(MouseEvent e) {
			if (DEBUG)
				System.out.println("Mouse Moved at ( " + e.getX() + ", "
						+ e.getY() + " )");
			moveVerticalLineTo(e.getX());
		}

		private void moveVerticalLineTo(int x) {
			Graphics g = _gCImage.getGraphics();
			g.drawImage(_gC.getGraphicElementImage(), 0, 0, null);
			Rectangle r = _curve.getBounds();
			// g.setClip(r);
			if (click1 && click2) {
				g.drawLine(x1, r.y, x1, r.y + r.height);
				g.drawLine(x2, r.y, x2, r.y + r.height);
			} else if (click1) {
				g.drawLine(x1, r.y, x1, r.y + r.height);
				g.drawLine(x, r.y, x, r.y + r.height);
			} else {
				g.drawLine(x, r.y, x, r.y + r.height);
			}
			_gC.getGraphics().drawImage(_gCImage, 0, 0, null);
		}

		public void mousePressed(MouseEvent e) {
			if (DEBUG)
				System.out.println("Mouse Pressed at ( " + e.getX() + ", "
						+ e.getY() + " )");
		}

		public void mouseReleased(MouseEvent e) {
			if (DEBUG)
				System.out.println("Mouse Released at ( " + e.getX() + ", "
						+ e.getY() + " )");
		}

		public void mouseEntered(MouseEvent e) {
			if (DEBUG)
				System.out.println("Mouse Entered at ( " + e.getX() + ", "
						+ e.getY() + " )");
		}

		public void mouseExited(MouseEvent e) {
			if (DEBUG)
				System.out.println("Mouse Exited at ( " + e.getX() + ", "
						+ e.getY() + " )");
		}

		public void mouseDragged(MouseEvent e) {
			if (DEBUG)
				System.out.println("Mouse Dragged at ( " + e.getX() + ", "
						+ e.getY() + " )");
		}
	} // end of Range Listener
}
