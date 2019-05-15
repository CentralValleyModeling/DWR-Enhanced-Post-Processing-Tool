/*
 * Enhanced Post Processing Tool (EPPT) Copyright (c) 2019.
 *
 * EPPT is copyrighted by the State of California, Department of Water Resources. It is licensed
 * under the GNU General Public License, version 2. This means it can be
 * copied, distributed, and modified freely, but you may not restrict others
 * in their ability to copy, distribute, and modify it. See the license below
 * for more details.
 *
 * GNU General Public License
 */
package vista.graph;

import java.awt.Graphics;
import java.awt.Image;
import java.awt.Rectangle;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;

/**
 * Selects a range on the x axis.
 * 
 * @author Nicky Sandhu
 * @version $Revision$
 */
public class RangeSelector implements RangeSelected {
	private RangeListener rl;
	private GECanvas _gC;
	private RangeActor _ra;
	private Curve curve;

	/**
	 * selects a range on the x axis of the curve in the graph canvas.
	 */
	public RangeSelector(GECanvas gC, Curve curve, RangeActor ra) {
		_gC = gC;
		_ra = ra;
		this.curve = curve;
		selectRange();
	}

	/**
   *
   */
	void doneSelecting() {
		_ra.selectedRange(rl.getXRangeMin(), rl.getXRangeMax(), rl
				.getYRangeMin(), rl.getYRangeMax());
	}

	/**
    *
    */
	public void selectRange() {
		_gC.addMouseListener(rl = new RangeListener(_gC, curve));
		_gC.addMouseMotionListener(rl);
	}

	/**
    *
    */
	private class RangeListener implements MouseListener, MouseMotionListener {
		private boolean DEBUG = false;
		private boolean click1 = false, click2 = false;
		private GECanvas _gC;
		private int x1, x2;
		private int y1, y2;
		private Image _gCImage;
		private CoordinateDisplayInteractor _cdi;
		private Curve curve;

		/**
   *
   */
		public RangeListener(GECanvas gC, Curve curve) {
			_gC = gC;
			this.curve = curve;
			_gC.addMouseMotionListener(_cdi = new CoordinateDisplayInteractor(
					_gC));
			Rectangle r = _gC.getBounds();
			_gCImage = _gC.createImage(r.width, r.height);
		}

		/**
		 *
		 */
		public int getXRangeMax() {
			return Math.max(x1, x2);
		}

		/**
		 *
		 */
		public int getXRangeMin() {
			return Math.min(x1, x2);
		}

		/**
		 *
		 */
		public int getYRangeMax() {
			return Math.max(y1, y2);
		}

		/**
		 *
		 */
		public int getYRangeMin() {
			return Math.min(y1, y2);
		}

		/**
		 * Invoked when the mouse has been clicked on a component.
		 */
		public void mouseClicked(MouseEvent e) {
			Rectangle r = curve.getBounds();
			if (!click1) {
				click1 = true;
				x1 = Math.min(Math.max(e.getX(), r.x), r.x + r.width);
				y1 = Math.min(Math.max(e.getY(), r.y), r.y + r.height);
			} else if (!click2) {
				click2 = true;
				x2 = Math.min(Math.max(e.getX(), r.x), r.x + r.width);
				y2 = Math.min(Math.max(e.getY(), r.y), r.y + r.height);
				_gC.removeMouseMotionListener(this);
				_gC.addMouseListener(this);
				_gC.removeMouseMotionListener(_cdi);
				_cdi.doneDisplaying();
				_gC.redoNextPaint();
				RangeSelector.this.doneSelecting();
			}
		}

		/**
		 * Invoked when the mouse button has been moved on a component (with no
		 * buttons no down).
		 */
		public void mouseMoved(MouseEvent e) {
			drawBox(e.getX(), e.getY());
		}

		/**
         *
         */
		private void drawBox(int x, int y) {
			if (_gCImage == null) {
				Rectangle r = _gC.getBounds();
				_gCImage = _gC.createImage(r.width, r.height);
			}
			Graphics g = _gCImage.getGraphics();
			g.drawImage(_gC.getGraphicElementImage(), 0, 0, null);
			if (click1 && click2) {
				g.drawLine(0, 0, 100, 100);
			} else if (click1) {
				g.drawRect(Math.min(x1, x), Math.min(y1, y), Math.abs(x - x1),
						Math.abs(y - y1));
			} else {
				int size = 8;
				g.drawLine(x - size, y, x + size, y);
				g.drawLine(x, y - size, x, y + size);
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

	@Override
	public double getXRangeMin() {
		Scale sc = curve.getXAxis().getScale();
		return sc.scaleToDC(rl.getXRangeMin());
	}
	
	@Override
	public double getXRangeMax() {
		Scale sc = curve.getXAxis().getScale();
		return sc.scaleToDC(rl.getXRangeMax());
	}

	@Override
	public double getYRangeMax() {
		Scale sc = curve.getYAxis().getScale();
		return sc.scaleToDC(rl.getYRangeMin());
	}

	@Override
	public double getYRangeMin() {
		Scale sc = curve.getYAxis().getScale();
		return sc.scaleToDC(rl.getYRangeMax());
	}
}
