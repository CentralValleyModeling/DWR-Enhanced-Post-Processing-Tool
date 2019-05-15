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
public class XRangeSelector implements RangeSelected
{
	private Curve _curve;
	private RangeListener rl;
	private GECanvas _gC;
	private double _rmin, _rmax;
	private RangeActor fe;

	/**
	 * selects a range on the x axis of the curve in the graph canvas.
	 */
	public XRangeSelector(RangeActor fe, GECanvas gC, Curve curve)
	{
		_curve = curve;
		_gC = gC;
		this.fe = fe;
		selectRange();
	}

	/**
	 *
	 */
	void doneSelecting()
	{
		fe.selectedRange(rl.getRangeMin(), rl.getRangeMax(), Integer.MIN_VALUE, Integer.MAX_VALUE);
	}

	/**
	 * gets the minimum of range
	 */
	public double getXRangeMin()
	{
		Scale sc = _curve.getXAxis().getScale();
		_rmin = sc.scaleToDC(rl.getRangeMin());
		return _rmin;
	}

	/**
	 * gets maximum of range
	 */
	public double getXRangeMax()
	{
		Scale sc = _curve.getXAxis().getScale();
		_rmax = sc.scaleToDC(rl.getRangeMax());
		return _rmax;
	}

	/**
	 * gets the minimum of range on y axis
	 */
	public double getYRangeMin()
	{
		return _curve.getYAxis().getScale().getDataMinimum();
	}

	/**
	 * gets the maximum of range on y axis
	 */
	public double getYRangeMax()
	{
		return _curve.getYAxis().getScale().getDataMaximum();
	}

	/**
	 *
	 */
	private void selectRange()
	{
		_gC.addMouseListener(rl = new RangeListener(_gC, _curve));
		_gC.addMouseMotionListener(rl);
	}

	/**
	 *
	 */
	private class RangeListener implements MouseListener, MouseMotionListener
	{
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
		public RangeListener(GECanvas gC, Curve c)
		{
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
		public int getRangeMax()
		{
			return Math.max(x1, x2);
		}

		/**
		 *
		 */
		public int getRangeMin()
		{
			return Math.min(x1, x2);
		}

		/**
		 * Invoked when the mouse has been clicked on a component.
		 */
		public void mouseClicked(MouseEvent e)
		{
			if(DEBUG)
			{
				System.out.println("Mouse Clicked at ( " + e.getX() + ", "
						+ e.getY() + " )");
			}
			Rectangle r = _curve.getBounds();
			if(!click1)
			{
				click1 = true;
				x1 = Math.min(Math.max(e.getX(), r.x), r.x + r.width);
				if(DEBUG)
				{
					System.out.println("x1 = " + x1);
				}
			}
			else if(!click2)
			{
				click2 = true;
				x2 = Math.min(Math.max(e.getX(), r.x), r.x + r.width);
				if(DEBUG)
				{
					System.out.println("x2 = " + x2);
				}
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
		public void mouseMoved(MouseEvent e)
		{
			if(DEBUG)
			{
				System.out.println("Mouse Moved at ( " + e.getX() + ", "
						+ e.getY() + " )");
			}
			moveVerticalLineTo(e.getX());
		}

		private void moveVerticalLineTo(int x)
		{
			Graphics g = _gCImage.getGraphics();
			g.drawImage(_gC.getGraphicElementImage(), 0, 0, null);
			Rectangle r = _curve.getBounds();
			// g.setClip(r);
			if(click1 && click2)
			{
				g.drawLine(x1, r.y, x1, r.y + r.height);
				g.drawLine(x2, r.y, x2, r.y + r.height);
			}
			else if(click1)
			{
				g.drawLine(x1, r.y, x1, r.y + r.height);
				g.drawLine(x, r.y, x, r.y + r.height);
			}
			else
			{
				g.drawLine(x, r.y, x, r.y + r.height);
			}
			_gC.getGraphics().drawImage(_gCImage, 0, 0, null);
		}

		public void mousePressed(MouseEvent e)
		{
			if(DEBUG)
			{
				System.out.println("Mouse Pressed at ( " + e.getX() + ", "
						+ e.getY() + " )");
			}
		}

		public void mouseReleased(MouseEvent e)
		{
			if(DEBUG)
			{
				System.out.println("Mouse Released at ( " + e.getX() + ", "
						+ e.getY() + " )");
			}
		}

		public void mouseEntered(MouseEvent e)
		{
			if(DEBUG)
			{
				System.out.println("Mouse Entered at ( " + e.getX() + ", "
						+ e.getY() + " )");
			}
		}

		public void mouseExited(MouseEvent e)
		{
			if(DEBUG)
			{
				System.out.println("Mouse Exited at ( " + e.getX() + ", "
						+ e.getY() + " )");
			}
		}

		public void mouseDragged(MouseEvent e)
		{
			if(DEBUG)
			{
				System.out.println("Mouse Dragged at ( " + e.getX() + ", "
						+ e.getY() + " )");
			}
		}
	} // end of Range Listener
}
