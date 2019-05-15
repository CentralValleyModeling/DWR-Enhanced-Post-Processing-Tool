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

import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Image;
import java.awt.Rectangle;
import java.lang.reflect.Constructor;
import javax.swing.*;

/**
 * This class handles the passing of messages to and from the GraphicElement
 * object. Furthermore it provides the GraphicElement object with the drawing
 * context and the drawing area. Double buffering is activated for performance
 * reasons.
 * 
 * @author Nicky Sandhu
 * @version $Id: GECanvas.java,v 1.1 2003/10/02 20:48:57 redwood Exp $
 */
public class GECanvas extends JPanel implements ElementContext {
	/**
	 * for debuggin' purposes
	 */
	public static boolean DEBUG = false;
	/**
	 * The GraphicElement which handles all the drawing.
	 */
	protected GraphicElement _ge;

	/**
	 * constructor
	 * 
	 * @see GraphicElement
	 */
	public GECanvas(GraphicElement ge) {
		_ge = ge;
		super.setDoubleBuffered(false);
	}

	/**
	 * returns the handle to the GraphicElement object
	 * 
	 * @see GraphicElement
	 */
	public GraphicElement getGraphicElement() {
		return _ge;
	}

	/**
	 * sets the currently drawn graphic element.
	 */
	public void setGraphicElement(GraphicElement ge) {
		_ge = ge;
	}

	/**
	 * This prepares the next paint call for redoing the whole GraphicElement.
	 * This means layout + actual drawing of components
	 */
	public void redoNextPaint() {
		updateNeeded = true;
	}

	/**
	 * paint method is overrided and graph layout + draw is called. This method
	 * handles double buffering as well.
	 */
	public void paint(Graphics g) {
		if (!isVisible())
			return;
		Rectangle r = this.getBounds();
		r.x = 0;
		r.y = 0;
		if (_clipIt)
			g.setClip(r);
		if (doubleBuffer) {
			if ((r.width != oldR.width || r.height != oldR.height)
					|| updateNeeded) {
				if (updateNeeded) {
					if (r.width <= 0 || r.height <= 0)
						return;
					if (r.width != oldR.width || r.height != oldR.height) {
						if (_geImage != null) {
							_geImage.flush();
							if (DEBUG)
								System.out
										.println("paint method => Killing image "
												+ _geImage);
						}
						_geImage = mkImage(r.width, r.height);
						if (DEBUG)
							System.out.println("paint method => Created image "
									+ _geImage);
					}
					Graphics gg = _geImage.getGraphics();
					synchronized (gg) {
						if (_clipIt)
							gg.setClip(r);
						_ge.draw(gg, r);
						_ge.animateNext();
						updateNeeded = false;
						oldR.setBounds(r);
					}
				} else {
					Graphics gg = _geImage.getGraphics();
					synchronized (gg) {
						if (_clipIt)
							gg.setClip(r);
						_ge.draw(gg);
						_ge.animateNext();
					}
				}
			}
			if (_geImage != null)
				g.drawImage(_geImage, 0, 0, this);
		} else {
			if (updateNeeded) {
				_ge.draw(g, r);
				_ge.animateNext();
				updateNeeded = false;
			} else {
				_ge.draw(g);
				_ge.animateNext();
			}
		}
	}

	/**
	 * flush the image resources
	 */
	public void finalize() throws java.lang.Throwable {
		if (_geImage != null) {
			if (DEBUG)
				System.out.println("finalize method => Killing image "
						+ _geImage);
			_geImage.flush();
			_geImage = null;
		}
		super.finalize();
	}

	/**
    *
    */
	public Image mkImage(int width, int height) {
		Image img = null;
		if (GraphUtils.isJDK2()) {
			// _geImage = new BufferedImage(int width, int height, int
			// imageType);
			// width = width, height = height, imageType =
			// BufferedImage.TYPE_INT_RGB or 1
			try {
				Class cl = Class.forName("java.awt.image.BufferedImage");
				Class[] params = { Integer.TYPE, Integer.TYPE, Integer.TYPE };
				Constructor cst = cl.getDeclaredConstructor(params);
				img = (Image) cst
						.newInstance(new Object[] { new Integer(width),
								new Integer(height), new Integer(1) });
			} catch (Exception exc) {
				img = createImage(width, height);
			}
		} else {
			img = createImage(width, height);
		}
		return img;
	}

	/**
	 * returns the image of the GraphicElement being drawn
	 */
	public Image getGraphicElementImage() {
		if ((_geImage == null) || updateNeeded) {
			Rectangle r = getBounds();
			r.x = 0;
			r.y = 0;
			if (_geImage == null
					|| (r.width != oldR.width || r.height != oldR.height))
				_geImage = mkImage(r.width, r.height);
			if (DEBUG)
				System.out
						.println("getGraphicElementImage method: => Created image "
								+ _geImage);
			_ge.draw(_geImage.getGraphics(), r);
			updateNeeded = false;
		}
		return _geImage;
	}

	/**
	 * The preferred size of this component is the preferrred size of the
	 * contained GraphicElement
	 */
	public Dimension getPreferredSize() {
		Graphics g = this.getGraphics();
		_ge.setGraphics(g);
		return _ge.getPreferredSize();
	}

	/**
	 * The minimum size of this component is the minimum size of the contained
	 * GraphicElement
	 */
	public Dimension getMinimumSize() {
		Graphics g = this.getGraphics();
		_ge.setGraphics(g);
		return _ge.getMinimumSize();
	}

	/**
	 * True if double buffering is being used
	 */
	public boolean isDoubleBuffered() {
		return doubleBuffer;
	}

	/**
	 * Returns true if double buffering is used
	 */
	public boolean getDoubleBuffered() {
		return doubleBuffer;
	}

	/**
	 * sets the double buffering flag
	 */
	public void setDoubleBuffered(boolean b) {
		super.setDoubleBuffered(false);
		doubleBuffer = b;
		updateNeeded = true;
	}

	/**
    *
    */
	public void setClipped(boolean b) {
		_clipIt = b;
	}

	/**
    *
    */
	public boolean isClipped() {
		return _clipIt;
	}

	/**
	 * true if update is needed, i.e. GraphicElement layout + paint
	 */
	protected boolean updateNeeded = true;
	/**
	 * allow double buffering
	 */
	protected boolean doubleBuffer = true;
	/**
	 * allow canvas to define its own clipping
	 */
	protected boolean _clipIt = true;
	/**
	 * stores the previous layout size of GraphicElement
	 */
	protected Rectangle oldR = new Rectangle(0, 0, 0, 0);
	/**
	 * stores the current image of GraphicElement if double buffered
	 */
	protected Image _geImage;
}
