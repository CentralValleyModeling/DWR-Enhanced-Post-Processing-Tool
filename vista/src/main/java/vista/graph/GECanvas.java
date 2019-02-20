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
import java.awt.Image;
import java.awt.Rectangle;
import java.lang.reflect.Constructor;

import javax.swing.JPanel;

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
