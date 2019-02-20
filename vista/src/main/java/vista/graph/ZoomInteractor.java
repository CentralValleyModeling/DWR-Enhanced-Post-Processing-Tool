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

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Container;
import java.awt.Frame;
import java.awt.Graphics;
import java.awt.Image;
import java.awt.Rectangle;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.MouseEvent;
import java.util.ArrayList;
import java.util.Iterator;

import javax.swing.AbstractAction;
import javax.swing.ActionMap;
import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.InputMap;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JFrame;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JToolBar;
import javax.swing.KeyStroke;

/**
 * This class handles interaction of mouse and mouse motion with Graph class and
 * its components.
 * 
 * @author Nicky Sandhu
 * @version $Id: ZoomInteractor.java,v 1.1 2003/10/02 20:49:12 redwood Exp $
 */
public class ZoomInteractor extends ElementInteractor {
	@SuppressWarnings("serial")
	private final static class PagingAction extends AbstractAction {

		private int axisDirection;
		private ZoomInteractor zi;

		public PagingAction(ZoomInteractor zi, int axisDirection) {
			super("", getIcon(axisDirection));
			this.zi = zi;
			this.axisDirection = axisDirection;
			putValue(SHORT_DESCRIPTION, getDescription(axisDirection));
			putValue(MNEMONIC_KEY, getMnemonicKey(axisDirection));
		}

		static String getDescription(int axisDirection) {
			switch (axisDirection) {
			case AxisAttr.TOP:
				return "Page Up";
			case AxisAttr.BOTTOM:
				return "Page Down";
			case AxisAttr.LEFT:
				return "Page Left";
			case AxisAttr.RIGHT:
				return "Page Right";
			default:
				return "Page Reset";
			}
		}

		static Icon getIcon(int axisDirection) {
			switch (axisDirection) {
			case AxisAttr.TOP:
				return new ImageIcon(ZoomInteractor.class
						.getResource("/vista/images/arrow-up-sharp.gif"));
			case AxisAttr.BOTTOM:
				return new ImageIcon(ZoomInteractor.class
						.getResource("/vista/images/arrow-dn-sharp.gif"));
			case AxisAttr.LEFT:
				return new ImageIcon(ZoomInteractor.class
						.getResource("/vista/images/arrow-lft-sharp.gif"));
			case AxisAttr.RIGHT:
				return new ImageIcon(ZoomInteractor.class
						.getResource("/vista/images/arrow-rit-sharp.gif"));
			default:
				return new ImageIcon(ZoomInteractor.class
						.getResource("/vista/images/center.gif"));
			}
		}

		static Integer getMnemonicKey(int axisDirection) {
			switch (axisDirection) {
			case AxisAttr.TOP:
				return new Integer(KeyEvent.VK_UP);
			case AxisAttr.BOTTOM:
				return new Integer(KeyEvent.VK_DOWN);
			case AxisAttr.LEFT:
				return new Integer(KeyEvent.VK_LEFT);
			case AxisAttr.RIGHT:
				return new Integer(KeyEvent.VK_RIGHT);
			default:
				return new Integer(KeyEvent.VK_HOME);
			}
		}

		@Override
		public void actionPerformed(ActionEvent e) {
			zi.pageToPosition(axisDirection);
		}
	}

	/**
	 * for debuggin' purposes
	 */
	public static final boolean DEBUG = false;
	private ArrayList<Zoom> _zooms = new ArrayList<Zoom>();
	private boolean _isPageMode = false;
	private int sensitivity = 25; // a movement of atleast 25 pixels is needed
	private JButton zob;
	private JPanel pagingPanel;

	/**
	 * Controls zooming in/out and storage of state for being able to do so.
	 * 
	 * @see Zoom
	 */
	private Zoom _zoom;
	/**
	 * Buffers image so as to avoid flickering while selecting zoom region
	 */
	private Image _gCImage;
	/**
	 * Graph canvas
	 */
	private ElementContext _gC;
	/**
	 * Stores flag to indicate if double buffering was being used before
	 */
	private boolean _previouslyDoubleBuffered = false;
	/**
	 * Flag to indicate zoom region selection is in progress.
	 */
	private boolean _drawDragRect = true;
	/**
	 * Flag to indicate whether mouse was dragged after mouse button was
	 * pressed.
	 */
	private boolean _mouseDragged = false;
	/**
	 * Initial point's x value
	 */
	private int _xi = 0;
	/**
	 * Initial point's y value
	 */
	private int _yi = 0;
	/**
	 * Final point's x value
	 */
	private int _xf = 0;
	/**
	 * Final point's y value
	 */
	private int _yf = 0;
	/**
	 * Current zooming region
	 */
	private Rectangle _zoomRect = new Rectangle(0, 0, 0, 0);
	/**
	 * color used to draw the zoom rectangle
	 */
	private Color _zoomRectColor = Color.black;
	/**
	 * Plot object
	 */
	private Plot _plot;
	private JButton upButton;
	private JButton downButton;
	private JButton rightButton;
	private JButton leftButton;
	private JButton centerButton;

	/**
	 * constructor
	 */
	public ZoomInteractor(ElementContext gC) {
		_gC = gC;
	}

	public void addZoomControlToolbar(JFrame fr) {
		JToolBar tb = new JToolBar();
		tb.setFloatable(false);
		tb.setFocusable(false);

		zob = new JButton(new ImageIcon(getClass().getResource(
				"/vista/images/zoom-out-icon.png")));
		zob.setToolTipText("Zoom Out");
		zob.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent evt) {
				zoomOut();
				((JComponent) _gC).requestFocus();
			}
		});
		//

		PagingAction leftPageAction = new PagingAction(this, AxisAttr.LEFT);
		PagingAction rightPageAction = new PagingAction(this, AxisAttr.RIGHT);
		PagingAction upPageAction = new PagingAction(this, AxisAttr.TOP);
		PagingAction downPageAction = new PagingAction(this, AxisAttr.BOTTOM);
		PagingAction homePageAction = new PagingAction(this, -1);
		pagingPanel = new JPanel();
		pagingPanel.setLayout(null);
		upButton = new JButton(upPageAction);
		downButton = new JButton(downPageAction);
		rightButton = new JButton(rightPageAction);
		leftButton = new JButton(leftPageAction);
		centerButton = new JButton(homePageAction);
		int s = 10;
		upButton.setBounds(s, 0, s, s);
		downButton.setBounds(s, s * 2, s, s);
		rightButton.setBounds(s * 2, s, s, s);
		leftButton.setBounds(0, s, s, s);
		centerButton.setBounds(s, s, s, s);
		pagingPanel.add(upButton);
		pagingPanel.add(downButton);
		pagingPanel.add(rightButton);
		pagingPanel.add(leftButton);
		pagingPanel.add(centerButton);
		pagingPanel.setSize(s * 4, s * 4);
		setPagingMode(true);

		setZoomOutPagingEnabled(false);
		tb.add(zob);
		tb.add(pagingPanel);

		Container contentPane = fr.getContentPane();
		if (contentPane instanceof JPanel) {
			JPanel mainPanel = (JPanel) contentPane;
			InputMap inputMap = mainPanel
					.getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW);
			inputMap.put(KeyStroke.getKeyStroke(KeyEvent.VK_LEFT, 0),
					KeyEvent.VK_LEFT + "");
			inputMap.put(KeyStroke.getKeyStroke(KeyEvent.VK_RIGHT, 0),
					KeyEvent.VK_RIGHT + "");
			inputMap.put(KeyStroke.getKeyStroke(KeyEvent.VK_UP, 0),
					KeyEvent.VK_UP + "");
			inputMap.put(KeyStroke.getKeyStroke(KeyEvent.VK_DOWN, 0),
					KeyEvent.VK_DOWN + "");
			inputMap.put(KeyStroke.getKeyStroke(KeyEvent.VK_HOME, 0),
					KeyEvent.VK_HOME + "");
			//
			ActionMap actionMap = mainPanel.getActionMap();
			actionMap.put(KeyEvent.VK_LEFT + "", leftPageAction);
			actionMap.put(KeyEvent.VK_RIGHT + "", rightPageAction);
			actionMap.put(KeyEvent.VK_UP + "", upPageAction);
			actionMap.put(KeyEvent.VK_DOWN + "", downPageAction);
			actionMap.put(KeyEvent.VK_HOME + "", homePageAction);
		}

		contentPane.add(tb, BorderLayout.NORTH);
	}

	public Object getMnemonicKey() {
		// TODO Auto-generated method stub
		return null;
	}

	public void setZoomOutPagingEnabled(boolean enable) {
		zob.setEnabled(enable);
		upButton.setEnabled(enable);
		downButton.setEnabled(enable);
		leftButton.setEnabled(enable);
		rightButton.setEnabled(enable);
		centerButton.setEnabled(enable);
	}

	/**
    *
    */
	public void releaseResources() {
		_gC = null;
		_gCImage = null;
	}

	/**
   *
   */
	private Plot getPlot(int x, int y) {
		Graph graph = (Graph) _gC.getGraphicElement();
		Plot plot = graph.getPlot();
		GraphicElement[] leafs = graph.getElements(MultiPlot.class);
		if (leafs == null) {
			return plot;
		}
		if (leafs.length == 1) {
			MultiPlot mp = (MultiPlot) leafs[0];
			leafs = mp.getElements(Plot.class);
		}
		if (leafs == null)
			return plot;
		for (int i = 0; i < leafs.length; i++) {
			if (leafs[i] instanceof MultiPlot)
				continue;
			Plot p = (Plot) leafs[i];
			if (p != null && p.contains(x, y))
				plot = p;
			if (DEBUG)
				System.out.println(i + ": " + leafs[i]);
		}
		if (DEBUG)
			System.out.println("Plot @ " + "(" + x + "," + y + "): "
					+ plot.getClass().getName() + "@"
					+ Integer.toHexString(plot.hashCode()));
		return plot;
	}

	/**
   *
   */
	private Zoom getZoomFor(Plot plot) {
		for (Iterator<Zoom> e = _zooms.iterator(); e.hasNext();) {
			Zoom z = e.next();
			if (z.getPlot().equals(plot))
				return z;
		}
		Zoom z = new Zoom(plot);
		_zooms.add(z);
		return z;
	}

	/**
	 * sets tool tip to current co-ordinates
	 */
	public void mouseMoved(MouseEvent e) {
	}

	/**
	 * Invoked when a mouse button has been pressed on a component. This sets
	 * the initial point of the zoom region
	 */
	public void mousePressed(MouseEvent e) {
		setInitialPoint(e.getX(), e.getY());
		// get Plot clicked on...
		_plot = getPlot(e.getX(), e.getY());
		// get zoom object for this plot or create a new one for it...
		_zoom = getZoomFor(_plot);
		if (DEBUG)
			System.out.println("Zooming: ");
		if (DEBUG)
			System.out.println("Initial Point: " + e.getX() + ", " + e.getY());
		_drawDragRect = true;

		_previouslyDoubleBuffered = _gC.isDoubleBuffered();
		if (!_previouslyDoubleBuffered)
			_gC.setDoubleBuffered(true);

		Rectangle r = _gC.getBounds();
		_gCImage = _gC.createImage(r.width, r.height);
	}

	/**
	 * Invoked when a mouse button has been released on a component. If mouse
	 * was dragged this marks the diagonally opposite point to the initial point
	 * of the zoom region. Then region is zoomed into.
	 * <p>
	 * If mouse was clicked without dragging then zoom out is done to the
	 * previous zoom state.
	 */
	public void mouseReleased(MouseEvent e) {
		if (_mouseDragged) {
			_drawDragRect = false;

			// check sensitivity
			if (Math.pow(_xf - _xi, 2) + Math.pow(_yf - _yi, 2) < sensitivity) {
				_mouseDragged = false;
				_gC.repaint();
				return;
			}

			setFinalPoint(e.getX(), e.getY());

			if (_zoomRect.width == 0 || _zoomRect.height == 0)
				return;
			_zoom.zoomIn(_zoomRect);

			_gC.redoNextPaint();
			_gC.repaint();

			_mouseDragged = false;

			setZoomOutPagingEnabled(true);
		} else {
		}
	}

	/**
	 * restores previous zoom
	 */
	public void zoomOut() {
		if (_zoom == null)
			return;
		if (_zoom.zoomOut()) {

			_gC.redoNextPaint();
			_gC.repaint();

			_mouseDragged = false;
		}
		if (_zoom.isZoomedOutAllTheWay()) {
			setZoomOutPagingEnabled(false);
		}
	}

	/**
	 * Invoked when a mouse button is pressed on a component and then dragged.
	 * As mouse is dragged a rectangle showing the currently selected zoom
	 * region is displayed as a rectangle. To achieve good performance double
	 * buffering was used, however there seems to be a bug in JDK 1.1.2 which
	 * does not draw the complete image when a complicated drawing is done. This
	 * affects multiple curve plot.
	 */
	public void mouseDragged(MouseEvent e) {
		_mouseDragged = true;
		if (_drawDragRect) {
			Graphics g = _gCImage.getGraphics();
			Rectangle bounds = _gC.getBounds();
			bounds.x = 0;
			bounds.y = 0;
			g.drawImage(_gC.getGraphicElementImage(), 0, 0, null);
			g.setClip(bounds);
			setFinalPoint(e.getX(), e.getY());
			Rectangle r = _zoomRect;
			g.setColor(_zoomRectColor);
			g.drawRect(r.x, r.y, r.width, r.height);
			_gC.getGraphics().drawImage(_gCImage, 0, 0, null);
		}
	}

	public void pageToPosition(int nPos) {
		if (!_isPageMode)
			return;
		if (_zoom == null) {
			return;
		}
		if (nPos == -1)
			_zoom.zoomReset();
		else
			_zoom.nextPage(nPos);
		_gC.redoNextPaint();
		_gC.repaint();
	}

	/**
	 * Sets initial point of zoom rectangle region
	 */
	public void setInitialPoint(int x, int y) {
		_xi = x;
		_yi = y;
	}

	/**
	 * Sets final point of zoom rectangle region. This would be the point
	 * diagonally opposite the initial point
	 */
	public void setFinalPoint(int x, int y) {
		_xf = x;
		_yf = y;
		constructRectangle();
	}

	/**
	 * Constructs rectangle from set of diagonally opposite points. Stores this
	 * rectangle into a stack
	 */
	private void constructRectangle() {
		_zoomRect.x = Math.min(_xi, _xf);
		_zoomRect.y = Math.min(_yi, _yf);
		_zoomRect.width = Math.abs(_xi - _xf);
		_zoomRect.height = Math.abs(_yi - _yf);

		Rectangle drawingRegion = _plot.getDrawingRegion();
		if (DEBUG)
			System.out.println(drawingRegion);
		if (DEBUG)
			System.out.println(_zoomRect);
		if (drawingRegion.intersects(_zoomRect)) {
			if (DEBUG)
				System.out.println("Drawing region intersects zoom rectangle");
			int x1 = Math.max(drawingRegion.x, _zoomRect.x);
			int x2 = Math.min(drawingRegion.x + drawingRegion.width,
					_zoomRect.x + _zoomRect.width);
			int y1 = Math.max(drawingRegion.y, _zoomRect.y);
			int y2 = Math.min(drawingRegion.y + drawingRegion.height,
					_zoomRect.y + _zoomRect.height);
			_zoomRect.x = x1;
			_zoomRect.y = y1;
			_zoomRect.width = x2 - x1;
			_zoomRect.height = y2 - y1;
			if (_zoomRect.width == 0)
				_zoomRect.width = 1;
			if (_zoomRect.height == 0)
				_zoomRect.height = 1;
		} else {
			_zoomRect.x = 0;
			_zoomRect.y = 0;
			_zoomRect.width = 0;
			_zoomRect.height = 0;
		}
	}

	/**
	 * Set the color of the rectangle drawn when zooming in to specify the
	 * zooming in region
	 */
	public void setZoomRectangleColor(Color c) {
		_zoomRectColor = c;
	}

	/**
	 * checks if paging is on
	 */
	public boolean isInPagingMode() {
		return _isPageMode;
	}

	/**
	 * sets the paging mode.
	 */
	public void setPagingMode(boolean b) {
		_isPageMode = b;
	}

}
