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

import java.awt.Color;
import java.awt.Graphics;
import java.awt.Rectangle;
import java.util.Properties;

import vista.set.DataSet;

/**
 * A plot is defined as an area with atmost four axes aligned so as to make up a
 * rectangle. A plot simply contains Axis and Curve objects. The GEBorderLayout
 * is used to manage the layout of these elements
 * <p>
 * 
 * It also manages the creation of axes and curves for a plot. interior area is
 * scaled from the axes ranges. The DataSet (s) to be plotted are added to the
 * plot and particular axis positions. A Curve element is then created for each
 * such DataSet.
 * 
 * @see Curve
 * @see DataSet
 * @see Axis
 * @author Nicky Sandhu
 * @version $Id: Plot.java,v 1.1 2003/10/02 20:49:06 redwood Exp $
 */
public class Plot extends GEContainer implements FontResizable {
	/**
	 * debuggin
	 */
	public static final boolean DEBUG = false;

	/**
	 * constructor
	 */
	public Plot() {
		this(new PlotAttr());
	}

	/**
	 * constructor
	 */
	public Plot(PlotAttr attributes) {
		super(attributes);
		// initialize a new curve container;
		if (_curveContainer == null) {
			_curveContainer = new GEContainer(new GEAttr());
			_curveContainer.setLayout(new GEOverlayLayout());
			super.setLayout(new GEMultiBorderLayout(null));
			this.add(GEBorderLayout.CENTER, _curveContainer);
			_lineContainer = new GEContainer(new GEAttr());
			_lineContainer.setLayout(new GEOverlayLayout());
			// set line container's axes
			_curveContainer.add(_lineContainer);
		}
		if (_legendContainer == null) {
			_legendContainer = new GEContainer(new GEAttr());
			_lineContainer.add(_legendContainer);
			Scale xs = new Scale(0.0, 1.0, 1, 10);
			Scale ys = new Scale(0.0, 1.0, 1, 10);
			_legendContainer.setLayout(new GEScaledLayout(xs, ys));
		}
	}

	/**
   *
   */
	public void addLegend(Legend legend, int position) {
		DoubleRect dr = null;
		if (position == AxisAttr.LEFT) {
			dr = new DoubleRect(0.10, 0.95, 0.25, -0.15);
		} else {
			dr = new DoubleRect(0.65, 1.0, 0.25, -0.15);
		}
		legend._ignoreBounds = true; // let the legend decide what size it wants
		// to be
		((LegendAttr) legend.getAttributes())._boundariesMarked = true; // draw
		// the
		// border
		// set the foreground coloar same as plot
		legend.getAttributes()._foregroundColor = getAttributes()._foregroundColor;
		_legendContainer.add(dr, legend);
	}

	/**
	 * add title only if top axis is not taken
	 */
	public void addTitle(String txt) {
		if (getAxis(AxisAttr.TOP) != null)
			throw new IllegalArgumentException("Cannot add title to plot"
					+ " with top axis taken");
		TextLine tl = new TextLine(new TextLineAttr(), txt);
		add(GEBorderLayout.NORTH, tl);
	}

	/**
	 * adds label at given x and y location where 0 < x,y < 1
	 */
	public void addLabel(String label, double x, double y) {
		DoubleRect dr = new DoubleRect(x, y, 0.2, 0.2);
		TextLine tl = new TextLine(new TextLineAttr(), label);
		tl.setBounds(new Rectangle(0, 0, 10, 10));
		_legendContainer.add(dr, tl);
	}

	/**
	 * adds label at given x and y location where 0 < x,y < 1
	 */
	public void addLabel(String label, double x, double y, int size, Color color) {
		DoubleRect dr = new DoubleRect(x, y, 0.2, 0.2);
		TextLineAttr attr = new TextLineAttr();
		attr._foregroundColor = color;
		attr._justification = TextLineAttr.LEFT;
		TextLine tl = new TextLine(new TextLineAttr(), label);
		tl.setFontSize(size);
		tl.setBounds(new Rectangle(0, 0, 10, 10));
		_legendContainer.add(dr, tl);
	}

	/**
	 * sets new layout manager after adding curve container to it.
	 */
	public void setLayout(GELayoutManager m) {
		super.setLayout(m);
		getLayout().addLayoutElement(GEBorderLayout.CENTER, _curveContainer); // adds
		// to
		// center
	}

	/**
	 * adds element to north, south, east, west or center.
	 */
	public void add(String name, GraphicElement comp) {
		getLayout().addLayoutElement(name, comp);
		super.add(comp);
	}

	/**
   *
   */
	public void addLineForCurve(int curveNumber, double val, int orientation) {
		if (curveNumber >= getNumberOfCurves())
			throw new IllegalArgumentException(
					"Curve number specified is greater"
							+ " than available number of curves: "
							+ getNumberOfCurves());
		Curve crv = getCurve(curveNumber);
		CurveDataModel cdm = crv.getModel();
		CurveDataModel ldm = new LineDataModel(cdm, val, orientation);
		DefaultCurve lc = new DefaultCurve((CurveAttr) crv.getAttributes(), ldm);
		lc.setClipping(false);
		lc.setXAxis(crv.getXAxis());
		lc.setYAxis(crv.getYAxis());
		_lineContainer.add(lc);
	}

	/**
   *
   */
	public void removeLine(int ln) {
		if (ln >= _lineContainer.getElementCount())
			throw new IllegalArgumentException("No line @ index: " + ln
					+ " found");
		_lineContainer.remove(_lineContainer.getElement(ln));
	}

	/**
	 * @return the Axis object for given position or null if none exists
	 */
	public Axis getAxis(int axisPosition) {
		switch (axisPosition) {
		case AxisAttr.TOP:
			return topAxis;
		case AxisAttr.BOTTOM:
			return bottomAxis;
		case AxisAttr.LEFT:
			return leftAxis;
		case AxisAttr.RIGHT:
			return rightAxis;
		}
		return null;
	}

	/**
	 * adds axis at given position.
	 */
	public void addAxis(Axis axis, int axisPosition) {
		switch (axisPosition) {
		case AxisAttr.TOP:
			add(GEBorderLayout.NORTH, axis);
		case AxisAttr.BOTTOM:
			add(GEBorderLayout.SOUTH, axis);
		case AxisAttr.LEFT:
			add(GEBorderLayout.WEST, axis);
		case AxisAttr.RIGHT:
			add(GEBorderLayout.EAST, axis);
		}
	}

	/**
	 * removes the given axis object.
	 */
	public void removeAxis(Axis a) {
		remove(a);
		getLayout().removeLayoutElement(a);
	}

	/**
	 * The preconditions are that axis positon can only be top, left, bottom or
	 * right and the layout has to be border layout.
	 * 
	 * @return the Axis object or if null then create an axis add it to layout
	 *         and return it.
	 */
	public Axis createAxis(int axisPosition) {
		//
		switch (axisPosition) {
		case AxisAttr.TOP:
			if (topAxis == null) {
				topAxis = new Axis(new AxisAttr(), AxisAttr.TOP);
				add(GEBorderLayout.NORTH, topAxis);
			}
			return topAxis;
		case AxisAttr.BOTTOM:
			if (bottomAxis == null) {
				bottomAxis = new Axis(new AxisAttr(), AxisAttr.BOTTOM);
				add(GEBorderLayout.SOUTH, bottomAxis);
			}
			return bottomAxis;
		case AxisAttr.LEFT:
			if (leftAxis == null) {
				leftAxis = new Axis(new AxisAttr(), AxisAttr.LEFT);
				add(GEBorderLayout.WEST, leftAxis);
			}
			return leftAxis;
		case AxisAttr.RIGHT:
			if (rightAxis == null) {
				rightAxis = new Axis(new AxisAttr(), AxisAttr.RIGHT);
				add(GEBorderLayout.EAST, rightAxis);
			}
			return rightAxis;
		}
		return null;
	}

	/**
	 * @return the curve container.
	 */
	public GEContainer getCurveContainer() {
		return _curveContainer;
	}

	/**
	 * adds a graphic element to the plot
	 */
	public void add(GraphicElement ge) {
		if (ge instanceof Curve) {
			addCurve((Curve) ge);
		} else if (ge instanceof Axis) {
			addAxis((Axis) ge, ((Axis) ge).getPosition());
		} else if (ge instanceof Legend) {
			addLegend((Legend) ge, AxisAttr.LEFT);
		} else {
			super.add(ge);
			// throw new IllegalArgumentException("Cannot add element: " + ge +
			// " to Plot");
		}
	}

	/**
	 * adds curve to plot.
	 */
	public void addCurve(Curve c) {
		// check if curve is already been added before
		if (_curveContainer != null) {
			if (_curveContainer.indexOf(c) != -1)
				throw new IllegalArgumentException(
						"curve is alread added to plot");
		}
		CurveDataModel cdm = c.getModel();
		// create axes for this curve or use existing ones
		int xapos = cdm.getXAxisPosition();
		int yapos = cdm.getYAxisPosition();
		Axis xAxis = createAxis(xapos);
		Axis yAxis = createAxis(yapos);
		if (xAxis.getOrientation() == yAxis.getOrientation()) {
			throw new IllegalArgumentException(
					"x and y axis orientation is the same");
		}
		// set axis
		c.setXAxis(xAxis);
		c.setYAxis(yAxis);
		// set name
		c.setLocalName("Curve#" + _curveNumber++);
		_curveContainer.add(c);
		drawFirst(_curveContainer);
	}

	/**
	 * removes curve from plot.
	 */
	public void removeCurve(Curve c) {
		if (c == null)
			return;
		if (_curveContainer != null) {
			int nremoved = _curveContainer.remove(c);
			if (nremoved == 0)
				return;
		} else {
			return;
		}
		// inform axes of detachment
		Axis xAxis = c.getXAxis();
		xAxis.detachCurve(c);
		if (xAxis.getNumberAttached() == 0)
			removeAxis(xAxis);
		Axis yAxis = c.getYAxis();
		yAxis.detachCurve(c);
		if (yAxis.getNumberAttached() == 0)
			removeAxis(yAxis);
	}

	/**
	 * gets the number of curves in plot
	 */
	public int getNumberOfCurves() {
		return _curveContainer.getElementCount() - 1;
	}

	/**
	 * get curve by number
	 */
	public Curve getCurve(int i) {
		if (i < 0 || i > _curveContainer.getElementCount() - 1)
			return null;
		return (Curve) _curveContainer.getElement(i + 1);
	}

	/**
	 * adds grid lines for the axis (lines perpendicular to axis running the
	 * distance of the plot area). The grid lines are placed at major tick marks
	 * for the given axis.
	 * 
	 * @param position
	 *            The position of the axis
	 */
	public void addGrid(int position) {
		Axis a = getAxis(position);
		if (a == null)
			return;
		GridAttr ga = new GridAttr();
		if (position == AxisAttr.TOP || position == AxisAttr.BOTTOM)
			ga._orientation = GridAttr.VERTICAL;
		if (position == AxisAttr.LEFT || position == AxisAttr.RIGHT)
			ga._orientation = GridAttr.HORIZONTAL;
		Grid grid = new Grid(ga, a);
		add(GEBorderLayout.CENTER, grid);
		drawFirst(grid);
	}

	/**
	 * set line containers layouer
	 */
	public void preDraw() {
		super.preDraw();
	}

	/**
	 * draws border around drawing area.
	 */
	public void Draw() {
		if (((PlotAttr) getAttributes())._drawBorder) {
			Graphics g = getGraphics();
			Rectangle r = getDrawingRegion();
			g.drawRect(r.x, r.y, r.width, r.height);
		}
	}

	/**
	 * returns a rectangle in which curves are drawn.
	 */
	public Rectangle getDrawingRegion() {
		return _curveContainer.getBounds();
	}

	/**
	 * Gets the current plot window
	 */
	public Plot getCurrentPlot() {
		return this;
	}

	/**
	 * Returns its properties in a Properties object. These are saved on disk.
	 * 
	 * @param prefixTag
	 *            A tag to assign the context for these properties e.g. if these
	 *            properties belong to Axis class then prefixTag will be "Axis."
	 */
	public void toProperties(Properties p, String prefixTag) {

		if (p == null)
			return;
		PlotAttr attr = (PlotAttr) getAttributes();
		super.toProperties(p, getPrefixTag(prefixTag));
	}

	/**
   *
   */
	public String getPrefixTag(String prefixTag) {
		return prefixTag + getName() + "Plot.";
	}

	/**
	 * initializes attributes and state from Properties object.
	 */
	public void fromProperties(Properties p, String prefixTag) {
		super.fromProperties(p, getPrefixTag(prefixTag));
	}

	/**
	 * The axis in top position
	 */
	private Axis topAxis = null;
	/**
	 * The axis in left position
	 */
	private Axis leftAxis = null;
	/**
	 * The axis in bottom position
	 */
	private Axis bottomAxis = null;
	/**
	 * The axis in right position
	 */
	private Axis rightAxis = null;
	/**
	 * Stores the size of the original area
	 */
	private Rectangle _originalArea = null;
	/**
   *
   */
	private GEContainer _curveContainer = null;
	/**
	 * a counter for number of curves.
	 */
	private int _curveNumber = 0;
	private GEContainer _lineContainer, _legendContainer;
}
