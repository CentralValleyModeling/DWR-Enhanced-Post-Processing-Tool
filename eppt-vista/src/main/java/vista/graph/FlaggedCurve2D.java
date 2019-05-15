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

import java.awt.BasicStroke;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Rectangle;
import java.awt.RenderingHints;
import java.awt.Shape;
import java.awt.Stroke;
import java.awt.geom.AffineTransform;
import java.awt.geom.GeneralPath;
import java.awt.geom.PathIterator;
import java.awt.geom.Point2D;
import java.awt.geom.Rectangle2D;
import java.util.Hashtable;

/**
 * Draws a default curve. It understands only linear and last value
 * interpolation types. Also both x and y coordinates are believed to be real
 * numbers
 * 
 * @author Nicky Sandhu
 * @version $Id: FlaggedCurve2D.java,v 1.1 2003/10/02 20:48:55 redwood Exp $
 */
public class FlaggedCurve2D extends Curve implements Shape {
	private double[] _points;
	private Symbol rSymbol, qSymbol;
	/**
	 * debuggin'
	 */
	public static boolean DEBUG = false;
	public static boolean FAST_GRAPHICS = false;
	public static RenderingHints FAST_RENDERING_HINTS;
	static {
		Hashtable hintmap = new Hashtable();
		hintmap.put(RenderingHints.KEY_ANTIALIASING,
				RenderingHints.VALUE_ANTIALIAS_OFF);
		hintmap.put(RenderingHints.KEY_RENDERING,
				RenderingHints.VALUE_RENDER_SPEED);
		hintmap.put(RenderingHints.KEY_DITHERING,
				RenderingHints.VALUE_DITHER_DISABLE);
		hintmap.put(RenderingHints.KEY_TEXT_ANTIALIASING,
				RenderingHints.VALUE_TEXT_ANTIALIAS_OFF);
		hintmap.put(RenderingHints.KEY_ALPHA_INTERPOLATION,
				RenderingHints.VALUE_ALPHA_INTERPOLATION_SPEED);
		hintmap.put(RenderingHints.KEY_COLOR_RENDERING,
				RenderingHints.VALUE_COLOR_RENDER_SPEED);
		FAST_RENDERING_HINTS = new RenderingHints(hintmap);
	}
	public static RenderingHints DEFAULT_RENDERING_HINTS = new RenderingHints(
			null);

	/**
   *
   */
	public FlaggedCurve2D(CurveAttr attributes, CurveDataModel cdm) {
		super(attributes, cdm);
		_points = new double[2];
		_curve = new FlaggedCurve(attributes, cdm);
		setName(cdm.getLegendText());
		setQuestionableSymbol(SymbolFactory.createSquare(false,
				attributes._foregroundColor, 2));
		setRejectSymbol(SymbolFactory.createCross(false,
				attributes._foregroundColor, 2));
	}

	/**
	 * sets the symbol to be drawn for questionable values
	 */
	public void setQuestionableSymbol(Symbol s) {
		_curve.setQuestionableSymbol(s);
		s.getAttributes()._foregroundColor=_curve.getAttributes()._foregroundColor;
	}

	/**
	 * sets the symbol to be drawn for questionable values
	 */
	public void setRejectSymbol(Symbol s) {
		_curve.setRejectSymbol(s);
		s.getAttributes()._foregroundColor=_curve.getAttributes()._foregroundColor;
	}

	/**
    *
    */
	public void setGraphics(Graphics g) {
		super.setGraphics(g);
		_curve.setGraphics(g);
	}

	/**
    *
    */
	public void setBounds(Rectangle bounds) {
		super.setBounds(bounds);
		_curve.setBounds(bounds);
	}

	/**
    *
    */
	public void setAttributes(GEAttr attr) {
		super.setAttributes(attr);
		_curve.setAttributes(attr);
	}

	/**
	 * creates path iterator for curve.
	 */
	private PathIterator createPathIterator(AffineTransform at) {
		_pathIterator = new CurvePathIterator(getModel(), at);
		return _pathIterator;
		// _gp = new GeneralPath(this);
	}

	/**
	 * no back ground/ transparent background
	 */
	public void preDraw() {
		CurveAttr attr = (CurveAttr) getAttributes();
		attr._backgroundColor = null;
		super.preDraw();
		_curve.preDraw();
		// easy way of getting dashed lines. Later more sophisticated methods
		// would be added.
		Graphics gc = getGraphics();
		if (gc instanceof Graphics2D && attr._drawLines) {
			if (attr._dashArray != null && attr._dashArray.length > 1) {
				Graphics2D gc2 = null;
				gc2 = (Graphics2D) gc;
				_previousStroke = gc2.getStroke();
				int cap = BasicStroke.CAP_ROUND;
				int linejoin = BasicStroke.JOIN_ROUND;
				float miterlimit = 0.25f;
				float dashphase = 0.25f;
				if (_previousStroke != null
						&& _previousStroke instanceof BasicStroke) {
					BasicStroke bs = (BasicStroke) _previousStroke;
					cap = bs.getEndCap();
					linejoin = bs.getLineJoin();
					miterlimit = bs.getMiterLimit();
					dashphase = bs.getDashPhase();
					if (DEBUG) {
						System.out
								.println("Getting values from previous stroke:"
										+ _previousStroke);
						System.out.println("cap:" + cap + " linejoin:"
								+ linejoin + " miterlimit: " + miterlimit
								+ " dashphase:" + dashphase);
					}
				}
				gc2.setStroke(new BasicStroke(attr._thickness, cap, linejoin,
						miterlimit, attr._dashArray, dashphase));
				if (FAST_GRAPHICS) {
					gc2.setRenderingHints(FAST_RENDERING_HINTS);
				} else {
					gc2.setRenderingHints(DEFAULT_RENDERING_HINTS);
				}
			}
		} else {
			_previousStroke = null;
			if (gc instanceof Graphics2D) {
				((Graphics2D) gc).setRenderingHints(DEFAULT_RENDERING_HINTS);
			}
		}
	}

	/**
    *
    */
	public void setXAxis(Axis xAxis) {
		super.setXAxis(xAxis);
		_curve.setXAxis(xAxis);
	}

	/**
    *
    */
	public void setYAxis(Axis yAxis) {
		super.setYAxis(yAxis);
		_curve.setYAxis(yAxis);
	}

	/**
	 * Draws the data by scaling it and joining consecutive data points and/or
	 * plotting symbols for data points.
	 */
	protected void drawCurve() {
		CurveAttr attr = (CurveAttr) getAttributes();
		CurveDataModel cdm = getModel();
		Axis xAxis = getXAxis();
		Axis yAxis = getYAxis();
		Graphics gc = getGraphics();
		Rectangle r = getBounds();
		if (cdm == null)
			return;

		Symbol symbol = attr._symbol;
		Rectangle symbolBounds = new Rectangle(0, 0, 25, 25);
		Rectangle qSymbolBounds = new Rectangle(0, 0, 20, 20);
		Rectangle rSymbolBounds = new Rectangle(0, 0, 20, 20);

		// gc.setColor(attr._foregroundColor);
		boolean doLines = false;
		//
		if (gc instanceof Graphics2D && attr._drawLines) {
			if (attr._dashArray != null && attr._dashArray.length > 1) {
				Graphics2D gc2 = (Graphics2D) gc;
				AffineTransform at = getAffineTransform(xAxis.getScale(), yAxis
						.getScale());
				// gc2.draw(at.createTransformedShape(getGeneralPath()));
				gc2.draw(at.createTransformedShape(this));
			} else {
				doLines = true;
			}
		}
		// cache previous state of lines
		boolean prevDrawLines = attr._drawLines;
		if (!doLines) {
			attr._drawLines = false;
		}
		// draw curve with or without lines and certainly with symbols if need
		// be
		_curve.drawCurve();
		// get back to preivous state
		attr._drawLines = prevDrawLines;
	}

	/**
   *
   */
	public GeneralPath getGeneralPath() {
		if (_gp != null) {
			return _gp;
		}
		_gp = new GeneralPath(GeneralPath.WIND_EVEN_ODD, 10000);
		CurveDataModel cdm = getModel();
		cdm.reset();
		double[] points = { 0, 0 };
		int type;
		boolean atStart = true;
		while (cdm.hasMorePoints()) {
			type = cdm.nextPoint(points);
			if (atStart) {
				_gp.moveTo((float) points[0], (float) points[1]);
				atStart = false;
				continue;
			}
			if (type == CurveDataModel.MOVE_TO) {
				_gp.moveTo((float) points[0], (float) points[1]);
			} else if (type == CurveDataModel.LINE_TO) {
				_gp.lineTo((float) points[0], (float) points[1]);
			} else { // to do appending of Symbol shape here for Q/R data
				_gp.moveTo((float) points[0], (float) points[1]);
			}
		}
		return _gp;
	}

	/**
   *
   */
	public void postDraw() {
		CurveAttr attr = (CurveAttr) getAttributes();
		_curve.postDraw();
		super.postDraw();
		Graphics gc = getGraphics();
		if (_previousStroke != null && gc instanceof Graphics2D) {
			Graphics2D gc2 = (Graphics2D) gc;
			gc2.setStroke(_previousStroke);
		}
	}

	/**
   *
   */
	public Rectangle2D getBounds2D() {
		Rectangle area1 = getBounds();
		if (area1 == null)
			return null;
		Rectangle2D.Double area = new Rectangle2D.Double();
		area.setRect(area1.x, area1.y, area1.width, area1.height);
		return area;
	}

	/**
	 * Test if a given coordinate is inside the boundary of the shape.
	 */
	public boolean contains(double x, double y) {
		Rectangle area = getBounds();
		if (area != null)
			return area.contains(x, y);
		else
			return false;
	}

	/**
	 * Test if a given Point is inside the boundary of the shape.
	 */
	public boolean contains(Point2D p) {
		Rectangle2D area = getBounds2D();
		if (area != null)
			return area.contains(p);
		else
			return false;
	}

	/**
	 * Test if the interior of the Shape intersects the interior of a given set
	 * of rectangular coordinates.
	 */
	public boolean intersects(double x, double y, double w, double h) {
		Rectangle area = getBounds();
		if (area != null)
			return area.intersects(x, y, w, h);
		else
			return false;
	}

	/**
	 * Test if the interior of the Shape intersects the interior of a given
	 * Rectangle.
	 */
	public boolean intersects(Rectangle2D r) {
		Rectangle2D area = getBounds2D();
		if (area != null) {
			return area.intersects(r);
		} else
			return false;
	}

	/**
	 * Test if the interior of the Shape entirely contains the given set of
	 * rectangular coordinates.
	 */
	public boolean contains(double x, double y, double w, double h) {
		Rectangle area = getBounds();
		if (area != null)
			return area.contains(x, y, w, h);
		else
			return false;
	}

	/**
	 * Test if the interior of the Shape entirely contains the given Rectangle.
	 */
	public boolean contains(Rectangle2D r) {
		Rectangle2D area = getBounds2D();
		if (area != null) {
			return area.contains(r);
		} else
			return false;
	}

	/**
	 * returns the curve legend line for this curve
	 */
	public CurveLegendLine getLegendLine() {
		return new CurveLegendLine2D(this);
	}

	/**
	 * Return an iterator object that iterates along the boundary of the shape
	 * and provides access to the geometry of the outline of the shape. An
	 * optional affine transform can be specified in which case the coordinates
	 * returned in the iteration will be transformed accordingly.
	 * 
	 * @param at
	 *            an optional AffineTransform to be applied to the coordinates
	 *            as they are returned in the iteration, or null if the
	 *            untransformed coordinates are desired.
	 */
	public PathIterator getPathIterator(AffineTransform at) {
		return createPathIterator(at);
		// GeneralPath gp = getGeneralPath();
		// return gp.getPathIterator(at);
	}

	/**
	 * Return an iterator object that iterates along the boundary of the shape
	 * and provides access to a flattened view of the geometry of the outline of
	 * the shape. Only SEG_MOVETO, SEG_LINETO, and SEG_CLOSE point types will be
	 * returned by the iterator. The amount of subdivision of the curved
	 * segments is controlled by the <code>flatness</code> parameter which
	 * specifies ?REMIND?. An optional affine transform can be specified in
	 * which case the coordinates returned in the iteration will be transformed
	 * accordingly.
	 * 
	 * @param at
	 *            an optional AffineTransform to be applied to the coordinates
	 *            as they are returned in the iteration, or null if the
	 *            untransformed coordinates are desired.
	 * @param flatness
	 *            the maximum amount that the control points for a given curve
	 *            can vary from colinear before a subdivided curve is replaced
	 *            by a straight line connecting the endpoints.
	 */
	public PathIterator getPathIterator(AffineTransform at, double flatness) {
		return createPathIterator(at);
		// GeneralPath gp = getGeneralPath();
		// return gp.getPathIterator(at,flatness);
		// return createPathIterator();
	}

	/**
   *
   */
	public AffineTransform getAffineTransform(Scale xS, Scale yS) {
		double m00 = 0, m10 = 0, m01 = 0, m11 = 0, m02 = 0, m12 = 0;
		m01 = 0;
		m10 = 0; // no rotation
		if (xS != null) {
			m02 = xS.getShift();
			m00 = xS.getScaling();
		}
		if (yS != null) {
			m12 = yS.getShift();
			m11 = yS.getScaling();
		}
		return new AffineTransform(m00, m10, m01, m11, m02, m12);
	}

	/**
	 * holder of stroke.
	 */
	private Stroke _previousStroke;
	private FlaggedCurve _curve;
	/**
   *
   */
	private PathIterator _pathIterator;
	private GeneralPath _gp;
}
