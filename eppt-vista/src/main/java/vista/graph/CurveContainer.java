/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package vista.graph;

import java.awt.Dimension;
import java.awt.Rectangle;

/**
 * A container for curves independent of plot
 * 
 * @author Nicky Sandhu
 * @version $Id: CurveContainer.java,v 1.3 1998/10/08 00:03:29 nsandhu Exp $
 */
public class CurveContainer extends GEContainer implements ScaledElement {
	private static int _curveNumber = 0;
	private Axis topAxis, leftAxis, bottomAxis, rightAxis;

	/**
   *
   */
	public CurveContainer() {
		this(new GEAttr());
	}

	/**
   *
   */
	public CurveContainer(GEAttr attr) {
		super(attr);
		Scale xs = new Scale(0.0, 1.0, 0, 10);
		Scale ys = new Scale(0.0, 1.0, 0, 10);
		setLayout(new GEScaledLayout(xs, ys));
	}

	/**
   *
   */
	public void addCurve(Curve c) {
		CurveDataModel cdm = c.getModel();
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
		add(new DoubleRect(0.0, 1.0, 1.0, 1.0), c);
	}

	/**
   *
   */
	public void removeCurve(Curve c) {
		remove(c);
		Axis xa = c.getXAxis();
		xa.detachCurve(c);
		Axis ya = c.getYAxis();
		ya.detachCurve(c);
		if (xa.getNumberAttached() == 0)
			removeAxis(xa);
		if (ya.getNumberAttached() == 0)
			removeAxis(ya);
	}

	/**
   *
   */
	void removeAxis(Axis a) {
		remove(a);
		getLayout().removeLayoutElement(a);
	}

	/**
	 * returns the Axis object for given position. If Axis object is null it
	 * returns null.
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
   *
   */
	public Axis createAxis(int pos) {
		Rectangle r = getBounds();
		Axis a = null;
		switch (pos) {
		case AxisAttr.TOP:
			if (topAxis == null) {
				topAxis = new Axis(new AxisAttr(), AxisAttr.TOP);
				Dimension d = topAxis.getPreferredSize();
				topAxis.setBounds(new Rectangle(r.x, r.y - d.height, r.width,
						d.height));
				add(new DoubleRect(0.0, 1.0, 1.0, -0.15), topAxis);
			}
			a = topAxis;
		case AxisAttr.BOTTOM:
			if (bottomAxis == null) {
				bottomAxis = new Axis(new AxisAttr(), AxisAttr.BOTTOM);
				Dimension d = bottomAxis.getPreferredSize();
				bottomAxis.setBounds(new Rectangle(r.x, r.y + r.height,
						r.width, d.height));
				add(new DoubleRect(0.0, 0.0, 1.0, 0.15), bottomAxis);
			}
			a = bottomAxis;
		case AxisAttr.LEFT:
			if (leftAxis == null) {
				leftAxis = new Axis(new AxisAttr(), AxisAttr.LEFT);
				Dimension d = leftAxis.getPreferredSize();
				leftAxis.setBounds(new Rectangle(r.x - d.width, r.y, d.width,
						r.height));
				add(new DoubleRect(0.0, 1.0, -0.15, 1.0), leftAxis);
			}
			a = leftAxis;
		case AxisAttr.RIGHT:
			if (rightAxis == null) {
				rightAxis = new Axis(new AxisAttr(), AxisAttr.RIGHT);
				Dimension d = rightAxis.getPreferredSize();
				rightAxis.setBounds(new Rectangle(r.x + r.width, r.y, d.width,
						r.height));
				add(new DoubleRect(1.0, 1.0, 0.15, 1.0), rightAxis);
			}
			a = rightAxis;
		}
		if (a != null)
			a.setTickGenerator(new SimpleTickGenerator());
		return a;
	}
}
