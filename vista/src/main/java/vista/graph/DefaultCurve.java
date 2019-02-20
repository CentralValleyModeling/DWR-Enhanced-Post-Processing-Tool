/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package vista.graph;

import java.awt.Graphics;
import java.awt.Rectangle;

/**
 * Draws a default curve. It understands only linear and last value
 * interpolation types. Also both x and y coordinates are believed to be real
 * numbers
 *
 * @author Nicky Sandhu
 * @version $Id: DefaultCurve.java,v 1.1 2003/10/02 20:48:53 redwood Exp $
 */
public class DefaultCurve extends Curve
{
	private double[] _points;
	private boolean _clipping = true;

	/**
	 * a default curve
	 */
	public DefaultCurve(CurveDataModel cdm)
	{
		this(new CurveAttr(), cdm);
	}

	/**
	 * a default curve
	 */
	public DefaultCurve(CurveAttr attributes, CurveDataModel cdm)
	{
		super(attributes, cdm);
		_points = new double[2];
		setName(cdm.getLegendText());
	}

	/**
	 *
	 */
	public boolean getClipping()
	{
		return _clipping;
	}

	/**
	 *
	 */
	public void setClipping(boolean b)
	{
		_clipping = b;
	}

	/**
	 * Draws the data by scaling it and joining consecutive data points and/or
	 * plotting symbols for data points.
	 */
	protected void drawCurve()
	{
		CurveDataModel cdm = getModel();
		Axis xAxis = getXAxis();
		Axis yAxis = getYAxis();
		if(cdm == null)
		{
			return;
		}

		Graphics gc = getGraphics();
		Rectangle r = getBounds();

		CurveAttr attr = (CurveAttr) getAttributes();
		Symbol symbol = attr._symbol;
		Rectangle symbolBounds = new Rectangle(0, 0, 25, 25);

		gc.setColor(attr._foregroundColor);

		int x1 = 0, y1 = 0, x2 = 0, y2 = 0;

		int lineType;
		int interType = cdm.getInterpolationType();
		Scale xScale = xAxis.getScale();
		Scale yScale = yAxis.getScale();
		// reset to beginning
		cdm.reset();
		if(!cdm.hasMorePoints())
		{
			return; // empty data set
		}
		lineType = cdm.nextPoint(_points);
		x1 = xScale.scaleToUC(_points[0]);
		y1 = yScale.scaleToUC(_points[1]);
		int index = 0;
		int numMoveTo = 0; // a counter for number of move_to's in a sequence
		// loop throught the points
		while(cdm.hasMorePoints())
		{
			lineType = cdm.nextPoint(_points);

			if(lineType == CurveDataModel.MOVE_TO)
			{
				x1 = xScale.scaleToUC(_points[0]);
				y1 = yScale.scaleToUC(_points[1]);
				numMoveTo++;
			}
			else
			{
				numMoveTo = 0;
			}

			x2 = xScale.scaleToUC(_points[0]);
			y2 = yScale.scaleToUC(_points[1]);
			if(numMoveTo == 2)
			{
				x2 = x1 + 3; // 2 MOVE_TO's in a row
			}
			if(attr._drawLines)
			{
				if(!_clipping || (r.contains(x1, y1) || r.contains(x2, y2)))
				{
					if(interType == CurveDataModel.INST_VAL)
					{
						gc.drawLine(x1, y1, x2, y2);
					}
					else if(interType == CurveDataModel.LAST_VAL)
					{
						gc.drawLine(x1, y1, x1, y2);
						gc.drawLine(x1, y2, x2, y2);
					}
					else
					{
						gc.drawLine(x1, y1, x2, y2);
					}
				}
			}
			// make sure first symbol gets drawn
			if(attr._drawSymbol && index % attr._dataPerSymbol == 0)
			{
				symbolBounds.x = x1;
				symbolBounds.y = y1;
				symbol.draw(gc, symbolBounds);
			}

			x1 = x2;
			y1 = y2;
			index++;
		} // end loop
		// for symbols make sure the last symbol gets drawn.
		if(attr._drawSymbol)
		{
			symbolBounds.x = x1;
			symbolBounds.y = y1;
			symbol.draw(gc, symbolBounds);
		}

	}
}
