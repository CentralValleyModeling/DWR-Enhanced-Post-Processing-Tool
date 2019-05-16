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

import java.awt.geom.AffineTransform;
import java.awt.geom.PathIterator;

/**
 * A PathIterator for the Curve to outline its shape. This implementation is to
 * avoid using GeneralPath as that would be very expensive in terms of memory
 * and speed.
 *
 * @author Nicky Sandhu
 * @version $Id: CurvePathIterator.java,v 1.1 2003/10/02 20:48:53 redwood Exp $
 */
public class CurvePathIterator implements PathIterator
{
	/**
	 *
	 */
	private CurveDataModel _cdm;
	private double[] _points = {0.0, 0.0};
	private int _type;
	private boolean atStart;
	private AffineTransform _at;

	/**
	 * initializes with a curve data model and resets itself
	 */
	public CurvePathIterator(CurveDataModel cdm, AffineTransform at)
	{
		_cdm = cdm;
		_cdm.reset();
		_type = _cdm.nextPoint(_points);
		atStart = true;
		_at = at;
	}

	/**
	 * winding rule set to WIND_EVEN_ODD
	 */
	public int getWindingRule()
	{
		return PathIterator.WIND_EVEN_ODD;
	}

	/**
	 *
	 */
	public boolean isDone()
	{
		return (!_cdm.hasMorePoints());
	}

	/**
	 *
	 */
	public void next()
	{
		_type = _cdm.nextPoint(_points);
	}

	/**
	 *
	 */
	public int currentSegment(float[] coords)
	{
		double m00 = _at.getScaleX();
		double m02 = _at.getTranslateX();
		double m10 = _at.getScaleY();
		double m12 = _at.getTranslateY();
		coords[0] = (float) Math.round(_points[0] * m00 + m02);
		coords[1] = (float) Math.round(_points[1] * m10 + m12);
		if(atStart)
		{
			atStart = false;
			return SEG_MOVETO;
		}
		else
		{
			if(_type == CurveDataModel.MOVE_TO)
			{
				return SEG_MOVETO;
			}
			else
			{
				return SEG_LINETO;
			}
		}
	}

	/**
	 *
	 */
	public int currentSegment(double[] coords)
	{
		if(atStart)
		{
			coords[0] = _points[0];
			coords[1] = _points[1];
			atStart = false;
			return SEG_MOVETO;
		}
		else
		{
			coords[0] = _points[0];
			coords[1] = _points[1];
			if(_type == CurveDataModel.MOVE_TO)
			{
				return SEG_MOVETO;
			}
			else
			{
				return SEG_LINETO;
			}
		}
	}
}
