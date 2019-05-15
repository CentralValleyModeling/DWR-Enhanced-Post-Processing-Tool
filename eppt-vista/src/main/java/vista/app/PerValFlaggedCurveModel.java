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

import vista.graph.AxisAttr;
import vista.graph.CurveDataModel;
import vista.graph.SimpleTickGenerator;
import vista.graph.TickGenerator;
import vista.graph.TimeTickGenerator;
import vista.set.DataSetElement;
import vista.set.DataSetIterator;
import vista.set.ElementFilter;
import vista.set.ElementFilterIterator;
import vista.set.FlagUtils;
import vista.set.RegularTimeSeries;
import vista.time.Time;
import vista.time.TimeFactory;
import vista.time.TimeInterval;

/**
 * An interface containing the data required for drawing a Curve.
 *
 * @author Nicky Sandhu
 * @version $Id: PerValFlaggedCurveModel.java,v 1.1 2003/10/02 20:48:38 redwood
 * Exp $
 */
public class PerValFlaggedCurveModel implements CurveDataModel
{
	private Object _obj;
	private RegularTimeSeries _ds;
	private DataSetIterator _dsi;
	private double _xmax, _xmin, _ymax, _ymin;
	private int _xap, _yap;
	private String _legendName;
	private int _interType = -1;
	private TimeInterval _ti;
	private long _interval;
	private boolean _contextDependentTI, _atIntervalStart, _lastQR;
	private ElementFilter _qF = FlagUtils.QUESTIONABLE_FILTER;
	private ElementFilter _rF = FlagUtils.REJECT_FILTER;

	/**
	 *
	 */
	public PerValFlaggedCurveModel(RegularTimeSeries ds, ElementFilter f,
								   int xAxisPosition, int yAxisPosition, String legend)
	{
		if(ds == null)
		{
			throw new IllegalArgumentException("Null data set");
		}
		_ds = ds;
		if(f == null)
		{
			_dsi = ds.getIterator();
		}
		else
		{
			// _dsi = new ElementFilterCachedIterator(ds.getIterator(), f);
			_dsi = new ElementFilterIterator(ds.getIterator(), f);
		}
		DataSetElement dse = _dsi.getMaximum();
		_xmax = dse.getX();
		_ymax = dse.getY();
		dse = _dsi.getMinimum();
		_xmin = dse.getX();
		_ymin = dse.getY();
		_xap = xAxisPosition;
		_yap = yAxisPosition;
		_legendName = legend;
		_ti = ds.getTimeInterval();
		_contextDependentTI = _ti.isTimeContextDependent();
		if(!_contextDependentTI)
		{
			_interval = _ti.getIntervalInMinutes(null);
		}
		_atIntervalStart = true;
		_lastQR = false;
	}

	/**
	 *
	 */
	public PerValFlaggedCurveModel(RegularTimeSeries ds, ElementFilter f,
								   int xap, int yap)
	{
		this(ds, f, xap, yap, ds.getName());
	}

	/**
	 *
	 */
	public PerValFlaggedCurveModel(RegularTimeSeries ds, ElementFilter f)
	{
		this(ds, f, AxisAttr.BOTTOM, AxisAttr.LEFT, ds.getName());
	}

	/**
	 * sets the maximum value for the current x axis
	 */
	public void setXViewMax(double max)
	{

	}

	/**
	 * gets the minimum value for the current x axis
	 */
	public void setXViewMin(double min)
	{

	}

	/**
	 * gets the maximum value for the x axis
	 */
	public double getXMax()
	{
		return _xmax;
	}

	/**
	 * gets the maximum value for the x axis
	 */
	public double getXMin()
	{
		return _xmin;
	}

	/**
	 * gets the maximum value for the x axis
	 */
	public double getYMax()
	{
		return _ymax;
	}

	/**
	 * gets the maximum value for the x axis
	 */
	public double getYMin()
	{
		return _ymin;
	}

	/**
	 * an object associated with this model.
	 */
	public Object getReferenceObject()
	{
		return _obj;
	}

	/**
	 * an object associated with this model.
	 */
	public void setReferenceObject(Object obj)
	{
		_obj = obj;
	}

	/**
	 * gets the interpolation type of the data
	 */
	public int getInterpolationType()
	{
		return PER_VAL;
	}

	/**
	 * resets the data iterator to beginning of curve
	 */
	public void reset()
	{
		_dsi.resetIterator();
		_atIntervalStart = true;
	}

	/**
	 * gets the next point
	 *
	 * @param points is an array wher points[0] contains the next x value and
	 *               points[1] contains the next y value
	 * @return an integer specifing movevment only or line drawing motion
	 */
	public int nextPoint(double[] points)
	{
		int lineType = LINE_TO;
		DataSetElement dse = _dsi.getElement();
		if(!_qF.isAcceptable(dse))
		{
			points[0] = dse.getX();
			points[1] = dse.getY();
			lineType = QUESTIONABLE_AT;
			_atIntervalStart = true;
			_lastQR = true;
			_dsi.advance();
		}
		else if(!_rF.isAcceptable(dse))
		{
			points[0] = dse.getX();
			points[1] = dse.getY();
			lineType = REJECT_AT;
			_atIntervalStart = true;
			_lastQR = true;
			_dsi.advance();
		}
		else if(_atIntervalStart)
		{
			double val = getBeginningIntervalValue(dse.getX());
			points[0] = val;
			points[1] = dse.getY();
			if(_dsi.atStart() || _dsi.hasSkipped() > 0 || _lastQR)
			{
				lineType = MOVE_TO;
			}
			else
			{
				lineType = LINE_TO;
			}
			_lastQR = false;
			_atIntervalStart = false;
		}
		else
		{
			points[0] = dse.getX();
			points[1] = dse.getY();
			lineType = LINE_TO;
			_atIntervalStart = true;
			_dsi.advance();
		}
		return lineType;
	}

	/**
	 *
	 */
	public double getBeginningIntervalValue(double x)
	{
		if(_contextDependentTI)
		{
			Time tm = TimeFactory.getInstance().createTime(Math.round(x));
			tm.incrementBy(_ti, -1);
			return tm.getTimeInMinutes();
		}
		else
		{
			return (x - _interval);
		}
	}

	/**
	 * @return true while has more points on curve
	 */
	public boolean hasMorePoints()
	{
		return !_dsi.atEnd() || !_atIntervalStart;
	}

	/**
	 * gets teh legend text for this curve
	 */
	public String getLegendText()
	{
		return _legendName;
	}

	/**
	 * get the x axis position for this curve
	 */
	public int getXAxisPosition()
	{
		return _xap;
	}

	/**
	 * geth the y axis position for this curve
	 */
	public int getYAxisPosition()
	{
		return _yap;
	}

	/**
	 * get the tick generator for x axis
	 */
	public TickGenerator getXAxisTickGenerator()
	{
		return new TimeTickGenerator();
	}

	/**
	 * get the tick generator for the y axis
	 */
	public TickGenerator getYAxisTickGenerator()
	{
		return new SimpleTickGenerator();
	}

	/**
	 * set filter on this model
	 */
	public void setFilter(Object f)
	{
		if(f == null)
		{
			_dsi = _ds.getIterator();
		}
		else
		{
			if(f instanceof ElementFilter)
			{
				_dsi = new ElementFilterIterator(_ds.getIterator(),
						(ElementFilter) f);
			}
		}
	}
}
