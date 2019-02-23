/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package vista.graph;

/**
 * This class encapsulates scaling information and methods. This handles
 * conversions between a linear data coordinate scale (DC) and a user coordinate
 * scale (UC). Note: For now DC is in units of double and UC in units of int.
 * This will be changed later.
 *
 * @author Nicky Sandhu (DWR).
 * @version $Id: Scale.java,v 1.1 2003/10/02 20:49:08 redwood Exp $
 */
public class Scale
{
	/**
	 * The data minimum value;
	 */
	private double _dmin;
	/**
	 * The data minimum value;
	 */
	private double _dmax;
	/**
	 * The user minimum value;
	 */
	private int _amin;
	/**
	 * The user minimum value;
	 */
	private int _amax;
	/**
	 * The scale or ratio of data range to user range.
	 */
	private double _scale;

	/**
	 * constructor
	 */
	public Scale(double dmin, double dmax, int amin, int amax)
	{
		_dmin = dmin;
		_dmax = dmax;
		_amin = amin;
		_amax = amax;
		_scale = (_dmax - _dmin) / (_amax - _amin);
	}

	/**
	 * sets scale with new user coordinates
	 */
	public synchronized void setUCRange(int amin, int amax)
	{
		_amin = amin;
		_amax = amax;
		_scale = (_dmax - _dmin) / (_amax - _amin);
	}

	/**
	 * sets scale with new data coordinates
	 */
	public synchronized void setDCRange(double dmin, double dmax)
	{
		_dmin = dmin;
		_dmax = dmax;
		_scale = (_dmax - _dmin) / (_amax - _amin);
	}

	/**
	 * scales to user coordinates
	 */
	public int scaleToUC(double v)
	{
		return (int) Math.round(_amin + (v - _dmin) / _scale);
	}

	/**
	 * scales to data coordinates
	 */
	public double scaleToDC(int v)
	{
		return (_dmin + (v - _amin) * _scale);
	}

	/**
	 * translate with respect to current scale
	 */
	public void translate(int t)
	{
		translate(_scale * t);
	}

	/**
	 * translate with respect to current scale
	 */
	public void translate(double dt)
	{
		_dmin += dt;
		_dmax += dt;
	}

	/**
	 * Description of instance
	 */
	public String toString()
	{
		return ("Scale: " + _scale + " Range : amin " + _amin + " amax "
				+ _amax + " dmin " + _dmin + " dmax " + _dmax);
	}

	/**
	 * @return maximum data value
	 */
	public double getDataMaximum()
	{
		return _dmax;
	}

	/**
	 * @return minimum data value
	 */
	public double getDataMinimum()
	{
		return _dmin;
	}

	/**
	 *
	 */
	public double getScaling()
	{
		return _amin + (1.0 - _dmin) / _scale - getShift();
	}

	/**
	 *
	 */
	public double getShift()
	{
		return _amin + (0 - _dmin) / _scale;
	}
}
