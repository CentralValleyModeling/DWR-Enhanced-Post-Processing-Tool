/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package vista.set;

/**
 * 
 * 
 * @author Nicky Sandhu
 * @version $Id: RegressionLine.java,v 1.1 2003/10/02 20:49:30 redwood Exp $
 */
public class RegressionLine {
	/**
   *
   */
	public RegressionLine(double a, double b, double siga, double sigb,
			double chiSq, double rab, double covab) {
		_a = a;
		_b = b;
		_siga = siga;
		_sigb = sigb;
		_chiSq = chiSq;
		_rab = rab;
		_covab = covab;
	}

	/**
   *
   */
	public double getSlope() {
		return _b;
	}

	/**
   *
   */
	public double getIntercept() {
		return _a;
	}

	/**
   *
   */
	public double getCovariance() {
		return _covab;
	}

	/**
   *
   */
	public double getCorelation() {
		return _rab;
	}

	/**
   *
   */
	public double getVarianceA() {
		return _siga;
	}

	/**
   *
   */
	public double getVarianceB() {
		return _sigb;
	}

	/**
   *
   */
	public double getChiSquare() {
		return _chiSq;
	}

	/**
   *
   */
	public DataSet generateDataSet(DataSet ds) {
		DataSetIterator dsi = ds.getIterator();
		double[] ym = new double[ds.size()];
		double[] xm = new double[ds.size()];
		int index = 0;
		while (true) {
			DataSetElement dse = dsi.getElement();
			double x = dse.getX(), y = dse.getY();
			//
			xm[index] = x;
			ym[index] = _a + _b * x;
			if (dsi.atEnd())
				break;
			dsi.advance();
			index++;
		}
		return new DefaultDataSet(ds.getName() + "(Regression Model)", xm, ym,
				null, ds.getAttributes());
	}

	/**
   *
   */
	private double _a, _b, _siga, _sigb, _chiSq, _rab, _covab;
} // endo of class RegressionLine
