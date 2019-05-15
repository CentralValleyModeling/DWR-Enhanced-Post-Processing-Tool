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
package vista.set;

/**
 * 
 * 
 * @author Nicky Sandhu
 * @version $Id: Utils.java,v 1.1 2003/10/02 20:49:34 redwood Exp $
 */
public class Utils {
	/**
	 * Fits a straight line y = ax+b where DataSet has (x,y) values. Returns the
	 * value of a, b, and goodness of fit.
	 */
	public static RegressionLine linearLSRegression(DataSet ds) {
		if (ds == null)
			throw new IllegalArgumentException(
					"Null data set? can't do regression");
		double sigXi = 0;
		double sigYi = 0;
		double sigXi2 = 0;
		double sigXiYi = 0;
		double n = ds.size();
		if (n <= 2)
			throw new IllegalArgumentException(
					"Too few points to do regression");
		//
		DataSetIterator dsi = new ElementFilterIterator(ds.getIterator(),
				Constants.DEFAULT_FILTER);
		while (!dsi.atEnd()) {
			DataSetElement dse = dsi.getElement();
			double x = dse.getX(), y = dse.getY();
			sigXi += x;
			sigYi += y;
			sigXi2 += x * x;
			sigXiYi += x * y;
			dsi.advance();
		}
		//
		double del = n * sigXi2 - (sigXi * sigXi);
		double a = (sigXi2 * sigYi - sigXi * sigXiYi) / del;
		double b = (n * sigXiYi - sigXi * sigYi) / del;
		double siga = Math.sqrt(sigXi2 / del);
		double sigb = Math.sqrt(n / del);
		double covab = -sigXi / del;
		double rab = -sigXi / (Math.sqrt(n * sigXi2));
		double chiSq = 0.0;
		dsi.resetIterator();
		while (!dsi.atEnd()) {
			DataSetElement dse = dsi.getElement();
			double x = dse.getX(), y = dse.getY();
			//
			double s = (y - a + b * x);
			chiSq += s * s;
			//
			dsi.advance();
		}
		double sigD = Math.sqrt(chiSq / (n - 2));
		siga *= sigD;
		sigb *= sigD;
		return new RegressionLine(a, b, siga, sigb, chiSq, rab, covab);
	}
}
