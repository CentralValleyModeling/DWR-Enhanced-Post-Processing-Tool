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

/**
 * An interface containing the data required for drawing a Curve.
 *
 * @author Nicky Sandhu
 * @version $Id: CurveDataModel.java,v 1.1 2003/10/02 20:48:52 redwood Exp $
 */
public interface CurveDataModel
{
	/**
	 * draws lines connecting points
	 */
	int INST_VAL = 0;
	/**
	 * draws a horizontal line for the period
	 */
	int PER_VAL = 1;
	/**
	 * draws the horizontal line at the last value
	 */
	int LAST_VAL = 2;
	/**
	 *
	 */
	int MOVE_TO = 1;
	/**
	 *
	 */
	int LINE_TO = 2;
	/**
	 *
	 */
	int QUESTIONABLE_AT = 3;
	/**
	 *
	 */
	int REJECT_AT = 4;

	/**
	 * an object associated with this model.
	 */
	Object getReferenceObject();

	/**
	 * an object associated with this model.
	 */
	void setReferenceObject(Object obj);

	/**
	 * sets the maximum value for the current x axis
	 */
	void setXViewMax(double max);

	/**
	 * gets the minimum value for the current x axis
	 */
	void setXViewMin(double min);

	/**
	 * gets the maximum value for the x axis
	 */
	double getXMax();

	/**
	 * gets the maximum value for the x axis
	 */
	double getXMin();

	/**
	 * gets the maximum value for the x axis
	 */
	double getYMax();

	/**
	 * gets the maximum value for the x axis
	 */
	double getYMin();

	/**
	 * gets the interpolation type of the data
	 */
	int getInterpolationType();

	/**
	 * resets the data iterator to beginning of curve
	 */
	void reset();

	/**
	 * gets the next point
	 *
	 * @param points is an array wher points[0] contains the next x value and
	 *               points[1] contains the next y value
	 * @return an integer specifing movevment only or line drawing motion
	 */
	int nextPoint(double[] points);

	/**
	 * @return true while has more points on curve
	 */
	boolean hasMorePoints();

	/**
	 * gets teh legend text for this curve
	 */
	String getLegendText();

	/**
	 * get the x axis position for this curve
	 */
	int getXAxisPosition();

	/**
	 * geth the y axis position for this curve
	 */
	int getYAxisPosition();

	/**
	 * get the tick generator for x axis
	 */
	TickGenerator getXAxisTickGenerator();

	/**
	 * get the tick generator for the y axis
	 */
	TickGenerator getYAxisTickGenerator();

	/**
	 * sets filter, I don't want to determine the exact class to not allow
	 * dependency between this package and other packages
	 */
	void setFilter(Object filter);
	/*
	 * a rudimentary method to be called if data changes. A more sophisticated
	 * model will have to be made when curve editing is to be done. public void
	 * dataChanged();
	 */
}
