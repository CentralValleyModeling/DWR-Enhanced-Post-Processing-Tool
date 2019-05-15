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
 * Generates information for tick placement and their associated labels using a
 * range of given maximum and minimum.
 *
 * @author Nicky Sandhu
 * @version $Id: TickGenerator.java,v 1.1 2003/10/02 20:49:10 redwood Exp $
 */
public interface TickGenerator
{
	/**
	 * Generates ticks and labels given the minimum and maximum of the data
	 * range.
	 *
	 * @param minimum The minimum value of the data range
	 * @param maximum The maximum value of the data range
	 */
	void generate(double minimum, double maximum);

	/**
	 * Returns an array of TickData. Be sure to call generate to initialize the
	 * generator before calling this function
	 *
	 * @return array of TickData
	 * @see TickData
	 * @see TickGenerator#generate
	 */
	TickData[] getTickData();

	/**
	 * Returns the array of labels for major ticks. Be sure to call generate to
	 * initialize the generator before calling this function
	 *
	 * @return array of strings
	 * @see TickGenerator#generate
	 */
	String[] getLabels();

	/**
	 * gets the format object used in generating the labels
	 */
	java.text.Format getFormatter();

	/**
	 * a suggestion to the tick generator to use the beginning and ending data
	 * points when generating labels.
	 */
	void useDataMinMax(boolean useIt);
}
