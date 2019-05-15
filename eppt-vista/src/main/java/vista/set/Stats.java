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
 * @version $Id: Stats.java,v 1.1 2003/10/02 20:49:33 redwood Exp $
 */
public class Stats {
	/**
   *
   */
	public static double max(DataSet ds) {
		return new ElementFilterIterator(ds.getIterator(),
				Constants.DEFAULT_FLAG_FILTER).getMaximum().getY();
	}

	/**
   *
   */
	public static double min(DataSet ds) {
		return new ElementFilterIterator(ds.getIterator(),
				Constants.DEFAULT_FLAG_FILTER).getMinimum().getY();
	}

	/**
   *
   */
	public static double avg(DataSet ds) {
		DataSetIterator dsi = new ElementFilterIterator(ds.getIterator(),
				Constants.DEFAULT_FLAG_FILTER);
		double sum = 0.0;
		int nsums = 0;
		for (dsi.resetIterator(); !dsi.atEnd(); dsi.advance()) {
			sum += dsi.getElement().getY();
			nsums++;
		}
		if (nsums == 0)
			throw new IllegalArgumentException("No values for data set: "
					+ ds.getName());
		return sum / nsums;
	}

	/**
	 * total of all values in the data set
	 */
	public static double total(DataSet ds) {
		DataSetIterator dsi = new ElementFilterIterator(ds.getIterator(),
				Constants.DEFAULT_FLAG_FILTER);
		double sum = 0.0;
		int nsums = 0;
		for (dsi.resetIterator(); !dsi.atEnd(); dsi.advance()) {
			sum += dsi.getElement().getY();
			nsums++;
		}
		if (nsums == 0)
			throw new IllegalArgumentException("No values for data set: "
					+ ds.getName());
		return sum;
	}

	/**
	 * standard deviation of all the acceptable elements y values
	 */
	public static double sdev(DataSet ds) {
		double xm = avg(ds);
		DataSetIterator dsi = new ElementFilterIterator(ds.getIterator(),
				Constants.DEFAULT_FLAG_FILTER);
		int count = 0;
		double sum = 0;
		for (; !dsi.atEnd(); dsi.advance()) {
			double d = dsi.getElement().getY() - xm;
			sum += d * d;
			count++;
		}
		return Math.sqrt(sum / (count - 1));
		// ??? have to check this ^
	}

	/**
   *
   */
	public static int countAcceptable(DataSet ds, ElementFilter f) {
		DataSetIterator dsi = ds.getIterator();
		int sum = 0;
		for (dsi.resetIterator(); !dsi.atEnd(); dsi.advance()) {
			if (f.isAcceptable(dsi.getElement()))
				sum++;
		}
		return sum;
	}

	/**
   *
   */
	public static int countUnacceptable(DataSet ds, ElementFilter f) {
		DataSetIterator dsi = ds.getIterator();
		int sum = 0;
		for (dsi.resetIterator(); !dsi.atEnd(); dsi.advance()) {
			if (!f.isAcceptable(dsi.getElement()))
				sum++;
		}
		return sum;
	}

	/**
   *
   */
	public static int countMissing(DataSet ds) {
		return countUnacceptable(ds, Constants.DEFAULT_FILTER);
	}

	/**
   *
   */
	public static int countOK(DataSet ds) {
		return countUnacceptable(ds, FlagUtils.OK_FILTER);
	}

	/**
   *
   */
	public static int countQuestionable(DataSet ds) {
		return countUnacceptable(ds, FlagUtils.QUESTIONABLE_FILTER);
	}

	/**
   *
   */
	public static int countReject(DataSet ds) {
		return countUnacceptable(ds, FlagUtils.REJECT_FILTER);
	}
}
