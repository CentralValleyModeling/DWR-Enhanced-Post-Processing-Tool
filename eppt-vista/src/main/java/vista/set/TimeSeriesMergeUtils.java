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

import vista.time.TimeInterval;
import vista.time.TimeWindow;

public class TimeSeriesMergeUtils
{

	private static final int INITIAL_SIZE = 10000;

	/**
	 * Replaces time series in place only if there is an overlay
	 *
	 * @param original
	 * @param replacer
	 */
	public static void replaceInPlace(TimeSeries original, TimeSeries replacer)
	{
		MultiIterator iterator = new MultiIterator(new TimeSeries[]{original,
				replacer});
		while(!iterator.atEnd())
		{
			DataSetElement dse = iterator.getElement();
			if(!Double.isNaN(dse.getX(2)) && !Double.isNaN(dse.getX(1)))
			{
				dse.setX(1, dse.getX(2));
				if(original.isFlagged() && replacer.isFlagged())
				{
					dse.setFlag(0, dse.getFlag(1));
				}
				iterator.putElement(dse);
			}
			iterator.advance();
		}
	}

	/**
	 * This function replaces the original series with values from the
	 * "replacer". Note that this applies even if the replacer has missing
	 * values and there are values that are not missing in the "original"
	 * series.
	 * <p>
	 * Timewindow of the resulting series either remains the same or is expanded
	 * to accomodate the replacer timewindow
	 *
	 * @param original
	 * @param replacer
	 * @return
	 */
	public static TimeSeries replace(TimeSeries original, TimeSeries replacer)
	{
		TimeWindow tw = original.getTimeWindow()
								.union(replacer.getTimeWindow());
		original = pad(original, tw);
		replacer = pad(replacer, tw);
		MultiIterator iterator = new MultiIterator(new TimeSeries[]{original,
				replacer});
		boolean isAllRegular = false;
		if(original instanceof RegularTimeSeries
				&& replacer instanceof RegularTimeSeries)
		{
			isAllRegular = true;
		}
		double[] y = new double[INITIAL_SIZE];
		int[] flags = new int[INITIAL_SIZE];
		double[] x = null;
		if(isAllRegular)
		{
			x = new double[INITIAL_SIZE];
		}
		int index = 0;
		while(!iterator.atEnd())
		{
			DataSetElement e = calculateReplacedElement(iterator.getElement());
			index = iterator.getIndex();
			if(index >= y.length)
			{
				y = resize(y);
				if(x != null)
				{
					x = resize(x);
				}
			}
			if(index >= flags.length)
			{
				flags = resize(flags);
			}
			if(x != null)
			{
				x[index] = e.getX();
			}
			y[index] = e.getY();
			flags[index] = e.getFlag();
			iterator.advance();
		}
		index++;
		if(x != null)
		{
			x = resizeTo(x, index);
		}
		y = resizeTo(y, index);
		flags = resizeTo(flags, index);

		if(isAllRegular)
		{
			DataSetAttr attr1 = original.getAttributes();
			DataSetAttr attr = new DataSetAttr(DataType.REGULAR_TIME_SERIES,
					"TIME", attr1.getYUnits(), "", attr1.getYType());
			String name = original.getName();
			TimeInterval ti = getInterval(new TimeSeries[]{original, replacer});
			return new RegularTimeSeries(name, tw.getStartTime(), ti, y, flags,
					attr);
		}
		else
		{
			DataSetAttr attr1 = original.getAttributes();
			DataSetAttr attr = new DataSetAttr(DataType.IRREGULAR_TIME_SERIES,
					"TIME", attr1.getYUnits(), "", attr1.getYType());
			String name = original.getName();
			return new IrregularTimeSeries(name, x, y, flags, attr);
		}
	}

	public static TimeSeries pad(TimeSeries original, TimeWindow tw)
	{
		if(original instanceof RegularTimeSeries)
		{
			TimeInterval ti = ((RegularTimeSeries) original).getTimeInterval();
			int size = (int) tw.getStartTime().getExactNumberOfIntervalsTo(
					tw.getEndTime(), ti) + 1;
			int si = (int) tw.getStartTime().getExactNumberOfIntervalsTo(
					original.getStartTime(), ti);
			int ei = (int) tw.getStartTime().getExactNumberOfIntervalsTo(
					original.getEndTime(), ti);
			int osi = (int) original.getStartTime()
									.getExactNumberOfIntervalsTo(tw.getStartTime(), ti);
			int oei = (int) original.getStartTime()
									.getExactNumberOfIntervalsTo(tw.getEndTime(), ti);

			double[] origy = ((RegularTimeSeries) original).getYArray();
			int[] origflags = ((RegularTimeSeries) original).getFlagArray();
			double[] newy = new double[size];
			int[] newflags = null;
			if(origflags != null)
			{
				newflags = new int[size];
			}
			for(int i = 0; i < newy.length; i++)
			{
				newy[i] = Double.NaN;
				if(origflags != null)
				{
					newflags[i] = 0;
				}
			}

			int origPos = 0;
			int origLen = 0;
			int newPos = 0;
			if(si > size || ei < 0)
			{ // non-overlapping
				//leave as is
			}
			if(si >= 0 && si < size)
			{
				origPos = 0;
				newPos = si;
				origLen = Math.min(origy.length, size - si);
			}
			if(si < 0 && ei > 0)
			{
				origPos = -si;
				newPos = 0;
				origLen = Math.min(ei + 1, size);
			}


			System.arraycopy(origy, origPos, newy, newPos, origLen);
			if(origflags != null)
			{
				System.arraycopy(origflags, origPos, newflags, newPos, origLen);
			}
			String name = original.getName();
			DataSetAttr attr = original.getAttributes().createClone();
			return new RegularTimeSeries(name, tw.getStartTime(), ti, newy,
					newflags, attr);
		}
		else
		{
			IrregularTimeSeries its = (IrregularTimeSeries) original;
			String name = original.getName();
			DataSetAttr attr = original.getAttributes().createClone();
			TimeWindow itsTimeWindow = its.getTimeWindow();
			double[] xArray = its.getXArray();
			double[] yArray = its.getYArray();
			int[] flagArray = its.getFlagArray();
			if(tw.getStartTime().compare(itsTimeWindow.getStartTime()) < 0)
			{
				double[] newXArray = new double[xArray.length + 1];
				newXArray[0] = tw.getStartTime().getTimeInMinutes();
				System.arraycopy(xArray, 0, newXArray, 1, xArray.length);
				xArray = newXArray;
				double[] newYArray = new double[yArray.length + 1];
				newYArray[0] = Constants.MISSING_VALUE;
				System.arraycopy(yArray, 0, newYArray, 1, yArray.length);
				yArray = newYArray;
				if(flagArray != null)
				{
					int[] newFlagArray = new int[flagArray.length + 1];
					newFlagArray[0] = 0;
					System.arraycopy(flagArray, 0, newFlagArray, 1,
							flagArray.length);
					flagArray = newFlagArray;
				}
			}
			if(tw.getEndTime().compare(itsTimeWindow.getStartTime()) > 0)
			{
				double[] newXArray = new double[xArray.length + 1];
				newXArray[newXArray.length - 1] = tw.getStartTime()
													.getTimeInMinutes();
				System.arraycopy(xArray, 0, newXArray, 0, xArray.length);
				xArray = newXArray;
				double[] newYArray = new double[yArray.length + 1];
				newYArray[newYArray.length - 1] = Constants.MISSING_VALUE;
				System.arraycopy(yArray, 0, newYArray, 0, yArray.length);
				yArray = newYArray;
				if(flagArray != null)
				{
					int[] newFlagArray = new int[flagArray.length + 1];
					newFlagArray[newYArray.length - 1] = 0;
					System.arraycopy(flagArray, 0, newFlagArray, 0,
							flagArray.length);
					flagArray = newFlagArray;
				}
			}
			return new IrregularTimeSeries(name, xArray, yArray, flagArray,
					attr);
		}
	}

	/**
	 * calculates the element replaced by the second value and flag unless those
	 * are missing
	 *
	 * @param element
	 * @return
	 */
	public static DataSetElement calculateReplacedElement(DataSetElement element)
	{
		DataSetElement e = new FlaggedDataSetElement();
		double y = element.getX(1);
		int flag = element.getFlag(0);
		if(!Double.isNaN(element.getX(2)))
		{
			y = element.getX(2);
			flag = element.getFlag(1);
		}
		e.setX(element.getX());
		e.setY(y);
		e.setFlag(flag);
		return e;
	}

	/**
	 * This method creates a merged time series based on the provided list of
	 * timeseries, the timewindow and certain rules. Rules are as follows a.)
	 * Merged value is the value sorted by order of list and flag value. Flag
	 * values are sorted by the rule GOOD > UNSCREENED or NOFLAGS > QUESTIONABLE
	 * > REJECT > MISSING. The first value that is not MISSING (-901.0 or
	 * -902.0) is selected for the merged value along with the flag if any
	 *
	 * @param tsList
	 * @param tw
	 * @return
	 */
	public static TimeSeries merge(TimeSeries[] tsArray, TimeWindow tw)
	{
		for(int i = 0; i < tsArray.length; i++)
		{
			TimeSeries ts = tsArray[i];
			tsArray[i] = pad(ts, tw);
		}
		MultiIterator iterator = new MultiIterator(tsArray);
		boolean isAllRegular = isAllRegular(tsArray);
		double[] y = new double[INITIAL_SIZE];
		int[] flags = new int[INITIAL_SIZE];
		double[] x = null;
		if(!isAllRegular)
		{
			x = new double[INITIAL_SIZE];
		}
		int index = 0;
		while(!iterator.atEnd())
		{
			DataSetElement e = calculateMergedElement(iterator.getElement());
			index = iterator.getIndex();
			if(index >= y.length)
			{
				y = resize(y);
				if(x != null)
				{
					x = resize(x);
				}
			}
			if(index >= flags.length)
			{
				flags = resize(flags);
			}
			if(x != null)
			{
				x[index] = e.getX();
			}
			y[index] = e.getY();
			flags[index] = e.getFlag();
			iterator.advance();
		}
		if(x != null)
		{
			x = resizeTo(x, index);
		}
		y = resizeTo(y, index);
		flags = resizeTo(flags, index);
		if(isAllRegular)
		{
			DataSetAttr attr1 = tsArray[0].getAttributes();
			DataSetAttr attr = new DataSetAttr(DataType.REGULAR_TIME_SERIES,
					"TIME", attr1.getYUnits(), "", attr1.getYType());
			String name = tsArray[0].getName();
			return new RegularTimeSeries(name, tw.getStartTime(),
					getInterval(tsArray), y, flags, attr);
		}
		else
		{
			DataSetAttr attr1 = tsArray[0].getAttributes();
			DataSetAttr attr = new DataSetAttr(DataType.IRREGULAR_TIME_SERIES,
					"TIME", attr1.getYUnits(), "", attr1.getYType());
			String name = tsArray[0].getName();
			return new IrregularTimeSeries(name, x, y, flags, attr);
		}
	}

	/**
	 * calculates the merge element value and flag based on this multi valued
	 * element
	 *
	 * @param element
	 * @return
	 */
	public static DataSetElement calculateMergedElement(DataSetElement element)
	{
		int n = element.getDimension();
		DataSetElement e = new FlaggedDataSetElement();
		double y = Constants.MISSING_VALUE;
		FlagUtils.setQualityFlag(e, FlagUtils.MISSING_FLAG, 0);
		int flag = e.getFlag();
		for(int i = 1; i < n; i++)
		{
			double ey = element.getX(i);
			int eflag = element.getFlag(i - 1);
			if(isBetterQuality(eflag, flag) && !Double.isNaN(ey))
			{
				y = ey;
				flag = eflag;
			}
		}
		e.setX(element.getX());
		e.setY(y);
		e.setFlag(flag);
		return e;
	}

	/**
	 * returns true if eflag is better quality flag than flag
	 *
	 * @param eflag
	 * @param flag
	 * @return
	 */
	public static boolean isBetterQuality(int eflag, int flag)
	{
		int eflagQuality = FlagUtils.getQualityFlag(eflag);
		int flagQuality = FlagUtils.getQualityFlag(flag);
		return getFlagRanking(eflagQuality) > getFlagRanking(flagQuality);
	}

	/**
	 * Assigns a ranking value to each flag, where the ranking value is higher
	 * for OK > UNSCREENED > QUESTIONABLE > REJECT > MISSING
	 *
	 * @param flagType
	 * @return
	 */
	public static int getFlagRanking(int flagType)
	{
		switch(flagType)
		{
			case FlagUtils.OK_FLAG:
				return 10000;
			case FlagUtils.UNSCREENED_FLAG:
				return 5000;
			case FlagUtils.QUESTIONABLE_FLAG:
				return 2000;
			case FlagUtils.REJECT_FLAG:
				return 1000;
			case FlagUtils.MISSING_FLAG:
				return 0;
			default:
				return 0;
		}
	}

	public static TimeInterval getInterval(TimeSeries[] tsArray)
	{
		if(isAllRegular(tsArray))
		{
			TimeInterval ti = null;
			for(int i = 0; i < tsArray.length; i++)
			{
				TimeSeries ts = tsArray[i];
				RegularTimeSeries rts = (RegularTimeSeries) ts;
				if(ti == null)
				{
					ti = rts.getTimeInterval();
				}
				else
				{
					int compare = rts.getTimeInterval().compare(ti);
					if(compare != 0)
					{
						String msg = "Time Series: " + rts.getName()
								+ "with time interval: "
								+ rts.getTimeInterval()
								+ " does not have expected time interval " + ti
								+ " as others in merge list";
						throw new RuntimeException(msg);
					}
				}
			}
			return ti;
		}
		else
		{
			return null;
		}

	}

	public static boolean isAllRegular(TimeSeries[] tsArray)
	{
		for(int i = 0; i < tsArray.length; i++)
		{
			TimeSeries ts = tsArray[i];
			if(!(ts instanceof RegularTimeSeries))
			{
				return false;
			}
		}
		return true;
	}

	private static double[] resize(double[] array)
	{
		double[] narray = new double[array.length * 2];
		System.arraycopy(array, 0, narray, 0, array.length);
		return narray;
	}

	private static double[] resizeTo(double[] array, int size)
	{
		double[] narray = new double[size];
		System.arraycopy(array, 0, narray, 0, size);
		return narray;
	}

	private static double[] expandTo(double[] array, int size)
	{
		double[] narray = new double[size];
		System.arraycopy(array, 0, narray, 0, array.length);
		return narray;
	}

	private static int[] resize(int[] array)
	{
		int[] narray = new int[array.length * 2];
		System.arraycopy(array, 0, narray, 0, array.length);
		return narray;
	}

	private static int[] resizeTo(int[] array, int size)
	{
		int[] narray = new int[size];
		System.arraycopy(array, 0, narray, 0, size);
		return narray;
	}

	private static int[] expandTo(int[] array, int size)
	{
		int[] narray = new int[size];
		System.arraycopy(array, 0, narray, 0, array.length);
		return narray;
	}

	public static TimeWindow getTimeWindow(TimeSeries[] timeSeries)
	{
		TimeWindow tw = null;
		for(int i = 0; i < timeSeries.length; i++)
		{
			if(tw == null)
			{
				tw = timeSeries[i].getTimeWindow();
			}
			else
			{
				tw = tw.union(timeSeries[i].getTimeWindow());
			}
		}
		return tw;
	}
}
