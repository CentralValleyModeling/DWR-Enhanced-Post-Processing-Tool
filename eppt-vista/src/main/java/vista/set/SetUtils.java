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

import java.io.BufferedWriter;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.LineNumberReader;
import java.io.PrintWriter;
import java.text.NumberFormat;
import java.util.Enumeration;
import java.util.StringTokenizer;
import java.util.Vector;

import vista.db.dss.DSSUtil;
import vista.time.Time;
import vista.time.TimeFactory;
import vista.time.TimeInterval;

/**
 * @author Nicky Sandhu
 * @version $Id: SetUtils.java,v 1.1 2003/10/02 20:49:32 redwood Exp $
 */
public class SetUtils
{
	static NumberFormat _formatter;
	private static NumberFormat nf = NumberFormat.getInstance();

	/**
	 * creates an array of doubles filled with x values from the data set
	 */
	public static double[] createXArray(DataSet ds)
	{
		double[] x = new double[ds.size()];
		int i = 0;
		for(DataSetIterator iter = ds.getIterator(); !iter.atEnd(); iter
				.advance(), i++)
		{
			x[i] = iter.getElement().getX();
		}
		return x;
	}

	/**
	 * creates an array of doubles filled with x values from the data set
	 */
	public static double[] createYArray(DataSet ds)
	{
		double[] y = new double[ds.size()];
		int i = 0;
		for(DataSetIterator iter = ds.getIterator(); !iter.atEnd(); iter
				.advance(), i++)
		{
			y[i] = iter.getElement().getY();
		}
		return y;
	}

	/**
	 * creates an array of ints filled with flag values from the data set
	 */
	public static int[] createFlagArray(DataSet ds)
	{
		int[] flags = new int[ds.size()];
		int i = 0;
		for(DataSetIterator iter = ds.getIterator(); !iter.atEnd(); iter
				.advance(), i++)
		{
			flags[i] = iter.getElement().getFlag();
		}
		return flags;
	}

	/**
	 *
	 */
	public static String getStats(DataSet ds)
	{
		StringBuffer buf = new StringBuffer(100);
		String ls = System.getProperty("line.separator");
		buf.append("DataSet   :").append(ds.getName()).append(ls);
		buf.append("SIZE      :").append(ds.size()).append(ls);
		buf.append("AVERAGE   :").append(nf.format(Stats.avg(ds))).append(ls);
		buf.append("MAXIMUM   :").append(nf.format(Stats.max(ds))).append(ls);
		buf.append("MINIMUM   :").append(nf.format(Stats.min(ds))).append(ls);
		buf.append("% MISSING      :").append(
				nf.format((100.0 * Stats.countMissing(ds)) / ds.size()))
		   .append(ls);
		buf.append("% OK           :").append(
				nf.format((100.0 * Stats.countOK(ds)) / ds.size())).append(ls);
		buf.append("% QUESTIONABLE :").append(
				nf.format((100.0 * Stats.countQuestionable(ds)) / ds.size()))
		   .append(ls);
		buf.append("% REJECT       :").append(
				nf.format((100.0 * Stats.countReject(ds)) / ds.size())).append(
				ls);
		return buf.toString();
	}

	/**
	 *
	 */
	public static void write(DataSet[] dataSets, String filename,
							 boolean outputFlags)
	{
		try
		{
			PrintWriter writer = new PrintWriter(new BufferedWriter(
					new FileWriter(filename), 1024 * 16));
			String tab = "\t";
			for(int k = 0; k < dataSets.length; k++)
			{
				DataSet ds = dataSets[k];
				if(ds == null)
				{
					continue;
				}
				writer.print(SetUtils.getHeader(ds).toString());
				DataSetIterator dsi = ds.getIterator();
				// new ElementFilterIterator(ds.getIterator(),
				// Constants.DEFAULT_FILTER);
				for(dsi.resetIterator(); !dsi.atEnd(); dsi.advance())
				{
					DataSetElement dse = dsi.getElement();
					writer.print(dse.getXString(0));
					for(int i = 1; i < dse.getDimension(); i++)
					{
						writer.print(tab);
						writer.print(dse.getXString(i));
					}
					if(outputFlags && ds.isFlagged())
					{
						writer.print(tab);
						writer.print(dse.getFlagString());
					}
					writer.println();
				}
			}
			writer.close();
		}
		catch(Exception e)
		{
			e.printStackTrace();
		}
	}

	/**
	 *
	 */
	public static void write(DataSet ds, String filename, boolean outputFlags)
	{
		try
		{
			PrintWriter writer = new PrintWriter(new BufferedWriter(
					new FileWriter(filename), 1024 * 16));
			String tab = "\t";
			writer.print(SetUtils.getHeader(ds).toString());
			DataSetIterator dsi = ds.getIterator();
			// new ElementFilterIterator(ds.getIterator(),
			// Constants.DEFAULT_FILTER);
			for(dsi.resetIterator(); !dsi.atEnd(); dsi.advance())
			{
				DataSetElement dse = dsi.getElement();
				writer.print(dse.getXString(0));
				for(int i = 1; i < dse.getDimension(); i++)
				{
					writer.print(tab);
					writer.print(dse.getXString(i));
				}
				if(outputFlags && ds.isFlagged())
				{
					writer.print(tab);
					writer.print(dse.getFlagString());
				}
				writer.println();
			}
			writer.close();
		}
		catch(Exception e)
		{
			e.printStackTrace();
		}
	}

	/**
	 * returns the header information for the dataset
	 */
	public static StringBuffer getHeader(DataSet ds)
	{
		StringBuffer buf = new StringBuffer(500);
		String ls = System.getProperty("line.separator");
		DataSetAttr attr = ds.getAttributes();
		buf.append(ls).append("Data : " + ds.getName());
		if(ds instanceof RegularTimeSeries)
		{
			RegularTimeSeries ts = (RegularTimeSeries) ds;
			buf.append(ls).append(
					"Regular Time Series: " + ts.getTimeWindow().toString());
			buf.append(ls).append(
					"Regular Interval: " + ts.getTimeInterval().toString());
		}
		else if(ds instanceof IrregularTimeSeries)
		{
			IrregularTimeSeries its = (IrregularTimeSeries) ds;
			buf.append(ls).append(
					"Irregular Time Series: " + its.getTimeWindow().toString());
		}
		else if(ds instanceof DefaultDataSet)
		{
			buf.append(ls).append("Paired Data: ");
		}
		else
		{
			buf.append(ls).append("Unknown Data Type: ");
		}
		if(attr == null)
		{
			buf.append(ls).append("No attributes available");
		}
		else
		{
			buf.append(ls).append(
					"Group: "
							+ (attr.getGroupName() == null ? "N/A" : attr
							.getGroupName()));
			buf.append(ls).append(
					"Location: "
							+ (attr.getLocationName() == null ? "N/A" : attr
							.getLocationName()));
			buf.append(ls).append(
					"Type: "
							+ (attr.getTypeName() == null ? "N/A" : attr
							.getTypeName()));
			buf.append(ls).append(
					"Source: "
							+ (attr.getSourceName() == null ? "N/A" : attr
							.getSourceName()));
			buf.append(ls).append(
					"Interpolation Type : "
							+ (attr.getYType() == null ? "N/A" : attr
							.getYType()));
			if(attr.getXUnits() != null)
			{
				buf.append(ls).append(
						"Units X : "
								+ (attr.getXUnits() == null ? "N/A" : attr
								.getXUnits()));
			}
			buf.append(ls).append(
					"Units : "
							+ (attr.getYUnits() == null ? "N/A" : attr
							.getYUnits()));
		}
		buf.append(ls);
		return buf;
	}

	/**
	 * replace all instances of replacee by replacer in orgStr and return that
	 * String
	 */
	public static String createReplacedString(String orgStr, String replacee,
											  String replacer)
	{
		if(replacee == null || replacer == null)
		{
			return orgStr;
		}
		if(replacee.length() == 0)
		{
			return orgStr;
		}
		StringBuffer buf = new StringBuffer(orgStr.length() + replacer.length()
				- replacee.length());
		int index = orgStr.indexOf(replacee);
		int rl = replacee.length();
		while(index >= 0)
		{
			buf.append(orgStr, 0, index).append(replacer);
			orgStr = orgStr.substring(index + rl);
			index = orgStr.indexOf(replacee);
		}
		return buf.append(orgStr).toString();
	}

	/**
	 * converts data to daily. If given data set has a time interval greater
	 * than daily it will be repeated for each day from the previous interval
	 * and each value will be that of that in the interval. For the case of time
	 * intervals less than daily,they will be period-averaged to give the
	 * desired result.
	 */
	public static RegularTimeSeries createDaily(RegularTimeSeries ts)
	{
		TimeInterval tifrom = ts.getTimeInterval();
		TimeInterval tiday = tifrom.create("1day");
		int c = tifrom.compare(tiday);
		if(c > 0)
		{
			ElementFilter filter = Constants.DEFAULT_FLAG_FILTER;
			double[] y_cfs = new double[ts.size() * 31]; // max size of
			DataSetIterator dsi = ts.getIterator();
			Time tm = ts.getStartTime();
			Time tm2 = tm.create(tm);
			int k = 0;
			while(!dsi.atEnd())
			{
				DataSetElement e = dsi.getElement();
				tm = tm.create(Math.round(e.getX()));
				tm2 = tm.create(tm);
				tm2.incrementBy(tifrom, -1); // go back one interval
				int nvals = (int) tm2.getExactNumberOfIntervalsTo(tm, tiday);
				double val = Constants.MISSING_VALUE;
				if(filter.isAcceptable(e))
				{
					val = e.getY();
				}
				else
				{
					val = Constants.MISSING_VALUE;
				}
				for(int i = 0; i < nvals; i++)
				{
					y_cfs[k] = val;
					k++;
				}
				dsi.advance();
			}
			// trim down the array
			double[] tmpArray = new double[k];
			System.arraycopy(y_cfs, 0, tmpArray, 0, k);
			y_cfs = tmpArray;
			// get start time for this time series
			Time stime = ts.getStartTime().create(ts.getStartTime());
			stime.incrementBy(tifrom, -1);
			stime.incrementBy(tiday);
			return new RegularTimeSeries(ts.getName() + " (daily)", stime
					.toString(), "1day", y_cfs);
		}
		else if(c < 0)
		{
			return TimeSeriesMath.doPeriodOperation(ts, tiday,
					TimeSeriesMath.PERIOD_AVERAGE);
		}
		else
		{
			return new RegularTimeSeries(ts.getName() + "(converted to daily)",
					ts.getStartTime(), tiday, SetUtils.createYArray(ts),
					SetUtils.createFlagArray(ts), ts.getAttributes());
		}

	}

	/**
	 * converts monthly regular time series data from thousand-acre-feet to cfs.
	 * Note: This is not a unit conversion in the ordinary sense as it the
	 * factor to be multiplied is dependent upon the month.
	 */
	public static RegularTimeSeries taf2cfs(RegularTimeSeries ts)
	{
		TimeInterval tifrom = ts.getTimeInterval();
		TimeInterval tiday = tifrom.create("1day");
		TimeInterval timonth = tifrom.create("1month");
		// check that data is a monthly regular time series
		int c = tifrom.compare(timonth);
		if(c != 0)
		{
			throw new IllegalArgumentException("Time Series: " + ts.getName()
					+ " is not a monthly regular time series ");
		}
		// check that if units are mentioned they are in TAF
		String units = ts.getAttributes().getYUnits();
		if(units.toUpperCase().indexOf("TAF") < 0)
		{
			throw new IllegalArgumentException("Time Series: " + ts.getName()
					+ " has units " + units + " != TAF ");
		}
		// set up loop and do conversion
		ElementFilter filter = Constants.DEFAULT_FLAG_FILTER;
		double[] y_cfs = new double[ts.size() * 31]; // max size of
		DataSetIterator dsi = ts.getIterator();
		Time tm = ts.getStartTime();
		Time tm2 = tm.create(tm);
		int k = 0;
		double factor = (1000.0 * 43560) / (24 * 60 * 60.0);
		while(!dsi.atEnd())
		{
			DataSetElement e = dsi.getElement();
			tm = tm.create(Math.round(e.getX()));
			tm2 = tm.create(tm);
			tm2.incrementBy(tifrom, -1); // go back one interval
			int nvals = (int) tm2.getExactNumberOfIntervalsTo(tm, tiday);
			double val = Constants.MISSING_VALUE;
			if(filter.isAcceptable(e))
			{
				val = e.getY() / nvals * factor;
			}
			else
			{
				val = Constants.MISSING_VALUE;
			}
			for(int i = 0; i < nvals; i++)
			{
				y_cfs[k] = val;
				k++;
			}
			dsi.advance();
		}
		// trim down the array
		double[] tmpArray = new double[k];
		System.arraycopy(y_cfs, 0, tmpArray, 0, k);
		y_cfs = tmpArray;
		// get start time for this time series
		Time stime = ts.getStartTime().create(ts.getStartTime());
		stime.incrementBy(tifrom, -1);
		stime.incrementBy(tiday);
		DataSetAttr oldattr = ts.getAttributes();
		DataSetAttr attr = new DataSetAttr(oldattr.getGroupName(), oldattr
				.getLocationName(), oldattr.getTypeName(), oldattr
				.getSourceName(), oldattr.getType(), oldattr.getXUnits(),
				"CFS", oldattr.getXType(), oldattr.getYType());
		return new RegularTimeSeries(ts.getName() + "(daily)",
				stime.toString(), "1day", y_cfs, attr);
	}

	/**
	 * converts monthly regular time series data from thousand-acre-feet to cfs.
	 * Note: This is not a unit conversion in the ordinary sense as it the
	 * factor to be multiplied is dependent upon the month.
	 */
	public static RegularTimeSeries cfs2taf(RegularTimeSeries ts)
	{
		TimeInterval tifrom = ts.getTimeInterval();
		TimeInterval tiday = tifrom.create("1day");
		TimeInterval timonth = tifrom.create("1month");
		// check that data is a daily regular time series
		int c = tifrom.compare(tiday);
		if(c != 0)
		{
			throw new IllegalArgumentException("Time Series: " + ts.getName()
					+ " is not a daily regular time series ");
		}
		// check that if units are mentioned they are in TAF
		String units = ts.getAttributes().getYUnits();
		if(units.toUpperCase().indexOf("CFS") < 0)
		{
			throw new IllegalArgumentException("Time Series: " + ts.getName()
					+ " has units " + units + " != CFS ");
		}
		// set up loop and do conversion
		ElementFilter filter = Constants.DEFAULT_FLAG_FILTER;
		double[] y_taf = new double[ts.size() * 31]; // max size of
		DataSetIterator dsi = ts.getIterator();
		Time tm = ts.getStartTime();
		Time tm2 = tm.create(tm);
		int k = 0;
		double factor = (1000.0 * 43560) / (24 * 60 * 60.0);
		while(!dsi.atEnd())
		{
			DataSetElement e = dsi.getElement();
			tm = tm.create(Math.round(e.getX()));
			tm2 = tm.create(tm);
			tm2.incrementBy(tifrom, -1); // go back one interval
			int nvals = (int) tm2.getExactNumberOfIntervalsTo(tm, tiday);
			double val = Constants.MISSING_VALUE;
			if(filter.isAcceptable(e))
			{
				val = e.getY() / nvals * factor;
			}
			else
			{
				val = Constants.MISSING_VALUE;
			}
			for(int i = 0; i < nvals; i++)
			{
				y_taf[k] = val;
				k++;
			}
			dsi.advance();
		}
		// trim down the array
		double[] tmpArray = new double[k];
		System.arraycopy(y_taf, 0, tmpArray, 0, k);
		y_taf = tmpArray;
		// get start time for this time series
		Time stime = ts.getStartTime().create(ts.getStartTime());
		stime.incrementBy(tifrom, -1);
		stime.incrementBy(tiday);
		DataSetAttr oldattr = ts.getAttributes();
		DataSetAttr attr = new DataSetAttr(oldattr.getGroupName(), oldattr
				.getLocationName(), oldattr.getTypeName(), oldattr
				.getSourceName(), oldattr.getType(), oldattr.getXUnits(),
				"CFS", oldattr.getXType(), oldattr.getYType());
		return new RegularTimeSeries(ts.getName() + "(daily)",
				stime.toString(), "1day", y_taf, attr);
	}

	/**
	 * imports data from dssts or dssits format
	 */
	public static DataReference[] importDataFromText(String file,
													 boolean isRegular) throws IOException
	{
		TimeFactory tf = TimeFactory.getInstance();
		Vector refs = new Vector();
		LineNumberReader reader = new LineNumberReader(new FileReader(file));
		String line = reader.readLine();
		String dssfile = null;
		if(line != null)
		{
			dssfile = line;
		}
		else
		{
			return null;
		}
		while(true)
		{
			Pathname path = null;
			String units = null, type = null;
			Time stime = null;
			Vector xvals = null, yvals = null;
			// get path
			line = reader.readLine();
			if(line == null)
			{
				break;
			}
			line = line.toUpperCase();
			if(line.equals("FINISH"))
			{
				break;
			}
			path = Pathname.createPathname(line);
			// get units
			line = reader.readLine();
			if(line == null)
			{
				break;
			}
			line = line.toUpperCase();
			units = line;
			// get type
			line = reader.readLine();
			if(line == null)
			{
				break;
			}
			line = line.toUpperCase();
			type = line;
			// decide if its dssts or dssits data
			if(isRegular)
			{
				line = reader.readLine();
				if(line == null)
				{
					break;
				}
				line = line.toUpperCase();
				stime = tf.createTime(line);
			}
			line = reader.readLine();
			if(line == null)
			{
				break;
			}
			line = line.toUpperCase();
			xvals = new Vector();
			yvals = new Vector();
			while(line != null && !line.equals("END"))
			{
				if(isRegular)
				{
					try
					{
						yvals.addElement(new Double(line));
					}
					catch(NumberFormatException nfe)
					{
						// add anyway as it'll get out of sync otherwise
						yvals.addElement(new Double(Constants.MISSING_VALUE));
						if(line.indexOf("M") < 0)
						{
							System.out.println("Error reading line: " + line
									+ " @ " + reader.getLineNumber());
						}
					}
				}
				else
				{
					try
					{
						StringTokenizer st = new StringTokenizer(line);
						Time tm = tf.createTime(st.nextToken());
						Double d = null;
						try
						{
							d = new Double(st.nextToken());
						}
						catch(NumberFormatException nfe)
						{
							yvals
									.addElement(new Double(
											Constants.MISSING_VALUE));
							if(line.indexOf("M") < 0)
							{
								throw new RuntimeException(
										"error reading value");
							}
						}
						xvals.addElement(tm);
						yvals.addElement(d);
					}
					catch(Exception e)
					{
						System.out.println("Error reading line: " + line
								+ " @ " + reader.getLineNumber());
					}
				}
				line = reader.readLine();
				line = line != null ? line.toUpperCase() : line;
			}
			if(yvals.size() == 0)
			{
				continue;
			}
			DataSet ds = null;
			if(isRegular)
			{
				DataSetAttr attr = new DataSetAttr(
						DataType.REGULAR_TIME_SERIES, "TIME", units, "", type);
				double[] vals = new double[yvals.size()];
				int count = 0;
				for(Enumeration e = yvals.elements(); e.hasMoreElements(); )
				{
					vals[count++] = ((Double) e.nextElement()).doubleValue();
				}
				ds = new RegularTimeSeries(path.toString(), stime.toString(),
						path.getPart(Pathname.E_PART), vals, null, attr);
			}
			else
			{
				Time[] xtime = new Time[xvals.size()];
				xvals.copyInto(xtime);
				double[] vals = new double[yvals.size()];
				int count = 0;
				for(Enumeration e = yvals.elements(); e.hasMoreElements(); )
				{
					vals[count++] = ((Double) e.nextElement()).doubleValue();
				}
				DataSetAttr attr = new DataSetAttr(
						DataType.IRREGULAR_TIME_SERIES, "TIME", units, "", type);
				ds = new IrregularTimeSeries(path.toString(), xtime, vals,
						null, attr);
			}
			if(ds == null)
			{
				continue;
			}
			// write to dss
			DSSUtil.writeData(dssfile, path.toString(), ds, true);
			// add to array of references
			refs.addElement(DSSUtil.createDataReference("local", dssfile, path
					.toString(), ds));
		}
		// force a recatalog
		DSSUtil.createGroup("local", dssfile).getNumberOfDataReferences();
		//
		if(refs.size() == 0)
		{
			return null;
		}
		else
		{
			DataReference[] drefs = new DataReference[refs.size()];
			refs.copyInto(drefs);
			return drefs;
		}
	}

	/**
	 * decides format for all doubles displayed and uses nice formatting if
	 * value is greater than 1e-03.
	 */
	public static String format(double d)
	{
		if(Math.abs(d) > 0.001)
		{
			return getValueFormatter().format(d);
		}
		else
		{
			return "" + d;
		}
	}

	/**
	 *
	 */
	public static NumberFormat getValueFormatter()
	{
		if(_formatter == null)
		{
			_formatter = NumberFormat.getInstance();
			_formatter.setGroupingUsed(false);
			_formatter.setMaximumFractionDigits(3);
		}
		return _formatter;
	}

	/**
	 * checks the quality of data by looking at value only.
	 */
	public static boolean isGoodValue(double y)
	{
		return y != Constants.MISSING_VALUE && y != Constants.MISSING
				&& y != Constants.MISSING_RECORD
				&& Double.doubleToLongBits(y) != 0x7ff8000000000000L;
	}

	/**
	 * Create data set with flags that are reject, questionable or missing converted to -901 data values
	 */
	public static TimeSeries convertFlagsToValues(TimeSeries ds)
	{
		TimeSeries ts = ds.createSlice(ds.getTimeWindow());
		if(!ds.isFlagged())
		{
			return ts;
		}
		DataSetIterator iterator = ts.getIterator();
		while(!iterator.atEnd())
		{
			DataSetElement element = iterator.getElement();
			int flagType = FlagUtils.getQualityFlag(element.getFlag());
			if(flagType == FlagUtils.REJECT_FLAG || flagType == FlagUtils.QUESTIONABLE_FLAG || flagType == FlagUtils.MISSING_FLAG)
			{
				element.setY(Constants.MISSING_VALUE);
				iterator.putElement(element);
			}
			iterator.advance();
		}
		return ts;

	}
}
