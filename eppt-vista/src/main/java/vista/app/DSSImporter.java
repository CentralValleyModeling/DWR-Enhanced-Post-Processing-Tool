/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package vista.app;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.io.LineNumberReader;
import java.text.MessageFormat;
import java.util.StringTokenizer;

import vista.db.dss.DSSUtil;
import vista.set.Constants;
import vista.set.DataSetAttr;
import vista.set.DataType;
import vista.set.FlagUtils;
import vista.set.FlaggedDataSetElement;
import vista.set.IrregularTimeSeries;
import vista.set.Pathname;
import vista.set.RegularTimeSeries;
import vista.set.TimeSeries;
import vista.time.Time;
import vista.time.TimeFactory;
import vista.time.TimeInterval;

/**
 * This class defines the functions for dssts and dssits functionality. In
 * addition it has the capability to read flags in as well.
 *
 * @author Nicky Sandhu
 * @version $Id: DSSImporter.java,v 1.6 2000/06/06 22:13:36 nsandhu Exp $
 */
public class DSSImporter
{
	public static boolean DEBUG = false;
	private static FlaggedDataSetElement _dummyDse = new FlaggedDataSetElement();

	/**
	 *
	 */
	public static void main(String[] args)
	{
		if(args == null || args.length == 0)
		{
			printUsage();
		}
		boolean dssts = true;
		boolean readFlags = false;
		String[] files = null;
		if(args.length >= 1)
		{
			int argIndex = 0;
			if(args[argIndex].equals("-i"))
			{
				// System.out.println("argIndex,args[argIndex]="+argIndex+", "+args[argIndex]);
				dssts = false;
				argIndex++;
			}
			if(args[argIndex].equals("-f"))
			{
				readFlags = true;
				argIndex++;
			}
			if(argIndex >= args.length)
			{
				printUsage();
			}
			for(; argIndex < args.length; argIndex++)
			{
				try
				{
					DSSImporter.readDSSText(args[argIndex], dssts, readFlags);
				}
				catch(Exception exc)
				{
					System.out.println("Could not read file : "
							+ args[argIndex]);
					exc.printStackTrace();
				}
			}
		}
		else
		{
			printUsage();
		}
	}

	/**
	 *
	 */
	public static void printUsage()
	{
		System.out
				.println("Usage: dssts|dssits [-i] [-f] textfile1 textfile2 ...");
		System.exit(2);
	}

	/**
	 *
	 */
	public static int makeFlagValue(String flag_val)
	{
		StringTokenizer st = new StringTokenizer(flag_val, "|");
		if(st.countTokens() != 2)
		{
			throw new RuntimeException("Invalid flag: " + flag_val);
		}
		int flagType = FlagUtils.getQualityFlagId(st.nextToken());
		int userId = DSSUtil.getUserId(st.nextToken().toLowerCase());
		_dummyDse.setFlag(0);
		if(flagType == 0)
		{
			FlagUtils.clearAllFlags(_dummyDse, userId);
		}
		else
		{
			FlagUtils.setQualityFlag(_dummyDse, flagType, userId);
		}
		return _dummyDse.getFlag();
	}

	/**
	 *
	 */
	public static void readDSSText(String file, boolean dssts, boolean flag)
			throws IOException
	{
		TimeFactory tf = TimeFactory.getInstance();
		LineNumberReader lnr = new LineNumberReader(new BufferedReader(
				new FileReader(file)));
		String line = lnr.readLine();
		String dssfile = line;
		String units = null, type = null;
		Time stime = null;
		Time ctime = null;
		TimeInterval rti = null;
		double[] yvals = null;
		double[] xvals = null;
		int[] flags = null;
		int initSize = 5000;
		Pathname path = null;
		boolean _timeStampPerLine = false;
		String xstr = null, ystr = null, flagstr = null;
		while(line != null)
		{
			try
			{
				line = lnr.readLine().toUpperCase();
				if(line.equals("FINISH"))
				{
					break;
				}
				path = Pathname.createPathname(line);
				if(DEBUG)
				{
					System.out.println("Pathname: " + path);
				}
				String epart = path.getPart(Pathname.E_PART);
				if(dssts)
				{
					rti = tf.createTimeInterval(epart);
				}
				else
				{
					if(epart.indexOf("IR-") >= 0)
					{
						epart = epart.substring(epart.indexOf("IR-") + 3);
					}
					rti = tf.createTimeInterval(epart);
				}
			}
			catch(Exception exc)
			{
				System.err.println(exc);
				System.out.println("Incorrect format for path: " + line);
				break;
			}
			try
			{
				units = lnr.readLine().toUpperCase();
				if(DEBUG)
				{
					System.out.println("Units: " + units);
				}
			}
			catch(Exception exc)
			{
				System.out.println("Incorrect format for units: " + line);
				break;
			}
			try
			{
				type = lnr.readLine().toUpperCase();
				if(DEBUG)
				{
					System.out.println("Type: " + type);
				}
			}
			catch(Exception exc)
			{
				System.out.println("Incorrect format for type: " + line);
				break;
			}
			if(dssts)
			{
				try
				{
					line = lnr.readLine();
					line = line.trim();
					StringTokenizer st = new StringTokenizer(line);
					if((flag && st.countTokens() == 4)
							|| st.countTokens() == 3)
					{
						_timeStampPerLine = true;
					}
					stime = tf.createTime(line.substring(0, 14));
					if(DEBUG)
					{
						System.out.println("Start Time: " + stime);
					}
				}
				catch(Exception exc)
				{
					System.out.println("Incorrect format for time: " + line);
					break;
				}
			}
			if(!_timeStampPerLine)
			{
				line = lnr.readLine().toUpperCase();
			}
			else
			{
				line = line;
			}
			yvals = new double[initSize];
			if(!dssts)
			{
				xvals = new double[initSize];
			}
			if(flag)
			{
				flags = new int[initSize];
			}
			int index = 0;
			while(!line.equals("END"))
			{
				if(DEBUG)
				{
					System.out
							.println("Index: " + index + " -> line = " + line);
				}
				if(dssts)
				{ // do DSSTS format
					if(_timeStampPerLine)
					{
						line = line.trim();
						xstr = line.substring(0, 14);
						Time strTime = tf.createTime(xstr);
						if(ctime == null)
						{
							ctime = strTime;
						}
						else
						{
							ctime.incrementBy(rti);
						}
						if(ctime.compare(strTime) != 0)
						{
							throw new IllegalArgumentException(
									"Regular Time Series: \n" + "Expecting "
											+ ctime + " got " + strTime + "\n"
											+ "Line # " + lnr.getLineNumber());
						}
						line = line.substring(14);
					}
					if(flag)
					{
						StringTokenizer st = new StringTokenizer(line);
						if(st.countTokens() != 2)
						{
							throw new RuntimeException(MessageFormat.format(
									"No flags in file {0} @ line: {1}",
									file, line));
						}
						ystr = st.nextToken();
						flagstr = st.nextToken();
					}
					else
					{
						ystr = line;
					}
				}
				else
				{ // do DSSITS format
					line = line.trim();
					xstr = line.substring(0, 14);
					if(flag)
					{
						StringTokenizer st = new StringTokenizer(line
								.substring(14));
						if(st.countTokens() != 2)
						{
							throw new RuntimeException(MessageFormat.format(
									"No flags in file {0} @ line: {1}",
									file, line));
						}
						ystr = st.nextToken();
						flagstr = st.nextToken();
					}
					else
					{
						ystr = line.substring(14);
					}
				}
				// create values
				if(DEBUG)
				{
					if(!dssts)
					{
						System.out.println("xstr: " + xstr);
					}
					System.out.println("ystr: " + ystr);
					if(flag)
					{
						System.out.println("flagstr: " + flagstr);
					}
				}
				if(ystr.indexOf("M") >= 0)
				{
					yvals[index] = Constants.MISSING_VALUE;
					flagstr = "MISSING|null";
				}
				else
				{
					yvals[index] = new Double(ystr).doubleValue();
				}
				if(flag)
				{
					flags[index] = makeFlagValue(flagstr);
				}
				if(!dssts)
				{
					xvals[index] = tf.createTime(xstr).getTimeInMinutes();
				}
				// get next line & increment counters
				line = lnr.readLine().toUpperCase();
				index++;
				if(index >= yvals.length)
				{
					yvals = expandDoubleArray(yvals);
					if(!dssts)
					{
						xvals = expandDoubleArray(xvals);
					}
					if(flag)
					{
						flags = expandIntArray(flags);
					}
				}
			}
			// create appropriate time series object
			if(index == 0)
			{
				throw new RuntimeException("No valid data found in file : "
						+ file);
			}
			yvals = trimDoubleArray(yvals, index);
			if(!dssts)
			{
				xvals = trimDoubleArray(xvals, index);
			}
			if(flag)
			{
				flags = trimIntArray(flags, index);
			}
			DataSetAttr attr = null;
			TimeSeries ts = null;
			if(dssts)
			{
				attr = new DataSetAttr(DataType.REGULAR_TIME_SERIES, "TIME",
						units, "", type);
				ts = new RegularTimeSeries("", stime.toString(), path
						.getPart(Pathname.E_PART), yvals, flags, attr);
			}
			else
			{
				attr = new DataSetAttr(DataType.IRREGULAR_TIME_SERIES, "TIME",
						units, "", type);
				ts = new IrregularTimeSeries("", xvals, yvals, flags, attr);
			}
			//
			DSSUtil.writeData(dssfile, path.toString(), ts, true);
		}
	}

	/**
	 *
	 */
	public static double[] expandDoubleArray(double[] array)
	{
		double[] array2 = new double[array.length + 5000];
		System.arraycopy(array, 0, array2, 0, array.length);
		return array2;
	}

	/**
	 *
	 */
	public static double[] trimDoubleArray(double[] array, int length)
	{
		double[] array2 = new double[length];
		// System.out.println("length = "+length);
		System.arraycopy(array, 0, array2, 0, length);
		return array2;
	}

	/**
	 *
	 */
	public static int[] expandIntArray(int[] array)
	{
		int[] array2 = new int[array.length + 5000];
		System.arraycopy(array, 0, array2, 0, array.length);
		return array2;
	}

	/**
	 *
	 */
	public static int[] trimIntArray(int[] array, int length)
	{
		int[] array2 = new int[length];
		System.arraycopy(array, 0, array2, 0, length);
		return array2;
	}
}
