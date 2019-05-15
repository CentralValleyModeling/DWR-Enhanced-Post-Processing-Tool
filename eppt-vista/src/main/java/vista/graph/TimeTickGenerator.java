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

import java.io.InputStreamReader;
import java.io.LineNumberReader;
import java.util.Enumeration;
import java.util.Vector;

import vista.time.Time;
import vista.time.TimeFactory;
import vista.time.TimeFormat;
import vista.time.TimeInterval;

/**
 * Generates information for tick placement and their associated labels using a
 * range of given maximum and minimum. This now includes better labeling for
 * days to month tick transitions.
 *
 * @author Nicky Sandhu
 * @version $Id: TimeTickGenerator.java,v 1.1 2003/10/02 20:49:11 redwood Exp $
 */
public class TimeTickGenerator implements TickGenerator
{
	/**
	 *
	 */
	private static final boolean DEBUG = false;
	private static TimeInterval _minusOneDay = TimeFactory.getInstance()
														  .createTimeInterval("-1day");
	/**
	 *
	 */
	private static TimeFactory tf = TimeFactory.getInstance();
	private static TimeFormat _formatter = TimeFactory.getInstance()
													  .getTimeFormatInstance().create("ddMMMyyyy HHmm");
	// new vista.time.SubTimeFormat("ddMMMyyyy HHmm");
	private static TimeInterval[] _minorTickInterval;
	private static TimeInterval[] _majorTickInterval;
	private static TimeInterval[] _goodRangeInterval;
	/**
	 *
	 */
	private TimeInterval _goodRange, _majorTI, _minorTI;
	private Time _niceStartTime, _niceEndTime, _startTime, _endTime;
	// round of stime and etimes to nice values if true
	private boolean useNiceEndings = true;

	/**
	 *
	 */
	public TimeTickGenerator()
	{
		if(_goodRangeInterval == null)
		{
			initFromFile("/vista/graph/tmtg.properties");
		}
	}

	/**
	 *
	 */
	private static void initFromFile(String file)
	{
		int nIntervals = 0;
		Vector minArray = new Vector(), majArray = new Vector(), ranArray = new Vector();
		try
		{
			LineNumberReader reader = new LineNumberReader(
					new InputStreamReader(vista.gui.VistaUtils
							.getResourceAsStream(file)));
			String line = null;
			while((line = reader.readLine()) != null)
			{
				if(line.startsWith("#"))
				{
					continue;
				}
				int eqPos = line.indexOf("=");
				String strInterval = line.substring(eqPos + 1);
				nIntervals++;
				if(line.startsWith("min"))
				{
					minArray.addElement(strInterval);
				}
				else if(line.startsWith("maj"))
				{
					majArray.addElement(strInterval);
				}
				else if(line.startsWith("ran"))
				{
					ranArray.addElement(strInterval);
				}
				else
				{
				}
			}
		}
		catch(Exception e)
		{
			initIntervals();
			return;
		}
		TimeInterval tiproto = tf.getTimeIntervalInstance();
		if(nIntervals > 0)
		{
			String[] strArray = new String[minArray.size()];
			minArray.copyInto(strArray);
			_minorTickInterval = new TimeInterval[minArray.size()];
			for(int i = 0; i < strArray.length; i++)
			{
				_minorTickInterval[i] = tiproto.create(strArray[i]);
			}
			strArray = new String[majArray.size()];
			majArray.copyInto(strArray);
			_majorTickInterval = new TimeInterval[majArray.size()];
			for(int i = 0; i < strArray.length; i++)
			{
				_majorTickInterval[i] = tiproto.create(strArray[i]);
			}
			strArray = new String[ranArray.size()];
			ranArray.copyInto(strArray);
			_goodRangeInterval = new TimeInterval[ranArray.size()];
			for(int i = 0; i < strArray.length; i++)
			{
				_goodRangeInterval[i] = tiproto.create(strArray[i]);
			}
		}
		else
		{
			initIntervals();
			return;
		}
	}

	/**
	 *
	 */
	private static void initIntervals()
	{
		int n = 30;
		_minorTickInterval = new TimeInterval[n];
		_majorTickInterval = new TimeInterval[n];
		_goodRangeInterval = new TimeInterval[n];
		TimeInterval tiproto = tf.getTimeIntervalInstance();
		int i = 0;
		// 0
		_minorTickInterval[i] = tiproto.create("1MIN");
		_majorTickInterval[i] = tiproto.create("2MIN");
		_goodRangeInterval[i++] = tiproto.create("10MIN");
		// 1
		_minorTickInterval[i] = tiproto.create("1MIN");
		_majorTickInterval[i] = tiproto.create("5MIN");
		_goodRangeInterval[i++] = tiproto.create("15MIN");
		// 2
		_minorTickInterval[i] = tiproto.create("1MIN");
		_majorTickInterval[i] = tiproto.create("5MIN");
		_goodRangeInterval[i++] = tiproto.create("30MIN");
		// 3
		_minorTickInterval[i] = tiproto.create("5MIN");
		_majorTickInterval[i] = tiproto.create("5MIN");
		_goodRangeInterval[i++] = tiproto.create("1HOUR");
		// 4
		_minorTickInterval[i] = tiproto.create("5MIN");
		_majorTickInterval[i] = tiproto.create("10MIN");
		_goodRangeInterval[i++] = tiproto.create("2HOUR");
		// 5
		_minorTickInterval[i] = tiproto.create("15MIN");
		_majorTickInterval[i] = tiproto.create("1HOUR");
		_goodRangeInterval[i++] = tiproto.create("6HOUR");
		// 6
		_minorTickInterval[i] = tiproto.create("15MIN");
		_majorTickInterval[i] = tiproto.create("2HOUR");
		_goodRangeInterval[i++] = tiproto.create("12HOUR");
		// 7
		_minorTickInterval[i] = tiproto.create("1HOUR");
		_majorTickInterval[i] = tiproto.create("6HOUR");
		_goodRangeInterval[i++] = tiproto.create("1DAY");
		// 8
		_minorTickInterval[i] = tiproto.create("1HOUR");
		_majorTickInterval[i] = tiproto.create("6HOUR");
		_goodRangeInterval[i++] = tiproto.create("2DAY");
		// 9
		_minorTickInterval[i] = tiproto.create("6HOUR");
		_majorTickInterval[i] = tiproto.create("1DAY");
		_goodRangeInterval[i++] = tiproto.create("10DAY");
		// 10
		_minorTickInterval[i] = tiproto.create("12HOUR");
		_majorTickInterval[i] = tiproto.create("1DAY");
		_goodRangeInterval[i++] = tiproto.create("15DAY");
		// 11
		_minorTickInterval[i] = tiproto.create("1DAY");
		_majorTickInterval[i] = tiproto.create("5DAY");
		_goodRangeInterval[i++] = tiproto.create("1MON");
		// 12
		_minorTickInterval[i] = tiproto.create("2DAY");
		_majorTickInterval[i] = tiproto.create("10DAY");
		_goodRangeInterval[i++] = tiproto.create("2MON");
		// 13
		_minorTickInterval[i] = tiproto.create("5DAY");
		_majorTickInterval[i] = tiproto.create("10DAY");
		_goodRangeInterval[i++] = tiproto.create("3MON");
		// 14
		_minorTickInterval[i] = tiproto.create("10DAY");
		_majorTickInterval[i] = tiproto.create("1MON");
		_goodRangeInterval[i++] = tiproto.create("6MON");
		// 15
		_minorTickInterval[i] = tiproto.create("15DAY");
		_majorTickInterval[i] = tiproto.create("1MON");
		_goodRangeInterval[i++] = tiproto.create("1YEAR");
		// 16
		_minorTickInterval[i] = tiproto.create("1MON");
		_majorTickInterval[i] = tiproto.create("2MON");
		_goodRangeInterval[i++] = tiproto.create("2YEAR");
		// 17
		_minorTickInterval[i] = tiproto.create("1MON");
		_majorTickInterval[i] = tiproto.create("6MON");
		_goodRangeInterval[i++] = tiproto.create("5YEAR");
		// 18
		_minorTickInterval[i] = tiproto.create("1MON");
		_majorTickInterval[i] = tiproto.create("1YEAR");
		_goodRangeInterval[i++] = tiproto.create("10YEAR");
		// 19
		_minorTickInterval[i] = tiproto.create("2MON");
		_majorTickInterval[i] = tiproto.create("2YEAR");
		_goodRangeInterval[i++] = tiproto.create("2DECADE");
		// 20
		_minorTickInterval[i] = tiproto.create("6MON");
		_majorTickInterval[i] = tiproto.create("2YEAR");
		_goodRangeInterval[i++] = tiproto.create("5DECADE");
		// 21
		_minorTickInterval[i] = tiproto.create("1YEAR");
		_majorTickInterval[i] = tiproto.create("5YEAR");
		_goodRangeInterval[i++] = tiproto.create("1CENTURY");
		// 22
		_minorTickInterval[i] = tiproto.create("1YEAR");
		_majorTickInterval[i] = tiproto.create("1DECADE");
		_goodRangeInterval[i++] = tiproto.create("2CENTURY");
		// 23
		_minorTickInterval[i] = tiproto.create("2YEAR");
		_majorTickInterval[i] = tiproto.create("2DECADE");
		_goodRangeInterval[i++] = tiproto.create("5CENTURY");
		// 24
		_minorTickInterval[i] = tiproto.create("2YEAR");
		_majorTickInterval[i] = tiproto.create("5DECADE");
		_goodRangeInterval[i++] = tiproto.create("5CENTURY");
		// 25
		_minorTickInterval[i] = tiproto.create("5YEAR");
		_majorTickInterval[i] = tiproto.create("5DECADE");
		_goodRangeInterval[i++] = tiproto.create("10CENTURY");
		// 26
		_minorTickInterval[i] = tiproto.create("5YEAR");
		_majorTickInterval[i] = tiproto.create("5DECADE");
		_goodRangeInterval[i++] = tiproto.create("10CENTURY");
		// 27
		_minorTickInterval[i] = tiproto.create("5YEAR");
		_majorTickInterval[i] = tiproto.create("5DECADE");
		_goodRangeInterval[i++] = tiproto.create("10CENTURY");
		// 28
		_minorTickInterval[i] = tiproto.create("1DECADE");
		_majorTickInterval[i] = tiproto.create("1CENTURY");
		_goodRangeInterval[i++] = tiproto.create("100CENTURY");
		// 29
		_minorTickInterval[i] = tiproto.create("1CENTURY");
		_majorTickInterval[i] = tiproto.create("10CENTURY");
		_goodRangeInterval[i++] = tiproto.create("1000CENTURY");
	}

	/**
	 * Generates ticks and labels given the minimum and maximum of the data
	 * range.
	 *
	 * @param minimum The minimum value of the data range
	 * @param maximum The maximum value of the data range
	 */
	public void generate(double minimum, double maximum)
	{
		_startTime = tf.getTimeInstance().create((long) minimum);
		_endTime = tf.getTimeInstance().create((long) maximum);
		setNiceRange(_startTime, _endTime);
	}

	/**
	 *
	 */
	private void setNiceRange(Time stime, Time etime)
	{
		if(stime.compare(etime) >= 0)
		{
			throw new IllegalArgumentException("Cannot have start time"
					+ " greater than equal to end time");
		}
		int i = 0;
		long minsTo = stime.getNumberOfMinutesTo(etime);
		while(i < _goodRangeInterval.length
				&& minsTo > _goodRangeInterval[i].getIntervalInMinutes(stime))
		{
			i++;
		}
		if(i >= _goodRangeInterval.length)
		{
			throw new IllegalArgumentException("Range too large, "
					+ "Time tick generator needs " + "to be modified");
		}
		_goodRange = _goodRangeInterval[i];
		_majorTI = _majorTickInterval[i];
		_minorTI = _minorTickInterval[i];
		if(useNiceEndings)
		{
			_niceStartTime = stime.create(stime.floor(_majorTI));
			_niceEndTime = stime.create(etime.ceiling(_majorTI));
			_startTime = _niceStartTime;
			_endTime = _niceEndTime;
		}
		else
		{
			_niceStartTime = stime.create(stime.ceiling(_majorTI));
			_niceEndTime = stime.create(etime.floor(_majorTI));
			if(_niceStartTime.compare(_niceEndTime) > 0)
			{
				_niceEndTime = _niceEndTime.create(_niceStartTime);
			}
		}
	}

	/**
	 * Returns an array of TickData. Be sure to call generate to initialize the
	 * generator before calling this function
	 *
	 * @return array of TickData
	 * @see TickData
	 * @see TickGenerator#generate
	 */
	public TickData[] getTickData()
	{
		Vector minorTicks = new Vector();
		Vector majorTicks = new Vector();
		Vector majorTicks2 = new Vector();
		TimeFormat tf1 = getMajorTimeFormat(_majorTI, false);
		TimeFormat tf2 = getMajor2TimeFormat(_majorTI, false);
		if(DEBUG)
		{
			System.out.println("Minor interval: " + _minorTI);
			System.out.println("Major interval: " + _majorTI);
		}
		TimeInterval major2TI = _majorTI;
		boolean doRounding = _goodRange
				.getNumberOfIntervals(TimeInterval.MONTH_INTERVAL) > 0
				&& _majorTI.getNumberOfIntervals(TimeInterval.MONTH_INTERVAL) == 0;
		boolean doMinorRounding = _majorTI
				.getNumberOfIntervals(TimeInterval.MONTH_INTERVAL) > 0
				&& _minorTI.getNumberOfIntervals(TimeInterval.MONTH_INTERVAL) == 0;

		if(doRounding)
		{
			major2TI = _majorTI.create("1MON");
		}
		if(DEBUG)
		{
			System.out.println("Major 2 interval: " + major2TI);
		}
		// if not nice begininig
		if(_startTime.compare(_niceStartTime) < 0)
		{
			for(Time tm = _startTime.create(_startTime); tm
					.compare(_niceStartTime) < 0; tm.incrementBy(_minorTI))
			{
				if(doMinorRounding)
				{
					tm = roundToNearestNice(tm, _minorTI, _majorTI);
				}
				minorTicks.addElement(tm.create(tm));
				if(DEBUG)
				{
					System.out.println("Not nice Minor Time: " + tm);
				}
			}
		}
		//
		for(Time tm = _niceStartTime.create(_niceStartTime); tm
				.compare(_endTime) <= 0; tm.incrementBy(_minorTI))
		{
			if(doMinorRounding)
			{
				tm = roundToNearestNice(tm, _minorTI, _majorTI);
			}
			minorTicks.addElement(tm.create(tm));
			if(DEBUG)
			{
				System.out.println("Minor Time: " + tm);
			}
		}
		// if not nice endings
		if(_endTime.compare(_niceEndTime) < 0)
		{
			for(Time tm = _niceEndTime.create(_endTime); tm
					.compare(_niceEndTime) <= 0; tm.incrementBy(_minorTI))
			{
				if(doMinorRounding)
				{
					tm = roundToNearestNice(tm, _minorTI, _majorTI);
				}
				minorTicks.addElement(tm.create(tm));
				if(DEBUG)
				{
					System.out.println("Minor Time: " + tm);
				}
			}
		}
		// major ticks
		for(Time tm = _niceStartTime.create(_niceStartTime); tm
				.compare(_niceEndTime) <= 0; tm.incrementBy(_majorTI))
		{
			if(doRounding)
			{
				Time tm2 = roundToNearestNice(tm, _majorTI, major2TI);
				tm = tm2.create(tm2);
				if(tm.ceiling(major2TI).compare(tm) != 0) // if end of month
				// don't correct
				{
					tm2.incrementBy(_minusOneDay); // correct to get beginning
				}
				// of nice day
				majorTicks.addElement(tm2);
			}
			else
			{
				majorTicks.addElement(tm.create(tm));
			}
			if(DEBUG)
			{
				System.out.println("Majorr Time: " + tm);
			}
		}
		// major 2 ticks
		int i = 0;
		int mts = majorTicks.size();
		while(i < mts)
		{
			Time tm = (Time) majorTicks.elementAt(i);
			String currentLabel = tm.format(tf2);
			long tmv = 0;
			int ntmv = 0;
			while(currentLabel.equals(tm.format(tf2)))
			{
				tmv += tm.getTimeInMinutes();
				ntmv++;
				i++;
				if(i >= mts)
				{
					break;
				}
				tm = (Time) majorTicks.elementAt(i);
				if(DEBUG)
				{
					System.out.println("Majorr 2 Time: " + tm);
				}
			}
			tmv /= ntmv; // attempt to center the major 2 tick label
			Time tm2 = tm.create(tmv);
			if(tm2.compare(_niceEndTime) > 0)
			{
				continue;
			}
			if(DEBUG)
			{
				System.out.println("Time major tick 2: " + tm.create(tmv));
			}
			majorTicks2.addElement(tm.create(tmv));
		}
		// generate tick data for minor ticks
		double dmin = 0.0f, dmax = 0.0f;
		dmin = _startTime.getTimeInMinutes();
		dmax = _endTime.getTimeInMinutes();
		// create minor tick data
		double[] valuesMinor = new double[minorTicks.size()];
		int index = 0;
		for(Enumeration e = minorTicks.elements(); e.hasMoreElements(); )
		{
			Time tm = (Time) e.nextElement();
			valuesMinor[index] = tm.getTimeInMinutes();
			index++;
		}
		TickData tdminor = new TickData(valuesMinor, null, dmin, dmax);
		// create major tick data
		double[] valuesMajor = new double[majorTicks.size()];
		String[] labelsMajor = new String[majorTicks.size()];
		index = 0;
		for(Enumeration e = majorTicks.elements(); e.hasMoreElements(); )
		{
			Time tm = (Time) e.nextElement();
			labelsMajor[index] = tm.format(tf1);
			if(DEBUG)
			{
				System.out.println("Major Tick Time: " + tm);
				System.out.println("Major Tick Labl: " + tm.format(tf1));
			}
			valuesMajor[index] = tm.getTimeInMinutes();
			index++;
		}
		TickData tdmajor = new TickData(valuesMajor, labelsMajor, dmin, dmax);
		// create major tick data 2
		double[] valuesMajor2 = new double[majorTicks2.size()];
		String[] labelsMajor2 = new String[majorTicks2.size()];
		index = 0;
		for(Enumeration e = majorTicks2.elements(); e.hasMoreElements(); )
		{
			Time tm = (Time) e.nextElement();
			labelsMajor2[index] = tm.format(tf2);
			valuesMajor2[index] = tm.getTimeInMinutes();
			index++;
		}
		TickData tdmajor2 = new TickData(valuesMajor2, labelsMajor2, dmin, dmax);
		return new TickData[]{tdmajor, tdmajor2, tdminor};
	}

	/**
	 * Returns the array of labels for major ticks. Be sure to call generate to
	 * initialize the generator before calling this function
	 *
	 * @return array of strings
	 * @see TickGenerator#generate
	 */
	public String[] getLabels()
	{
		Vector majorTicks = new Vector();
		// major ticks
		for(Time tm = _niceStartTime.create(_niceStartTime); tm
				.compare(_niceEndTime) <= 0; tm.incrementBy(_majorTI))
		{
			majorTicks.addElement(tm.create(tm));
		}
		String[] labelsMajor = new String[majorTicks.size()];
		int index = 0;
		for(Enumeration e = majorTicks.elements(); e.hasMoreElements(); )
		{
			Time tm = (Time) e.nextElement();
			labelsMajor[index] = tm.format(_formatter);
		}
		return labelsMajor;
	}

	/**
	 * gets the format object used in generating the labels
	 */
	public java.text.Format getFormatter()
	{
		return _formatter;
	}

	/**
	 * rounds the time to the nearest nice value given the context of the major
	 * tick interval and minor tick interval
	 */
	public Time roundToNearestNice(Time tm, TimeInterval smti, TimeInterval bgti)
	{
		Time tmr = tm.round(smti);
		Time tmc = tmr.ceiling(bgti);
		long nMins = tmr.getNumberOfMinutesTo(tmc);
		long tiMins = smti.getIntervalInMinutes(tmr);
		if(2 * nMins < tiMins)
		{
			tmc = tmc.create(tmc);
			return tmc;
		}
		else
		{
			tmr = tmr.create(tmr);
			return tmr;
		}
	}

	/**
	 *
	 */
	private TimeFormat getMajorTimeFormat(TimeInterval ti, boolean rounding)
	{
		TimeFormat tfproto = TimeFactory.getInstance().getTimeFormatInstance();
		if(rounding)
		{
			tfproto = new vista.time.SubTimeFormat("ddMMMyyyy HHmm");
		}
		if(ti.toString().indexOf("MIN") >= 0)
		{
			return tfproto.create("HHmm");
		}
		else if(ti.toString().indexOf("HOUR") >= 0)
		{
			return tfproto.create("HHmm");
		}
		else if(ti.toString().indexOf("DAY") >= 0)
		{
			return tfproto.create("dd");
		}
		else if(ti.toString().indexOf("MON") >= 0)
		{
			return tfproto.create("MMM");
		}
		else if(ti.toString().indexOf("YEAR") >= 0)
		{
			return tfproto.create("yyyy");
		}
		else
		{
			return tfproto.create("yyyy");
		}
	}

	/**
	 *
	 */
	private TimeFormat getMajor2TimeFormat(TimeInterval ti, boolean rounding)
	{
		TimeFormat tfproto = TimeFactory.getInstance().getTimeFormatInstance();
		if(rounding)
		{
			tfproto = _formatter;
		}
		tfproto = _formatter;
		if(ti.toString().indexOf("MIN") >= 0)
		{
			return tfproto.create("ddMMMyyyy");
		}
		else if(ti.toString().indexOf("HOUR") >= 0)
		{
			return tfproto.create("ddMMMyyyy");
		}
		else if(ti.toString().indexOf("DAY") >= 0)
		{
			return tfproto.create("MMMyyyy");
		}
		else if(ti.toString().indexOf("MON") >= 0)
		{
			return tfproto.create("yyyy");
		}
		else if(ti.toString().indexOf("YEAR") >= 0)
		{
			return tfproto.create("");
		}
		else
		{
			return tfproto.create("");
		}
	}

	/**
	 *
	 */
	private TimeInterval getMajor2TimeInterval(TimeInterval ti)
	{
		TimeInterval tiproto = tf.getTimeIntervalInstance();
		if(ti.toString().indexOf("MIN") >= 0)
		{
			return tiproto.create("1HOUR");
		}
		else if(ti.toString().indexOf("HOUR") >= 0)
		{
			return tiproto.create("1DAY");
		}
		else if(ti.toString().indexOf("DAY") >= 0)
		{
			return tiproto.create("1MON");
		}
		else if(ti.toString().indexOf("MON") >= 0)
		{
			return tiproto.create("1YEAR");
		}
		else if(ti.toString().indexOf("YEAR") >= 0)
		{
			return tiproto.create("1YEAR");
		}
		else
		{
			return tiproto.create("YEAR");
		}
	}

	/**
	 *
	 */
	public void useDataMinMax(boolean b)
	{
		useNiceEndings = !b;
	}
}
