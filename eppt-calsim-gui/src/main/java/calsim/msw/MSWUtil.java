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

package calsim.msw;

import java.util.Vector;

import calsim.app.CSVParser;
import vista.time.Time;
import vista.time.TimeFactory;
import vista.time.TimeInterval;
import vista.time.TimeWindow;

public class MSWUtil
{
	public static boolean position = false;
	public static int nperiods = 0;
	public static int posyr = 0;
	public static int dvnum = 0;
	public static int ndv = 1;
	public static boolean firstPass = true;
	public static String positionStartMonth = "OCT";
	public static String positionContinueMonth = "OCT";
	public static int posStartYear = 1921;
	public static MYDate positionStart = new MYDate();
	public static MYDate positionEnd = new MYDate();
	protected static TimeInterval dayTI, monTI;
	private static TimeFactory tF;

	//Generate the TimeFactory.  The TimeFactory object will be used
	//many times and there is only a need for one of them.
	protected static void createTimeFactory()
	{
		tF = TimeFactory.getInstance();
		dayTI = tF.createTimeInterval(1, TimeInterval.DAY_INTERVAL);
		monTI = tF.createTimeInterval(1, TimeInterval.MONTH_INTERVAL);
	}

	//The following method returns a TimeWindow object.
	protected static TimeWindow createTimeWindow(String st, String et)
	{
		return createTimeWindow(tF.createTime(st), tF.createTime(et));
	}

	//The following method returns a TimeWindow object.
	protected static TimeWindow createTimeWindow(Time sTime, Time eTime)
	{
		return tF.createTimeWindow(sTime, eTime);
	}

	protected static void parseFile(String file, Vector details, Vector loc)
	{

		CSVParser parser = new CSVParser(file);

		//Fill vector and track operation line locations.
		int iL = 0;
		String[] fields;
		while(true)
		{
			fields = parser.nextLine();
			if(fields == null)
			{
				break;
			}
			if(fields[0].indexOf("*") == 0)
			{
				loc.addElement(new Integer(iL));
			}
			details.addElement(fields);
			iL++;
		}
		parser.close();
	}
}
