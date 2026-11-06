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

import java.util.Hashtable;

/**
 * @author Joel Fenolio
 * @author Clay Booher (added compareTo)
 */
public class MYDate
{ // for jdk 1.5+ implements Comparable<MYDate> {

	private static Hashtable monthIndex, monthName;
	private String month;
	private Integer year;

	protected MYDate()
	{
		if(monthIndex == null || monthName == null)
		{
			createMonthTables();
		}
		month = null;
		year = null;
	}

	protected MYDate(String m, int y)
	{
		if(monthIndex == null || monthName == null)
		{
			createMonthTables();
		}
		month = m.toUpperCase();
		year = Integer.valueOf(y);
	}

	protected MYDate(String m, Integer y)
	{
		if(monthIndex == null || monthName == null)
		{
			createMonthTables();
		}
		month = m.toUpperCase();
		year = y;
	}

	protected MYDate(int m, int y)
	{
		if(monthIndex == null || monthName == null)
		{
			createMonthTables();
		}
		month = monthName.get(Integer.valueOf(m)).toString();
		year = Integer.valueOf(y);
	}

	protected MYDate(Integer m, Integer y)
	{
		if(monthIndex == null || monthName == null)
		{
			createMonthTables();
		}
		month = monthName.get(m).toString();
		year = y;
	}

	private void createMonthTables()
	{
		monthIndex = new Hashtable(12);
		monthIndex.put("JAN", Integer.valueOf(1));
		monthIndex.put("FEB", Integer.valueOf(2));
		monthIndex.put("MAR", Integer.valueOf(3));
		monthIndex.put("APR", Integer.valueOf(4));
		monthIndex.put("MAY", Integer.valueOf(5));
		monthIndex.put("JUN", Integer.valueOf(6));
		monthIndex.put("JUL", Integer.valueOf(7));
		monthIndex.put("AUG", Integer.valueOf(8));
		monthIndex.put("SEP", Integer.valueOf(9));
		monthIndex.put("OCT", Integer.valueOf(10));
		monthIndex.put("NOV", Integer.valueOf(11));
		monthIndex.put("DEC", Integer.valueOf(12));

		monthName = new Hashtable(12);
		monthName.put(Integer.valueOf(1), "JAN");
		monthName.put(Integer.valueOf(2), "FEB");
		monthName.put(Integer.valueOf(3), "MAR");
		monthName.put(Integer.valueOf(4), "APR");
		monthName.put(Integer.valueOf(5), "MAY");
		monthName.put(Integer.valueOf(6), "JUN");
		monthName.put(Integer.valueOf(7), "JUL");
		monthName.put(Integer.valueOf(8), "AUG");
		monthName.put(Integer.valueOf(9), "SEP");
		monthName.put(Integer.valueOf(10), "OCT");
		monthName.put(Integer.valueOf(11), "NOV");
		monthName.put(Integer.valueOf(12), "DEC");
	}

	protected String getMonth()
	{
		return month;
	}

	void setMonth(String m)
	{
		month = m.toUpperCase();
	}

	void setMonth(int m)
	{
		month = monthName.get(Integer.valueOf(m)).toString();
	}

	protected int getMonthIndex()
	{
		Integer index = (Integer) monthIndex.get(month);
		return index.intValue();
	}

	protected int getYearIndex()
	{
		return year.intValue();
	}

	public Integer getYear()
	{
		return year;
	}

	void setYear(Integer y)
	{
		year = y;
	}

	void setYear(int y)
	{
		year = Integer.valueOf(y);
	}

	public String toString()
	{
		return month + year.toString();
	}

	/**
	 * Compares this with other (month and year only) to determine if they differ, and which is
	 * earlier if they differ.
	 *
	 * @param other
	 * @return -1 is date is earlier than other's date; 0 if
	 */
	public int compareTo(MYDate other)
	{
		if(getYearIndex() < other.getYearIndex())
		{
			return -1;
		}
		else if(getYearIndex() > other.getYearIndex())
		{
			return 1;
		}
		else
		{
			if(getMonthIndex() < other.getMonthIndex())
			{
				return -1;
			}
			else if(getMonthIndex() > other.getMonthIndex())
			{
				return 1;
			}
			else
			{
				return 0;
			}
		}
	}
}

