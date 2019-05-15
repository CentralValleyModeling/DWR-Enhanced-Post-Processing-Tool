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
 * The type of the data..
 *
 * @author Nicky Sandhu
 * @version $Id: DataType.java,v 1.1 2003/10/02 20:49:21 redwood Exp $
 */
public class DataType
{
	/**
	 * Undefined
	 */
	public static final int UNDEFINED = 0;
	/**
	 * Regular - Interval Time Series Data
	 */
	public static final int REGULAR_TIME_SERIES = 100;
	public static final int REGULAR_TIME_SERIES_DOUBLE = 105;
	/**
	 * Irregular - Interval Time Series Data
	 */
	public static final int IRREGULAR_TIME_SERIES = 110;
	public static final int IRREGULAR_TIME_SERIES_DOUBLE = 115;
	/**
	 * Paired Data
	 */
	public static final int PAIRED = 200;
	/**
	 * Text Data
	 */
	public static final int TEXT = 300;

	/**
	 *
	 */
	public static int getType(String repr)
	{
		if(repr == getTypeRepresentation(REGULAR_TIME_SERIES))
		{
			return REGULAR_TIME_SERIES;
		}
		else if(repr == getTypeRepresentation(IRREGULAR_TIME_SERIES))
		{
			return IRREGULAR_TIME_SERIES;
		}
		else if(repr == getTypeRepresentation(PAIRED))
		{
			return PAIRED;
		}
		else
		{
			return UNDEFINED;
		}
	}

	/**
	 * gets string representation of types
	 */
	public static String getTypeRepresentation(int type)
	{
		switch(type)
		{
			case REGULAR_TIME_SERIES:
				return "REGULAR TIME SERIES";
			case IRREGULAR_TIME_SERIES:
				return "IRREGULAR TIME SERIES";
			case PAIRED:
				return "PAIRED";
			default:
				return "UNDEFINED";
		}
	}

	public static String[] getDataTypes()
	{
		return new String[]{"PER-AVER", "PER-CUM", "INST-VAL", "INST-CUM"};
	}
}
