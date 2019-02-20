/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */

package gov.ca.water.calgui.bo;
//! Representation of GUI_Links3 (GUI -> DSS retrieval) record

/**
 * This is used to hold the information of GUI_Links3.csv in memory. Please
 * refer to the Model Developers' documentation for table for more information.
 *
 * @author Mohan
 */
public class GUILinks3BO
{
	/**
	 * The numeric portion of the gui ID from GUI_Links3.csv, It is combined
	 * with the string "ckbp" padded with leading zeros (e.g."1" is mapped to
	 * "ckbp001").
	 */
	private String iD;
	/**
	 * One or more C-PART/D-PART DSS record identifiers specifying the primary
	 * data set(s) plotted. Multiple identifiers can be entered with a plus sign
	 * "+" between, like "D_CCWDINTK/FLOW-DELIVERY+D_CCWDVCOR/FLOW-DELIVERY"
	 */
	private String primary;
	/**
	 * A single C-PART/D-PART DSS record identifiers specifying the secondary
	 * data set plotted against the right y-axis. Appending the string (-1)
	 * shifts the data by one month, like "JP_EC_STD/SALINITY(-1)"
	 */
	private String secondary;
	/**
	 * Left-hand (primary) axis title.
	 */
	private String yTitle;
	/**
	 * Chart title.
	 */
	private String title;
	/**
	 * Right-hand (secondary) title
	 */
	private String yTitle2;

	public GUILinks3BO(String iD, String primary, String secondary, String yTitle, String title, String yTitle2)
	{
		this.iD = iD;
		this.primary = primary;
		this.secondary = secondary;
		this.yTitle = yTitle;
		this.title = title;
		this.yTitle2 = yTitle2;
	}

	public String getiD()
	{
		return iD;
	}

	public String getPrimary()
	{
		return primary;
	}

	public String getSecondary()
	{
		return secondary;
	}

	public String getyTitle()
	{
		return yTitle;
	}

	public String getTitle()
	{
		return title;
	}

	public String getyTitle2()
	{
		return yTitle2;
	}

}