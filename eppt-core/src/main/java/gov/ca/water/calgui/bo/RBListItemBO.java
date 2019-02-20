/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */

package gov.ca.water.calgui.bo;
//! Special class combining string and radio button in a list

/**
 * RBListItem is a helper for the FileDialog class. It holds DV and SV path information and selection (base) status of scenarios
 * listed in the results control panel.
 *
 * @author tslawecki
 */
public class RBListItemBO
{

	private final String label;
	private final String fullname;
	private boolean isSelected = false;
	private String svFilename;

	public RBListItemBO(String label, String label2)
	{
		this.label = label2;
		this.fullname = label;
		this.svFilename = "";
	}

	public boolean isSelected()
	{
		return isSelected;
	}

	public void setSelected(boolean isSelected)
	{
		this.isSelected = isSelected;
	}

	public String getSVFilename()
	{
		return svFilename;
	}

	public void setSVFilename(String svf)
	{
		svFilename = svf;
	}

	@Override
	public String toString()
	{
		return fullname;
	}

	public String toString2()
	{
		return label;
	}
}