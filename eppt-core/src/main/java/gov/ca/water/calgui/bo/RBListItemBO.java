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

	private final String _label;
	private final String _fullname;
	private boolean _isSelected = false;
	private String _svFilename;

	public RBListItemBO(String label, String label2)
	{
		this._label = label2;
		this._fullname = label;
		this._svFilename = "";
	}

	public boolean isSelected()
	{
		return _isSelected;
	}

	public void setSelected(boolean isSelected)
	{
		this._isSelected = isSelected;
	}

	public String getSVFilename()
	{
		return _svFilename;
	}

	public void setSVFilename(String svf)
	{
		_svFilename = svf;
	}

	@Override
	public String toString()
	{
		return _fullname;
	}

	public String getLabel()
	{
		return _label;
	}
}
