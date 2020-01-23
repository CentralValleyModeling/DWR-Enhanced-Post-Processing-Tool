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

package gov.ca.water.calgui.bo;

public class DetailedIssue
{

	private final int _detailedIssueId;
	private final int _subModuleID;
	private final String _linkedVar;
	private final int _guiLink;
	private final int _thresholdLink;
	private final String _title;
	private boolean _isExecutiveReport;

	public DetailedIssue(int detailedIssueId, int subModuleID, String linkedVar, int guiLink, int thresholdLink, String title,
						 boolean isExecutiveReport)
	{
		_detailedIssueId = detailedIssueId;
		_subModuleID = subModuleID;
		_linkedVar = linkedVar;
		_guiLink = guiLink;
		_thresholdLink = thresholdLink;
		_title = title;
		_isExecutiveReport = isExecutiveReport;
	}

	public int getDetailedIssueId()
	{
		return _detailedIssueId;
	}

	public String getTitle()
	{
		return _title;
	}

	public String getLinkedVar()
	{
		return _linkedVar;
	}

	public int getSubModuleID()
	{
		return _subModuleID;
	}

	public int getGuiLink()
	{
		return _guiLink;
	}

	public int getThresholdLink()
	{
		return _thresholdLink;
	}

	public boolean isExecutiveReport()
	{
		return _isExecutiveReport;
	}
}
