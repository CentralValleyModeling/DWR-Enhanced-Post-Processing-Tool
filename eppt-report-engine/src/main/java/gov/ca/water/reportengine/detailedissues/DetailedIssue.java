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

package gov.ca.water.reportengine.detailedissues;

public class DetailedIssue
{

	private final int _subModuleID;
	private final String _linkedVar;
	private final int _guiLink;
	private final int _thresholdLink;

	public DetailedIssue(int subModuleID, String linkedVar, int guiLink, int thresholdLink)
	{
		_subModuleID = subModuleID;
		_linkedVar = linkedVar;
		_guiLink = guiLink;
		_thresholdLink = thresholdLink;

	}

	public String getLinkedVar()
	{
		return _linkedVar;
	}

	public int getSubModuleID()
	{
		return _subModuleID;
	}

	int getGuiLink()
	{
		return _guiLink;
	}

	int getThresholdLink()
	{
		return _thresholdLink;
	}

}
