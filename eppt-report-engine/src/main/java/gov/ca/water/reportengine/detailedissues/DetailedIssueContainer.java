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

import java.util.ArrayList;
import java.util.List;

public class DetailedIssueContainer
{

	private final String _name;
	private final List<DetailedIssue> _detailedIssues = new ArrayList<>();

	public DetailedIssueContainer(String name)
	{

		_name = name;
	}

	void addDetailedIssue(DetailedIssue di)
	{
		_detailedIssues.add(di);
	}


}
