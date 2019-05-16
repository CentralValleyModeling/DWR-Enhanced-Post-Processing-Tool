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

package gov.ca.water.reportengine.filechanges;

import java.util.ArrayList;
import java.util.List;

public class CodeChangesSubType
{

	private final List<String> _wreslFiles = new ArrayList<>();
	private final String _name;

	public CodeChangesSubType(String name)
	{
		_name = name;
	}


	public void addWreslFile(String file)
	{
		_wreslFiles.add(file);
	}

	public String getName()
	{
		return _name;
	}

	public List<String> getWreslFiles()
	{
		return _wreslFiles;
	}

}
