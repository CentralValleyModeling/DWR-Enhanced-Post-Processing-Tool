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

package gov.ca.water.reportengine.reportheader;

import java.util.List;

public class ReportHeader
{
	private final String _author;
	private final String _title;
	private final String _baseFile;
	private final List<String> _alternatives;
	private final String _subtitle;

	public ReportHeader(String author, String title, String subtitle, String baseFile, List<String> alternatives)
	{
		_author = author;
		_title = title;
		_baseFile = baseFile;
		_alternatives = alternatives;
		_subtitle = subtitle;
	}

	public String getAuthor()
	{
		return _author;
	}

	public String getBaseFile()
	{
		return _baseFile;
	}

	public List<String> getAlternativeNames()
	{
		return _alternatives;
	}

	public String getTitle()
	{
		return _title;
	}

	public String getSubTitle()
	{
		return _subtitle;
	}
}
