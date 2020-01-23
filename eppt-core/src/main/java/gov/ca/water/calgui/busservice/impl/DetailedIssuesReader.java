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

package gov.ca.water.calgui.busservice.impl;

import java.io.BufferedReader;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Level;

import com.google.common.flogger.FluentLogger;
import gov.ca.water.calgui.EpptInitializationException;
import gov.ca.water.calgui.bo.DetailedIssue;
import gov.ca.water.calgui.constant.Constant;

import hec.lang.Const;

public class DetailedIssuesReader
{
	private static final FluentLogger LOGGER = FluentLogger.forEnclosingClass();

	private static final int DETAILS_ID = 0;
	private static final int MODULE_ID = 1;
	private static final int LINKED_VAR = 2;
	private static final int SECTION_ID = 3;
	private static final int GUI_LINK = 4;
	private static final int THRESHOLD_LINK = 5;
	private static final int TITLE = 6;

	private static final String EXEC_DETAILS_REPORT = "ED";
	private static final String SUMMARY_STATS_REPORT = "ST";
	private static DetailedIssuesReader instance;


	private final Path _detailedIssuesCSVPath;
	private final List<DetailedIssue> _detailedIssues;

	private DetailedIssuesReader(Path detailedIssuesCSVPath) throws EpptInitializationException
	{
		_detailedIssuesCSVPath = detailedIssuesCSVPath;
		_detailedIssues = read();
	}

	public static void createDetailedIssues() throws EpptInitializationException
	{
		instance = new DetailedIssuesReader(Constant.DETAILS_CSV);
	}

	public List<DetailedIssue> getDetailedIssues()
	{
		return _detailedIssues;
	}

	public static DetailedIssuesReader getInstance()
	{
		return instance;
	}

	private List<DetailedIssue> read() throws EpptInitializationException
	{
		String line;
		String csvSplitBy = ",";
		List<DetailedIssue> retval = new ArrayList<>();
		LOGGER.at(Level.INFO).log("Reading DTS Configuration %s", _detailedIssuesCSVPath);
		try(BufferedReader br = Files.newBufferedReader(_detailedIssuesCSVPath))
		{
			//skip first line
			int i = 0;
			while((line = br.readLine()) != null)
			{
				if(i == 0 || line.startsWith("!") || line.startsWith("#"))
				{
					i++;
					continue;
				}
				String[] row = line.split(csvSplitBy);

				//skip comments
				if(row.length > 0)
				{
					String firstString = row[0];
					String trimmedString = firstString.trim();
					//if there is no model id then we continue the for loop
					if(trimmedString.length() > 0)
					{
						char firstChar = trimmedString.charAt(0);

						if(firstChar == '#' || firstChar == '!')
						{
							continue;
						}
					}
					else
					{
						continue;
					}
				}

				int detailsId = Integer.parseInt(row[DETAILS_ID]);
				String subModuleId = row[MODULE_ID];
				int subModuleID;
				try
				{
					subModuleID = Integer.parseInt(subModuleId);
				}
				catch(NumberFormatException e)
				{
					subModuleID = -1;
					LOGGER.atFiner().withCause(e).log("Not a valid sub module ID");
				}
				String linkedVar = row[LINKED_VAR];

				int guiLink = Const.UNDEFINED_INT;
				int thresholdLink = Const.UNDEFINED_INT;

				if(row.length > GUI_LINK && !"".equals(row[GUI_LINK]) && !"N/A".equals(row[GUI_LINK]))
				{
					guiLink = Integer.parseInt(row[GUI_LINK]);
				}
				if(row.length > THRESHOLD_LINK && !"".equals(row[THRESHOLD_LINK]))
				{
					try
					{
						thresholdLink = Integer.parseInt(row[THRESHOLD_LINK]);
					}
					catch(NumberFormatException e)
					{
						thresholdLink = -1;
						LOGGER.atFiner().withCause(e).log("Not a valid sub module ID");
					}
				}
				boolean isExecutiveReport = false;
				if(row.length > THRESHOLD_LINK && !"".equals(row[SECTION_ID]))
				{
					isExecutiveReport = row[SECTION_ID].contains(EXEC_DETAILS_REPORT);
				}
				String title = "";
				if(row.length > TITLE && !"".equals(row[TITLE]))
				{
					title = row[TITLE];
				}

				retval.add(new DetailedIssue(detailsId, subModuleID, linkedVar, guiLink, thresholdLink, title, isExecutiveReport));

			}
		}
		catch(IOException e)
		{
			throw new EpptInitializationException("Error reading the csv file: " + _detailedIssuesCSVPath, e);
		}
		return retval;
	}

}
