/*
 * Enhanced Post Processing Tool (EPPT) Copyright (c) 2020.
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

import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.stream.Collectors;

import gov.ca.water.calgui.EpptInitializationException;
import gov.ca.water.calgui.bo.CalLiteGUIException;
import gov.ca.water.calgui.constant.Constant;
import gov.ca.water.calgui.techservice.IFileSystemSvc;
import gov.ca.water.calgui.techservice.impl.FilePredicates;
import gov.ca.water.calgui.techservice.impl.FileSystemSvcImpl;
import org.apache.log4j.Logger;

import static gov.ca.water.calgui.constant.Constant.CONFIG_DIR;
import static gov.ca.water.calgui.constant.Constant.CSV_EXT;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 11-12-2019
 */
public class EpptParameters
{

	private static final Logger LOG = Logger.getLogger(EpptParameters.class.getName());
	private static final List<EpptParameter> PARAMETERS = new ArrayList<>();
	private static final String PARAMETERS_FILENAME = CONFIG_DIR + "trendreporting/TrendReportingParameters" + CSV_EXT;
	private static EpptParameters seedDataSvc;

	private EpptParameters() throws EpptInitializationException
	{
		LOG.debug("Building Trend Reporting Parameters Object.");
		initParameters();
	}

	/**
	 * This method is for implementing the singleton. It will return the
	 * instance of this class if it is empty it will create one.
	 *
	 * @return Will return the instance of this class if it is empty it will
	 * create one.
	 */
	public static EpptParameters getTrendReportingParameters()
	{
		if(seedDataSvc == null)
		{
			throw new IllegalArgumentException();
		}
		else
		{
			return seedDataSvc;
		}
	}

	public List<EpptParameter> getTrendParameters()
	{
		return new ArrayList<>(PARAMETERS);
	}

	public static void createTrendReportingParametersInstance() throws EpptInitializationException
	{
		if(seedDataSvc == null)
		{
			seedDataSvc = new EpptParameters();
		}
	}

	private void initParameters() throws EpptInitializationException
	{
		IFileSystemSvc fileSystemSvc = new FileSystemSvcImpl();
		String errorStr = "";
		try
		{
			List<String> monthPeriodStrings = fileSystemSvc.getFileData(Paths.get(PARAMETERS_FILENAME), true,
					FilePredicates.commentFilter())
														   .stream()
														   .filter(FilePredicates.commentFilter())
														   .collect(Collectors.toList());
			for(String guiLinkString : monthPeriodStrings)
			{
				errorStr = guiLinkString;
				String[] list = guiLinkString.split(Constant.DELIMITER);
				Integer guiLinkId = null;
				String overrideTitle = null;
				int index = 0;
				if(list.length > 0)
				{
					index = Integer.parseInt(list[0]);
				}
				if(list.length > 1)
				{
					String strId = list[1];
					if(strId != null && !strId.isEmpty())
					{
						guiLinkId = Integer.valueOf(strId);
					}
				}
				if(list.length > 2)
				{
					String strOverride = list[2];
					if(strOverride != null && !strOverride.isEmpty())
					{
						overrideTitle = strOverride;
					}
				}
				PARAMETERS.add(new EpptParameter(index, guiLinkId, overrideTitle));
			}
			PARAMETERS.sort(Comparator.comparingInt(EpptParameter::getIndex));
		}
		catch(ArrayIndexOutOfBoundsException ex)
		{
			String errorMessage = "In file \"" + PARAMETERS_FILENAME + "\" has corrupted data at line \"" + errorStr + "\""
					+ Constant.NEW_LINE + "The column number which the data is corrupted is " + ex.getMessage();
			throw new EpptInitializationException(errorMessage, ex);
		}
		catch(NumberFormatException ex)
		{
			String errorMessage = "In file \"" + PARAMETERS_FILENAME + "\" has corrupted data at line \"" + errorStr + "\""
					+ Constant.NEW_LINE + "The checkbox id is not an Integer " + ex.getMessage();
			throw new EpptInitializationException(errorMessage, ex);
		}
		catch(IllegalArgumentException ex)
		{
			String errorMessage = "In file \"" + PARAMETERS_FILENAME + "\" has corrupted data at line \"" + errorStr + "\""
					+ Constant.NEW_LINE + "The model could not be parsed " + ex.getMessage();
			throw new EpptInitializationException(errorMessage, ex);
		}
		catch(CalLiteGUIException ex)
		{
			throw new EpptInitializationException("Failed to get file data for file: " + PARAMETERS_FILENAME, ex);
		}
	}
}
