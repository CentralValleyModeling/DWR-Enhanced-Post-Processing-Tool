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

package gov.ca.water.trendreporting;

import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

import gov.ca.water.calgui.EpptInitializationException;
import gov.ca.water.calgui.bo.CalLiteGUIException;
import gov.ca.water.calgui.bo.GUILinksAllModelsBO;
import gov.ca.water.calgui.busservice.IGuiLinksSeedDataSvc;
import gov.ca.water.calgui.busservice.impl.GuiLinksSeedDataSvcImpl;
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
public class TrendReportingParameters
{

	private static final Logger LOG = Logger.getLogger(TrendReportingParameters.class.getName());
	private static final List<TrendParameter> PARAMETERS = new ArrayList<>();
	private static final String PARAMETERS_FILENAME = CONFIG_DIR + "trendreporting/TrendReportingParameters" + CSV_EXT;
	private static TrendReportingParameters seedDataSvc;

	private TrendReportingParameters() throws EpptInitializationException
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
	public static TrendReportingParameters getTrendReportingParameters()
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

	public List<TrendParameter> getTrendParameters()
	{
		return new ArrayList<>(PARAMETERS);
	}

	public static void createTrendReportingParametersInstance() throws EpptInitializationException
	{
		if(seedDataSvc == null)
		{
			seedDataSvc = new TrendReportingParameters();
		}
	}

	private void initParameters() throws EpptInitializationException
	{
		IFileSystemSvc fileSystemSvc = new FileSystemSvcImpl();
		String errorStr = "";
		try
		{
			List<String> monthPeriodStrings = fileSystemSvc.getFileData(Paths.get(PARAMETERS_FILENAME), true,
					GuiLinksSeedDataSvcImpl::isNotComments)
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
				PARAMETERS.add(new TrendParameter(index, guiLinkId, overrideTitle));
			}
			PARAMETERS.sort(Comparator.comparingInt(TrendParameter::getIndex));
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

	public static class TrendParameter
	{
		private final int _index;
		private final GUILinksAllModelsBO _guiLink;
		private final String _titleOverride;

		private TrendParameter(int index, Integer guiLinkId, String titleOverride)
		{
			_index = index;
			IGuiLinksSeedDataSvc guiLinkService = GuiLinksSeedDataSvcImpl.getSeedDataSvcImplInstance();
			if(guiLinkId != null)
			{
				_guiLink = guiLinkService.getGuiLink(guiLinkId.toString());
			}
			else
			{
				_guiLink = null;
			}
			_titleOverride = titleOverride;
		}

		private TrendParameter(int index, GUILinksAllModelsBO guiLink, String title)
		{
			_index = index;
			_guiLink = guiLink;
			_titleOverride = title;
		}

		public static TrendParameter create(int index, GUILinksAllModelsBO guiLink, String titleOverride)
		{
			return new TrendParameter(index, guiLink, titleOverride);
		}

		GUILinksAllModelsBO getGuiLink()
		{
			return _guiLink;
		}

		private int getIndex()
		{
			return _index;
		}

		@Override
		public String toString()
		{
			String title = _titleOverride;
			GUILinksAllModelsBO guiLink = getGuiLink();
			if(guiLink != null)
			{
				if(title == null || title.isEmpty())
				{
					title = guiLink.getPlotTitle();
				}
				if(title == null || title.isEmpty())
				{
					title = guiLink.getPrimary()
								   .values()
								   .stream()
								   .filter(p -> !p.isEmpty())
								   .findAny()
								   .orElse("");
				}
			}
			return Optional.ofNullable(title).orElse("");
		}
	}
}
