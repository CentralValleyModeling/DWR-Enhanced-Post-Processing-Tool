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
import java.time.LocalDate;
import java.time.Month;
import java.time.YearMonth;
import java.time.format.DateTimeFormatter;
import java.time.format.TextStyle;
import java.time.temporal.ChronoUnit;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Locale;
import java.util.Objects;
import java.util.stream.LongStream;

import gov.ca.water.calgui.EpptInitializationException;
import gov.ca.water.calgui.bo.CalLiteGUIException;
import gov.ca.water.calgui.bo.GUILinksAllModelsBO;
import gov.ca.water.calgui.busservice.impl.GuiLinksSeedDataSvcImpl;
import gov.ca.water.calgui.constant.Constant;
import gov.ca.water.calgui.techservice.IFileSystemSvc;
import gov.ca.water.calgui.techservice.impl.FileSystemSvcImpl;
import org.apache.log4j.Logger;
import org.joda.time.Months;

import static gov.ca.water.calgui.constant.Constant.CONFIG_DIR;
import static gov.ca.water.calgui.constant.Constant.CSV_EXT;
import static java.util.stream.Collectors.toList;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 07-25-2019
 */
public final class EpptReportingMonths
{
	private static final List<MonthPeriod> MONTH_PERIODS = new ArrayList<>();
	private static final Logger LOG = Logger.getLogger(GuiLinksSeedDataSvcImpl.class.getName());
	private static final String MONTH_PERIODS_FILENAME = CONFIG_DIR + "/TrendReportingPeriod" + CSV_EXT;
	private static EpptReportingMonths seedDataSvc;

	/**
	 * This will read the gui_links.csv files and build the list and maps of {@link GUILinksAllModelsBO}
	 */
	private EpptReportingMonths() throws EpptInitializationException
	{
		LOG.debug("Building SeedDataSvcImpl Object.");
		initMonthPeriods();
	}

	/**
	 * This method is for implementing the singleton. It will return the
	 * instance of this class if it is empty it will create one.
	 *
	 * @return Will return the instance of this class if it is empty it will
	 * create one.
	 */
	public static EpptReportingMonths getTrendReportingMonths()
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

	public static void createTrendReportingMonthsInstance() throws EpptInitializationException
	{
		if(seedDataSvc == null)
		{
			seedDataSvc = new EpptReportingMonths();
		}
	}

	/**
	 * This will tell whether the line is comment or not.
	 *
	 * @param line The line to be checked.
	 * @return Will return true if the line id not comment.
	 */
	private static boolean isNotComments(String line)
	{
		return !line.startsWith(Constant.EXCLAMATION);
	}

	private void initMonthPeriods() throws EpptInitializationException
	{
		IFileSystemSvc fileSystemSvc = new FileSystemSvcImpl();
		String errorStr = "";
		try
		{
			List<String> monthPeriodStrings = fileSystemSvc.getFileData(Paths.get(MONTH_PERIODS_FILENAME), true,
					GuiLinksSeedDataSvcImpl::isNotComments);
			for(String guiLinkString : monthPeriodStrings)
			{
				errorStr = guiLinkString;
				//don't count comment rows
				if(guiLinkString.length() > 0)
				{
					if(guiLinkString.trim().charAt(0) == '\uFEFF' || guiLinkString.trim().charAt(0) == '!' || guiLinkString.trim().charAt(0) == '#')
					{
						continue;
					}
				}
				else
				{
					continue;
				}


				String[] list = guiLinkString.split(Constant.DELIMITER);
				Month start = null;
				Month end;
				DateTimeFormatter.ofPattern("MMM");
				if(list.length > 0)
				{
					start = Month.valueOf(list[0].toUpperCase());
				}
				if(list.length > 1)
				{
					end = Month.valueOf(list[1].toUpperCase());
				}
				else
				{
					end = start;
				}
				if(start == null)
				{
					throw new IllegalArgumentException("Start Month cannot be null");
				}
				MONTH_PERIODS.add(new MonthPeriod(start, end));
			}
		}
		catch(ArrayIndexOutOfBoundsException ex)
		{
			String errorMessage = "In file \"" + MONTH_PERIODS_FILENAME + "\" has corrupted data at line \"" + errorStr + "\""
					+ Constant.NEW_LINE + "The column number which the data is corrupted is " + ex.getMessage();
			throw new EpptInitializationException(errorMessage, ex);
		}
		catch(NumberFormatException ex)
		{
			String errorMessage = "In file \"" + MONTH_PERIODS_FILENAME + "\" has corrupted data at line \"" + errorStr + "\""
					+ Constant.NEW_LINE + "The checkbox id is not an Integer " + ex.getMessage();
			throw new EpptInitializationException(errorMessage, ex);
		}
		catch(IllegalArgumentException ex)
		{
			String errorMessage = "In file \"" + MONTH_PERIODS_FILENAME + "\" has corrupted data at line \"" + errorStr + "\""
					+ Constant.NEW_LINE + "The model could not be parsed " + ex.getMessage();
			throw new EpptInitializationException(errorMessage, ex);
		}
		catch(CalLiteGUIException ex)
		{
			throw new EpptInitializationException("Failed to get file data for file: " + MONTH_PERIODS_FILENAME, ex);
		}
	}

	public static List<MonthPeriod> getAllMonthPeriods()
	{
		return new ArrayList<>(MONTH_PERIODS);
	}

	public static List<Month> getMonths(MonthPeriod monthPeriod)
	{
		long between = ChronoUnit.MONTHS.between(LocalDate.now().withDayOfMonth(1).withMonth(monthPeriod._start.getValue()),
				LocalDate.now().withDayOfMonth(1).withMonth(monthPeriod._end.getValue()));
		if(between < 0)
		{
			between += 12;
		}
		return LongStream.iterate(0, i -> ++i)
						 .limit(between + 1)
						 .mapToObj(monthPeriod._start::plus)
						 .collect(toList());
	}

	public static List<Month> getMonths(List<MonthPeriod> monthPeriod)
	{
		return monthPeriod.stream()
						  .map(EpptReportingMonths::getMonths)
						  .flatMap(Collection::stream)
						  .collect(toList());
	}

	public static class MonthPeriod
	{
		private final Month _start;
		private final Month _end;

		private MonthPeriod(Month start, Month end)
		{
			_start = start;
			_end = end;
		}

		public Month getStart()
		{
			return _start;
		}

		public Month getEnd()
		{
			return _end;
		}

		@Override
		public String toString()
		{
			if(Objects.equals(_start, _end))
			{
				return formatMonth(_start);
			}
			else
			{
				return formatMonth(_start) + " - " + formatMonth(_end);
			}
		}

		private String formatMonth(Month month)
		{
			return month.getDisplayName(TextStyle.FULL, Locale.US);
		}

		public List<YearMonth> getYearMonths(int year)
		{
			List<YearMonth> retval = new ArrayList<>();
			if(_start != _end)
			{
				if(needLookback())
				{
					year--;
				}
				retval.add(YearMonth.of(year, _start));
				int i = 1;
				while(_start.plus(i) != _end.plus(1))
				{
					if(_start.plus(i) == Month.JANUARY)
					{
						year++;
					}
					retval.add(YearMonth.of(year, _start.plus(i)));
					i++;
				}
			}
			else
			{
				retval.add(YearMonth.of(year, _start));
			}
			return retval;
		}

		public boolean needLookback()
		{
			return Month.values().length + 1 - _start.getValue() < _end.getValue();
		}
	}
}
