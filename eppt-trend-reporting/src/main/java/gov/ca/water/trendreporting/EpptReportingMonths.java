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
import java.util.Comparator;
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
	private static final Logger LOG = Logger.getLogger(EpptReportingMonths.class.getName());
	private static final String MONTH_PERIODS_FILENAME = CONFIG_DIR + "trendreporting/TrendReportingPeriod" + CSV_EXT;
	private static EpptReportingMonths seedDataSvc;

	private EpptReportingMonths() throws EpptInitializationException
	{
		LOG.debug("Building SeedDataSvcImpl Object.");
		initMonthPeriods();
	}

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
				Month end = null;
				int index = 0;
				if(list.length > 0)
				{
					index = Integer.parseInt(list[0]);
				}
				if(list.length > 1)
				{
					start = Month.valueOf(list[1].toUpperCase());
				}
				if(list.length > 2)
				{
					end = Month.valueOf(list[2].toUpperCase());
				}
				MONTH_PERIODS.add(new MonthPeriod(index, start, end));
			}
			MONTH_PERIODS.sort(Comparator.comparingInt(MonthPeriod::getIndex));
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
		if(monthPeriod.getStart() == null || monthPeriod.getEnd() == null)
		{
			return new ArrayList<>();
		}
		long between = ChronoUnit.MONTHS.between(LocalDate.now().withDayOfMonth(1).withMonth(monthPeriod.getStart().getValue()),
				LocalDate.now().withDayOfMonth(1).withMonth(monthPeriod.getEnd().getValue()));
		if(between < 0)
		{
			between += 12;
		}
		return LongStream.iterate(0, i -> ++i)
						 .limit(between + 1)
						 .mapToObj(monthPeriod.getStart()::plus)
						 .collect(toList());
	}

	public static final class MonthPeriod
	{
		private final int _index;
		private final Month _start;
		private final Month _end;

		private MonthPeriod(int index, Month start, Month end)
		{
			_index = index;
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
			if(month != null)
			{
				return month.getDisplayName(TextStyle.FULL, Locale.US);
			}
			else
			{
				return "";
			}
		}

		List<YearMonth> getYearMonths(int year)
		{
			List<YearMonth> retval = new ArrayList<>();
			if(_start == null || _end == null)
			{
				return retval;
			}
			if(_start != _end)
			{
				int i = 0;
				long between = ChronoUnit.MONTHS.between(YearMonth.of(2001, _start), YearMonth.of(2010, _end)) % 12;
				Month month = _start.plus(i);
				YearMonth yearMonth;
				if(isPreviousYear(month))
				{
					yearMonth = YearMonth.of(year - 1, month);
				}
				else
				{
					yearMonth = YearMonth.of(year, month);
				}
				while(retval.size() < between + 1)
				{
					retval.add(yearMonth.plusMonths(i));
					i++;
				}
			}
			else
			{
				if(isPreviousYear(_start))
				{
					retval.add(YearMonth.of(year - 1, _start));
				}
				else
				{
					retval.add(YearMonth.of(year, _start));
				}
			}
			return retval;
		}

		private boolean isPreviousYear(Month month)
		{
			long totalMonths = 1 + ChronoUnit.MONTHS.between(YearMonth.of(2001, _start), YearMonth.of(2010, _end)) % 12;
			if(totalMonths == 12 && _start == Month.JANUARY)
			{
				return false;
			}
			return month == Month.OCTOBER || month == Month.NOVEMBER || month == Month.DECEMBER;
		}

		public int getIndex()
		{
			return _index;
		}
	}
}
