package gov.ca.water.trendreporting.monthpicker;

import java.text.DateFormatSymbols;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.util.Calendar;
import java.util.Date;
import java.util.Locale;
import java.util.logging.Logger;

import javafx.geometry.Pos;
import javafx.scene.Group;
import javafx.scene.Node;
import javafx.scene.layout.StackPane;
import javafx.scene.paint.Color;
import javafx.scene.shape.Circle;
import javafx.scene.shape.Line;

public class FXCalendarUtility
{

	private static final Logger LOG = Logger.getLogger(FXCalendarUtility.class.getCanonicalName());
	private static final int MILLIS_IN_DAY = 1000 * 60 * 60 * 24;
	private static final String DATE_FORMAT = "MM/yyyy";
	private final SimpleDateFormat _displayDateFormat = new SimpleDateFormat("MMM/yyyy");
	private final DateTimeFormatter _monthYearFormatter = DateTimeFormatter.ofPattern("MMM/yyyy");
	private String[] _shortestWeekDays;  // "","S","M","T","W","T","F","S"
	private String[] _shortWeekDays;     // "","Sun","Mon","Tue","Wed","Thu","Fri","Sat"
	private String[] _weekDays;           // "","Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"
	private String[] _shortMonths;        // "Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec",""
	private String[] _months;              // "January","February","March","April","May","June","July","August","September","October","November","December",""

	public static Calendar getCalendar()
	{
		return Calendar.getInstance();
	}

	public static Date getCurrentDate()
	{
		return new Date();
	}

	public static Calendar getCurrentDateCalendar()
	{
		Calendar c = Calendar.getInstance();
		c.setTime(new Date());
		return c;
	}

	public static Calendar getDateCalendar(Date date)
	{
		Calendar c = Calendar.getInstance();
		c.setTime(date);
		return c;
	}

	public static long getCurrentTime()
	{
		return (new Date()).getTime();
	}

	public static String getCurrentFormattedDate()
	{
		return new SimpleDateFormat(DATE_FORMAT).format(getCurrentTime());
	}

	public static String getPreviousDate()
	{
		return new SimpleDateFormat(DATE_FORMAT).format(getCurrentTime() - MILLIS_IN_DAY);
	}

	public static String getNextDate()
	{
		return new SimpleDateFormat(DATE_FORMAT).format(getCurrentTime() + MILLIS_IN_DAY);
	}

	public static Calendar getDate(Integer day, Integer month, Integer year)
	{
		try
		{
			String strDate = day + "/" + (month + 1) + "/" + year;
			Date date = new SimpleDateFormat(DATE_FORMAT).parse(strDate);
			Calendar c = getCalendar();
			c.setTime(date);
			return c;
		}
		catch(ParseException e)
		{
			LOG.fine(e.getMessage());
		}
		return null;
	}

	public static void setBaseColorToNode(Node node, Color baseColor)
	{
		node.setStyle("-fx-base:" + rgbToHex(baseColor) + ";");
	}

	public static String rgbToHex(Color color)
	{
		int i = (int) Math.round(color.getRed() * 255D);
		int j = (int) Math.round(color.getGreen() * 255D);
		int k = (int) Math.round(color.getBlue() * 255D);
		return "#" + toHex(i) + toHex(j) + toHex(k);
	}

	private static String toHex(int code)
	{
		String str = "0123456789ABCDEF";
		return str.charAt(code / 16) + "" + str.charAt(code % 16);
	}

	public static Group getDateImage()
	{
		Group gp = new Group();
		StackPane img = new StackPane();
		double imgSize = 15.0;
		double imgSizeQuar = imgSize / 4;
		img.setPrefSize(imgSize, imgSize);
		img.getStyleClass().add("calendar-image");
		img.setAlignment(Pos.TOP_LEFT);

		/* Vertical Lines */
		Line l = getLine(0, 0, 0, imgSize, imgSizeQuar, 0);
		Line l1 = getLine(0, 0, 0, imgSize, imgSizeQuar * 2, 0);
		Line l2 = getLine(0, 0, 0, imgSize, imgSizeQuar * 3, 0);
		/* Horizontal Lines */
		Line l3 = getLine(0, 0, imgSize, 0, 0, imgSizeQuar);
		Line l4 = getLine(0, 0, imgSize, 0, 0, imgSizeQuar * 2);
		Line l5 = getLine(0, 0, imgSize, 0, 0, imgSizeQuar * 3);
		/* Circle */
		Circle c = new Circle();
		c.getStyleClass().add("calendar-image-circle");
		c.setRadius(imgSizeQuar / 2);
		c.setTranslateX(imgSizeQuar * 3);
		c.setTranslateY(imgSizeQuar);
		img.getChildren().addAll(l, l1, l2, l3, l4, l5, c);

		gp.getChildren().add(img);
		gp.setTranslateX(5);
		gp.setTranslateY(1);
		return gp;
	}

	private static Line getLine(double startX, double startY, double endX, double endY, double translateX, double translateY)
	{
		Line l = new Line();
		l.getStyleClass().add("calendar-image-line");
		l.setStartX(startX);
		l.setStartY(startY);
		l.setEndX(endX);
		l.setEndY(endY);
		l.setSmooth(true);
		l.setTranslateX(translateX);
		l.setTranslateY(translateY);
		return l;
	}

	public Date convertStringtoDate(String str)
	{
		try
		{
			return _displayDateFormat.parse(str);
		}
		catch(ParseException e)
		{
			LOG.severe(e.getMessage());
			return null;
		}
	}

	public String getFormattedDate(Integer day, Integer month, Integer year)
	{
		LocalDate date = LocalDate.of(year, month, day);
		return _monthYearFormatter.format(date);
	}

	public void resetShortestWeekDays(Locale locale)
	{
		_shortestWeekDays = null;
		getShortestWeekDays(locale);
	}

	public String[] getShortestWeekDays(Locale locale)
	{
		if(_shortestWeekDays == null || _shortestWeekDays.length == 0)
		{
			_shortestWeekDays = getDayNames("xs", locale);
			// If Monday is first day of week.
			if(Calendar.getInstance(locale).getFirstDayOfWeek() == 2)
			{
				String dum = _shortestWeekDays[1];
				for(int i = 1; i < 7; i++)
				{
					_shortestWeekDays[i] = _shortestWeekDays[i + 1];
				}
				_shortestWeekDays[7] = dum;
			}
		}
		return _shortestWeekDays;
	}

	public String[] getShortWeekDays(Locale locale)
	{
		if(_shortWeekDays == null || _shortWeekDays.length == 0)
		{
			_shortWeekDays = getDayNames("s", locale);
		}
		// If Monday is first day of week.
		if(Calendar.getInstance(locale).getFirstDayOfWeek() == 2)
		{
			String dum = _shortWeekDays[1];
			for(int i = 1; i < 7; i++)
			{
				_shortWeekDays[i] = _shortWeekDays[i + 1];
			}
			_shortWeekDays[7] = dum;
		}
		return _shortWeekDays;
	}

	public String[] getWeekDays(Locale locale)
	{
		if(_weekDays == null || _weekDays.length == 0)
		{
			_weekDays = getDayNames(null, locale);
		}
		// If Monday is first day of week.
		if(Calendar.getInstance(locale).getFirstDayOfWeek() == 2)
		{
			String dum = _weekDays[1];
			for(int i = 1; i < 7; i++)
			{
				_weekDays[i] = _weekDays[i + 1];
			}
			_weekDays[7] = dum;
		}
		return _weekDays;
	}

	public void resetShortMonths(Locale locale)
	{
		_shortMonths = null;
		getShortMonths(locale);
	}

	public String[] getShortMonths(Locale locale)
	{
		if(_shortMonths == null || _shortMonths.length == 0)
		{
			_shortMonths = getMonthNames("s", locale);
		}
		return _shortMonths;
	}

	public void resetMonths(Locale locale)
	{
		_months = null;
		getMonths(locale);
	}

	public String[] getMonths(Locale locale)
	{
		if(_months == null || _months.length == 0)
		{
			_months = getMonthNames(null, locale);
		}
		return _months;
	}

	private String[] getDayNames(String type, Locale locale)
	{
		if("xs".equalsIgnoreCase(type))
		{
			String[] days = new DateFormatSymbols(locale).getShortWeekdays();
			String[] xsDays = new String[days.length];
			for(int i = 0; i < days.length; i++)
			{
				xsDays[i] = ("".equals(days[i])) ? days[i] : (days[i].charAt(0) + "");
			}
			return xsDays;
		}
		if("s".equalsIgnoreCase(type))
		{
			return new DateFormatSymbols(locale).getShortWeekdays();
		}
		else
		{
			return new DateFormatSymbols(locale).getWeekdays();
		}
	}

	private String[] getMonthNames(String type, Locale locale)
	{
		if("s".equalsIgnoreCase(type))
		{
			return new DateFormatSymbols(locale).getShortMonths();
		}
		else
		{
			return new DateFormatSymbols(locale).getMonths();
		}
	}

}


