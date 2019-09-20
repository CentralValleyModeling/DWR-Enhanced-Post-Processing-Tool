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
	private static int MILLIS_IN_DAY = 1000 * 60 * 60 * 24;
	private static SimpleDateFormat DATE_FORMAT = new SimpleDateFormat("MM/yyyy");
	private SimpleDateFormat DISPLAY_DATE_FORMAT = new SimpleDateFormat("MMM/yyyy");
	private DateTimeFormatter MONTH_YEAR_FORMATTER = DateTimeFormatter.ofPattern("MMM/yyyy");
	private String[] SHORTEST_WEEK_DAYS;  // {"","S","M","T","W","T","F","S"}
	private String[] SHORT_WEEK_DAYS;     // {"","Sun","Mon","Tue","Wed","Thu","Fri","Sat"}
	private String[] WEEK_DAYS;           // {"","Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"}
	private String[] SHORT_MONTHS;        // {"Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec",""}
	private String[] MONTHS;              // {"January","February","March","April","May","June","July","August","September","October","November","December",""}

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
		return DATE_FORMAT.format(getCurrentTime());
	}

	public static String getPreviousDate()
	{
		return DATE_FORMAT.format(getCurrentTime() - MILLIS_IN_DAY);
	}

	public static String getNextDate()
	{
		return DATE_FORMAT.format(getCurrentTime() + MILLIS_IN_DAY);
	}

	public static Calendar getDate(Integer day, Integer month, Integer year)
	{
		try
		{
			String str_date = day + "/" + (month + 1) + "/" + year;
			Date date = DATE_FORMAT.parse(str_date);
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
			return DISPLAY_DATE_FORMAT.parse(str);
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
		return MONTH_YEAR_FORMATTER.format(date);
	}

	public void resetShortestWeekDays(Locale locale)
	{
		SHORTEST_WEEK_DAYS = null;
		getShortestWeekDays(locale);
	}

	public String[] getShortestWeekDays(Locale locale)
	{
		if(SHORTEST_WEEK_DAYS == null || SHORTEST_WEEK_DAYS.length == 0)
		{
			SHORTEST_WEEK_DAYS = getDayNames("xs", locale);
			// If Monday is first day of week.
			if(Calendar.getInstance(locale).getFirstDayOfWeek() == 2)
			{
				String dum = SHORTEST_WEEK_DAYS[1];
				for(int i = 1; i < 7; i++)
				{
					SHORTEST_WEEK_DAYS[i] = SHORTEST_WEEK_DAYS[i + 1];
				}
				SHORTEST_WEEK_DAYS[7] = dum;
			}
		}
		return SHORTEST_WEEK_DAYS;
	}

	public String[] getShortWeekDays(Locale locale)
	{
		if(SHORT_WEEK_DAYS == null || SHORT_WEEK_DAYS.length == 0)
		{
			SHORT_WEEK_DAYS = getDayNames("s", locale);
		}
		// If Monday is first day of week.
		if(Calendar.getInstance(locale).getFirstDayOfWeek() == 2)
		{
			String dum = SHORT_WEEK_DAYS[1];
			for(int i = 1; i < 7; i++)
			{
				SHORT_WEEK_DAYS[i] = SHORT_WEEK_DAYS[i + 1];
			}
			SHORT_WEEK_DAYS[7] = dum;
		}
		return SHORT_WEEK_DAYS;
	}

	public String[] getWeekDays(Locale locale)
	{
		if(WEEK_DAYS == null || WEEK_DAYS.length == 0)
		{
			WEEK_DAYS = getDayNames(null, locale);
		}
		// If Monday is first day of week.
		if(Calendar.getInstance(locale).getFirstDayOfWeek() == 2)
		{
			String dum = WEEK_DAYS[1];
			for(int i = 1; i < 7; i++)
			{
				WEEK_DAYS[i] = WEEK_DAYS[i + 1];
			}
			WEEK_DAYS[7] = dum;
		}
		return WEEK_DAYS;
	}

	public void resetShortMonths(Locale locale)
	{
		SHORT_MONTHS = null;
		getShortMonths(locale);
	}

	public String[] getShortMonths(Locale locale)
	{
		if(SHORT_MONTHS == null || SHORT_MONTHS.length == 0)
		{
			SHORT_MONTHS = getMonthNames("s", locale);
		}
		return SHORT_MONTHS;
	}

	public void resetMonths(Locale locale)
	{
		MONTHS = null;
		getMonths(locale);
	}

	public String[] getMonths(Locale locale)
	{
		if(MONTHS == null || MONTHS.length == 0)
		{
			MONTHS = getMonthNames(null, locale);
		}
		return MONTHS;
	}

	private String[] getDayNames(String type, Locale locale)
	{
		if(type != null && type.equalsIgnoreCase("xs"))
		{
			String[] days = new DateFormatSymbols(locale).getShortWeekdays();
			String[] xsDays = new String[days.length];
			for(int i = 0; i < days.length; i++)
			{
				xsDays[i] = (days[i].equals("")) ? days[i] : days[i].charAt(0) + "";
			}
			return xsDays;
		}
		if(type != null && type.equalsIgnoreCase("s"))
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
		if(type != null && type.equalsIgnoreCase("s"))
		{
			return new DateFormatSymbols(locale).getShortMonths();
		}
		else
		{
			return new DateFormatSymbols(locale).getMonths();
		}
	}

}


