package gov.ca.water.trendreporting.monthpicker;

import java.util.Locale;

import javafx.beans.property.SimpleIntegerProperty;
import javafx.geometry.Pos;
import javafx.geometry.Rectangle2D;
import javafx.scene.layout.StackPane;
import javafx.scene.paint.Color;

public class DatePicker extends StackPane
{

	private SimpleIntegerProperty selectedDate = new SimpleIntegerProperty();
	private SimpleIntegerProperty selectedMonth = new SimpleIntegerProperty();
	private SimpleIntegerProperty selectedYear = new SimpleIntegerProperty();
	private Rectangle2D calendarBounds = new Rectangle2D(100, 100, 205, 196);
	private FXCalendar fxCalendar;
	private BasePane basePane;
	private TopPane topPane;

	public DatePicker(FXCalendar fxCalendar)
	{
		super();
		super.getStylesheets().add("gov/ca/water/trendreporting/monthpicker/styles/calendar_styles.css");
		this.fxCalendar = fxCalendar;
		selectedDate.set(fxCalendar.getSelectedDate());
		selectedMonth.set(fxCalendar.getSelectedMonth());
		selectedYear.set(fxCalendar.getSelectedYear());
		fxCalendar.setLocale(Locale.ENGLISH);
		setPrefHeight(calendarBounds.getHeight());
		setPrefWidth(calendarBounds.getWidth());
		setAlignment(Pos.TOP_LEFT);
		FXCalendarUtility.setBaseColorToNode(this, fxCalendar.getBaseColor());
		basePane = new BasePane(this);
		topPane = new TopPane(this);
		getChildren().addAll(basePane, topPane);
		showTopPane();
	}

	/* GETTER'S FROM FXCALENDAR * */
	public Color getBaseColor()
	{
		return this.fxCalendar.getBaseColor();
	}

	public FXCalendarUtility getFXCalendarUtility()
	{
		return this.fxCalendar.getFXCalendarUtility();
	}

	public Locale getLocale()
	{
		return this.fxCalendar.getLocale();
	}

	public boolean getShowWeekNumber()
	{
		return this.fxCalendar.getShowWeekNumber();
	}

	public FXCalendar getFxCalendar()
	{
		return this.fxCalendar;
	}

	public int getSelectedDate()
	{
		return selectedDate.get();
	}

	public void setSelectedDate(int selectedDate)
	{
		this.selectedDate.set(selectedDate);
	}

	public int getSelectedMonth()
	{
		return selectedMonth.get();
	}

	public void setSelectedMonth(int selectedMonth)
	{
		this.selectedMonth.set(selectedMonth);
	}

	public int getSelectedYear()
	{
		return selectedYear.get();
	}

	public void setSelectedYear(int selectedYear)
	{
		this.selectedYear.set(selectedYear);
	}

	public SimpleIntegerProperty selectedDateProperty()
	{
		return selectedDate;
	}

	public SimpleIntegerProperty selectedMonthProperty()
	{
		return selectedMonth;
	}

	public SimpleIntegerProperty selectedYearProperty()
	{
		return selectedYear;
	}

	/* GETTER'S FROM DATEPICKER * */
	public Rectangle2D getBounds()
	{
		return calendarBounds;
	}

	public BasePane getBasePane()
	{
		return basePane;
	}

	public TopPane getTopPane()
	{
		return topPane;
	}

	public void showTopPane()
	{
		topPane.resetYearButtons();
		basePane.setVisible(false);
		topPane.setVisible(true);
	}

	public void incrementMonth()
	{
		int currentMonth = this.selectedMonth.get();
		if(currentMonth >= (fxCalendar.getFXCalendarUtility().getMonths(this.getLocale()).length - 2))
		{
			this.selectedMonth.set(0);
			this.selectedYear.set(this.selectedYear.get() + 1);
		}
		else
		{
			this.selectedMonth.set(currentMonth + 1);
		}
	}

	public void decrementMonth()
	{
		int currentMonth = this.selectedMonth.get();
		if(currentMonth <= 0)
		{
			this.selectedMonth.set(fxCalendar.getFXCalendarUtility().getMonths(this.getLocale()).length - 2);
			this.selectedYear.set(this.selectedYear.get() - 1);
		}
		else
		{
			this.selectedMonth.set(currentMonth - 1);
		}
	}

}
