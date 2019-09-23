package gov.ca.water.trendreporting.monthpicker;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.List;

import gov.ca.water.trendreporting.monthpicker.FXCalendarCell.DateCell;
import gov.ca.water.trendreporting.monthpicker.FXCalendarCell.WeekCell;
import gov.ca.water.trendreporting.monthpicker.FXCalendarControls.BaseNavigatorArrowButton;
import gov.ca.water.trendreporting.monthpicker.FXCalendarControls.NormalButton;
import javafx.beans.value.ChangeListener;
import javafx.beans.value.ObservableValue;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.event.ActionEvent;
import javafx.event.Event;
import javafx.event.EventHandler;
import javafx.geometry.Rectangle2D;
import javafx.geometry.Side;
import javafx.scene.Group;
import javafx.scene.control.Label;
import javafx.scene.input.MouseEvent;
import javafx.scene.layout.StackPane;
import javafx.scene.layout.TilePane;
import javafx.scene.paint.Color;

public class BasePane extends Group
{
	public static final String WEEKNUMER_LABEL = "Wk.";
	private DatePicker _datePicker;
	private StackPane _navigatorPane;
	private StackPane _weekPane;
	private StackPane _deskPane;
	private StackPane _footerPane;
	private Label _displayLabel;
	private ObservableList<WeekCell> _weekCellList = FXCollections.observableArrayList();
	private ObservableList<DateCell> _dateCellList = FXCollections.observableArrayList();
	private BaseNavigatorArrowButton _prevMonthBtn;

	public BasePane(DatePicker datePicker)
	{
		super();
		super.getStylesheets().add("gov/ca/water/trendreporting/monthpicker/styles/calendar_styles.css");
		this._datePicker = datePicker;
		configureNavigator();
		configureWeekHeader();
		configureDesk();
		configureFooter();
	}

	/*
	 * *********************************************************************************************************************
	 * ****************************** MONTH NAVIGATOR
	 * ******************************
	 * *******************************************
	 * *******************************
	 * *******************************************
	 */
	private void configureNavigator()
	{
		_navigatorPane = new StackPane();
		/*
		 * Changes to be done in BasePaneNavigator on change of selectedMonth
		 * and selectedYear in DatePicker.
		 */
		ChangeListener<Object> listener = (arg0, arg1, arg2) -> setLabelText();

		_datePicker.selectedMonthProperty().addListener(listener);
		_datePicker.selectedYearProperty().addListener(listener);

		FXCalendarUtility.setBaseColorToNode(_navigatorPane, _datePicker.getBaseColor());
		_navigatorPane.setPrefWidth(_datePicker.getBounds().getWidth());
		_navigatorPane.setPrefHeight(26);
		_navigatorPane.getStyleClass().add("fx-calendar-navigator");

		/* Displaying the Month & Year of the selected date. */
		_displayLabel = new Label();
		_displayLabel.getStyleClass().add("fx-calendar-navigator-label");
		_displayLabel.setGraphic(new gov.ca.water.trendreporting.monthpicker.FXCalendarControls().new Arrow());
		setLabelText();
		_displayLabel.setOnMouseClicked((EventHandler<Event>) arg0 -> _datePicker.showTopPane());

		/* Calculating the distance for the arrow buttons from the center. */
		double pos = (_datePicker.getBounds().getWidth() / 2) - 12;

		/* Getting the Next Month Button. */
		BaseNavigatorArrowButton nextMonthBtn = new FXCalendarControls().new BaseNavigatorArrowButton(Side.RIGHT,
				_datePicker.getBaseColor());
		nextMonthBtn.setTranslateX(pos);
		nextMonthBtn.setOnMouseClicked((EventHandler<Event>) arg0 -> _datePicker.incrementMonth());

		/* Getting the Previous Month Button. */
		_prevMonthBtn = new FXCalendarControls().new BaseNavigatorArrowButton(Side.LEFT, _datePicker.getBaseColor());
		_prevMonthBtn.setTranslateX(-pos);
		_prevMonthBtn.setOnMouseClicked((EventHandler<Event>) arg0 -> {
			if(!(_datePicker.getSelectedMonth() == 0 && _datePicker.getSelectedYear() == 1))
			{
				_datePicker.decrementMonth();
			}
		});

		_navigatorPane.getChildren().addAll(_displayLabel, nextMonthBtn, _prevMonthBtn);
		getChildren().add(_navigatorPane);
	}

	public void setLabelText()
	{
		_displayLabel.setText(this._datePicker.getFXCalendarUtility().getMonths(this._datePicker.getLocale())[this._datePicker.getSelectedMonth()] + " "
				+ this._datePicker.getSelectedYear());
	}

	/*
	 * *********************************************************************************************************************
	 * ****************************** WEEK HEADER ******************************
	 * *
	 * *************************************************************************
	 * *******************************************
	 */
	private void configureWeekHeader()
	{
		_weekPane = new StackPane();

		FXCalendarUtility.setBaseColorToNode(_weekPane, _datePicker.getBaseColor());
		_weekPane.setPrefWidth(_datePicker.getBounds().getWidth());
		_weekPane.setPrefHeight(18);
		_weekPane.getStyleClass().add("fx-calendar-weekpane");

		int count = _datePicker.getShowWeekNumber() ? 8 : 7;

		TilePane tp = new TilePane();
		tp.setPrefColumns(count);

		generateWeekCells(count);
		for(WeekCell weekCell : _weekCellList)
		{
			tp.getChildren().add(weekCell);
		}

		_weekPane.getChildren().add(tp);
		_weekPane.setTranslateY(_navigatorPane.getPrefHeight());
		getChildren().add(_weekPane);
	}

	private void generateWeekCells(int count)
	{
		Rectangle2D cellBounds = calculateBounds();
		WeekCell cell;
		List<WeekCell> wkCells = new ArrayList<>(count);
		if(_datePicker.getShowWeekNumber())
		{
			cell = new FXCalendarCell().new WeekCell("week_num", WEEKNUMER_LABEL, cellBounds.getWidth(),
					cellBounds.getHeight());
			FXCalendarUtility.setBaseColorToNode(cell.getTxt(), Color.BLUE);
			wkCells.add(cell);
		}

		String[] wks = _datePicker.getFXCalendarUtility().getShortestWeekDays(_datePicker.getLocale());
		for(int i = 1; i < wks.length; i++)
		{
			cell = new FXCalendarCell().new WeekCell("week_" + wks[i], wks[i], cellBounds.getWidth(), cellBounds.getHeight());
			FXCalendarUtility.setBaseColorToNode(cell.getTxt(), _datePicker.getBaseColor());
			wkCells.add(cell);
		}
		_weekCellList.addAll(wkCells);
	}

	public void setWeekLabels()
	{
		String[] wks = _datePicker.getFXCalendarUtility().getShortestWeekDays(_datePicker.getLocale());
		int pos = _datePicker.getShowWeekNumber() ? 1 : 0;
		for(int i = 1; i < wks.length; i++)
		{
			_weekCellList.get(pos).setContent(wks[i]);
			pos++;
		}
	}

	private Rectangle2D calculateBounds()
	{
		int divFactor = getColCount();
		double width = _datePicker.getBounds().getWidth() / divFactor;
		double height = 18;
		return new Rectangle2D(0, 0, width, height);
	}

	/*
	 * *********************************************************************************************************************
	 * ****************************** DATE DESK ******************************
	 * **
	 * ************************************************************************
	 * *******************************************
	 */

	private void configureDesk()
	{
		_deskPane = new StackPane();
		FXCalendarUtility.setBaseColorToNode(_deskPane, _datePicker.getBaseColor());
		_deskPane.setPrefWidth(_datePicker.getBounds().getWidth());
		_deskPane.setPrefHeight(120);
		_deskPane.getStyleClass().add("fx-calendar-desk");

		TilePane tp = new TilePane();
		tp.setPrefColumns(getColCount());

		generateDateCells();

		for(DateCell dateCell : _dateCellList)
		{
			tp.getChildren().add(dateCell);
		}

		generateDates();

		/*
		 * Changes to be done in BasePaneDesk on change of selectedMonth and
		 * selectedYear in DatePicker.
		 */
		ChangeListener<Object> listener = new ChangeListener<Object>()
		{
			@Override
			public void changed(ObservableValue<? extends Object> arg0, Object arg1, Object arg2)
			{
				generateDates();
			}
		};

		_datePicker.selectedDateProperty().addListener(listener);
		_datePicker.selectedMonthProperty().addListener(listener);
		_datePicker.selectedYearProperty().addListener(listener);

		_deskPane.getChildren().add(tp);
		_deskPane.setTranslateY(_navigatorPane.getPrefHeight() + _weekPane.getPrefHeight());
		getChildren().add(_deskPane);

	}

	private int getColCount()
	{
		return _datePicker.getShowWeekNumber() ? 8 : 7;
	}

	private void generateDateCells()
	{
		int count = getColCount();
		Rectangle2D cellBounds = calculateDeskBounds();
		DateCell dateCell;
		List<DateCell> dateCells = new ArrayList<DateCell>(count * 6);

		for(int i = 0; i < (count * 6); i++)
		{
			dateCell = new FXCalendarCell().new DateCell("cell" + i, cellBounds.getWidth(), cellBounds.getHeight());
			FXCalendarUtility.setBaseColorToNode(dateCell, _datePicker.getBaseColor());
			// For Week Number cells
			if(_datePicker.getShowWeekNumber() && i % 8 == 0)
			{
				FXCalendarUtility.setBaseColorToNode(dateCell.getTxt(), Color.BLUE);
				dateCell.setWeekNumCell(true);
				dateCell.getTxt().getStyleClass().add("fx-calendar-weektext");
			}
			// For actual Date cells
			else
			{
				// TODO : Anything to configure on date cell.
			}

			dateCells.add(dateCell);
		}
		_dateCellList.addAll(dateCells);
	}

	public void generateDates()
	{

		Calendar firstDayOfMonth = FXCalendarUtility.getDate(1, _datePicker.getSelectedMonth(), _datePicker.getSelectedYear());
		Calendar paneFirstDate = (Calendar) firstDayOfMonth.clone();

		// If Monday is first day of week.
		if(Calendar.getInstance(_datePicker.getLocale()).getFirstDayOfWeek() == 2)
		{
			int diff = 0;
			if(firstDayOfMonth.get(Calendar.DAY_OF_WEEK) == 1)
			{
				diff = 6;
			}
			else
			{
				diff = firstDayOfMonth.get(Calendar.DAY_OF_WEEK) - 2;
			}
			paneFirstDate.add(Calendar.DAY_OF_YEAR, -diff);
		}
		else
		{
			// If Sunday is first day of week.
			paneFirstDate.add(Calendar.DAY_OF_YEAR, -(firstDayOfMonth.get(Calendar.DAY_OF_WEEK) - 1));
		}

		Calendar dummyDate = (Calendar) paneFirstDate.clone();
		Calendar systemDate = FXCalendarUtility.getCurrentDateCalendar();

		int fxDate = _datePicker.getFxCalendar().getSelectedDate();
		int fxMonth = _datePicker.getFxCalendar().getSelectedMonth();
		int fxYear = _datePicker.getFxCalendar().getSelectedYear();

		for(final DateCell dateCell : _dateCellList)
		{
			if(!dateCell.isWeekNumCell())
			{
				dateCell.getStyleClass().remove("fx-calendar-basic-datecell-selected");
				dateCell.getTxt().setText(dummyDate.get(Calendar.DAY_OF_MONTH) + "");

				// Setting the date details of the cell.
				dateCell.setCellDate(dummyDate.get(Calendar.DAY_OF_MONTH));
				dateCell.setCellMonth(dummyDate.get(Calendar.MONTH));
				dateCell.setCellYear(dummyDate.get(Calendar.YEAR));

				// Highlighting the current month cells.
				if(dummyDate.get(Calendar.MONTH) == _datePicker.getSelectedMonth())
				{
					dateCell.getTxt().setDisable(false);
				}
				else
				{
					dateCell.getTxt().setDisable(true);
					// Not showing the dates below 01/01/01
					if((_datePicker.getSelectedMonth() == 0 && _datePicker.getSelectedYear() == 1) && dateCell.getCellMonth() != 1)
					{
						dateCell.setCellYear(0);
					}
				}

				// Highlighting the current system date.
				if(systemDate.get(Calendar.DAY_OF_MONTH) == dummyDate.get(Calendar.DAY_OF_MONTH) && systemDate.get(Calendar.MONTH) == dummyDate.get(
						Calendar.MONTH)
						&& systemDate.get(Calendar.YEAR) == dummyDate.get(Calendar.YEAR))
				{
					dateCell.setCellFocused(true);
				}
				else
				{
					dateCell.setCellFocused(false);
				}

				// Highlighting the Selected date.
				if(fxDate == dummyDate.get(Calendar.DAY_OF_MONTH) && fxMonth == dummyDate.get(Calendar.MONTH) && fxYear == dummyDate.get(
						Calendar.YEAR))
				{
					// Overriding the dotted line with selected class.
					if(dateCell.getCellFocused())
					{
						dateCell.setCellFocused(false);
					}
					dateCell.getStyleClass().add("fx-calendar-basic-datecell-selected");
				}

				// Setting the event handler for the selected date.
				dateCell.setOnMouseClicked(event -> {
					int year = dateCell.getCellYear();
					int month = dateCell.getCellMonth();
					int date = dateCell.getCellDate();
					_datePicker.setSelectedYear(year);
					_datePicker.setSelectedMonth(month);
					_datePicker.setSelectedDate(date);

					_datePicker.getFxCalendar().setSelectedDate(date);
					_datePicker.getFxCalendar().setSelectedMonth(month);
					_datePicker.getFxCalendar().setSelectedYear(year);
					_datePicker.getFxCalendar().setTriggered(true);

					_datePicker.getFxCalendar().getTextField().requestFocus();
					_datePicker.getFxCalendar().showDateInTextField();
					_datePicker.getFxCalendar().hidePopup();
				});

				// Incrementing the date.
				dummyDate.add(Calendar.DAY_OF_YEAR, 1);
			}
			else
			{
				// Updating the week number
				if(dummyDate.get(Calendar.DAY_OF_WEEK) == 1)
				{
					dateCell.getTxt().setText((dummyDate.get(Calendar.WEEK_OF_YEAR) - 1) + "");
					dateCell.getTxt().getStyleClass().add("fx-calendar-weektext");
				}
			}
		}
	}

	private Rectangle2D calculateDeskBounds()
	{
		int divFactor = getColCount();
		double width = _datePicker.getBounds().getWidth() / divFactor;
		double height = 120.0 / 6;
		return new Rectangle2D(0, 0, width, height);
	}

	/*
	 * *********************************************************************************************************************
	 * ****************************** FOOTER ******************************
	 * *****
	 * *********************************************************************
	 * *******************************************
	 */
	private void configureFooter()
	{
		_footerPane = new StackPane();
		FXCalendarUtility.setBaseColorToNode(_footerPane, _datePicker.getBaseColor());
		_footerPane.setPrefWidth(_datePicker.getBounds().getWidth());
		_footerPane.setPrefHeight(32);
		_footerPane.getStyleClass().add("fx-calendar-footer");
		NormalButton todayBtn = new FXCalendarControls().new NormalButton("Today");

		/**
		 * Event triggering to set the current date of the system.
		 */
		todayBtn.setOnAction(new EventHandler<ActionEvent>()
		{
			@Override
			public void handle(ActionEvent event)
			{
				Calendar today = FXCalendarUtility.getCurrentDateCalendar();
				_datePicker.getFxCalendar().setSelectedDate(today.get(Calendar.DAY_OF_MONTH));
				_datePicker.getFxCalendar().setSelectedMonth(today.get(Calendar.MONTH));
				_datePicker.getFxCalendar().setSelectedYear(today.get(Calendar.YEAR));
				_datePicker.getFxCalendar().hidePopup();
			}
		});

		_footerPane.getChildren().add(todayBtn);
		_footerPane.setTranslateY(_navigatorPane.getPrefHeight() + _weekPane.getPrefHeight() + _deskPane.getPrefHeight());
		getChildren().add(_footerPane);
	}
}
