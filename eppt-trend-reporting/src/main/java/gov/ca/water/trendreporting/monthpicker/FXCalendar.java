package gov.ca.water.trendreporting.monthpicker;

import java.time.LocalDate;
import java.util.Calendar;
import java.util.Date;
import java.util.Locale;

import javafx.beans.property.SimpleBooleanProperty;
import javafx.beans.property.SimpleDoubleProperty;
import javafx.beans.property.SimpleIntegerProperty;
import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.value.ChangeListener;
import javafx.beans.value.ObservableValue;
import javafx.collections.ListChangeListener;
import javafx.event.ActionEvent;
import javafx.event.EventHandler;
import javafx.geometry.Bounds;
import javafx.geometry.Pos;
import javafx.scene.Node;
import javafx.scene.Parent;
import javafx.scene.control.Button;
import javafx.scene.control.TextField;
import javafx.scene.input.KeyCode;
import javafx.scene.input.KeyEvent;
import javafx.scene.layout.HBox;
import javafx.scene.paint.Color;
import javafx.stage.Popup;

public class FXCalendar extends HBox
{

	private static final String DEFAULT_STYLE_CLASS = "fx-calendar";
	private final SimpleObjectProperty<Color> _baseColor = new SimpleObjectProperty<>();
	private final SimpleObjectProperty<Locale> _locale = new SimpleObjectProperty<>();
	private SimpleIntegerProperty _selectedDate = new SimpleIntegerProperty(1);
	private SimpleIntegerProperty _selectedMonth = new SimpleIntegerProperty();
	private SimpleIntegerProperty _selectedYear = new SimpleIntegerProperty();
	private SimpleBooleanProperty _triggered = new SimpleBooleanProperty();
	private SimpleDoubleProperty _dateTextWidth = new SimpleDoubleProperty(74);
	private SimpleObjectProperty<Date> _value = new SimpleObjectProperty<>();
	private boolean _showWeekNumber;
	private FXCalendarUtility _fxCalendarUtility;
	private DateTextField _dateTxtField;
	private ChangeListener<Boolean> _focusOutListener;
	private Popup _popup;
	private DatePicker _datePicker;

	public FXCalendar()
	{
		super();
		super.getStylesheets().add("gov/ca/water/trendreporting/monthpicker/styles/calendar_styles.css");
		super.getStyleClass().add(DEFAULT_STYLE_CLASS);
		this._locale.set(Locale.ENGLISH);
		this._baseColor.set(Color.web("#313131"));
		//setSpacing(6);
		setAlignment(Pos.CENTER_LEFT);
		configureCalendar();
		configureListeners();
	}

	private void configureCalendar()
	{
		final DateFormatValidator dateFormatValidator = new DateFormatValidator();
		_fxCalendarUtility = new FXCalendarUtility();

		_popup = new Popup();
		_popup.setAutoHide(true);
		_popup.setAutoFix(true);
		_popup.setHideOnEscape(true);

		addEventFilter(KeyEvent.KEY_PRESSED, new EventHandler<KeyEvent>()
		{
			public void handle(KeyEvent event)
			{
				if(KeyCode.UP.equals(event.getCode()) || KeyCode.DOWN.equals(event.getCode()) || KeyCode.ENTER.equals(event.getCode()))
				{
					initiatePopUp();
					showPopup();
				}
				else if(KeyCode.TAB.equals(event.getCode()))
				{
					hidePopup();
				}
			}
		});

		/* Creating the date text field. */
		_dateTxtField = new DateTextField();
		_dateTxtField.prefWidthProperty().bind(_dateTextWidth);
		this.prefWidthProperty().bind(_dateTextWidth.add(26));
		this._focusOutListener = new ChangeListener<Boolean>()
		{
			@Override
			public void changed(ObservableValue<? extends Boolean> arg0, Boolean arg1, Boolean arg2)
			{
				// Handling only when focus is out.
				if(!arg2)
				{
					String value = _dateTxtField.getText();
					if(!dateFormatValidator.isValid(value))
					{
						_dateTxtField.setText(value);
					}
					else
					{
						Date date = _fxCalendarUtility.convertStringtoDate(value);
						if(date != null)
						{
							setValue(date);
						}
						else
						{
							// TODO : Error styling the text field for invalid date
							// entry.
							clear();
						}
					}
				}
			}
		};
		_dateTxtField.focusedProperty().addListener(this._focusOutListener);

		/* Creating the date button. */
		Button popupButton = new Button();
		popupButton.getStyleClass().add("dateButton");
		popupButton.setGraphic(FXCalendarUtility.getDateImage());
		popupButton.setFocusTraversable(false);
		popupButton.setOnAction(new EventHandler<ActionEvent>()
		{
			@Override
			public void handle(ActionEvent paramT)
			{
				initiatePopUp();
				showPopup();
			}
		});

		getChildren().addAll(_dateTxtField, popupButton);
	}

	private void configureListeners()
	{

		/* Adding listeners when the date cell is selected. */
		triggeredProperty().addListener(new ChangeListener<Boolean>()
		{
			@Override
			public void changed(ObservableValue<? extends Boolean> paramObservableValue, Boolean paramT1, Boolean paramT2)
			{
				if(paramT2)
				{
					FXCalendarUtility cu = new FXCalendarUtility();
					final Integer day = selectedDateProperty().get();
					final Integer month = selectedMonthProperty().get();
					final Integer year = selectedYearProperty().get();
					if(day != 0 && month > -1 && year != 0)
					{
						String d = cu.getFormattedDate(day, month, year);
						valueProperty().set(cu.convertStringtoDate(d));
					}
					setTriggered(false);
				}
			}
		});

		/*
		 * Changes to be done in text box on change of seletedDate ,
		 * selectedMonth and selectedYear in DatePicker.
		 */
		ChangeListener<Object> listener = new ChangeListener<Object>()
		{
			@Override
			public void changed(ObservableValue<? extends Object> arg0, Object arg1, Object arg2)
			{
				showDateInTextField();
			}
		};

		selectedDateProperty().addListener(listener);
		selectedMonthProperty().addListener(listener);
		selectedYearProperty().addListener(listener);
		showDateInTextField();

		/* Adding change listeners for locale. */
		ChangeListener<Locale> localeListener = new ChangeListener<Locale>()
		{
			@Override
			public void changed(ObservableValue<? extends Locale> arg0, Locale arg1, Locale arg2)
			{
				if(_datePicker != null)
				{
					refreshLocale(arg2);
				}
			}
		};
		localeProperty().addListener(localeListener);

		/* Adding listeners for styles. */
		getStyleClass().addListener(new ListChangeListener<String>()
		{
			@Override
			public void onChanged(javafx.collections.ListChangeListener.Change<? extends String> paramChange)
			{
				_dateTxtField.getStyleClass().clear();
				_dateTxtField.getStyleClass().addAll("text-input", "text-field");
				for(String clazz : getStyleClass())
				{
					if(!clazz.equals(DEFAULT_STYLE_CLASS))
					{
						_dateTxtField.getStyleClass().add(clazz);
					}
				}
			}
		});
	}

	public void showDateInTextField()
	{
		int date = selectedDateProperty().get();
		int month = selectedMonthProperty().get();
		int year = selectedYearProperty().get();
		if(date != 0 && month > 0 && year != 0)
		{
			_dateTxtField.setText(this._fxCalendarUtility.getFormattedDate(date, month, year));
		}
		else
		{
			_dateTxtField.setText("");
		}
	}

	public LocalDate getLocalDate()
	{
		return LocalDate.of(getSelectedYear(), getSelectedMonth() + 1, getSelectedDate());
	}

	public void refreshLocale(Locale locale)
	{
		_fxCalendarUtility.resetShortestWeekDays(locale);
		_fxCalendarUtility.resetShortMonths(locale);
		_fxCalendarUtility.resetMonths(locale);
		_datePicker.getBasePane().setLabelText();
		_datePicker.getBasePane().setWeekLabels();
		_datePicker.getTopPane().setTopMonths();
	}

	/**
	 * Method to initiate the pop up before showing.
	 */
	private void initiatePopUp()
	{
		if(_datePicker == null)
		{
			_datePicker = new DatePicker(FXCalendar.this);
			_popup.getContent().add(_datePicker);
		}

		// If there is no date selected, then setting the system date.
		if(FXCalendar.this.getSelectedYear() == 0)
		{
			Calendar currentDate = FXCalendarUtility.getCurrentDateCalendar();
			_datePicker.selectedDateProperty().set(currentDate.get(Calendar.DAY_OF_MONTH));
			_datePicker.selectedMonthProperty().set(currentDate.get(Calendar.MONTH));
			_datePicker.selectedYearProperty().set(currentDate.get(Calendar.YEAR));
		}
		else
		{
			// Copying the date from calendar to date picker.
			_datePicker.selectedDateProperty().set(selectedDateProperty().get());
			_datePicker.selectedMonthProperty().set(selectedMonthProperty().get());
			_datePicker.selectedYearProperty().set(selectedYearProperty().get());
		}

		_datePicker.getBasePane().generateDates();
		_datePicker.showTopPane();
	}

	/**
	 * Method to show the pop up.
	 */
	private void showPopup()
	{
		Parent parent = getParent();
		Bounds childBounds = getBoundsInParent();
		Bounds parentBounds = parent.localToScene(parent.getBoundsInLocal());
		double layoutX = childBounds.getMinX() + parentBounds.getMinX() + parent.getScene().getX() + parent.getScene().getWindow().getX();
		double layoutY = childBounds.getMaxY() + parentBounds.getMinY() + parent.getScene().getY() + parent.getScene().getWindow().getY();
		_popup.show(this, layoutX, layoutY);
	}

	/**
	 * Method to hide the pop up.
	 */
	public void hidePopup()
	{
		_popup.hide();
	}

	/**
	 * @return the baseColor
	 */
	public Color getBaseColor()
	{
		return _baseColor.get();
	}

	/**
	 * @param baseColor the baseColor to set
	 */
	public void setBaseColor(Color color)
	{
		this._baseColor.set(color);
	}

	/**
	 * @return baseColor Property
	 */
	public SimpleObjectProperty<Color> baseColorProperty()
	{
		return _baseColor;
	}

	/**
	 * @return the locale
	 */
	public Locale getLocale()
	{
		return _locale.get();
	}

	/**
	 * @param locale the locale to set
	 */
	public void setLocale(Locale locale)
	{
		this._locale.set(locale);
	}

	/**
	 * @return locale Property
	 */
	public SimpleObjectProperty<Locale> localeProperty()
	{
		return _locale;
	}

	/**
	 * @return the dateTextWidth
	 */
	public Double getDateTextWidth()
	{
		return _dateTextWidth.get();
	}

	/**
	 * @param dateTextWidth the dateTextWidth to set
	 */
	public void setDateTextWidth(Double width)
	{
		this._dateTextWidth.set(width);
	}

	/**
	 * @return dateTextWidth Property
	 */
	public SimpleDoubleProperty dateTextWidthProperty()
	{
		return _dateTextWidth;
	}

	public int getSelectedDate()
	{
		return _selectedDate.get();
	}

	public void setSelectedDate(int selectedDate)
	{
		this._selectedDate.set(selectedDate);
	}

	public int getSelectedMonth()
	{
		return _selectedMonth.get();
	}

	public void setSelectedMonth(int selectedMonth)
	{
		this._selectedMonth.set(selectedMonth);
	}

	public int getSelectedYear()
	{
		return _selectedYear.get();
	}

	public void setSelectedYear(int selectedYear)
	{
		this._selectedYear.set(selectedYear);
	}

	public SimpleIntegerProperty selectedDateProperty()
	{
		return _selectedDate;
	}

	public SimpleIntegerProperty selectedMonthProperty()
	{
		return _selectedMonth;
	}

	public SimpleIntegerProperty selectedYearProperty()
	{
		return _selectedYear;
	}

	public boolean getShowWeekNumber()
	{
		return _showWeekNumber;
	}

	public void setShowWeekNumber(boolean showWeekNumber)
	{
		this._showWeekNumber = showWeekNumber;
	}

	/**
	 * @return the value
	 */
	public Date getValue()
	{
		return this._value.get();
	}

	/**
	 * @param value the value to set
	 */
	public void setValue(Date date)
	{
		this._value.set(date);
		if(date != null)
		{
			Calendar c = FXCalendarUtility.getDateCalendar(date);
			selectedDateProperty().set(c.get(Calendar.DAY_OF_MONTH));
			selectedMonthProperty().set(c.get(Calendar.MONTH));
			selectedYearProperty().set(c.get(Calendar.YEAR));
		}
		else
		{
			selectedDateProperty().set(1);
			selectedMonthProperty().set(1);
			selectedYearProperty().set(1921);
		}
	}

	/**
	 * Method to clear the value in the calendar.
	 */
	public void clear()
	{
		setValue(null);
	}

	public SimpleObjectProperty<Date> valueProperty()
	{
		return _value;
	}

	public FXCalendarUtility getFXCalendarUtility()
	{
		return _fxCalendarUtility;
	}

	public void setTriggered(Boolean triggered)
	{
		this._triggered.set(triggered);
	}

	public SimpleBooleanProperty triggeredProperty()
	{
		return _triggered;
	}

	public TextField getTextField()
	{
		return _dateTxtField;
	}

	public ChangeListener<Boolean> getFocusOutListener()
	{
		return this._focusOutListener;
	}

	/**
	 * Cell Inteface
	 *
	 * @param <ItemType>
	 * @author Sai.Dandem
	 */
	public interface Cell
	{
		Node getNode();

		void updateItem(String item);

	}

	/**
	 * Simple Cell Class
	 *
	 * @param <ItemType>
	 * @author Sai.Dandem
	 */
	public static class DateTextField extends TextField implements Cell
	{
		public DateTextField()
		{
			setEditable(true);
			setPrefHeight(22);
			setPromptText("Select Date");
		}

		public Node getNode()
		{
			return this;
		}

		public void updateItem(String item)
		{
			setText(item != null ? item : "");
		}
	}

}
