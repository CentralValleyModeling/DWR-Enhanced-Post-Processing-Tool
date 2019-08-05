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

	private final SimpleObjectProperty<Color> baseColor = new SimpleObjectProperty<Color>();
	private final SimpleObjectProperty<Locale> locale = new SimpleObjectProperty<Locale>();
	private final String DEFAULT_STYLE_CLASS = "fx-calendar";
	private SimpleIntegerProperty selectedDate = new SimpleIntegerProperty(1);
	private SimpleIntegerProperty selectedMonth = new SimpleIntegerProperty();
	private SimpleIntegerProperty selectedYear = new SimpleIntegerProperty();
	private SimpleBooleanProperty triggered = new SimpleBooleanProperty();
	private SimpleDoubleProperty dateTextWidth = new SimpleDoubleProperty(74);
	private SimpleObjectProperty<Date> value = new SimpleObjectProperty<Date>();
	private boolean showWeekNumber;
	private FXCalendarUtility fxCalendarUtility;
	private DateTextField dateTxtField;
	private ChangeListener<Boolean> focusOutListener;
	private Popup popup;
	private DatePicker datePicker;

	public FXCalendar()
	{
		super();
		super.getStylesheets().add("gov/ca/water/trendreporting/monthpicker/styles/calendar_styles.css");
		super.getStyleClass().add(DEFAULT_STYLE_CLASS);
		this.locale.set(Locale.ENGLISH);
		this.baseColor.set(Color.web("#313131"));
		//setSpacing(6);
		setAlignment(Pos.CENTER);
		configureCalendar();
		configureListeners();
	}

	private void configureCalendar()
	{
		final DateFormatValidator dateFormatValidator = new DateFormatValidator();
		fxCalendarUtility = new FXCalendarUtility();

		popup = new Popup();
		popup.setAutoHide(true);
		popup.setAutoFix(true);
		popup.setHideOnEscape(true);

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
		dateTxtField = new DateTextField();
		dateTxtField.prefWidthProperty().bind(dateTextWidth);
		this.prefWidthProperty().bind(dateTextWidth.add(26));
		this.focusOutListener = new ChangeListener<Boolean>()
		{
			@Override
			public void changed(ObservableValue<? extends Boolean> arg0, Boolean arg1, Boolean arg2)
			{
				// Handling only when focus is out.
				if(!arg2)
				{
					String value = dateTxtField.getText();
					if(!dateFormatValidator.isValid(value))
					{
						dateTxtField.setText(value);
					}
					else
					{
						Date date = fxCalendarUtility.convertStringtoDate(value);
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
		dateTxtField.focusedProperty().addListener(this.focusOutListener);

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

		getChildren().addAll(dateTxtField, popupButton);
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
				if(datePicker != null)
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
				dateTxtField.getStyleClass().clear();
				dateTxtField.getStyleClass().addAll("text-input", "text-field");
				for(String clazz : getStyleClass())
				{
					if(!clazz.equals(DEFAULT_STYLE_CLASS))
					{
						dateTxtField.getStyleClass().add(clazz);
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
			dateTxtField.setText(this.fxCalendarUtility.getFormattedDate(date, month, year));
		}
		else
		{
			dateTxtField.setText("");
		}
	}

	public LocalDate getLocalDate()
	{
		return LocalDate.of(getSelectedYear(), getSelectedMonth() + 1, getSelectedDate());
	}

	public void refreshLocale(Locale locale)
	{
		fxCalendarUtility.resetShortestWeekDays(locale);
		fxCalendarUtility.resetShortMonths(locale);
		fxCalendarUtility.resetMonths(locale);
		datePicker.getBasePane().setLabelText();
		datePicker.getBasePane().setWeekLabels();
		datePicker.getTopPane().setTopMonths();
	}

	/**
	 * Method to initiate the pop up before showing.
	 */
	private void initiatePopUp()
	{
		if(datePicker == null)
		{
			datePicker = new DatePicker(FXCalendar.this);
			popup.getContent().add(datePicker);
		}

		// If there is no date selected, then setting the system date.
		if(FXCalendar.this.getSelectedYear() == 0)
		{
			Calendar currentDate = FXCalendarUtility.getCurrentDateCalendar();
			datePicker.selectedDateProperty().set(currentDate.get(Calendar.DAY_OF_MONTH));
			datePicker.selectedMonthProperty().set(currentDate.get(Calendar.MONTH));
			datePicker.selectedYearProperty().set(currentDate.get(Calendar.YEAR));
		}
		else
		{
			// Copying the date from calendar to date picker.
			datePicker.selectedDateProperty().set(selectedDateProperty().get());
			datePicker.selectedMonthProperty().set(selectedMonthProperty().get());
			datePicker.selectedYearProperty().set(selectedYearProperty().get());
		}

		datePicker.getBasePane().generateDates();
		datePicker.showTopPane();
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
		popup.show(this, layoutX, layoutY);
	}

	/**
	 * Method to hide the pop up.
	 */
	public void hidePopup()
	{
		popup.hide();
	}

	/**
	 * @return the baseColor
	 */
	public Color getBaseColor()
	{
		return baseColor.get();
	}

	/**
	 * @param baseColor the baseColor to set
	 */
	public void setBaseColor(Color color)
	{
		this.baseColor.set(color);
	}

	/**
	 * @return baseColor Property
	 */
	public SimpleObjectProperty<Color> baseColorProperty()
	{
		return baseColor;
	}

	/**
	 * @return the locale
	 */
	public Locale getLocale()
	{
		return locale.get();
	}

	/**
	 * @param locale the locale to set
	 */
	public void setLocale(Locale locale)
	{
		this.locale.set(locale);
	}

	/**
	 * @return locale Property
	 */
	public SimpleObjectProperty<Locale> localeProperty()
	{
		return locale;
	}

	/**
	 * @return the dateTextWidth
	 */
	public Double getDateTextWidth()
	{
		return dateTextWidth.get();
	}

	/**
	 * @param dateTextWidth the dateTextWidth to set
	 */
	public void setDateTextWidth(Double width)
	{
		this.dateTextWidth.set(width);
	}

	/**
	 * @return dateTextWidth Property
	 */
	public SimpleDoubleProperty dateTextWidthProperty()
	{
		return dateTextWidth;
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

	public boolean getShowWeekNumber()
	{
		return showWeekNumber;
	}

	public void setShowWeekNumber(boolean showWeekNumber)
	{
		this.showWeekNumber = showWeekNumber;
	}

	/**
	 * @return the value
	 */
	public Date getValue()
	{
		return this.value.get();
	}

	/**
	 * @param value the value to set
	 */
	public void setValue(Date date)
	{
		this.value.set(date);
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
		return value;
	}

	public FXCalendarUtility getFXCalendarUtility()
	{
		return fxCalendarUtility;
	}

	public void setTriggered(Boolean triggered)
	{
		this.triggered.set(triggered);
	}

	public SimpleBooleanProperty triggeredProperty()
	{
		return triggered;
	}

	public TextField getTextField()
	{
		return dateTxtField;
	}

	public ChangeListener<Boolean> getFocusOutListener()
	{
		return this.focusOutListener;
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
