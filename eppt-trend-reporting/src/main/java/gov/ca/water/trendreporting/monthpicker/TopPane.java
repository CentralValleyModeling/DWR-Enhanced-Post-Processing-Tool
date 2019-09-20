package gov.ca.water.trendreporting.monthpicker;

import gov.ca.water.trendreporting.monthpicker.FXCalendarControls.CalendarToggleButton;
import gov.ca.water.trendreporting.monthpicker.FXCalendarControls.NormalButton;
import gov.ca.water.trendreporting.monthpicker.FXCalendarControls.YearNavigatorArrowButton;
import javafx.beans.property.SimpleIntegerProperty;
import javafx.beans.value.ChangeListener;
import javafx.beans.value.ObservableValue;
import javafx.event.ActionEvent;
import javafx.event.EventHandler;
import javafx.geometry.Pos;
import javafx.geometry.Side;
import javafx.scene.Group;
import javafx.scene.input.MouseEvent;
import javafx.scene.layout.HBox;
import javafx.scene.layout.StackPane;
import javafx.scene.layout.TilePane;

public class TopPane extends Group
{
	private DatePicker datePicker;
	private StackPane monthPane;
	private StackPane yearPane;
	private StackPane footerPane;
	private SimpleIntegerProperty month = new SimpleIntegerProperty();
	private SimpleIntegerProperty year = new SimpleIntegerProperty();
	private CalendarToggleButton[] monthButtons = new CalendarToggleButton[12];
	private CalendarToggleButton[] yearButtons = new CalendarToggleButton[10];
	private YearNavigatorArrowButton prevBtn;

	public TopPane(DatePicker datePicker)
	{
		super();
		super.getStylesheets().add("gov/ca/water/trendreporting/monthpicker/styles/calendar_styles.css");
		this.datePicker = datePicker;

		setMonth(datePicker.getSelectedMonth());
		setYear(datePicker.getSelectedYear());

		HBox hb = new HBox();
		configureMonthPane(hb);
		configureYearPane(hb);
		getChildren().add(hb);
		configureFooter();

		/*
		 * Changes to be done in TopMonthPane on change of selectedMonth and
		 * selectedYear in DatePicker.
		 */
		ChangeListener<Object> listener = new ChangeListener<Object>()
		{
			@Override
			public void changed(ObservableValue<? extends Object> arg0, Object arg1, Object arg2)
			{
				styleTopMonthPane();
				styleTopYearPane();
			}
		};
		monthProperty().addListener(listener);
		yearProperty().addListener(listener);

		/*
		 * Changes to be done in TopPane on change of selectedMonth and
		 * selectedYear in DatePicker.
		 */
		datePicker.selectedMonthProperty().addListener(new ChangeListener<Object>()
		{
			@Override
			public void changed(ObservableValue<? extends Object> arg0, Object arg1, Object arg2)
			{
				setMonth((Integer) arg2);
			}
		});

		datePicker.selectedYearProperty().addListener(new ChangeListener<Object>()
		{
			@Override
			public void changed(ObservableValue<? extends Object> arg0, Object arg1, Object arg2)
			{
				setYear((Integer) arg2);
			}
		});
	}

	/*
	 * *********************************************************************************************************************
	 * ****************************** MONTH PANE ******************************
	 * *
	 * *************************************************************************
	 * *******************************************
	 */
	private void configureMonthPane(HBox hb)
	{
		monthPane = new StackPane();
		FXCalendarUtility.setBaseColorToNode(monthPane, datePicker.getBaseColor());

		monthPane.setPrefWidth((datePicker.getBounds().getWidth() - 1) / 2);
		monthPane.setPrefHeight(164);
		monthPane.getStyleClass().add("fx-calendar-top-monthpane");

		TilePane tilePane = new TilePane();
		tilePane.setPrefColumns(2);
		tilePane.setHgap(5);
		tilePane.setVgap(8);
		tilePane.setTranslateX(5);

		generateMonthButtons();

		for(CalendarToggleButton btn : monthButtons)
		{
			tilePane.getChildren().add(btn);
		}
		monthPane.setAlignment(Pos.CENTER);
		monthPane.getChildren().add(tilePane);

		styleTopMonthPane();
		hb.getChildren().add(monthPane);
	}

	private void generateMonthButtons()
	{
		String[] months = datePicker.getFXCalendarUtility().getShortMonths(datePicker.getLocale());
		int evenValue = 0;
		int oddValue = 6;

		for(int i = 0; i < 12; i++)
		{
			int pos = 0;
			if(i % 2 == 0)
			{
				pos = evenValue;
				evenValue++;
			}
			else
			{
				pos = oddValue;
				oddValue++;
			}

			final CalendarToggleButton btn = new FXCalendarControls().new CalendarToggleButton(months[pos], pos);
			btn.setBaseColor(datePicker.getBaseColor());
			btn.setOnMouseClicked(new EventHandler<MouseEvent>()
			{
				@Override
				public void handle(MouseEvent e)
				{
					setMonth((Integer) btn.getUserData());
					styleTopMonthPane();
				}
			});
			monthButtons[i] = btn;
		}
	}

	public void setTopMonths()
	{
		String[] months = datePicker.getFXCalendarUtility().getShortMonths(datePicker.getLocale());
		int evenValue = 0;
		int oddValue = 6;
		for(int i = 0; i < 12; i++)
		{
			int pos = 0;
			if(i % 2 == 0)
			{
				pos = evenValue;
				evenValue++;
			}
			else
			{
				pos = oddValue;
				oddValue++;
			}
			monthButtons[i].setText(months[pos]);
		}
	}

	public void styleTopMonthPane()
	{
		for(int i = 0; i < 12; i++)
		{
			if(getMonth() == (Integer) monthButtons[i].getUserData())
			{
				monthButtons[i].setDisable(true);
			}
			else
			{
				monthButtons[i].setDisable(false);
			}
		}
	}

	/*
	 * *********************************************************************************************************************
	 * ****************************** YEAR PANE ******************************
	 * **
	 * ************************************************************************
	 * *******************************************
	 */
	private void configureYearPane(HBox hb)
	{
		yearPane = new StackPane();
		FXCalendarUtility.setBaseColorToNode(yearPane, datePicker.getBaseColor());

		yearPane.setPrefWidth(datePicker.getBounds().getWidth() / 2);
		yearPane.setPrefHeight(164);
		yearPane.getStyleClass().add("fx-calendar-top-yearpane");

		TilePane tilePane = new TilePane();
		tilePane.setPrefColumns(2);
		tilePane.setHgap(5);
		tilePane.setVgap(8);
		tilePane.setTranslateX(5);

		prevBtn = new FXCalendarControls().new YearNavigatorArrowButton(Side.LEFT, datePicker.getBaseColor());
		YearNavigatorArrowButton nextBtn = new FXCalendarControls().new YearNavigatorArrowButton(Side.RIGHT, datePicker.getBaseColor());

		prevBtn.setOnMouseClicked(new EventHandler<MouseEvent>()
		{
			@Override
			public void handle(MouseEvent arg0)
			{
				int start = (Integer) yearButtons[0].getUserData();
				resetYearButtons(start - 6);
				styleTopYearPane();
			}
		});
		nextBtn.setOnMouseClicked(new EventHandler<MouseEvent>()
		{
			@Override
			public void handle(MouseEvent arg0)
			{
				int start = (Integer) yearButtons[9].getUserData();
				resetYearButtons(start + 5);
				styleTopYearPane();
			}
		});

		tilePane.getChildren().add(prevBtn);
		tilePane.getChildren().add(nextBtn);

		generateYearButtons();
		for(CalendarToggleButton btn : yearButtons)
		{
			tilePane.getChildren().add(btn);
		}

		yearPane.getChildren().add(tilePane);
		styleTopYearPane();
		hb.getChildren().add(yearPane);

	}

	private void generateYearButtons()
	{
		int[] arr = getYearArray(getYear());
		int evenValue = 0;
		int oddValue = 5;

		for(int i = 0; i < 10; i++)
		{
			int pos = 0;
			if(i % 2 == 0)
			{
				pos = evenValue;
				evenValue++;
			}
			else
			{
				pos = oddValue;
				oddValue++;
			}

			final CalendarToggleButton btn = new FXCalendarControls().new CalendarToggleButton(arr[pos] + "", new Integer(arr[pos]));
			btn.setBaseColor(datePicker.getBaseColor());
			btn.setOnMouseClicked(new EventHandler<MouseEvent>()
			{
				@Override
				public void handle(MouseEvent e)
				{
					setYear((Integer) btn.getUserData());
					styleTopYearPane();
				}
			});
			yearButtons[i] = btn;
		}
	}

	public void resetYearButtons()
	{
		resetYearButtons(getYear());
		styleTopYearPane();
	}

	public void resetYearButtons(int year)
	{
		int[] arr = getYearArray(year);
		int evenValue = 0;
		int oddValue = 5;
		for(int i = 0; i < 10; i++)
		{
			int pos = 0;
			if(i % 2 == 0)
			{
				pos = evenValue;
				evenValue++;
			}
			else
			{
				pos = oddValue;
				oddValue++;
			}
			yearButtons[i].setText(arr[pos] + "");
			yearButtons[i].setUserData(new Integer(arr[pos]));
		}
	}

	public void styleTopYearPane()
	{
		for(int i = 0; i < 10; i++)
		{
			if(getYear() == (Integer) yearButtons[i].getUserData())
			{
				yearButtons[i].setDisable(true);
			}
			else
			{
				yearButtons[i].setDisable(false);
			}
		}
	}

	private int[] getYearArray(int year)
	{
		int[] arr = new int[10];
		int startYear = year > 5 ? year - 4 : 1; // Not showing negative years
		if(year == 1)
		{
			prevBtn.setDisable(true);
		}
		else
		{
			prevBtn.setDisable(false);
		}

		for(int i = 0; i < 10; i++)
		{
			arr[i] = startYear;
			startYear++;
		}
		return arr;
	}

	/*
	 * *********************************************************************************************************************
	 * ****************************** FOOTER PANE ******************************
	 * *
	 * *************************************************************************
	 * *******************************************
	 */
	private void configureFooter()
	{
		footerPane = new StackPane();
		FXCalendarUtility.setBaseColorToNode(footerPane, datePicker.getBaseColor());

		footerPane.setPrefWidth(datePicker.getBounds().getWidth());
		footerPane.setPrefHeight(32);
		footerPane.getStyleClass().add("fx-calendar-footer");

		NormalButton okBtn = new FXCalendarControls().new NormalButton("OK");
		/**
		 * Event triggering to set the current date of the system.
		 */
		okBtn.setOnAction(new EventHandler<ActionEvent>()
		{
			@Override
			public void handle(ActionEvent event)
			{
				datePicker.getFxCalendar().setSelectedMonth(getMonth());
				datePicker.getFxCalendar().setSelectedYear(getYear());
				datePicker.getFxCalendar().setTriggered(true);

				datePicker.getFxCalendar().getTextField().requestFocus();
				datePicker.getFxCalendar().showDateInTextField();
				datePicker.getFxCalendar().hidePopup();
			}
		});

		NormalButton cancelBtn = new FXCalendarControls().new NormalButton("Cancel");
		/**
		 * Event triggering to set the current date of the system.
		 */
		cancelBtn.setOnAction(new EventHandler<ActionEvent>()
		{
			@Override
			public void handle(ActionEvent event)
			{
				datePicker.getFxCalendar().hidePopup();
			}
		});
		okBtn.prefWidthProperty().set(60);
		cancelBtn.prefWidthProperty().set(60);
		HBox hb = new HBox();
		hb.setSpacing(5);
		hb.getChildren().addAll(okBtn, cancelBtn);
		Group gp = new Group();
		gp.getChildren().add(hb);
		footerPane.getChildren().add(gp);

		footerPane.setTranslateY(monthPane.getPrefHeight());
		getChildren().add(footerPane);

	}

	public int getMonth()
	{
		return month.get();
	}

	public void setMonth(int month)
	{
		this.month.set(month);
	}

	public int getYear()
	{
		return year.get();
	}

	public void setYear(int year)
	{
		this.year.set(year);
	}

	public SimpleIntegerProperty monthProperty()
	{
		return month;
	}

	public SimpleIntegerProperty yearProperty()
	{
		return year;
	}
}
