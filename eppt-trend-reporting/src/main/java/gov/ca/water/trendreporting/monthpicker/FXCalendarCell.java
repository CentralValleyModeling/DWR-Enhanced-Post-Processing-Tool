package gov.ca.water.trendreporting.monthpicker;

import javafx.beans.InvalidationListener;
import javafx.beans.Observable;
import javafx.beans.property.SimpleIntegerProperty;
import javafx.event.EventHandler;
import javafx.scene.input.MouseEvent;
import javafx.scene.layout.StackPane;
import javafx.scene.text.Text;

public class FXCalendarCell
{

	/**
	 * AbstractCell
	 *
	 * @author Sai.Dandem
	 */
	abstract class AbstractCell extends StackPane
	{

		protected Text txt;

		public AbstractCell()
		{
			super();
		}

		public abstract void setCellId(String id);

		public abstract void setCellWidth(double width);

		public abstract void setCellHeight(double height);

		public abstract void setCellStyle(String styleClass);


		/**
		 * @return the txt
		 */
		public Text getTxt()
		{
			return txt;
		}

		/**
		 * @param txt the txt to set
		 */
		public void setTxt(Text txt)
		{
			this.txt = txt;
		}

	}

	/**
	 * DateCell
	 *
	 * @author Sai.Dandem
	 */
	class DateCell extends AbstractCell
	{

		private SimpleIntegerProperty cellDate = new SimpleIntegerProperty();
		private SimpleIntegerProperty cellMonth = new SimpleIntegerProperty();
		private SimpleIntegerProperty cellYear = new SimpleIntegerProperty();
		private boolean previousState = false;
		private boolean weekNumCell = false;

		public DateCell(String id, double width, double height)
		{
			super();
			setCellId(id);
			setCellWidth(width - 1);
			setCellHeight(height - 1);
			getStyleClass().add("fx-calendar-basic-datecell");

			super.txt = new Text();
			txt.getStyleClass().add("fx-calendar-datetext");
			getChildren().add(txt);

			setOnMouseEntered(new EventHandler<MouseEvent>()
			{
				@Override
				public void handle(MouseEvent arg0)
				{
					previousState = txt.isDisable();
					txt.setDisable(false);
				}
			});

			setOnMouseExited(new EventHandler<MouseEvent>()
			{
				@Override
				public void handle(MouseEvent arg0)
				{
					txt.setDisable(previousState);
				}
			});
			// Disabling the cell and clearing the text if the date is below 01/01/01.
			cellYearProperty().addListener(new InvalidationListener()
			{
				@Override
				public void invalidated(Observable paramObservable)
				{
					if(getCellYear() < 1)
					{
						txt.setText("");
						DateCell.this.setDisable(true);
					}
					else
					{
						DateCell.this.setDisable(false);
					}
				}
			});
		}

		@Override
		public void setCellId(String id)
		{
			super.setId(id);
		}

		@Override
		public void setCellWidth(double width)
		{
			super.setPrefWidth(width);
		}

		@Override
		public void setCellHeight(double height)
		{
			super.setPrefHeight(height);
		}

		public void setCellStyle(String styleClass)
		{
			getStyleClass().add(styleClass);
		}

		/**
		 * @return the cellDate
		 */
		public SimpleIntegerProperty cellDateProperty()
		{
			return cellDate;
		}

		/**
		 * @return the cellDate
		 */
		public Integer getCellDate()
		{
			return cellDate.getValue();
		}

		/**
		 * @param cellDate the cellDate to set
		 */
		public void setCellDate(Integer cellDate)
		{
			this.cellDate.set(cellDate);
		}

		/**
		 * @return the cellMonth
		 */
		public SimpleIntegerProperty cellMonthProperty()
		{
			return cellMonth;
		}

		/**
		 * @return the cellMonth
		 */
		public Integer getCellMonth()
		{
			return cellMonth.getValue();
		}

		/**
		 * @param cellMonth the cellMonth to set
		 */
		public void setCellMonth(Integer cellMonth)
		{
			this.cellMonth.set(cellMonth);
		}

		/**
		 * @return the cellYear
		 */
		public SimpleIntegerProperty cellYearProperty()
		{
			return cellYear;
		}

		/**
		 * @return the cellYear
		 */
		public Integer getCellYear()
		{
			return cellYear.getValue();
		}

		/**
		 * @param cellYear the cellYear to set
		 */
		public void setCellYear(Integer cellYear)
		{
			this.cellYear.set(cellYear);
		}

		/**
		 * @return the weekNumCell
		 */
		public boolean isWeekNumCell()
		{
			return weekNumCell;
		}

		/**
		 * @param weekNumCell the weekNumCell to set
		 */
		public void setWeekNumCell(boolean weekNumCell)
		{
			this.weekNumCell = weekNumCell;
		}

		public boolean getCellFocused()
		{
			return super.isFocused();
		}

		public void setCellFocused(boolean b)
		{
			super.setFocused(b);
		}
	}

	/**
	 * WeekCell
	 *
	 * @author Sai.Dandem
	 */
	class WeekCell extends AbstractCell
	{


		public WeekCell(String id, String content, double width, double height)
		{
			super();
			setCellId(id);
			setCellWidth(width - 1);
			setCellHeight(height);

			super.txt = new Text(content);
			txt.getStyleClass().add("fx-calendar-weektext");
			getChildren().add(txt);
		}


		public void setContent(String str)
		{
			super.txt.setText(str);
		}

		@Override
		public void setCellId(String id)
		{
			super.setId(id);
		}

		@Override
		public void setCellWidth(double width)
		{
			super.setPrefWidth(width);
		}

		@Override
		public void setCellHeight(double height)
		{
			super.setPrefHeight(height);
		}

		public void setCellStyle(String styleClass)
		{
			getStyleClass().add(styleClass);
		}

	}

}
