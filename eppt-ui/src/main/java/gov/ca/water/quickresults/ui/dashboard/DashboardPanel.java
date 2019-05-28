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

package gov.ca.water.quickresults.ui.dashboard;


import gov.ca.water.plots.JavascriptPanel;
import javafx.application.Platform;
import javafx.embed.swing.JFXPanel;
import javafx.event.ActionEvent;
import javafx.geometry.Insets;
import javafx.geometry.Orientation;
import javafx.geometry.Pos;
import javafx.scene.Scene;
import javafx.scene.control.ScrollPane;
import javafx.scene.control.TextField;
import javafx.scene.control.ToggleButton;
import javafx.scene.layout.Background;
import javafx.scene.layout.BackgroundFill;
import javafx.scene.layout.BorderPane;
import javafx.scene.layout.CornerRadii;
import javafx.scene.layout.FlowPane;
import javafx.scene.paint.Color;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 05-21-2019
 */
public class DashboardPanel extends JFXPanel
{
	private TextField _resourceField;
	private FlowPane _javascriptPane;
	private JavascriptPanel _summaryPane;
	private JavascriptPanel _monthlyPane;
	private JavascriptPanel _timeseriesPane;
	private JavascriptPanel _exceedancePanel;
	private JavascriptPanel _waterYearPanel;

	public DashboardPanel()
	{
		Platform.setImplicitExit(false);
		Platform.runLater(this::initFx);
	}

	private void initFx()
	{
		BorderPane borderPane = new BorderPane();
		BorderPane inner = new BorderPane();
		ToggleButton summaryButton = new ToggleButton("Summary");
		summaryButton.setOnAction(this::loadSummary);
		ToggleButton monthlyButton = new ToggleButton("Monthly");
		monthlyButton.setOnAction(this::loadMonthly);
		ToggleButton timeseriesButton = new ToggleButton("Timeseries");
		timeseriesButton.setOnAction(this::loadTimeseries);
		ToggleButton exceedanceButton = new ToggleButton("Exceedance");
		exceedanceButton.setOnAction(this::loadExceedance);
		ToggleButton waterYearTypeButton = new ToggleButton("Water Year Type");
		waterYearTypeButton.setOnAction(this::loadWaterYear);
		FlowPane flowPane = new FlowPane();
		flowPane.alignmentProperty().set(Pos.CENTER);
		_resourceField = new TextField("highcharts/index.html");
		flowPane.getChildren().addAll(summaryButton, monthlyButton, timeseriesButton, exceedanceButton, waterYearTypeButton, _resourceField);
		inner.setTop(flowPane);
		ScrollPane scrollPane = new ScrollPane();
		_javascriptPane = new FlowPane(Orientation.HORIZONTAL);
		Animator animator = new Animator();
		animator.observe(_javascriptPane.getChildren());
		scrollPane.setContent(_javascriptPane);
		scrollPane.setFitToWidth(true);
		Background background = new Background(new BackgroundFill(Color.WHITE, CornerRadii.EMPTY, Insets.EMPTY));
		scrollPane.setBackground(background);
		inner.setCenter(scrollPane);
		inner.setBackground(background);
		borderPane.setCenter(inner);
		Scene scene = new Scene(borderPane);
		borderPane.setBackground(background);
		setScene(scene);
	}

	private void loadSummary(ActionEvent event)
	{
		String url = "highcharts/examples/column-stacked/index.htm";
		if(_summaryPane == null)
		{
			_summaryPane = new JavascriptPanel(url, "");
		}
		loadPane(event, _summaryPane, url);
	}

	private void loadMonthly(ActionEvent event)
	{
		String url = "highcharts/examples/line-basic/index.htm";
		if(_monthlyPane == null)
		{
			_monthlyPane = new JavascriptPanel(url, "");
		}
		loadPane(event, _monthlyPane, url);
	}

	private void loadTimeseries(ActionEvent event)
	{
		String url = "highcharts/examples/line-time-series/index.htm";
		if(_timeseriesPane == null)
		{
			_timeseriesPane = new JavascriptPanel(url, "");
		}
		loadPane(event, _timeseriesPane, url);
	}

	private void loadExceedance(ActionEvent event)
	{
		String url = "highcharts/examples/area-basic/index.htm";
		if(_exceedancePanel == null)
		{
			_exceedancePanel = new JavascriptPanel(url, "");
		}
		loadPane(event, _exceedancePanel, url);
	}

	private void loadWaterYear(ActionEvent event)
	{
		String url = "highcharts/examples/polygon/index.htm";
		if(_waterYearPanel == null)
		{
			_waterYearPanel = new JavascriptPanel(url, "");
		}
		loadPane(event, _waterYearPanel, url);
	}

	private void loadPane(ActionEvent event, JavascriptPanel panel, String url)
	{
		Object source = event.getSource();
		if(source instanceof ToggleButton)
		{
			boolean selected = ((ToggleButton) source).isSelected();
			if(selected)
			{
				_javascriptPane.getChildren().add(panel);
			}
			else
			{
				_javascriptPane.getChildren().remove(panel);
			}
		}
	}


}
