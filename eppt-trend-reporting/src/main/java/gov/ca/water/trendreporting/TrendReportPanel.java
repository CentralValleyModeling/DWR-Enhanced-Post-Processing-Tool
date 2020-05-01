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


import java.awt.Dimension;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.CancellationException;
import java.util.concurrent.CompletableFuture;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.stream.Stream;
import javax.xml.parsers.ParserConfigurationException;

import gov.ca.water.calgui.bo.WaterYearDefinition;
import gov.ca.water.calgui.bo.WaterYearPeriodRangesFilter;
import gov.ca.water.calgui.busservice.impl.EpptParameter;
import gov.ca.water.calgui.busservice.impl.EpptStatistic;
import gov.ca.water.calgui.busservice.impl.MonthPeriod;
import gov.ca.water.calgui.constant.Constant;
import gov.ca.water.calgui.project.EpptConfigurationController;
import gov.ca.water.calgui.project.EpptScenarioRun;
import gov.ca.water.quickresults.ui.global.EpptParametersPane;
import javafx.application.Platform;
import javafx.embed.swing.JFXPanel;
import javafx.geometry.Insets;
import javafx.geometry.Orientation;
import javafx.geometry.Pos;
import javafx.scene.Node;
import javafx.scene.Scene;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.control.ProgressBar;
import javafx.scene.control.ToggleButton;
import javafx.scene.control.ToggleGroup;
import javafx.scene.image.ImageView;
import javafx.scene.layout.Background;
import javafx.scene.layout.BackgroundFill;
import javafx.scene.layout.BorderPane;
import javafx.scene.layout.FlowPane;
import javafx.scene.layout.Pane;
import javafx.scene.paint.Color;
import org.json.JSONObject;
import org.xml.sax.SAXException;

import static java.util.stream.Collectors.toList;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 05-21-2019
 */
public class TrendReportPanel extends JFXPanel
{
	private static final Logger LOGGER = Logger.getLogger(TrendReportPanel.class.getName());

	private final List<EpptScenarioRun> _scenarioRuns = new ArrayList<>();
	private final ToggleGroup _toggleGroup = new ToggleGroup();
	private final EpptConfigurationController _epptConfigurationController;
	private TrendReportFlowPane _javascriptPane;
	private EpptParametersPane _parametersPane;
	private ProgressBar _progressBar;
	private Label _progressTextLabel;
	private Button _applyBtn;

	public TrendReportPanel(EpptConfigurationController epptConfigurationController)
	{
		_epptConfigurationController = epptConfigurationController;
		setMinimumSize(new Dimension(1300, 800));
		setPreferredSize(new Dimension(1300, 800));
		Platform.setImplicitExit(false);
		Platform.runLater(this::init);
	}

	private void init()
	{
		_javascriptPane = new TrendReportFlowPane();
		_parametersPane = new EpptParametersPane();
		_applyBtn = new Button("Refresh");

		_applyBtn.setOnAction(e -> inputsChanged());
		ImageView imageView = new ImageView("gov/ca/water/trendreporting/refresh.png");
		imageView.setFitHeight(16);
		imageView.setFitWidth(16);
		_applyBtn.setGraphic(imageView);

		Insets insets = new Insets(4);
		BorderPane mainPane = new BorderPane();
		mainPane.setStyle("-fx-font-size: 11");
		mainPane.setBackground(new Background(new BackgroundFill(Color.WHITE, null, null)));
		BorderPane center = buildJavascriptPane();
		BorderPane.setMargin(center, insets);
		mainPane.setCenter(center);
		Pane left = buildControls();
		BorderPane flowPane = new BorderPane(null, null, _applyBtn, null, left);
		flowPane.setPrefHeight(40);
		BorderPane.setMargin(left, insets);
		BorderPane.setMargin(_applyBtn, insets);
		mainPane.setTop(flowPane);
		left.setMaxWidth(Double.MAX_VALUE);
		flowPane.setMaxWidth(Double.MAX_VALUE);
		Node bottom = buildProgressControls();
		BorderPane.setMargin(bottom, insets);
		mainPane.setBottom(bottom);
		Scene scene = new Scene(mainPane);
		setScene(scene);
	}

	private Node buildProgressControls()
	{
		FlowPane flowPane = new FlowPane(Orientation.HORIZONTAL);
		_progressBar = new ProgressBar();
		_progressBar.setVisible(false);
		_progressTextLabel = new Label();
		flowPane.setHgap(10);
		flowPane.getChildren().addAll(_progressTextLabel, _progressBar);
		flowPane.setAlignment(Pos.BOTTOM_LEFT);
		return flowPane;
	}

	private Pane buildControls()
	{
		Insets insets = new Insets(0, 4, 0, 4);
		Pane toggleControls = buildToggleControls();
		BorderPane borderPane = new BorderPane();
		FlowPane center = new FlowPane(Orientation.VERTICAL, 5.0, 0, toggleControls);
		borderPane.setLeft(_parametersPane);
		borderPane.setCenter(center);
		BorderPane.setMargin(center, insets);
		return borderPane;
	}

	private BorderPane buildJavascriptPane()
	{
		BorderPane borderPane = new BorderPane();
		borderPane.setCenter(_javascriptPane.getDashboardPane());
		BorderPane.setMargin(borderPane, new Insets(5.0, 0, 0, 0));
		return borderPane;
	}

	private Pane buildToggleControls()
	{
		try(Stream<Path> paths = Files.walk(Paths.get(Constant.TREND_REPORTING_DIR), 1))
		{
			paths.filter(path -> path.toString().endsWith(".htm") || path.toString().endsWith(".html"))
				 .map(this::buildToggleButton)
				 .filter(Objects::nonNull)
				 .forEach(_toggleGroup.getToggles()::add);
		}
		catch(IOException ex)
		{
			LOGGER.log(Level.SEVERE, "Unable to build javascript panels", ex);
		}
		FlowPane tilePane = new FlowPane();
		tilePane.alignmentProperty().set(Pos.CENTER_LEFT);
		tilePane.getChildren().addAll(_toggleGroup.getToggles()
												  .stream()
												  .filter(b -> b instanceof ToggleButton)
												  .map(b -> (ToggleButton) b)
												  .collect(toList()));
		Platform.runLater(() ->
				_toggleGroup.getToggles()
							.stream()
							.filter(b -> b instanceof ToggleButton)
							.map(b -> (ToggleButton) b)
							.findAny()
							.ifPresent(b -> b.setSelected(true))
		);
		tilePane.setPrefWidth(550);
		return tilePane;
	}

	private ToggleButton buildToggleButton(Path path)
	{
		try
		{
			TrendReportTabConfig trendReportTabConfig = new TrendReportTabConfig(path);
			TrendReportToggleButton trendReportToggleButton = new TrendReportToggleButton(trendReportTabConfig);
			trendReportToggleButton.selectedProperty().addListener((o, old, n) ->
			{
				if(n)
				{
					inputsChanged();
				}
			});
			return trendReportToggleButton;
		}
		catch(IOException | RuntimeException | ParserConfigurationException | SAXException ex)
		{
			LOGGER.log(Level.SEVERE, "Unable to extract toggle button text from: " + path, ex);
			return null;
		}
	}

	private void inputsChanged()
	{
		TrendReportToggleButton button = (TrendReportToggleButton) _toggleGroup.getSelectedToggle();
		if(button != null)
		{
			_scenarioRuns.clear();
			_epptConfigurationController.getEpptScenarioBase().ifPresent(_scenarioRuns::add);
			_scenarioRuns.addAll(_epptConfigurationController.getEpptScenarioAlternatives());
			int startYear = _epptConfigurationController.getStartYear();
			int endYear = _epptConfigurationController.getEndYear();
			boolean taf = _epptConfigurationController.isTaf();
			List<EpptStatistic> statistic = _epptConfigurationController.getSelectedStatistics();
			List<MonthPeriod> monthPeriod = _epptConfigurationController.getSelectedMonthlyPeriods();
			WaterYearDefinition waterYearDefinition = _epptConfigurationController.getWaterYearDefinition();
			TrendReportTabConfig trendReportTabConfig = button.getTrendReportTabConfig();
			List<EpptParameter> guiLink = new ArrayList<>(_parametersPane.getSelectedItems());
			List<EpptScenarioRun> scenarioRuns = _scenarioRuns.stream().filter(Objects::nonNull).collect(toList());
			boolean difference = _epptConfigurationController.isDifference();
			List<Map<EpptScenarioRun, WaterYearPeriodRangesFilter>> waterYearPeriodRanges = _epptConfigurationController.getWaterYearPeriodRanges();
			TrendReportDataLoader trendReportDataLoader = new TrendReportDataLoader(scenarioRuns, guiLink, startYear, endYear, taf, difference,
					waterYearDefinition, statistic, monthPeriod, waterYearPeriodRanges);
			loadJavascript(trendReportTabConfig.getPath(),
					trendReportDataLoader);
		}
	}

	private void loadJavascript(Path path, TrendReportDataLoader trendReportDataLoader)
	{
		CompletableFuture.runAsync(this::setUiLoading, Platform::runLater)
						 .thenApplyAsync(e -> trendReportDataLoader.computeScenarios())
						 .whenCompleteAsync((jsonObjects, t) -> handleWhenComplete(path, jsonObjects, t), Platform::runLater);
	}

	private void handleWhenComplete(Path path, List<JSONObject> jsonObjects, Throwable t)
	{
		if(t != null)
		{
			if(t.getCause() instanceof CancellationException)
			{
				LOGGER.log(Level.FINE, "Plotting canceled", t);
			}
			else if(t.getCause() instanceof TrendReportDataLoader.TrendReportException)
			{
				_progressTextLabel.setText(t.getCause().getMessage());
			}
			else
			{
				String msg = "Error plotting Trend Report";
				LOGGER.log(Level.SEVERE, msg, t);
				_progressTextLabel.setText(msg);
			}
		}
		if(jsonObjects != null)
		{
			_javascriptPane.removeDashboardPanes();
			try
			{
				for(JSONObject jsonObject : jsonObjects)
				{
					_javascriptPane.addDashboardPane(path, "plot(" + jsonObject.toString(0) + ");");
					_progressTextLabel.setText("");
				}
			}
			catch(RuntimeException e)
			{
				LOGGER.log(Level.SEVERE, "Error plotting Trend Report with serialized JSON object", e);
			}
		}
		_applyBtn.setDisable(false);
		_progressBar.setVisible(false);
	}

	private void setUiLoading()
	{
		_progressBar.setVisible(true);
		_progressTextLabel.setText("Loading...");
		_applyBtn.setDisable(true);
	}

}
