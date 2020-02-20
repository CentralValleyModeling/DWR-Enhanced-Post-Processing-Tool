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
import java.time.LocalDate;
import java.time.YearMonth;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.TreeMap;
import java.util.concurrent.CancellationException;
import java.util.concurrent.CompletableFuture;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.stream.Stream;
import javax.xml.parsers.ParserConfigurationException;

import gov.ca.water.calgui.EpptInitializationException;
import gov.ca.water.calgui.bo.GUILinksAllModelsBO;
import gov.ca.water.calgui.bo.WaterYearIndex;
import gov.ca.water.calgui.busservice.impl.DSSGrabber1SvcImpl;
import gov.ca.water.calgui.busservice.impl.EpptReportingComputedSet;
import gov.ca.water.calgui.busservice.impl.EpptStatistic;
import gov.ca.water.calgui.busservice.impl.MonthPeriod;
import gov.ca.water.calgui.busservice.impl.TrendReportingParameters;
import gov.ca.water.calgui.busservice.impl.WaterYearIndexAliasReader;
import gov.ca.water.calgui.busservice.impl.WaterYearTableReader;
import gov.ca.water.calgui.constant.Constant;
import gov.ca.water.calgui.project.EpptScenarioRun;
import gov.ca.water.calgui.project.PlotConfigurationState;
import gov.ca.water.quickresults.ui.projectconfig.ProjectConfigurationPanel;
import gov.ca.water.quickresults.ui.trendreporting.TrendReportParametersPane;
import gov.ca.water.quickresults.ui.trendreporting.TrendReportPeriodPane;
import gov.ca.water.quickresults.ui.trendreporting.TrendReportStatisticsPane;
import javafx.application.Platform;
import javafx.beans.value.ObservableValue;
import javafx.embed.swing.JFXPanel;
import javafx.geometry.Insets;
import javafx.geometry.Orientation;
import javafx.geometry.Pos;
import javafx.scene.Cursor;
import javafx.scene.Node;
import javafx.scene.Scene;
import javafx.scene.control.Label;
import javafx.scene.control.ProgressBar;
import javafx.scene.control.TitledPane;
import javafx.scene.control.ToggleButton;
import javafx.scene.control.ToggleGroup;
import javafx.scene.layout.Background;
import javafx.scene.layout.BackgroundFill;
import javafx.scene.layout.BorderPane;
import javafx.scene.layout.FlowPane;
import javafx.scene.layout.Pane;
import javafx.scene.layout.TilePane;
import javafx.scene.paint.Color;
import org.json.JSONObject;
import org.xml.sax.SAXException;

import hec.io.TimeSeriesContainer;

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
	private TrendReportFlowPane _javascriptPane;
	private TrendReportStatisticsPane _statisticsPane;
	private TrendReportParametersPane _parametersPane;
	private TrendReportPeriodPane _seasonalPeriodPane;
	private ProgressBar _progressBar;
	private Label _progressTextLabel;
	private CompletableFuture<Void> _waitFuture;

	public TrendReportPanel()
	{
		setMinimumSize(new Dimension(1300, 800));
		setPreferredSize(new Dimension(1300, 800));
		Platform.setImplicitExit(false);
		Platform.runLater(this::init);
	}

	private void init()
	{
		_javascriptPane = new TrendReportFlowPane();
		_statisticsPane = new TrendReportStatisticsPane();
		_parametersPane = new TrendReportParametersPane();
		_seasonalPeriodPane = new TrendReportPeriodPane();

		_statisticsPane.addListener(this::inputsChanged);
		_parametersPane.addListener(this::inputsChanged);
		_seasonalPeriodPane.addListener(this::inputsChanged);

		Insets insets = new Insets(4);
		BorderPane mainPane = new BorderPane();
		mainPane.setBackground(new Background(new BackgroundFill(Color.WHITE, null, null)));
		BorderPane center = buildJavascriptPane();
		BorderPane.setMargin(center, insets);
		mainPane.setCenter(center);
		Pane top = buildControls();
		top.setPrefHeight(30);
		BorderPane.setMargin(top, insets);
		mainPane.setTop(top);
		Node bottom = buildProgressControls();
		BorderPane.setMargin(bottom, insets);
		mainPane.setBottom(bottom);
		Scene scene = new Scene(mainPane);
		setScene(scene);
		try
		{
			ProjectConfigurationPanel.getProjectConfigurationPanel().addScenarioChangedListener(this::setScenarioRuns);
		}
		catch(RuntimeException e)
		{
			LOGGER.log(Level.SEVERE, "Error listening to scenario configuration changes", e);
		}
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
		FlowPane flowPane = new FlowPane(Orientation.HORIZONTAL, 10.0, 5.0);
		flowPane.alignmentProperty().set(Pos.TOP_CENTER);
		flowPane.getChildren().addAll(_parametersPane, _statisticsPane, _seasonalPeriodPane);
		_parametersPane.expandedProperty().bindBidirectional(_statisticsPane.expandedProperty());
		_parametersPane.expandedProperty().bindBidirectional(_seasonalPeriodPane.expandedProperty());
		addExpansionListener(_parametersPane, _parametersPane, _statisticsPane, _seasonalPeriodPane);
		addExpansionListener(_statisticsPane, _parametersPane, _statisticsPane, _seasonalPeriodPane);
		addExpansionListener(_seasonalPeriodPane, _parametersPane, _statisticsPane, _seasonalPeriodPane);
		return flowPane;
	}

	private void addExpansionListener(TitledPane pane, TitledPane parametersPane, TitledPane statisticsListView, TitledPane seasonalPeriodListView)
	{
		pane.expandedProperty().addListener((e, o, n) ->
		{
			if(pane.isExpanded())
			{
				double max = Math.max(20,
						Math.max(Math.max(parametersPane.getHeight(), statisticsListView.getHeight()), seasonalPeriodListView.getHeight()));
				pane.setPrefHeight(max);
			}
			else
			{
				pane.setPrefHeight(20);
			}
		});
	}

	private BorderPane buildJavascriptPane()
	{
		BorderPane borderPane = new BorderPane();
		borderPane.setTop(buildToggleControls());
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
		TilePane tilePane = new TilePane();
		tilePane.alignmentProperty().set(Pos.CENTER);
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
					inputsChanged(null, null, null);
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

	public void setScenarioRuns(EpptScenarioRun baseRun, List<EpptScenarioRun> alternatives)
	{
		_scenarioRuns.clear();
		_scenarioRuns.add(baseRun);
		_scenarioRuns.addAll(alternatives);
		boolean taf = ProjectConfigurationPanel.getProjectConfigurationPanel().isTaf();
		YearMonth startMonth = ProjectConfigurationPanel.getProjectConfigurationPanel().getStartMonth();
		YearMonth endMonth = ProjectConfigurationPanel.getProjectConfigurationPanel().getEndMonth();
		WaterYearIndexAliasReader.WaterYearIndexAlias waterYearIndex = ProjectConfigurationPanel.getProjectConfigurationPanel().getWaterYearIndex();
		Platform.runLater(() -> loadPane(startMonth, endMonth, taf, waterYearIndex));
	}

	private void loadPane(YearMonth start, YearMonth end, boolean taf, WaterYearIndexAliasReader.WaterYearIndexAlias waterYearIndex)
	{
		TrendReportToggleButton button = (TrendReportToggleButton) _toggleGroup.getSelectedToggle();
		if(button != null)
		{
			TrendReportTabConfig trendReportTabConfig = button.getTrendReportTabConfig();
			_statisticsPane.setDisable(trendReportTabConfig.isStatsDisabled());
			_seasonalPeriodPane.setDisable(trendReportTabConfig.isSeasonalPeriodDisabled());
			loadJavascript(trendReportTabConfig.getPath(), start, end, taf, waterYearIndex);
		}
	}

	private void inputsChanged(ObservableValue<?> e, Object o, Object n)
	{
		YearMonth startMonth = ProjectConfigurationPanel.getProjectConfigurationPanel().getStartMonth();
		YearMonth endMonth = ProjectConfigurationPanel.getProjectConfigurationPanel().getEndMonth();
		boolean taf = ProjectConfigurationPanel.getProjectConfigurationPanel().isTaf();
		WaterYearIndexAliasReader.WaterYearIndexAlias waterYearIndex = ProjectConfigurationPanel.getProjectConfigurationPanel().getWaterYearIndex();
		loadPane(startMonth, endMonth, taf, waterYearIndex);
	}

	private void loadJavascript(Path path, YearMonth start, YearMonth end, boolean taf, WaterYearIndexAliasReader.WaterYearIndexAlias waterYearIndex)
	{
		List<TrendReportingParameters.TrendParameter> guiLink = new ArrayList<>(_parametersPane.getSelectedItems());
		List<EpptStatistic> statistic = new ArrayList<>(_statisticsPane.getSelectedItems());
		List<MonthPeriod> monthPeriod = new ArrayList<>(_seasonalPeriodPane.getSelectedItems());
		List<EpptScenarioRun> scenarioRuns = _scenarioRuns.stream().filter(Objects::nonNull).collect(toList());
		Optional<String> error = getError(scenarioRuns, guiLink, statistic, monthPeriod, waterYearIndex);
		if(!error.isPresent())
		{
			setUiLoading(true, Cursor.WAIT, "Loading...");
			if(_waitFuture != null)
			{
				try
				{
					_waitFuture.cancel(true);
				}
				catch(CancellationException ex)
				{
					LOGGER.log(Level.FINE, "Future canceled", ex);
				}
			}
			_waitFuture = CompletableFuture.runAsync(() ->
			{
				try
				{
					Thread.sleep(800L);
				}
				catch(InterruptedException e)
				{
					Thread.currentThread().interrupt();
					LOGGER.log(Level.FINE, "Thread interrupted", e);
				}
			});
			_waitFuture.thenRunAsync(() -> setUiLoading(true, Cursor.WAIT, "Loading..."), Platform::runLater)
					   .thenApplyAsync(e -> computeScenarios(guiLink, statistic, monthPeriod, start, end, taf, scenarioRuns, waterYearIndex))
					   .whenCompleteAsync((jsonObjects, t) ->
					   {
						   handleWhenComplete(path, jsonObjects, t);
						   setUiLoading(false, Cursor.DEFAULT, null);
					   }, Platform::runLater);
		}
		else
		{
			if(_progressTextLabel != null)
			{
				_progressTextLabel.setText(error.get());
			}
		}
	}

	private Optional<String> getError(List<EpptScenarioRun> scenarioRuns,
									  List<TrendReportingParameters.TrendParameter> guiLink, List<EpptStatistic> statistic,
									  List<MonthPeriod> monthPeriod,
									  WaterYearIndexAliasReader.WaterYearIndexAlias waterYearIndex)
	{
		Optional<String> retval = Optional.empty();
		if(scenarioRuns.isEmpty())
		{
			retval = Optional.of("No Scenario Runs defined");
		}
		else if(guiLink.isEmpty() || guiLink.get(0).getGuiLink() == null)
		{
			retval = Optional.of("No Parameter defined");
		}
		else if(statistic.isEmpty() || statistic.get(0).getName().isEmpty())
		{
			retval = Optional.of("No Statistic defined");
		}
		else if(monthPeriod.isEmpty() || monthPeriod.get(0).getStart() == null)
		{
			retval = Optional.of("No Seasonal Period defined");
		}
		else if(waterYearIndex == null)
		{
			retval = Optional.of("No Water Year Index defined");
		}
		return retval;
	}

	private void handleWhenComplete(Path path, List<JSONObject> jsonObjects, Throwable t)
	{
		if(t != null)
		{
			if(t.getCause() instanceof CancellationException)
			{
				LOGGER.log(Level.FINE, "Plotting canceled", t);
			}
			else
			{
				LOGGER.log(Level.SEVERE, "Error plotting Trend Report", t);
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
				}
			}
			catch(RuntimeException e)
			{
				LOGGER.log(Level.SEVERE, "Error plotting Trend Report with serialized JSON object", e);
			}
		}
	}

	private void setUiLoading(boolean progressBar, Cursor cursorType, String text)
	{
		_progressBar.setVisible(progressBar);
		getScene().setCursor(cursorType);
		_progressTextLabel.setText(text);
	}

	private synchronized List<JSONObject> computeScenarios(List<TrendReportingParameters.TrendParameter> parameters, List<EpptStatistic> statistics,
														   List<MonthPeriod> monthPeriods, YearMonth startMonth, YearMonth endMonth,
														   boolean taf, List<EpptScenarioRun> scenarioRuns,
														   WaterYearIndexAliasReader.WaterYearIndexAlias selectedIndex)
	{
		Map<EpptScenarioRun, WaterYearIndex> selectedIndexes = new HashMap<>();
		Map<EpptScenarioRun, List<WaterYearIndex>> allIndexes = new HashMap<>();
		addIndicies(scenarioRuns, selectedIndex, selectedIndexes, allIndexes);
		List<JSONObject> retval = new ArrayList<>();
		for(TrendReportingParameters.TrendParameter parameter : parameters)
		{
			for(EpptStatistic statistic : statistics)
			{
				for(MonthPeriod monthPeriod : monthPeriods)
				{
					GUILinksAllModelsBO guiLink = parameter.getGuiLink();
					Map<EpptScenarioRun, List<TimeSeriesContainer>> scenarioRunData = new TreeMap<>(Comparator.comparing(scenarioRuns::indexOf));
					LocalDate start = LocalDate.of(startMonth.getYear(), startMonth.getMonth(), 1).minusDays(2);
					LocalDate end = LocalDate.of(endMonth.getYear(), endMonth.getMonth(), 1).plusMonths(1).plusDays(2);
					for(EpptScenarioRun epptScenarioRun : scenarioRuns)
					{
						DSSGrabber1SvcImpl dssGrabber = buildDssGrabber(epptScenarioRun, guiLink, taf, start, end);
						TimeSeriesContainer primarySeries = dssGrabber.getPrimarySeries()[0];
						scenarioRunData.put(epptScenarioRun, Collections.singletonList(primarySeries));
					}
					EpptReportingComputedSet epptReportingComputedSet = EpptReportingComputedSet.computeForMetrics(guiLink.getPlotTitle(),
							statistic, monthPeriod, taf, new HashMap<>(), scenarioRunData, new HashMap<>(), new HashMap<>(),
							PlotConfigurationState.ComparisonType.COMPARISON, selectedIndexes, allIndexes);
					JSONObject jsonObject = epptReportingComputedSet.toJson();
					LOGGER.log(Level.FINE, "{0}", jsonObject);
					retval.add(jsonObject);
				}
			}
		}
		return retval;
	}

	private void addIndicies(List<EpptScenarioRun> scenarioRuns, WaterYearIndexAliasReader.WaterYearIndexAlias waterYearIndex,
							 Map<EpptScenarioRun, WaterYearIndex> selectedIndicies,
							 Map<EpptScenarioRun, List<WaterYearIndex>> allIndicies)
	{
		for(EpptScenarioRun epptScenarioRun : scenarioRuns)
		{
			try
			{
				WaterYearTableReader tableReader = new WaterYearTableReader(epptScenarioRun.getLookupDirectory());
				List<WaterYearIndex> read = tableReader.read();
				Optional<WaterYearIndex> opt = read.stream()
												   .filter(index -> waterYearIndex.isAliasFor(index))
												   .findAny();
				if(opt.isPresent())
				{
					selectedIndicies.put(epptScenarioRun, opt.get());
					allIndicies.put(epptScenarioRun, read);
				}
			}
			catch(EpptInitializationException e)
			{
				LOGGER.log(Level.SEVERE, "Error reading water year index file for scenario: " + e);
			}
		}
	}

	private DSSGrabber1SvcImpl buildDssGrabber(EpptScenarioRun epptScenarioRun, GUILinksAllModelsBO guiLink, boolean isCFS, LocalDate start,
											   LocalDate end)
	{
		DSSGrabber1SvcImpl dssGrabber = new DSSGrabber1SvcImpl();
		dssGrabber.setIsCFS(isCFS);
		dssGrabber.setScenarioRuns(epptScenarioRun, Collections.emptyList());
		dssGrabber.setGuiLink(guiLink);
		dssGrabber.setDateRange(start, end);
		return dssGrabber;
	}

}
