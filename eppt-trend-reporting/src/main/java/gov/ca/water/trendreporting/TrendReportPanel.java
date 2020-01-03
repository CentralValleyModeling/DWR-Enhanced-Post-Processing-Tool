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


import java.awt.Frame;
import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.TreeSet;
import java.util.concurrent.CancellationException;
import java.util.concurrent.CompletableFuture;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.stream.Stream;
import javax.swing.*;
import javax.xml.parsers.ParserConfigurationException;

import gov.ca.water.calgui.EpptInitializationException;
import gov.ca.water.calgui.bo.GUILinksAllModelsBO;
import gov.ca.water.calgui.bo.WaterYearDefinition;
import gov.ca.water.calgui.bo.WaterYearIndex;
import gov.ca.water.calgui.busservice.impl.GuiLinksSeedDataSvcImpl;
import gov.ca.water.calgui.busservice.impl.WaterYearDefinitionSvc;
import gov.ca.water.calgui.busservice.impl.WaterYearTableReader;
import gov.ca.water.calgui.constant.Constant;
import gov.ca.water.calgui.project.EpptScenarioRun;
import gov.ca.water.quickresults.ui.projectconfig.ProjectConfigurationPanel;
import javafx.application.Platform;
import javafx.beans.value.ObservableValue;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.collections.transformation.FilteredList;
import javafx.embed.swing.JFXPanel;
import javafx.event.ActionEvent;
import javafx.geometry.Insets;
import javafx.geometry.Orientation;
import javafx.geometry.Pos;
import javafx.scene.Cursor;
import javafx.scene.Node;
import javafx.scene.Scene;
import javafx.scene.control.Button;
import javafx.scene.control.CheckBox;
import javafx.scene.control.ComboBox;
import javafx.scene.control.Label;
import javafx.scene.control.ListCell;
import javafx.scene.control.ListView;
import javafx.scene.control.ProgressBar;
import javafx.scene.control.SelectionMode;
import javafx.scene.control.TitledPane;
import javafx.scene.control.ToggleButton;
import javafx.scene.control.ToggleGroup;
import javafx.scene.layout.Background;
import javafx.scene.layout.BackgroundFill;
import javafx.scene.layout.BorderPane;
import javafx.scene.layout.FlowPane;
import javafx.scene.layout.GridPane;
import javafx.scene.layout.HBox;
import javafx.scene.layout.Pane;
import javafx.scene.layout.TilePane;
import javafx.scene.layout.VBox;
import javafx.scene.paint.Color;
import org.json.JSONObject;
import org.xml.sax.SAXException;

import static gov.ca.water.trendreporting.TrendType.ALL_TREND_TYPE;
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
	private final Set<TrendType> _trendTypes = getTrendTypes();
	private TrendReportFlowPane _javascriptPane;
	private ListView<TrendReportingParameters.TrendParameter> _parameterListView;
	private ListView<TrendStatistics> _statisticsListView;
	private ListView<EpptReportingMonths.MonthPeriod> _seasonalPeriodListView;
	private CheckBox _tafCheckbox;
	private ComboBox<WaterYearIndex> _waterYearIndexComboBox;
	private ComboBox<WaterYearDefinition> _waterYearDefinitionComboBox;
	private ProgressBar _progressBar;
	private Label _progressTextLabel;
	private CompletableFuture<Void> _waitFuture;
	private ListView<TrendType> _typeListView;
	private FilteredList<TrendReportingParameters.TrendParameter> _filteredParameters;
	private ObservableList<TrendReportingParameters.TrendParameter> _backingParameters;

	public TrendReportPanel()
	{
		Platform.setImplicitExit(false);
		Platform.runLater(this::init);
		CompletableFuture.supplyAsync(this::getTrendStatistics)
						 .whenComplete((o, t) ->
						 {
							 if(t != null)
							 {
								 LOGGER.log(Level.SEVERE, "Error loading Trend Report Statistics", t);
							 }
							 else
							 {
								 Platform.runLater(() ->
								 {
									 _statisticsListView.getItems().addAll(o);
									 _statisticsListView.getSelectionModel().select(0);
								 });
							 }
						 });
	}

	private void init()
	{
		_javascriptPane = new TrendReportFlowPane();
		_typeListView = new ListView<>();
		_parameterListView = new ListView<>();
		_statisticsListView = new ListView<>();
		_seasonalPeriodListView = new ListView<>();

		_typeListView.setStyle("-fx-selection-bar:-fx-focus-color ;-fx-selection-bar-non-focused: -fx-focus-color ;");
		_parameterListView.setStyle("-fx-selection-bar:-fx-focus-color ;-fx-selection-bar-non-focused: -fx-focus-color ;");
		_statisticsListView.setStyle("-fx-selection-bar:-fx-focus-color ;-fx-selection-bar-non-focused: -fx-focus-color ;");
		_seasonalPeriodListView.setStyle("-fx-selection-bar:-fx-focus-color ;-fx-selection-bar-non-focused: -fx-focus-color ;");

		_tafCheckbox = new CheckBox("CFS -> TAF/mon");
		_waterYearIndexComboBox = new ComboBox<>();
		_waterYearDefinitionComboBox = new ComboBox<>();
		fillWaterYearDefinitionCombo();

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
		TitledPane parametersPane = buildParametersPane();
		TitledPane statisticsListView = buildStatisticsListView();
		TitledPane seasonalPeriodListView = buildSeasonalPeriodListView();
		HBox timeWindowControls = buildTimeWindowControls();
		flowPane.getChildren().addAll(parametersPane, statisticsListView, seasonalPeriodListView, timeWindowControls);
		parametersPane.expandedProperty().bindBidirectional(statisticsListView.expandedProperty());
		parametersPane.expandedProperty().bindBidirectional(seasonalPeriodListView.expandedProperty());
		addExpansionListener(parametersPane, parametersPane, statisticsListView, seasonalPeriodListView);
		addExpansionListener(statisticsListView, parametersPane, statisticsListView, seasonalPeriodListView);
		addExpansionListener(seasonalPeriodListView, parametersPane, statisticsListView, seasonalPeriodListView);
		_trendTypes.removeIf(t -> _backingParameters.stream().noneMatch(n->t.matchesGuiLink(n.getGuiLink())));
		updateTrendTypes();
		_typeListView.getSelectionModel().select(0);
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

	private HBox buildTimeWindowControls()
	{
		_waterYearIndexComboBox.setMaxWidth(Double.MAX_VALUE);
		_waterYearDefinitionComboBox.setMaxWidth(Double.MAX_VALUE);
		_tafCheckbox.selectedProperty().addListener(this::inputsChanged);
		_waterYearIndexComboBox.getSelectionModel().selectedIndexProperty().addListener(this::inputsChanged);
		_waterYearDefinitionComboBox.getSelectionModel().selectedIndexProperty().addListener(this::inputsChanged);
		Label label = new Label("Water Year Index: ");
		label.setAlignment(Pos.CENTER_LEFT);
		HBox hBox = new HBox(10, label, _waterYearIndexComboBox);
		hBox.setAlignment(Pos.CENTER_LEFT);
		return hBox;
	}

	private TitledPane buildParametersPane()
	{
		TilePane innerPane = new TilePane(Orientation.VERTICAL, 5, 5, buildTypeListView(), buildParameterListView());
		TitledPane titledPane = new TitledPane("Parameters", innerPane);
		titledPane.setPrefWidth(525);
		titledPane.setPrefHeight(200);
		return titledPane;
	}

	private static Set<TrendType> getTrendTypes()
	{
		Set<TrendType> retval = new TreeSet<>(Comparator.comparing(trendType -> trendType.getTitle().toLowerCase()));
		List<GUILinksAllModelsBO> allGuiLinks = GuiLinksSeedDataSvcImpl.getSeedDataSvcImplInstance().getAllGuiLinks();
		for(GUILinksAllModelsBO guiLink : allGuiLinks)
		{
			String plotAxisLabel = guiLink.getPlotAxisLabel();
			if(plotAxisLabel != null && !plotAxisLabel.isEmpty())
			{
				retval.add(new TrendType(plotAxisLabel));
			}
			else
			{
				retval.add(new TrendType(guiLink));
			}
		}
		return retval;
	}

	private Pane buildTypeListView()
	{
		_typeListView.getSelectionModel().setSelectionMode(SelectionMode.SINGLE);
		_typeListView.getSelectionModel().select(0);
		_typeListView.getSelectionModel().selectedItemProperty().addListener(this::typeSelected);
		_typeListView.setCellFactory(param -> new ListCell<TrendType>()
		{
			@Override
			protected void updateItem(TrendType item, boolean empty)
			{
				super.updateItem(item, empty);

				if(empty || item == null || item.getTitle() == null)
				{
					setText(null);
				}
				else
				{
					String title = item.getTitle();
					setText(title);
				}
			}
		});
		BorderPane borderPane = new BorderPane();
		Label typeLabel = new Label("Type Name");
		typeLabel.setPrefHeight(20);
		borderPane.setLeft(typeLabel);
		//Invisible spacer button
		Button addButton = new Button("");
		borderPane.setRight(addButton);
		addButton.setVisible(false);
		VBox vBox = new VBox(5, borderPane, _typeListView);
		vBox.setPrefHeight(200);
		return vBox;
	}

	private void typeSelected(ObservableValue<? extends TrendType> e, TrendType o, TrendType n)
	{
		_filteredParameters.setPredicate(trendParameter ->
		{
			boolean retval = false;
			if(n != null)
			{
				retval = n.matchesGuiLink(trendParameter.getGuiLink());
			}
			return retval;
		});
	}

	private Pane buildParameterListView()
	{
		TrendReportingParameters trendReportingParameters = TrendReportingParameters.getTrendReportingParameters();
		_backingParameters = FXCollections.observableArrayList(trendReportingParameters.getTrendParameters());
		_filteredParameters = new FilteredList<>(_backingParameters, s -> true);
		_parameterListView.setItems(_filteredParameters);
		_parameterListView.getSelectionModel().setSelectionMode(SelectionMode.SINGLE);
		_parameterListView.getSelectionModel().select(0);
		_parameterListView.getSelectionModel().selectedItemProperty().addListener(this::inputsChanged);
		_parameterListView.setCellFactory(param -> new ListCell<TrendReportingParameters.TrendParameter>()
		{
			@Override
			protected void updateItem(TrendReportingParameters.TrendParameter item, boolean empty)
			{
				super.updateItem(item, empty);

				if(empty || item == null || item.getTitle() == null)
				{
					setText(null);
				}
				else
				{
					String plotTitle = item.getTitle();
					setText(plotTitle);
				}
			}
		});
		BorderPane borderPane = new BorderPane();
		Label parameterLabel = new Label("Parameter Name");
		parameterLabel.setPrefHeight(20);
		borderPane.setLeft(parameterLabel);
		Button addButton = new Button("Add");
		addButton.setOnAction(this::addParameter);
		borderPane.setRight(addButton);
		VBox vBox = new VBox(5, borderPane, _parameterListView);
		vBox.setPrefHeight(200);
		return vBox;
	}

	private void addParameter(ActionEvent e)
	{
		try
		{
			GUILinksAllModelsBO guiLink = createGuiLink();
			if(guiLink != null)
			{
				//Both elements should be added to the list
				boolean added1 = _trendTypes.add(new TrendType(guiLink.getPlotAxisLabel()));
				boolean added2 = _trendTypes.add(new TrendType(guiLink));
				if(added1 || added2)
				{
					updateTrendTypes();
				}
				int size = _backingParameters.size();
				_backingParameters.add(TrendReportingParameters.TrendParameter.create(size, guiLink, ""));
			}
		}
		catch(InterruptedException ex)
		{
			LOGGER.log(Level.FINE, "Swing thread interrupted. Unable to create new parameter", ex);
			Thread.currentThread().interrupt();
		}
		catch(InvocationTargetException | RuntimeException ex)
		{
			LOGGER.log(Level.SEVERE, "Error creating new parameter", ex);
		}
	}

	private void updateTrendTypes()
	{
		TrendType selectedItem = _typeListView.getSelectionModel().getSelectedItem();
		_trendTypes.removeIf(t -> t.getTitle() == null || t.getTitle().isEmpty());
		_typeListView.getItems().clear();
		_typeListView.getItems().add(ALL_TREND_TYPE);
		_typeListView.getItems().addAll(_trendTypes);
		if(selectedItem != null)
		{
			_typeListView.getSelectionModel().select(selectedItem);
		}
	}

	private GUILinksAllModelsBO createGuiLink() throws InvocationTargetException, InterruptedException
	{
		AddParameterDialog addParameterDialog = new AddParameterDialog((Frame) SwingUtilities.windowForComponent(this));
		SwingUtilities.invokeAndWait(() -> addParameterDialog.setVisible(true));
		if(!addParameterDialog.isCanceled())
		{
			String type = addParameterDialog.getDataType();
			String parameter = addParameterDialog.getParameter();
			String bAndCPart = addParameterDialog.getBAndCPart();
			GUILinksAllModelsBO guiLink = new GUILinksAllModelsBO("", type, parameter, "");
			GUILinksAllModelsBO.Model.values()
									 .forEach(m -> guiLink.addModelMapping(m.toString(), bAndCPart, ""));
			return guiLink;
		}
		else
		{
			return null;
		}
	}

	private TitledPane buildStatisticsListView()
	{
		_statisticsListView.getSelectionModel().setSelectionMode(SelectionMode.SINGLE);
		_statisticsListView.getSelectionModel().select(0);
		_statisticsListView.getSelectionModel().selectedItemProperty().addListener(this::inputsChanged);
		_statisticsListView.setCellFactory(param -> new ListCell<TrendStatistics>()
		{
			@Override
			protected void updateItem(TrendStatistics item, boolean empty)
			{
				super.updateItem(item, empty);

				if(empty || item == null || item.getName() == null)
				{
					setText(null);
				}
				else
				{
					setText(item.getName());
				}
			}
		});
		VBox vBox = new VBox(_statisticsListView);
		vBox.setPrefWidth(250);
		vBox.setPrefHeight(200);
		TilePane innerPane = new TilePane(Orientation.VERTICAL, 5, 5, vBox);
		TitledPane titledPane = new TitledPane("Statistic", innerPane);
		titledPane.setPrefWidth(250);
		titledPane.setPrefHeight(200);
		return titledPane;
	}

	private List<TrendStatistics> getTrendStatistics()
	{
		List<TrendStatistics> retval = new ArrayList<>();
		Path jython = Paths.get(Constant.TREND_REPORTING_DIR).resolve("jython");
		try(Stream<Path> stream = Files.walk(jython, 5))
		{
			Platform.runLater(() -> _progressBar.setVisible(true));
			retval = stream.filter(p -> p.toFile().isFile()).filter(p -> p.toString().endsWith("py"))
						   .map(TrendStatistics::new)
						   .collect(toList());
		}
		catch(IOException e)
		{
			LOGGER.log(Level.SEVERE, "Unable to load Statistics for Trend Reporting dashboard", e);
		}
		finally
		{
			Platform.runLater(() ->
			{
				if(_progressBar != null)
				{
					_progressBar.setVisible(false);
				}
			});
		}
		return retval;
	}

	private TitledPane buildSeasonalPeriodListView()
	{
		List<EpptReportingMonths.MonthPeriod> allMonthPeriods = EpptReportingMonths.getAllMonthPeriods();
		_seasonalPeriodListView.getItems().addAll(allMonthPeriods);
		_seasonalPeriodListView.getSelectionModel().setSelectionMode(SelectionMode.SINGLE);
		_seasonalPeriodListView.getSelectionModel().select(0);
		_seasonalPeriodListView.getSelectionModel().selectedItemProperty().addListener(this::inputsChanged);
		VBox vBox = new VBox(_seasonalPeriodListView);
		vBox.setPrefHeight(200);
		TilePane innerPane = new TilePane(Orientation.VERTICAL, 5, 5, vBox);
		TitledPane titledPane = new TitledPane("Seasonal Period", innerPane);
		titledPane.setPrefWidth(200);
		titledPane.setPrefHeight(200);
		return titledPane;
	}

	private BorderPane buildJavascriptPane()
	{
		BorderPane borderPane = new BorderPane();
		borderPane.setTop(buildToggleControls());
		borderPane.setCenter(_javascriptPane.getDashboardPane());
		BorderPane.setMargin(borderPane, new Insets(5.0, 0, 0, 0));

		borderPane.setMaxWidth(1300);
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
		LocalDate startMonth = ProjectConfigurationPanel.getProjectConfigurationPanel().getStartMonth();
		LocalDate endMonth = ProjectConfigurationPanel.getProjectConfigurationPanel().getEndMonth();
		Platform.runLater(() ->
		{
			if(baseRun != null)
			{
				fillWaterYearIndexCombo(baseRun);
			}
			else if(!alternatives.isEmpty())
			{
				fillWaterYearIndexCombo(alternatives.get(0));
			}
			else
			{
				_waterYearIndexComboBox.getItems().clear();
			}
			loadPane(startMonth, endMonth, taf);
		});
	}

	private void fillWaterYearIndexCombo(EpptScenarioRun baseRun)
	{
		try
		{
			if(baseRun != null)
			{
				WaterYearTableReader waterYearTableReader = new WaterYearTableReader(baseRun.getWaterYearTable());
				WaterYearIndex selectedItem = _waterYearIndexComboBox.selectionModelProperty().get().getSelectedItem();
				List<WaterYearIndex> indices = waterYearTableReader.read();
				_waterYearIndexComboBox.itemsProperty().get().clear();
				_waterYearIndexComboBox.itemsProperty().get().addAll(indices);
				if(selectedItem == null)
				{
					_waterYearIndexComboBox.selectionModelProperty().get().select(0);
				}
				else
				{
					WaterYearIndex waterYearIndex = indices.stream().filter(f -> f.toString().equals(selectedItem.toString()))
														   .findAny().orElse(indices.get(0));
					_waterYearIndexComboBox.selectionModelProperty().get().select(waterYearIndex);
				}
			}
		}
		catch(EpptInitializationException e)
		{
			LOGGER.log(Level.SEVERE, "Error reading water year table from: " + baseRun.getName(), e);
		}
	}

	private void fillWaterYearDefinitionCombo()
	{
		WaterYearDefinition selectedItem = _waterYearDefinitionComboBox.selectionModelProperty().get().getSelectedItem();
		List<WaterYearDefinition> definitions = WaterYearDefinitionSvc.getWaterYearDefinitionSvc().getDefinitions();
		_waterYearDefinitionComboBox.itemsProperty().get().clear();
		_waterYearDefinitionComboBox.itemsProperty().get().addAll(definitions);
		if(selectedItem == null)
		{
			_waterYearDefinitionComboBox.selectionModelProperty().get().select(0);
		}
		else
		{
			WaterYearDefinition waterYearIndex = definitions.stream().filter(f -> f.toString().equals(selectedItem.toString()))
															.findAny().orElse(definitions.get(0));
			_waterYearDefinitionComboBox.selectionModelProperty().get().select(waterYearIndex);
		}
	}

	private void loadPane(LocalDate start, LocalDate end, boolean taf)
	{
		TrendReportToggleButton button = (TrendReportToggleButton) _toggleGroup.getSelectedToggle();
		if(button != null)
		{
			TrendReportTabConfig trendReportTabConfig = button.getTrendReportTabConfig();
			_statisticsListView.setDisable(trendReportTabConfig.isStatsDisabled());
			_seasonalPeriodListView.setDisable(trendReportTabConfig.isSeasonalPeriodDisabled());
			_waterYearIndexComboBox.setDisable(trendReportTabConfig.isWaterYearIndexDisabled());
			loadJavascript(trendReportTabConfig.getPath(), start, end, taf);
		}
	}

	private void inputsChanged(ObservableValue<?> e, Object o, Object n)
	{
		LocalDate startMonth = ProjectConfigurationPanel.getProjectConfigurationPanel().getStartMonth();
		LocalDate endMonth = ProjectConfigurationPanel.getProjectConfigurationPanel().getEndMonth();
		boolean taf = ProjectConfigurationPanel.getProjectConfigurationPanel().isTaf();
		loadPane(startMonth, endMonth, taf);
	}

	private void loadJavascript(Path path, LocalDate start, LocalDate end, boolean taf)
	{
		List<TrendReportingParameters.TrendParameter> guiLink = new ArrayList<>(_parameterListView.getSelectionModel().getSelectedItems());
		List<TrendStatistics> statistic = new ArrayList<>(_statisticsListView.getSelectionModel().getSelectedItems());
		List<EpptReportingMonths.MonthPeriod> monthPeriod = new ArrayList<>(_seasonalPeriodListView.getSelectionModel().getSelectedItems());
		WaterYearIndex waterYearIndex = _waterYearIndexComboBox.getSelectionModel().getSelectedItem();
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
					   .thenApplyAsync(e -> computeScenarios(guiLink, statistic, monthPeriod, start, end, taf, scenarioRuns))
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
									  List<TrendReportingParameters.TrendParameter> guiLink, List<TrendStatistics> statistic,
									  List<EpptReportingMonths.MonthPeriod> monthPeriod,
									  WaterYearIndex waterYearIndex)
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

	private void setUiLoading(boolean progresBar, Cursor cursorType, String text)
	{
		_progressBar.setVisible(progresBar);
		getScene().setCursor(cursorType);
		_progressTextLabel.setText(text);
	}

	private synchronized List<JSONObject> computeScenarios(List<TrendReportingParameters.TrendParameter> parameters, List<TrendStatistics> statistics,
														   List<EpptReportingMonths.MonthPeriod> monthPeriods, LocalDate start,
														   LocalDate end, boolean taf, List<EpptScenarioRun> scenarioRuns)
	{
		List<JSONObject> retval = new ArrayList<>();
		for(TrendReportingParameters.TrendParameter parameter : parameters)
		{
			for(TrendStatistics statistic : statistics)
			{
				for(EpptReportingMonths.MonthPeriod monthPeriod : monthPeriods)
				{
					EpptReportingComputedSet epptReportingComputedSet = computeForMetrics(parameter.getGuiLink(), statistic, monthPeriod,
							start, end, taf, scenarioRuns);
					JSONObject jsonObject = epptReportingComputedSet.toJson();
					LOGGER.log(Level.FINE, "{0}", jsonObject);
					retval.add(jsonObject);
				}
			}
		}
		return retval;
	}

	private EpptReportingComputedSet computeForMetrics(GUILinksAllModelsBO guiLink, TrendStatistics statistic,
													   EpptReportingMonths.MonthPeriod monthPeriod, LocalDate start,
													   LocalDate end, boolean taf, List<EpptScenarioRun> scenarioRuns)
	{
		WaterYearDefinition waterYearDefinition = _waterYearDefinitionComboBox.getSelectionModel().getSelectedItem();
		WaterYearIndex waterYearIndex = _waterYearIndexComboBox.getSelectionModel().getSelectedItem();
		ObservableList<WaterYearIndex> waterYearIndices = _waterYearIndexComboBox.getItems();
		EpptReportingComputer trendReportingComputer = new EpptReportingComputer(guiLink, statistic, monthPeriod, waterYearDefinition,
				waterYearIndex, waterYearIndices);
		List<EpptReportingComputed> trendReportingComputed = new ArrayList<>();

		for(EpptScenarioRun scenarioRun : scenarioRuns)
		{
			try
			{

				if(taf)
				{
					trendReportingComputed.add(trendReportingComputer.computeTaf(scenarioRun, start, end));
				}
				else
				{
					trendReportingComputed.add(trendReportingComputer.computeCfs(scenarioRun, start, end));
				}
			}
			catch(EpptInitializationException e)
			{
				LOGGER.log(Level.SEVERE, "Error calculating Trend Reporting for scenario run: " + scenarioRun, e);
			}
		}
		return new EpptReportingComputedSet(guiLink, statistic, monthPeriod,
				_tafCheckbox.isSelected(), trendReportingComputed);
	}

}
