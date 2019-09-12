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


import java.io.BufferedReader;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.time.Month;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.stream.Stream;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import gov.ca.water.calgui.bo.GUILinksAllModelsBO;
import gov.ca.water.calgui.busservice.IGuiLinksSeedDataSvc;
import gov.ca.water.calgui.busservice.impl.GuiLinksSeedDataSvcImpl;
import gov.ca.water.calgui.compute.EpptReportingComputed;
import gov.ca.water.calgui.compute.EpptReportingComputedSet;
import gov.ca.water.calgui.compute.EpptReportingComputer;
import gov.ca.water.calgui.compute.EpptReportingMonths;
import gov.ca.water.calgui.constant.Constant;
import gov.ca.water.calgui.project.EpptScenarioRun;
import gov.ca.water.quickresults.ui.projectconfig.ProjectConfigurationPanel;
import gov.ca.water.trendreporting.monthpicker.FXCalendar;
import gov.ca.water.calgui.compute.stats.Statistics;
import javafx.application.Platform;
import javafx.beans.value.ObservableValue;
import javafx.embed.swing.JFXPanel;
import javafx.geometry.Insets;
import javafx.geometry.Orientation;
import javafx.geometry.Pos;
import javafx.scene.Cursor;
import javafx.scene.Node;
import javafx.scene.Scene;
import javafx.scene.control.CheckBox;
import javafx.scene.control.Label;
import javafx.scene.control.ListCell;
import javafx.scene.control.ListView;
import javafx.scene.control.ProgressBar;
import javafx.scene.control.ScrollPane;
import javafx.scene.control.SelectionMode;
import javafx.scene.control.ToggleButton;
import javafx.scene.control.ToggleGroup;
import javafx.scene.layout.Background;
import javafx.scene.layout.BackgroundFill;
import javafx.scene.layout.BorderPane;
import javafx.scene.layout.FlowPane;
import javafx.scene.layout.GridPane;
import javafx.scene.layout.Pane;
import javafx.scene.layout.TilePane;
import javafx.scene.paint.Color;
import org.json.JSONObject;
import org.w3c.dom.Document;
import org.w3c.dom.NodeList;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;

import rma.util.lookup.Lookup;

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
	private ListView<GUILinksAllModelsBO> _parameterListView;
	private ListView<Statistics> _statisticsListView;
	private ListView<EpptReportingMonths.MonthPeriod> _seasonalPeriodListView;
	private CheckBox _tafCheckbox;
	private FXCalendar _startCalendar;
	private FXCalendar _endCalendar;
	private Path _currentScript;
	private ProgressBar _progressBar;

	public TrendReportPanel()
	{
		setBackground(java.awt.Color.WHITE);
		Platform.setImplicitExit(false);
		Platform.runLater(this::init);
	}

	private void init()
	{
		_javascriptPane = new TrendReportFlowPane();
		_parameterListView = new ListView<>();
		_statisticsListView = new ListView<>();
		_seasonalPeriodListView = new ListView<>();
		_tafCheckbox = new CheckBox("CFS -> TAF/mon");

		Insets insets = new Insets(10);
		BorderPane mainPane = new BorderPane();
		mainPane.setBackground(new Background(new BackgroundFill(Color.WHITE, null, null)));
		BorderPane center = buildJavascriptPane();
		BorderPane.setMargin(center,insets);
		mainPane.setCenter(center);
		Pane top = buildParameterControls();
		BorderPane.setMargin(top,insets);
		mainPane.setTop(top);
		Node bottom = buildProgressControls();
		BorderPane.setMargin(bottom,insets);
		mainPane.setBottom(bottom);
		Scene scene = new Scene(mainPane);
		setScene(scene);
		ProjectConfigurationPanel.getProjectConfigurationPanel().addScenarioChangedListener(this::setScenarioRuns);
	}

	private Node buildProgressControls()
	{
		FlowPane flowPane = new FlowPane(Orientation.HORIZONTAL);
		_progressBar = new ProgressBar();
		_progressBar.setVisible(false);
		flowPane.getChildren().add(_progressBar);
		flowPane.setAlignment(Pos.BOTTOM_RIGHT);
		return flowPane;
	}

	private Pane buildParameterControls()
	{
		FlowPane flowPane = new FlowPane(Orientation.HORIZONTAL);
		flowPane.alignmentProperty().set(Pos.CENTER);
		TilePane tilePane = new TilePane(Orientation.HORIZONTAL, 10.0, 5.0);
		tilePane.alignmentProperty().set(Pos.CENTER);
		tilePane.setMinWidth(800);
		tilePane.getChildren().addAll(buildParameterListView(), buildStatisticsListView(), buildSeasonalPeriodListView(), buildTimeWindowControls());
		flowPane.getChildren().addAll(tilePane);
		return flowPane;
	}

	private GridPane buildTimeWindowControls()
	{
		_startCalendar = new FXCalendar();
		_startCalendar.setSelectedMonth(Month.OCTOBER.getValue());
		_startCalendar.setSelectedYear(1921);
		_endCalendar = new FXCalendar();
		_endCalendar.setSelectedMonth(Month.SEPTEMBER.getValue());
		_endCalendar.setSelectedYear(2007);
		GridPane gridPane = new GridPane();
		gridPane.add(new Label("Start: "), 0, 0);
		gridPane.add(_startCalendar, 1, 0);
		gridPane.add(new Label("End: "), 0, 1);
		gridPane.add(_endCalendar, 1, 1);
//		gridPane.add(_tafCheckbox, 0, 3, 2, 1);
		gridPane.setHgap(10);
		gridPane.setVgap(10);
		gridPane.setPrefHeight(125);
		_tafCheckbox.selectedProperty().addListener(this::inputsChanged);
		_startCalendar.selectedMonthProperty().addListener(this::inputsChanged);
		_startCalendar.selectedYearProperty().addListener(this::inputsChanged);
		_endCalendar.selectedMonthProperty().addListener(this::inputsChanged);
		_endCalendar.selectedYearProperty().addListener(this::inputsChanged);
		return gridPane;
	}

	private Pane buildParameterListView()
	{
		IGuiLinksSeedDataSvc seedDataSvcImplInstance = GuiLinksSeedDataSvcImpl.getSeedDataSvcImplInstance();
		_parameterListView.getItems().addAll(seedDataSvcImplInstance.getAllGuiLinks());
		_parameterListView.getSelectionModel().setSelectionMode(SelectionMode.SINGLE);
		_parameterListView.getSelectionModel().select(0);
		_parameterListView.getSelectionModel().selectedItemProperty().addListener(this::inputsChanged);
		_parameterListView.setPrefHeight(125);
		_parameterListView.setPrefWidth(250);
		_parameterListView.setCellFactory(param -> new ListCell<GUILinksAllModelsBO>()
		{
			@Override
			protected void updateItem(GUILinksAllModelsBO item, boolean empty)
			{
				super.updateItem(item, empty);

				if(empty || item == null || item.getPlotTitle() == null)
				{
					setText(null);
				}
				else
				{
					setText(item.getPlotTitle());
				}
			}
		});
		BorderPane borderPane = new BorderPane();
		borderPane.setCenter(_parameterListView);
		borderPane.setTop(new Label("Parameter"));
		return borderPane;
	}

	private Pane buildStatisticsListView()
	{
		Collection<? extends Statistics> statistics = Lookup.getDefault().lookupAll(Statistics.class);
		_statisticsListView.getItems().addAll(statistics);
		_statisticsListView.getSelectionModel().setSelectionMode(SelectionMode.SINGLE);
		_statisticsListView.getSelectionModel().select(0);
		_statisticsListView.getSelectionModel().selectedItemProperty().addListener(this::inputsChanged);
		_statisticsListView.setCellFactory(param -> new ListCell<Statistics>()
		{
			@Override
			protected void updateItem(Statistics item, boolean empty)
			{
				super.updateItem(item, empty);

				if(empty || item == null || item.getName() == null)
				{
					setText(null);
				}
				else
				{
					setDisable(item.isDisabled());
					setTextFill(item.isDisabled() ? Color.GRAY : Color.BLACK);
					setText(item.getName());
				}
			}
		});
		_statisticsListView.setPrefHeight(125);
		_statisticsListView.setPrefWidth(225);
		BorderPane borderPane = new BorderPane();
		borderPane.setCenter(_statisticsListView);
		borderPane.setTop(new Label("Statistic"));
		return borderPane;
	}

	private Pane buildSeasonalPeriodListView()
	{
		List<EpptReportingMonths.MonthPeriod> allMonthPeriods = EpptReportingMonths.getAllMonthPeriods();
		_seasonalPeriodListView.getItems().addAll(allMonthPeriods);
		_seasonalPeriodListView.getSelectionModel().setSelectionMode(SelectionMode.SINGLE);
		_seasonalPeriodListView.getSelectionModel().select(0);
		_seasonalPeriodListView.getSelectionModel().selectedItemProperty().addListener(this::inputsChanged);
		_seasonalPeriodListView.setPrefHeight(125);
		_seasonalPeriodListView.setPrefWidth(200);
		BorderPane borderPane = new BorderPane();
		borderPane.setCenter(_seasonalPeriodListView);
		borderPane.setTop(new Label("Seasonal Period"));
		return borderPane;
	}

	private BorderPane buildJavascriptPane()
	{
		BorderPane borderPane = new BorderPane();
		borderPane.setTop(buildToggleControls());
		borderPane.setCenter(buildJavascriptControls());
		borderPane.setBackground(new Background(new BackgroundFill(Color.WHITE, null, null)));
		BorderPane.setMargin(borderPane, new Insets(5.0, 0, 0, 0));
		return borderPane;
	}

	private ScrollPane buildJavascriptControls()
	{
		ScrollPane scrollPane = new ScrollPane();
		scrollPane.setBackground(new Background(new BackgroundFill(Color.WHITE, null, null)));
		scrollPane.setContent(_javascriptPane.getDashboardPane());
		scrollPane.setFitToWidth(true);
		scrollPane.setFitToHeight(true);
		return scrollPane;
	}

	private Pane buildToggleControls()
	{
		try(Stream<Path> paths = Files.walk(Paths.get(Constant.TREND_REPORTING_DIR)))
		{
			paths.filter(path -> path.toString().endsWith(".htm") || path.toString().endsWith(".html"))
				 .map(this::buildToggleButton)
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
		String text = path.getFileName().toString();
		try(BufferedReader reader = Files.newBufferedReader(path))
		{
			DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
			dbf.setValidating(false);
			dbf.setNamespaceAware(true);
			dbf.setIgnoringComments(false);
			dbf.setIgnoringElementContentWhitespace(false);
			dbf.setExpandEntityReferences(false);
			DocumentBuilder db = dbf.newDocumentBuilder();
			Document parse = db.parse(new InputSource(reader));
			NodeList head = parse.getElementsByTagName("head");
			if(head != null && head.getLength() > 0)
			{
				text = head.item(0).getTextContent();
			}
		}
		catch(IOException | RuntimeException | ParserConfigurationException | SAXException ex)
		{
			LOGGER.log(Level.SEVERE, "Unable to extract toggle button text from: " + path, ex);
		}

		ToggleButton toggleButton = new ToggleButton(text.trim());
		toggleButton.setMaxWidth(Double.MAX_VALUE);
		toggleButton.selectedProperty().addListener((o, old, n) -> loadPane(n, path));
		return toggleButton;
	}

	public void setScenarioRuns(EpptScenarioRun baseRun, List<EpptScenarioRun> alternatives)
	{
		_scenarioRuns.clear();
		_scenarioRuns.add(baseRun);
		_scenarioRuns.addAll(alternatives);
		Platform.runLater(this::reload);
	}

	private void loadPane(boolean selected, Path path)
	{
		if(selected)
		{
			loadJavascript(path);
		}
	}

	private void reload()
	{
		if(_currentScript != null)
		{
			loadJavascript(_currentScript);
		}
	}

	private void inputsChanged(ObservableValue<?> e, Object o, Object n)
	{
		reload();
	}

	private void loadJavascript(Path path)
	{
		_javascriptPane.removeDashboardPanes();
		_currentScript = path;
		List<GUILinksAllModelsBO> guiLink = _parameterListView.getSelectionModel().getSelectedItems();
		List<Statistics> statistic = _statisticsListView.getSelectionModel().getSelectedItems();
		List<EpptReportingMonths.MonthPeriod> monthPeriod = _seasonalPeriodListView.getSelectionModel().getSelectedItems();
		if(!guiLink.isEmpty() && !statistic.isEmpty() && !monthPeriod.isEmpty() && !_scenarioRuns.contains(null))
		{

			_progressBar.setVisible(true);
			getScene().setCursor(Cursor.WAIT);
			CompletableFuture.supplyAsync(() -> computeScenarios(guiLink, statistic, monthPeriod))
							 .thenAcceptAsync(jsonObjects ->
							 {
								 for(JSONObject jsonObject : jsonObjects)
								 {
									 _javascriptPane.addDashboardPane(path, "plot(" + jsonObject + ");");
								 }
								 _progressBar.setVisible(false);
								 getScene().setCursor(Cursor.DEFAULT);
							 }, Platform::runLater);
		}
	}

	private List<JSONObject> computeScenarios(List<GUILinksAllModelsBO> guiLinks, List<Statistics> statistics,
											  List<EpptReportingMonths.MonthPeriod> monthPeriods)
	{
		List<JSONObject> retval = new ArrayList<>();
		for(GUILinksAllModelsBO guiLink : guiLinks)
		{
			for(Statistics statistic : statistics)
			{
				for(EpptReportingMonths.MonthPeriod monthPeriod : monthPeriods)
				{
					EpptReportingComputedSet epptReportingComputedSet = computeForMetrics(guiLink, statistic, monthPeriod);
					JSONObject jsonObject = epptReportingComputedSet.toJson();
					LOGGER.log(Level.INFO, jsonObject.toString());
					retval.add(jsonObject);
				}
			}
		}
		return retval;
	}

	private EpptReportingComputedSet computeForMetrics(GUILinksAllModelsBO guiLink, Statistics statistic,
													   EpptReportingMonths.MonthPeriod monthPeriod)
	{
		EpptReportingComputer trendReportingComputer = new EpptReportingComputer(guiLink, statistic, monthPeriod);
		List<EpptReportingComputed> trendReportingComputed = new ArrayList<>();

		for(EpptScenarioRun scenarioRun : _scenarioRuns)
		{
			if(_tafCheckbox.isSelected())
			{
				trendReportingComputed.add(trendReportingComputer.computeTaf(scenarioRun, _startCalendar.getLocalDate(),
						_endCalendar.getLocalDate()));
			}
			else
			{
				trendReportingComputed.add(trendReportingComputer.computeCfs(scenarioRun, _startCalendar.getLocalDate(),
						_endCalendar.getLocalDate()));
			}
		}
		return new EpptReportingComputedSet(guiLink, statistic, monthPeriod,
				_tafCheckbox.isSelected(), trendReportingComputed);
	}

}
