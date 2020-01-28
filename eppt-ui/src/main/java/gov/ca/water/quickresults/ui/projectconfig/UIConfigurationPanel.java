/*
 * Enhanced Post Processing Tool (EPPT) Copyright (c) 2020.
 *
 * EPPT is copyrighted by the State of California, Department of Water Resources. It is licensed
 * under the GNU General Public License, version 2. This means it can be
 * copied, distributed, and modified freely, but you may not restrict others
 * in their ability to copy, distribute, and modify it. See the license below
 * for more details.
 *
 * GNU General Public License
 */

package gov.ca.water.quickresults.ui.projectconfig;

import java.awt.Dimension;
import java.time.Month;
import java.time.format.TextStyle;
import java.util.Arrays;
import java.util.Locale;
import java.util.stream.IntStream;

import gov.ca.water.quickresults.ui.trendreporting.TrendReportPeriodPane;
import gov.ca.water.quickresults.ui.trendreporting.TrendReportStatisticsPane;
import javafx.application.Platform;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.embed.swing.JFXPanel;
import javafx.scene.Scene;
import javafx.scene.control.Accordion;
import javafx.scene.control.CheckBox;
import javafx.scene.control.Label;
import javafx.scene.control.ListView;
import javafx.scene.control.Spinner;
import javafx.scene.control.TitledPane;
import javafx.scene.layout.GridPane;

import static java.util.stream.Collectors.toList;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 01-27-2020
 */
class UIConfigurationPanel extends JFXPanel
{
	UIConfigurationPanel()
	{
		setPreferredSize(new Dimension(600, 600));
		Platform.setImplicitExit(false);
		Platform.runLater(this::init);
	}

	private void init()
	{
		GridPane outerPane = new GridPane();
		TitledPane globalControlsTitle = buildGlobalControls();
		TitledPane quickCustomControls = buildQuickCustomControls();
		outerPane.add(globalControlsTitle, 0, 0);
		outerPane.add(quickCustomControls, 0, 1);
		setScene(new Scene(outerPane));
	}

	private TitledPane buildQuickCustomControls()
	{
		TitledPane quickCustomTitle = new TitledPane();
		quickCustomTitle.setText("Quick/Custom Results");
		GridPane outerGrid = new GridPane();
		outerGrid.setVgap(5);
		GridPane visibilityGrid = new GridPane();
		visibilityGrid.setHgap(5);
		visibilityGrid.setVgap(5);
		visibilityGrid.add(new CheckBox("Display Scenario Difference"), 0, 0);
		visibilityGrid.add(new CheckBox("Display Time Series Plot"), 1, 0);
		visibilityGrid.add(new CheckBox("Display Monthly Table"), 0, 1);
		visibilityGrid.add(new CheckBox("Display Summary Table"), 1, 1);
		outerGrid.add(visibilityGrid, 0, 0);
		TitledPane exceedancePlots = buildExceedancePane();
		TitledPane summaryPane = buildSummaryPane();
		Accordion accordion = new Accordion(exceedancePlots, summaryPane);
		outerGrid.add(accordion, 0, 1);
		quickCustomTitle.setContent(outerGrid);
		return quickCustomTitle;
	}

	private TitledPane buildExceedancePane()
	{
		TitledPane exceedanceTitle = new TitledPane();
		exceedanceTitle.setGraphic(new CheckBox("Exceedance Plots"));
		ListView<String> exceedances = new ListView<>();
		ObservableList<String> months = FXCollections.observableList(
				Arrays.stream(Month.values()).map(m -> m.getDisplayName(TextStyle.SHORT, Locale.getDefault())).collect(toList()));
		months.add("ALL");
		months.add("Annual Flow");
		exceedances.setItems(months);
		exceedanceTitle.setContent(exceedances);
		exceedances.setPrefHeight(225);
		return exceedanceTitle;
	}

	private TitledPane buildSummaryPane()
	{
		TitledPane exceedanceTitle = new TitledPane();
		exceedanceTitle.setGraphic(new CheckBox("Summary Table"));
		Accordion accordion = new Accordion(new TrendReportStatisticsPane(), new TrendReportPeriodPane());
		exceedanceTitle.setContent(accordion);
		return exceedanceTitle;
	}

	private TitledPane buildGlobalControls()
	{
		TitledPane globalControlsTitle = new TitledPane();
		globalControlsTitle.setText("Global Controls");
		GridPane globalControls = new GridPane();
		globalControls.setHgap(5);
		globalControls.setVgap(5);
		globalControls.add(new Label("Month"), 1, 0);
		globalControls.add(new Label("Year"), 2, 0);
		globalControls.add(new Label("Start: "), 0, 1);
		globalControls.add(new Label("End: "), 0, 2);
		ObservableList<Month> months = FXCollections.observableList(Arrays.asList(Month.values()));
		ObservableList<Integer> years = FXCollections.observableList(IntStream.iterate(1990, i -> i + 1)
																			  .limit(150)
																			  .boxed()
																			  .collect(toList()));
		Spinner<Month> startMonthSpinner = new Spinner<>(months);
		globalControls.add(startMonthSpinner, 1, 1);
		Spinner<Month> endMonthSpinner = new Spinner<>(months);
		globalControls.add(endMonthSpinner, 1, 2);
		Spinner<Integer> startYearSpinner = new Spinner<>(years);
		globalControls.add(startYearSpinner, 2, 1);
		Spinner<Integer> endYearSpinner = new Spinner<>(years);
		globalControls.add(endYearSpinner, 2, 2);
		globalControlsTitle.setContent(globalControls);
		return globalControlsTitle;
	}
}
