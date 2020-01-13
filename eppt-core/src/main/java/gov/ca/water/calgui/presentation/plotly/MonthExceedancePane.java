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

package gov.ca.water.calgui.presentation.plotly;

import java.nio.file.Paths;
import java.time.LocalDate;
import java.time.Month;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import gov.ca.water.calgui.EpptInitializationException;
import gov.ca.water.calgui.bo.GUILinksAllModelsBO;
import gov.ca.water.calgui.bo.WaterYearIndex;
import gov.ca.water.calgui.busservice.impl.EpptReportingComputedSet;
import gov.ca.water.calgui.busservice.impl.MonthPeriod;
import gov.ca.water.calgui.busservice.impl.NoopEpptStatistic;
import gov.ca.water.calgui.busservice.impl.WaterYearTableReader;
import gov.ca.water.calgui.constant.Constant;
import gov.ca.water.calgui.presentation.JavaFxChartsPane;
import gov.ca.water.calgui.project.EpptScenarioRun;
import gov.ca.water.calgui.project.PlotConfigurationState;
import javafx.application.Platform;
import javafx.embed.swing.JFXPanel;
import javafx.scene.Scene;

public class MonthExceedancePane extends JFXPanel
{
	private static final Logger LOGGER = Logger.getLogger(MonthExceedancePane.class.getName());
	private JavaFxChartsPane _javaFxChartsPane;

	public MonthExceedancePane(Month month, GUILinksAllModelsBO guiLink, List<EpptScenarioRun> scenarioRuns,
							   PlotConfigurationState.ComparisonType comparisonType,
							   LocalDate start, LocalDate end, boolean taf)
	{
		List<WaterYearIndex> waterYearIndices = new ArrayList<>();
		EpptScenarioRun epptScenarioRun = scenarioRuns.get(0);
		try
		{
			waterYearIndices = new WaterYearTableReader(epptScenarioRun.getLookupDirectory()).read();
		}
		catch(EpptInitializationException e)
		{
			LOGGER.log(Level.SEVERE, "Error reading water year table for scenario: " + epptScenarioRun, e);
		}
		EpptReportingComputedSet epptReportingComputedSet;
		if(comparisonType == PlotConfigurationState.ComparisonType.DIFF)
		{
			List<EpptScenarioRun> alternatives = new ArrayList<>(scenarioRuns);
			alternatives.remove(0);
			epptReportingComputedSet = EpptReportingComputedSet.computeDiffForMetrics(guiLink,
					new NoopEpptStatistic(),
					new MonthPeriod(month, month),
					start,
					end,
					taf,
					scenarioRuns.get(0),
					alternatives,
					waterYearIndices.get(0),
					waterYearIndices);
		}
		else
		{
			epptReportingComputedSet = EpptReportingComputedSet.computeForMetrics(guiLink,
					new NoopEpptStatistic(),
					new MonthPeriod(month, month),
					start,
					end,
					taf,
					scenarioRuns,
					waterYearIndices.get(0),
					waterYearIndices);
		}
		Platform.setImplicitExit(false);
		Platform.runLater(() ->
		{
			_javaFxChartsPane = new JavaFxChartsPane(Paths.get(Constant.CONFIG_DIR).resolve("plots").resolve("exceedance_aggregate.htm"),
					"plot(" + epptReportingComputedSet.toJson() + ");");
			setScene(new Scene(_javaFxChartsPane));
		});
	}
}
