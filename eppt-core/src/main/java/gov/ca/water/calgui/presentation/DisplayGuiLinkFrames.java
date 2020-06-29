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

package gov.ca.water.calgui.presentation;

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.TreeMap;
import javax.swing.*;

import gov.ca.water.calgui.bo.GUILinksAllModelsBO;
import gov.ca.water.calgui.busservice.IDSSGrabber1Svc;
import gov.ca.water.calgui.busservice.impl.DSSGrabber1SvcImpl;
import gov.ca.water.calgui.busservice.impl.MonthPeriod;
import gov.ca.water.calgui.project.EpptConfigurationController;
import gov.ca.water.calgui.project.EpptScenarioRun;
import gov.ca.water.calgui.project.PlotConfigurationState;
import gov.ca.water.calgui.techservice.IErrorHandlingSvc;
import gov.ca.water.calgui.techservice.impl.DialogSvcImpl;
import gov.ca.water.calgui.techservice.impl.ErrorHandlingSvcImpl;
import org.apache.log4j.Logger;

import hec.io.TimeSeriesContainer;

import static java.util.stream.Collectors.toList;

/**
 * DisplayFrame class provides a frame for showing charts.
 *
 * @author tslawecki
 */
final class DisplayGuiLinkFrames extends DisplayFrames
{

	private static final Logger LOG = Logger.getLogger(DisplayGuiLinkFrames.class.getName());
	private static final IErrorHandlingSvc ERROR_HANDLING_SVC = new ErrorHandlingSvcImpl();
	private final List<GUILinksAllModelsBO> _guiLinks;

	DisplayGuiLinkFrames(EpptConfigurationController epptConfigurationController, PlotConfigurationState plotConfigurationState,
						 List<GUILinksAllModelsBO> guiLinks)
	{
		super(epptConfigurationController, plotConfigurationState);
		_guiLinks = guiLinks;
	}

	/**
	 * showDisplayFrames method creates a frame showing multiple charts
	 * according to parameters.
	 */
	List<JTabbedPane> showDisplayFrames()
	{
		List<JTabbedPane> tabbedPanes = new ArrayList<>();
		try
		{
			Optional<EpptScenarioRun> baseRun = getBaseRun();
			if(baseRun.isPresent())
			{
				Optional<String> error = getError();
				if(error.isPresent())
				{
					DialogSvcImpl.getDialogSvcInstance().getOK("Unable to generate plots - " + error.get(), JOptionPane.WARNING_MESSAGE);
				}
				else
				{
					IDSSGrabber1Svc dssGrabber = new DSSGrabber1SvcImpl();
					dssGrabber.setIsCFS(!getEpptConfigurationController().isTaf());
					dssGrabber.setScenarioRuns(baseRun.get(), getAlternatives());

					if(_guiLinks.isEmpty())
					{
						String message = "No Location selected.";
						DialogSvcImpl.getDialogSvcInstance().getOK(message, JOptionPane.WARNING_MESSAGE);
					}
					for(GUILinksAllModelsBO guiLink : _guiLinks)
					{
						dssGrabber.setGuiLink(guiLink);
						displayForLocation(tabbedPanes, dssGrabber, guiLink, baseRun.get());
					}
				}
			}
			else
			{
				DialogSvcImpl.getDialogSvcInstance().getOK("Must select Base Scenario", JOptionPane.WARNING_MESSAGE);
			}
		}
		catch(RuntimeException e)
		{
			LOG.error(e.getMessage());
			String messageText = "Unable to display frame.";
			ERROR_HANDLING_SVC.businessErrorHandler(messageText, e);
		}
		return tabbedPanes;
	}

	private void displayForLocation(List<JTabbedPane> tabbedPanes, IDSSGrabber1Svc dssGrabber, GUILinksAllModelsBO guiLink, EpptScenarioRun baseRun)
	{
		if(dssGrabber.getPrimaryDSSName() == null)
		{
			String message = "No GUI_Links3.csv entry found for " + guiLink + "/" + guiLink + ".";
			DialogSvcImpl.getDialogSvcInstance().getOK(message, JOptionPane.WARNING_MESSAGE);
		}
		else if(dssGrabber.getPrimaryDSSName().isEmpty())
		{
			String message = "No DSS time series specified for " + guiLink + "/" + guiLink + ".";
			DialogSvcImpl.getDialogSvcInstance().getOK(message, JOptionPane.WARNING_MESSAGE);
		}
		else
		{
			dssGrabber.setDateRange(getStart(), getEnd());
			JTabbedPane tabbedPane = displayFrameForData(dssGrabber, guiLink, baseRun);
			tabbedPanes.add(tabbedPane);
		}
	}

	private JTabbedPane displayFrameForData(IDSSGrabber1Svc dssGrabber, GUILinksAllModelsBO guiLink, EpptScenarioRun baseRun)
	{
		JTabbedPane tabbedPane = new JTabbedPane();
		Map<GUILinksAllModelsBO.Model, List<String>> missing = dssGrabber.getMissingList();
		List<EpptScenarioRun> scenarioRuns = new ArrayList<>();
		scenarioRuns.add(baseRun);
		scenarioRuns.addAll(getAlternatives());
		Map<EpptScenarioRun, List<TimeSeriesContainer>> primaryScenarioRunData;
		if(getEpptConfigurationController().isDifference())
		{
			primaryScenarioRunData = getDiffData(guiLink, scenarioRuns);
		}
		else
		{
			primaryScenarioRunData = getComparisonData(guiLink, scenarioRuns);
		}
		if(getPlotConfigurationState().isDisplayTimeSeriesAll())
		{
			plotTimeSeriesDiscrete(primaryScenarioRunData, guiLink.getPlotTitle(), tabbedPane);
		}
		if(getPlotConfigurationState().isDisplayTimeSeriesAggregate())
		{
			plotTimeSeriesAggregate(primaryScenarioRunData, guiLink.getPlotTitle(), tabbedPane);
		}
		if(getPlotConfigurationState().isDisplayExceedanceAll())
		{
			plotAllExceedance(primaryScenarioRunData, guiLink.getPlotTitle(), tabbedPane);
		}
		if(getPlotConfigurationState().isDisplayExceedanceAggregate())
		{
			plotAggregateExceedance(primaryScenarioRunData, guiLink.getPlotTitle(), tabbedPane);
		}
		if(getPlotConfigurationState().isDisplayMonthlyLine())
		{
			plotMonthlyLine(primaryScenarioRunData, guiLink.getPlotTitle(), tabbedPane);
		}
		if(getPlotConfigurationState().isDisplayBarCharts())
		{
			plotBarCharts(primaryScenarioRunData, guiLink.getPlotTitle(), tabbedPane);
		}
		if(getPlotConfigurationState().isDisplayBoxAndWhiskerAll())
		{
			plotBoxPlotDiscrete(primaryScenarioRunData, guiLink.getPlotTitle(), tabbedPane);
		}
		if(getPlotConfigurationState().isDisplayBoxAndWhiskerAggregate())
		{
			plotBoxPlotAggregate(primaryScenarioRunData, guiLink.getPlotTitle(), tabbedPane);
		}
		if(getPlotConfigurationState().isDisplayMonthlyTable())
		{
			plotMonthlyTable(primaryScenarioRunData, guiLink.getPlotTitle(), tabbedPane);
		}
		if(getPlotConfigurationState().isDisplaySummaryTable())
		{
			plotSummaryTable(primaryScenarioRunData, guiLink.getPlotTitle(), tabbedPane);
		}
		List<String> collect = missing.values()
									  .stream()
									  .flatMap(Collection::stream)
									  .collect(toList());
		if(!collect.isEmpty())
		{
			insertEmptyTab(tabbedPane, missing);
		}
		String title = guiLink.getPlotTitle();
		tabbedPane.setName(title);
		return tabbedPane;
	}

	private Map<EpptScenarioRun, List<TimeSeriesContainer>> getDiffData(GUILinksAllModelsBO guiLink, List<EpptScenarioRun> scenarioRuns)
	{
		Map<EpptScenarioRun, List<TimeSeriesContainer>> primaryScenarioRunData = new TreeMap<>(Comparator.comparing(scenarioRuns::indexOf));
		Optional<EpptScenarioRun> base = getEpptConfigurationController().getEpptScenarioBase();
		if(base.isPresent())
		{
			DSSGrabber1SvcImpl dssGrabber1Svc = buildDssGrabber(base.get(), guiLink, getEpptConfigurationController().isTaf());
			TimeSeriesContainer basePrimarySeries = dssGrabber1Svc.getPrimarySeries()[0];
			TimeSeriesContainer baseSecondarySeries = dssGrabber1Svc.getSecondarySeries()[0];

			for(EpptScenarioRun epptScenarioRun : scenarioRuns)
			{
				if(!epptScenarioRun.isBaseSelected())
				{
					dssGrabber1Svc = buildDssGrabber(epptScenarioRun, guiLink, getEpptConfigurationController().isTaf());
					TimeSeriesContainer primarySeries = DSSGrabber1SvcImpl.diffSeries(basePrimarySeries, dssGrabber1Svc.getPrimarySeries()[0]);
					TimeSeriesContainer secondarySeries = DSSGrabber1SvcImpl.diffSeries(baseSecondarySeries, dssGrabber1Svc.getSecondarySeries()[0]);
					List<TimeSeriesContainer> tscList = new ArrayList<>();
					if(primarySeries != null)
					{
						tscList.add(primarySeries);
					}
					if(secondarySeries != null)
					{
						secondarySeries.setFullName(guiLink.getLegend());
						tscList.add(secondarySeries);
					}
					primaryScenarioRunData.put(epptScenarioRun, tscList);
				}
			}
		}
		return primaryScenarioRunData;
	}

	private Map<EpptScenarioRun, List<TimeSeriesContainer>> getComparisonData(GUILinksAllModelsBO guiLink, List<EpptScenarioRun> scenarioRuns)
	{
		Map<EpptScenarioRun, List<TimeSeriesContainer>> primaryScenarioRunData = new TreeMap<>(Comparator.comparing(scenarioRuns::indexOf));
		for(EpptScenarioRun epptScenarioRun : scenarioRuns)
		{
			DSSGrabber1SvcImpl dssGrabber1Svc = buildDssGrabber(epptScenarioRun, guiLink, getEpptConfigurationController().isTaf());
			TimeSeriesContainer primarySeries = dssGrabber1Svc.getPrimarySeries()[0];
			TimeSeriesContainer secondarySeries = dssGrabber1Svc.getSecondarySeries()[0];
			List<TimeSeriesContainer> tscList = new ArrayList<>();
			if(primarySeries != null)
			{
				tscList.add(primarySeries);
			}
			if(secondarySeries != null)
			{
				secondarySeries.setFullName(guiLink.getLegend());
				tscList.add(secondarySeries);
			}
			primaryScenarioRunData.put(epptScenarioRun, tscList);
		}
		return primaryScenarioRunData;
	}

	private DSSGrabber1SvcImpl buildDssGrabber(EpptScenarioRun epptScenarioRun, GUILinksAllModelsBO guiLink, boolean isCFS)
	{
		DSSGrabber1SvcImpl dssGrabber = new DSSGrabber1SvcImpl();
		dssGrabber.setIsCFS(isCFS);
		dssGrabber.setScenarioRuns(epptScenarioRun, Collections.emptyList());
		dssGrabber.setGuiLink(guiLink);
		dssGrabber.setDateRange(getStart(), getEnd());
		return dssGrabber;
	}

}
