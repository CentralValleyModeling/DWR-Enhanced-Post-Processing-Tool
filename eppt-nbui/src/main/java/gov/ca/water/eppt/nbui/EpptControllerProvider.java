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

package gov.ca.water.eppt.nbui;

import java.io.IOException;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import gov.ca.water.calgui.constant.EpptPreferences;
import gov.ca.water.calgui.project.EpptProject;
import gov.ca.water.eppt.nbui.projectconfig.ProjectConfigurationIO;
import gov.ca.water.calgui.project.EpptConfigurationController;
import gov.ca.water.quickresults.ui.projectconfig.scenariotable.ScenarioProjectUpdater;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 02-26-2020
 */
public final class EpptControllerProvider
{
	private static final Logger LOGGER = Logger.getLogger(EpptControllerProvider.class.getName());
	private static final List<Runnable> LISTENERS = new ArrayList<>();

	private EpptControllerProvider()
	{
		throw new AssertionError("Utility class");
	}
	private static EpptConfigurationController epptConfigurationController;

	public static void setEpptController(Path path)
	{
		if(EpptControllerProvider.epptConfigurationController == null)
		{

			EpptControllerProvider.epptConfigurationController = new EpptConfigurationController();
		}
		try
		{
			if(path.toFile().exists())
			{
				EpptControllerProvider.epptConfigurationController.setStatistics(new ArrayList<>());
				EpptControllerProvider.epptConfigurationController.setMonthlyPeriods(new ArrayList<>());
				EpptControllerProvider.epptConfigurationController.setWaterYearPeriodRangesFilters(new ArrayList<>());
				ProjectConfigurationIO projectConfigurationIO = new ProjectConfigurationIO();
				EpptProject epptProject = projectConfigurationIO.loadConfiguration(path, epptConfigurationController);
				EpptControllerProvider.epptConfigurationController.setProjectName(epptProject.getName());
				EpptControllerProvider.epptConfigurationController.setProjectDescription(epptProject.getDescription());
				EpptControllerProvider.epptConfigurationController.setScenarioRuns(epptProject.getScenarioRuns());
				EpptControllerProvider.epptConfigurationController.setStartYear(epptProject.getStartYear());
				EpptControllerProvider.epptConfigurationController.setEndYear(epptProject.getEndYear());
				ScenarioProjectUpdater.updateWithAllDssFiles(epptProject.getScenarioRuns());
				EpptPreferences.setLastProjectConfiguration(path);
			}
		}
		catch(IOException | RuntimeException ex)
		{
			LOGGER.log(Level.SEVERE,
					"Unable to load last Project Configuration EPPT Home: " + EpptPreferences.getLastProjectConfiguration(), ex);
		}
		LISTENERS.forEach(Runnable::run);
	}

	public static EpptConfigurationController getEpptConfigurationController()
	{
		return epptConfigurationController;
	}

	public static void addListener(Runnable listener)
	{
		LISTENERS.add(listener);
	}
}
