/*
 * Enhanced Post Processing Tool (EPPT) Copyright (c) 2020.
 *
 *  EPPT is copyrighted by the State of California, Department of Water Resources. It is licensed
 *  under the GNU General Public License, version 2. This means it can be
 *  copied, distributed, and modified freely, but you may not restrict others
 *  in their ability to copy, distribute, and modify it. See the license below
 *  for more details.
 *
 *  GNU General Public License
 */

package gov.ca.water.calgui.project;

import java.nio.file.Path;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.stream.Collectors;

import gov.ca.water.calgui.bo.WaterYearDefinition;
import gov.ca.water.calgui.bo.WaterYearPeriodRangesFilter;
import gov.ca.water.calgui.busservice.ScenarioChangeListener;
import gov.ca.water.calgui.busservice.impl.EpptStatistic;
import gov.ca.water.calgui.busservice.impl.MonthPeriod;
import gov.ca.water.calgui.busservice.impl.WaterYearDefinitionSvc;
import javafx.beans.property.SimpleBooleanProperty;
import javafx.beans.property.SimpleIntegerProperty;
import javafx.beans.property.SimpleObjectProperty;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 02-25-2020
 */
public class EpptConfigurationController
{
	private static final Logger LOGGER = Logger.getLogger(EpptConfigurationController.class.getName());
	private final Set<ScenarioChangeListener> _scenarioChangeListeners = new HashSet<>();
	private final SimpleBooleanProperty _modifiedProperty = new SimpleBooleanProperty(false);
	private final List<EpptScenarioRun> _scenarioRuns = new ArrayList<>();
	private final List<MonthPeriod> _monthlyPeriods = new ArrayList<>();
	private final List<EpptStatistic> _statistics = new ArrayList<>();
	private final List<Map<EpptScenarioRun, WaterYearPeriodRangesFilter>> _annualPeriodFilters = new ArrayList<>();
	private String _projectName = "Default Project";
	private String _projectDescription = "";
	private SimpleIntegerProperty _startYear = new SimpleIntegerProperty(1921);
	private SimpleIntegerProperty _endYear = new SimpleIntegerProperty(2003);
	private SimpleObjectProperty<WaterYearDefinition> _waterYearDefinition = new SimpleObjectProperty<>(
			WaterYearDefinitionSvc.getWaterYearDefinitionSvc().getDefinitions().get(0));
	private boolean _taf;
	private Path _selectedDssPath;
	private boolean _difference;

	public List<EpptScenarioRun> getScenarioRuns()
	{
		return _scenarioRuns;
	}

	public void setScenarioRuns(List<EpptScenarioRun> scenarioRuns)
	{
		_scenarioRuns.clear();
		_scenarioRuns.addAll(scenarioRuns);
		_scenarioChangeListeners.forEach(this::postScenarioChanged);
	}

	public void addScenarioChangedListener(ScenarioChangeListener scenarioChangeListener)
	{
		_scenarioChangeListeners.add(scenarioChangeListener);
	}

	public boolean isModified()
	{
		return _modifiedProperty.get();
	}

	public void setModified()
	{
		_modifiedProperty.setValue(false);
		_modifiedProperty.setValue(true);
	}

	public SimpleBooleanProperty modifiedProperty()
	{
		return _modifiedProperty;
	}

	public void clearModified()
	{
		_modifiedProperty.setValue(false);
	}

	public void setProjectDescription(String n)
	{
		_projectDescription = n;
	}

	public void setProjectName(String n)
	{
		_projectName = n;
	}

	public String getProjectName()
	{
		return _projectName;
	}

	public String getProjectDescription()
	{
		return _projectDescription;
	}

	private void postScenarioChanged(ScenarioChangeListener scenarioChangeListener)
	{
		Optional<EpptScenarioRun> base = getScenarioRuns().stream().filter(EpptScenarioRun::isBaseSelected).findAny();
		List<EpptScenarioRun> alternatives = getScenarioRuns().stream().filter(EpptScenarioRun::isAltSelected).collect(Collectors.toList());
		if(scenarioChangeListener != null)
		{
			try
			{
				scenarioChangeListener.fillScenarioRuns(base.orElse(null), alternatives);
			}
			catch(RuntimeException e)
			{
				LOGGER.log(Level.SEVERE, "Unable to update scenarios", e);
			}
		}
	}

	public List<EpptScenarioRun> getEpptScenarioAlternatives()
	{
		return getScenarioRuns().stream().filter(EpptScenarioRun::isAltSelected).collect(Collectors.toList());
	}

	public Optional<EpptScenarioRun> getEpptScenarioBase()
	{
		return getScenarioRuns().stream().filter(EpptScenarioRun::isBaseSelected).findAny();
	}

	public int getStartYear()
	{
		return _startYear.get();
	}

	public int getEndYear()
	{
		return _endYear.get();
	}

	public WaterYearDefinition getWaterYearDefinition()
	{
		return _waterYearDefinition.get();
	}

	public List<Map<EpptScenarioRun, WaterYearPeriodRangesFilter>> getWaterYearPeriodRanges()
	{
		return _annualPeriodFilters;
	}

	public boolean isTaf()
	{
		return _taf;
	}

	public List<MonthPeriod> getSelectedMonthlyPeriods()
	{
		return _monthlyPeriods;
	}

	public List<EpptStatistic> getSelectedStatistics()
	{
		return _statistics;
	}

	public void setStartYear(int startYear)
	{
		_startYear.set(startYear);
	}

	public void setEndYear(int endYear)
	{
		_endYear.set(endYear);
	}

	public SimpleIntegerProperty startYearProperty()
	{
		return _startYear;
	}

	public SimpleIntegerProperty endYearProperty()
	{
		return _endYear;
	}

	public Optional<Path> getSelectedDssPath()
	{
		return Optional.ofNullable(_selectedDssPath);
	}

	public void setWaterYearDefinition(WaterYearDefinition waterYearDefinition)
	{
		_waterYearDefinition.set(waterYearDefinition);
	}

	public SimpleObjectProperty<WaterYearDefinition> waterYearDefinitionProperty()
	{
		return _waterYearDefinition;
	}

	public void setMonthlyPeriods(List<MonthPeriod> selectedMonthlyPeriods)
	{
		_monthlyPeriods.clear();
		_monthlyPeriods.addAll(selectedMonthlyPeriods);
	}

	public void setStatistics(List<EpptStatistic> selectedStatistic)
	{
		_statistics.clear();
		_statistics.addAll(selectedStatistic);
	}

	public void setTaf(boolean taf)
	{
		_taf = taf;
	}

	public void setDifference(boolean difference)
	{
		_difference = difference;
	}

	public void setSelectedDssPath(Path dssPath)
	{
		_selectedDssPath = dssPath;
	}

	public boolean isDifference()
	{
		return _difference;
	}

	public void setWaterYearPeriodRangesFilters(List<Map<EpptScenarioRun, WaterYearPeriodRangesFilter>> filters)
	{
		_annualPeriodFilters.clear();
		_annualPeriodFilters.addAll(filters);
	}
}
