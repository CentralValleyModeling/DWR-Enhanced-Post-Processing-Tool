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

package gov.ca.water.calgui.busservice;

import java.time.LocalDate;
import java.util.List;
import java.util.Map;

import gov.ca.water.calgui.bo.DetailedIssue;
import gov.ca.water.calgui.bo.GUILinksAllModelsBO;
import gov.ca.water.calgui.bo.ThresholdLinksBO;
import gov.ca.water.calgui.project.EpptScenarioRun;

import hec.io.TimeSeriesContainer;

public interface IDSSGrabber1Svc
{

	/**
	 * Sets the isCFS flag.
	 *
	 * @param isCFS set to True if flows are to be displayed in CFS, False for
	 *              TAFY.
	 */
	void setIsCFS(boolean isCFS);

	/**
	 * Sets the date range. Results are trimmed to the date range when read in
	 * GetOneSeries method.
	 *
	 */
	void setDateRange(LocalDate start, LocalDate end);

	/**
	 * Sets base scenario for calculations and display. The base scenario is
	 * listed first in legends and used as the basis for difference
	 * calculations.
	 *
	 * @param scenarioRun scenario to use as base.
	 */
	void setScenarioRuns(EpptScenarioRun scenarioRun, List<EpptScenarioRun> alternatives);

	void setDtsLink(DetailedIssue dtsLink);

	void setGuiLink(GUILinksAllModelsBO guiLinksAllModelsBO);

	void setThresholdLink(ThresholdLinksBO objById);

	/**
	 * Reads the DSS results for the primary series for each scenario. Also
	 * stores for reference the units of measure for the primary series in the
	 * private variable originalUnits.
	 *
	 * @return Array of HEC TimeSeriesContainer - one TSC for each scenario
	 */
	TimeSeriesContainer[] getPrimarySeries();

	/**
	 * Reads the DSS results for the secondary series for each scenario.
	 *
	 * @return Array of HEC TimeSeriesContainer - one TSC for each scenario
	 */
	TimeSeriesContainer[] getSecondarySeries();

	String getOriginalUnits();

	Map<GUILinksAllModelsBO.Model, String> getPrimaryDSSName();

	/**
	 * Clears list of DSS records that were not found in scenario DV.DSS files
	 */
	void clearMissingList();

	/**
	 * Provide access to list of DSS records not found during processing
	 *
	 * @return list, or null if not tracked due to property setting
	 */
	Map<GUILinksAllModelsBO.Model, List<String>> getMissingList();

}
