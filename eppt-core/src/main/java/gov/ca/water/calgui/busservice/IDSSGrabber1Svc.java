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

import gov.ca.water.calgui.bo.GUILinksAllModelsBO;
import gov.ca.water.calgui.project.EpptScenarioRun;
import org.jfree.data.time.Month;

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
	 * Gets name part of base scenario file in baseName.
	 *
	 * @return name of the base scenario file WITHOUT the file extension
	 */
	String getBaseRunName();

	/**
	 * Sets base scenario for calculations and display. The base scenario is
	 * listed first in legends and used as the basis for difference
	 * calculations.
	 *
	 * @param scenarioRun scenario to use as base.
	 */
	void setScenarioRuns(EpptScenarioRun scenarioRun, List<EpptScenarioRun> alternatives);

	/**
	 * Sets dataset (DSS) names to read from scenario DSS files, title, and axis
	 * labels according to location specified using a coded string. The string
	 * is currently used as a lookup into either Schematic_DSS_Links4.table (if
	 * it starts with Constant.SCHEMATIC_PREFIX) or into GUI_Links3.table. These
	 * tables may be combined in Phase 2.
	 *
	 * @param locationName
	 */
	void setLocation(String locationName);

	/**
	 * Gets primary y-axis label assigned by DSS_Grabber to the results read in
	 * from DSS file.
	 *
	 * @return string containing primary y-axis label
	 */
	String getYLabel();

	/**
	 * Gets secondary y-axis label assigned by DSS_Grabber to the results read
	 * in from DSS file.
	 *
	 * @return string containing secondary y-axis label
	 */
	String getSLabel();

	/**
	 * Gets chart/table title assigned by DSS_Grabber to the results read in
	 * from DSS file. If no title is set, the name of the primary DSS dataset is
	 * returned instad.,
	 *
	 * @return string containing title
	 */
	String getPlotTitle();

	/**
	 * Checks if a DSS file has records with "HYDROPOWER" as the A-PART
	 *
	 * @param dssFilename
	 * @return
	 */
	boolean hasPower(String dssFilename);

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

	/**
	 * Calculates the difference between scenarios and the base for a set of DSS
	 * results.
	 *
	 * @param timeSeriesResults array of HEC TimeSeriesContainer objects, each representing a
	 *                          set of results for a scenario. Base is in position [0].
	 * @return array of HEC TimeSeriesContainer objects (size one less than
	 * timeSeriesResult. Position [0] contains difference [1]-[0],
	 * position [1] contains difference [2]-[0], ...
	 */
	TimeSeriesContainer[] getDifferenceSeries(TimeSeriesContainer[] timeSeriesResults);

	/**
	 * Calculates annual volume in TAF for any CFS dataset, and replaces monthly
	 * values if TAF flag is checked.
	 *
	 * @param primaryResults
	 * @param secondaryResults
	 */
	void calcTAFforCFS(TimeSeriesContainer[] primaryResults, TimeSeriesContainer[] secondaryResults);

	double getAnnualTAF(int i, int wy);

	double getAnnualTAFDiff(int i, int wy);

	/**
	 * Generates exceedance time series from monthly DSS results.
	 *
	 * @param timeSeriesResults array of HEC TimeSeriesContainer objects, each representing a
	 *                          set of results for a scenario.
	 * @return array of HEC TimeSeriesContainer objects - 14 for each input.
	 * Exceedances for all values are in [index=0], for each month's
	 * values [1..12], and [13] for annual totals [Index=13]
	 */
	TimeSeriesContainer[][] getExceedanceSeries(TimeSeriesContainer[] timeSeriesResults);

	/**
	 * Generated exceedance for difference of time series
	 *
	 * @param timeSeriesResults
	 * @return
	 */
	TimeSeriesContainer[][] getExceedanceSeriesD(TimeSeriesContainer[] timeSeriesResults);

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
