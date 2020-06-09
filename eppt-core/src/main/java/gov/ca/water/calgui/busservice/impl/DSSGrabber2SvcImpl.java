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

package gov.ca.water.calgui.busservice.impl;

//! Variant on DSSGrabber1BO for MTS (multiple time series)

import java.util.Arrays;
import java.util.Calendar;
import java.util.Objects;
import java.util.Optional;
import java.util.Vector;
import java.util.logging.Level;
import java.util.logging.Logger;

import calsim.app.AppUtils;
import calsim.app.DerivedTimeSeries;
import calsim.app.MultipleTimeSeries;
import gov.ca.water.calgui.bo.GUILinksAllModelsBO;
import gov.ca.water.calgui.bo.ResultUtilsBO;
import gov.ca.water.calgui.busservice.IGuiLinksSeedDataSvc;
import gov.ca.water.calgui.project.EpptScenarioRun;
import gov.ca.water.calgui.project.NamedDssPath;

import hec.heclib.util.HecTime;
import hec.io.TimeSeriesContainer;

/**
 * Class to grab (generate) DSS time series BASED ON DTS list for a set of
 * scenarios passed in a JList. Each scenario is a string that corresponds to a
 * fully qualified (?) DSS file name. The DSS_Grabber instance provides access
 * to the following:
 * <ul>
 * <li>Main time series (result, including necessary math) for each scenario<br>
 * <li>Secondary series (control) where indicated for each scenario<br>
 * <li>Difference for main time series for each scenario<br>
 * <li>Exceedance for main time series for each scenario<br>
 * </ul>
 * Typical usage sequence:
 * <ul>
 * <li>DSS_Grabber</li>
 * <li>setIsCFS</li>
 * <li>setScenarioRuns</li>
 * <li>setLocation</li>
 * <li>setDateRange</li>
 * <li>getPrimarySeries</li>
 * <li>getSecondarySeries</li>
 * <li>Other calculations</li>
 * </ul>
 */
public class DSSGrabber2SvcImpl extends DSSGrabber1SvcImpl
{
	private static final Logger LOGGER = Logger.getLogger(DSSGrabber2SvcImpl.class.getName());
	private final DerivedTimeSeries _dts;
	private final MultipleTimeSeries _mts;
	private final IGuiLinksSeedDataSvc _seedDataSvc = GuiLinksSeedDataSvcImpl.getSeedDataSvcImplInstance();
	private double[][][] _annualTAFs;
	private double[][][] _annualTAFsDiff;

	public DSSGrabber2SvcImpl(DerivedTimeSeries dts, MultipleTimeSeries mts)
	{
		this._dts = dts;
		this._mts = mts;
	}

	/**
	 * Sets dataset (DSS) names to read from scenario DSS files, title, and axis
	 * labels according to location specified using a coded string. The string
	 * is currently used as a lookup into either Schematic_DSS_Links4.table (if
	 * it starts with Constant.SCHEMATIC_PREFIX) or into GUI_Links3.table. These
	 * tables may be combined in Phase 2.
	 *
	 * @param locationName index into GUI_Links3.table or Schematic_DSS_Link4.table
	 */
	public void setLocation(String locationName)
	{

		try
		{
			// TODO: Combine lookup tables AND review use of complex names
			locationName = locationName.trim();

			if(locationName.startsWith("@@"))
			{
				// @@ indicates MTS/DTS title
				locationName = locationName.substring(2);
				_primaryDSSName.clear();
				_primaryDSSName.put(_baseScenarioRun.getModel(), locationName);
				_secondaryDSSName.clear();
				_axisLabel = "";
				_legend = "";
				_plotTitle = locationName;
			}
			else if(locationName.startsWith("/"))
			{
				// Handle names passed from WRIMS GUI
				String[] parts = locationName.split("/");
				_plotTitle = locationName;
				_primaryDSSName.clear();
				_primaryDSSName.put(_baseScenarioRun.getModel(), parts[2] + "/" + parts[3]);
				_secondaryDSSName.clear();
				_axisLabel = "";
				_legend = "";
			}
			else
			{
				// Location name is otherwise assumed coded as "ckpbxxx"

				GUILinksAllModelsBO guiLinksAllModelsBO = _seedDataSvc.getGuiLink(locationName);
				if(guiLinksAllModelsBO != null)
				{
					_primaryDSSName.clear();
					_primaryDSSName.putAll(guiLinksAllModelsBO.getPrimary());
					_secondaryDSSName.clear();
					_secondaryDSSName.putAll(guiLinksAllModelsBO.getSecondary());
					_axisLabel = guiLinksAllModelsBO.getPlotAxisLabel();
					_plotTitle = guiLinksAllModelsBO.getPlotTitle();
					_legend = guiLinksAllModelsBO.getLegend();
				}
			}
		}
		catch(Exception ex)
		{
			LOGGER.log(Level.SEVERE, "Unable to set location.", ex);
		}
	}

	/**
	 * Reads the DSS results for the primary series for each scenario. Also
	 * stores for reference the units of measure for the primary series in the
	 * private variable originalUnits.
	 *
	 * @return Array of HEC TimeSeriesContainer - one TSC for each scenario
	 */
	public TimeSeriesContainer[] getPrimarySeries()
	{

		try
		{
			TimeSeriesContainer[] results;
			checkReadiness();

			// Store number of scenarios

			results = new TimeSeriesContainer[_alternatives.size() + 1];

			// Base first
			Optional<TimeSeriesContainer> dtsContainer = getDtsContainer(_baseScenarioRun);
			if(dtsContainer.isPresent())
			{
				results[0] = dtsContainer.get();
			}
			if(results != null && results[0] != null)
			{
				_originalUnits = results[0].units;

				// Then scenarios

				for(int i = 0; i < _alternatives.size(); i++)
				{
					dtsContainer = getDtsContainer(_alternatives.get(i));
					if(dtsContainer.isPresent())
					{
						results[i + 1] = dtsContainer.get();
					}
				}

			}
			return results;
		}
		catch(Exception ex)
		{
			LOGGER.log(Level.SEVERE, "Unable to get time-series.", ex);
		}
		return new TimeSeriesContainer[1];
	}

	public TimeSeriesContainer[] getMultipleTimeSeries(int mtsI)
	{

		try
		{
			checkReadiness();

			// Store number of scenarios
			TimeSeriesContainer[] results = new TimeSeriesContainer[_alternatives.size() + 1];

			// Base first

			Optional<TimeSeriesContainer> tsOpt = getMtsContainer(_baseScenarioRun, mtsI);
			if(tsOpt.isPresent())
			{
				results[0] = tsOpt.get();
			}
			if(results[0] != null)
			{
				_originalUnits = results[0].units;
			}

			// Then scenarios

			for(int i = 0; i < _alternatives.size(); i++)
			{
				EpptScenarioRun epptScenarioRun = _alternatives.get(i);
				tsOpt = getMtsContainer(epptScenarioRun, mtsI);
				if(tsOpt.isPresent())
				{
					results[i + 1] = tsOpt.get();
				}
			}

			return results;
		}
		catch(Exception ex)
		{
			LOGGER.log(Level.SEVERE, "Unable to get time-series.", ex);
		}
		return new TimeSeriesContainer[1];
	}

	private Optional<TimeSeriesContainer> getMtsContainer(EpptScenarioRun epptScenarioRun, int mtsI)
	{
		return epptScenarioRun.getDssContainer()
							  .getAllDssFiles()
							  .stream()
							  .map(p -> getOneSeriesWRIMS(p,
									  epptScenarioRun.getModel(),
									  mtsI, _mts))
							  .filter(Objects::nonNull)
							  .findFirst();
	}


	private Optional<TimeSeriesContainer> getDtsContainer(EpptScenarioRun epptScenarioRun)
	{
		return epptScenarioRun.getDssContainer()
							  .getAllDssFiles()
							  .stream()
							  .map(p -> getOneSeriesWRIMS(p,
									  epptScenarioRun.getModel(),
									  _dts))
							  .filter(Objects::nonNull)
							  .findFirst();
	}

	private TimeSeriesContainer getOneSeriesWRIMS(NamedDssPath dssPath, GUILinksAllModelsBO.Model model,
												  DerivedTimeSeries dts2)
	{

		try
		{
			Vector<?> dtsNames = dts2.getDtsNames();

			TimeSeriesContainer result = null;
			boolean first = true;
			for(int i = 0; i < dts2.getNumberOfDataReferences(); i++)
			{
				TimeSeriesContainer interimResult = null;
				if(!((String) dtsNames.get(i)).isEmpty())
				{
					// Operand is reference to another DTS
					DerivedTimeSeries adt = ResultUtilsBO.getResultUtilsInstance().getProject()
														 .getDTS((String) dtsNames.get(i));
					interimResult = getOneSeriesWRIMS(dssPath, model, adt);
				}
				else
				{
					// Operand is a DSS time series
					_primaryDSSName.clear();
					_primaryDSSName.put(model, dts2.getBPartAt(i) + "/" + dts2.getCPartAt(i));
					interimResult = getOneSeries(dssPath, (dts2.getBPartAt(i) + "/" + dts2.getCPartAt(i)), model);
				}
				if(interimResult != null)
				{
					if(first)
					{

						// First time through, copy Interim result into result
						result = interimResult;
						if(dts2.getOperationIdAt(i) < 1)

						// Iff operation is "?", treat as a control and
						// convert to on/off
						{
							for(int j = 0; j < interimResult.numberValues; j++)
							{
								result.values[j] = (result.values[j] > 0.1) ? 9876.5 : 0;
							}
						}
						first = false;
					}
					else
					{
						result.location += ' ' + AppUtils.getOperationName(dts2.getOperationIdAt(i)) + " " + interimResult.location;
						result.parameter += ' ' + AppUtils.getOperationName(dts2.getOperationIdAt(i)) + " "  + interimResult.parameter;
						result.version += ' ' + AppUtils.getOperationName(dts2.getOperationIdAt(i)) + " "  + interimResult.version;
						switch(dts2.getOperationIdAt(i))
						{
							case 0:

								// Iff operation is "?", treat as a control

								for(int j = 0; j < interimResult.numberValues; j++)
								{
									if(result.values.length < j && interimResult.values.length < j)
									{
										result.values[j] = ((result.values[j] > 0.1) && (interimResult.values[j] > 0.1))
												? 9876.5 : 0;
									}
								}
								break;

							case 1:
								for(int j = 0; j < interimResult.numberValues; j++)
								{
									if(j < result.values.length  && j < interimResult.values.length)
									{
										result.values[j] = result.values[j] + interimResult.values[j];
									}
								}
								break;

							case 2:
								for(int j = 0; j < interimResult.numberValues; j++)
								{
									if(j < result.values.length  && j < interimResult.values.length)
									{
										result.values[j] = result.values[j] - interimResult.values[j];
									}
								}
								break;

							case 3:
								for(int j = 0; j < interimResult.numberValues; j++)
								{
									if(j < result.values.length  && j < interimResult.values.length)
									{
										result.values[j] = result.values[j] * interimResult.values[j];
									}
								}
								break;

							case 4:
								for(int j = 0; j < interimResult.numberValues; j++)
								{
									if(j < result.values.length  && j < interimResult.values.length)
									{
										result.values[j] = result.values[j] / interimResult.values[j];
									}
								}
								break;

							default:
								break;

						}
					}
				}
			}
			if(result != null)
			{
				result.setFullName(dts2.getName());
			}
			return result;
		}
		catch(Exception ex)
		{
			LOGGER.log(Level.SEVERE, "Unable to get time-series.", ex);
		}
		return null;

	}

	private TimeSeriesContainer getOneSeriesWRIMS(NamedDssPath dssPath, GUILinksAllModelsBO.Model model,
												  int i, MultipleTimeSeries mts2)
	{

		try
		{
			TimeSeriesContainer result = null;
			if(!mts2.getDTSNameAt(i).isEmpty())
			{
				// Operand is reference to a DTS
				DerivedTimeSeries adt = ResultUtilsBO.getResultUtilsInstance().getProject()
													 .getDTS(_mts.getDTSNameAt(i));
				result = getOneSeriesWRIMS(dssPath, model, adt);
				_primaryDSSName.clear();
				_primaryDSSName.put(model, _mts.getDTSNameAt(i));

			}
			else
			{
				// Operand is a DSS time series
				_primaryDSSName.clear();
				_primaryDSSName.put(model, mts2.getBPartAt(i) + "//" + mts2.getCPartAt(i));
				result = getOneSeries(dssPath, (mts2.getBPartAt(i) + "/" + mts2.getCPartAt(i)), model);
			}
			return result;
		}
		catch(Exception ex)
		{
			LOGGER.log(Level.SEVERE, "Unable to get time series from.", ex);
		}
		return null;
	}
}
