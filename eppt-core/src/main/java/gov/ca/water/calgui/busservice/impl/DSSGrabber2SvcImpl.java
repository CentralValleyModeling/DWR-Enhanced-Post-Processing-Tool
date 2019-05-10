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
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.Vector;
import java.util.logging.Level;
import java.util.logging.Logger;

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
	@Override
	public void setLocation(String locationName, GUILinksAllModelsBO.Model model)
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
				_primaryDSSName.put(model, locationName);
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
				_primaryDSSName.put(model, parts[2] + "/" + parts[3]);
				_secondaryDSSName.clear();
				_axisLabel = "";
				_legend = "";
			}
			else
			{
				// Location name is otherwise assumed coded as "ckpbxxx"

				GUILinksAllModelsBO guiLinksAllModelsBO = _seedDataSvc.getObjById(locationName);
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

			return results;
		}
		catch(Exception ex)
		{
			LOGGER.log(Level.SEVERE, "Unable to get time-series.", ex);
		}
		return null;
	}

	public TimeSeriesContainer[] getMultipleTimeSeries(int mtsI)
	{

		try
		{
			TimeSeriesContainer[] results = null;
			checkReadiness();

			// Store number of scenarios

			results = new TimeSeriesContainer[_alternatives.size() + 1];

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
		return null;
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
					if("DVAR".equals(dts2.getVarTypeAt(i)))
					{
						interimResult = getOneSeries(dssPath,
								(dts2.getBPartAt(i) + "/" + dts2.getCPartAt(i) + "/LOOKUP"), model);
					}
					else
					{
						//						String svFilename = "";

						//						if(dssFilename.equals(_project.getDVFile()))
						//						{
						//							svFilename = _project.getSVFile();
						//						}
						//						else if(dssFilename.equals(_project.getDV2File()))
						//						{
						//							svFilename = _project.getSV2File();
						//						}
						//						else if(dssFilename.equals(_project.getDV3File()))
						//						{
						//							svFilename = _project.getSV3File();
						//						}
						//						else if(dssFilename.equals(_project.getDV4File()))
						//						{
						//							svFilename = _project.getSV4File();
						//						}

						//						interimResult = getOneSeries(svFilename,
						//								(dts2.getBPartAt(i) + "/" + dts2.getCPartAt(i) + "/LOOKUP"), model);
					}
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
						switch(dts2.getOperationIdAt(i))
						{

							case 0:

								// Iff operation is "?", treat as a control

								for(int j = 0; j < interimResult.numberValues; j++)
								{
									result.values[j] = ((result.values[j] > 0.1) && (interimResult.values[j] > 0.1))
											? 9876.5 : 0;
								}
								break;

							case 1:
								for(int j = 0; j < interimResult.numberValues; j++)
								{
									result.values[j] = result.values[j] + interimResult.values[j];
								}
								break;

							case 2:
								for(int j = 0; j < interimResult.numberValues; j++)
								{
									result.values[j] = result.values[j] - interimResult.values[j];
								}
								break;

							case 3:
								for(int j = 0; j < interimResult.numberValues; j++)
								{
									result.values[j] = result.values[j] * interimResult.values[j];
								}
								break;

							case 4:
								for(int j = 0; j < interimResult.numberValues; j++)
								{
									result.values[j] = result.values[j] / interimResult.values[j];
								}
								break;

							default:
								break;

						}
					}
				}
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
				if("DVAR".equals(mts2.getVarTypeAt(i)))
				{
					result = getOneSeries(dssPath, (mts2.getBPartAt(i) + "/" + mts2.getCPartAt(i)), model);
				}
				else
				{
					//					String svFilename = "";
					//
					//					if(dssFilename.equals(_project.getDVFile()))
					//					{
					//						svFilename = _project.getSVFile();
					//					}
					//					else if(dssFilename.equals(_project.getDV2File()))
					//					{
					//						svFilename = _project.getSV2File();
					//					}
					//					else if(dssFilename.equals(_project.getDV3File()))
					//					{
					//						svFilename = _project.getSV3File();
					//					}
					//					else if(dssFilename.equals(_project.getDV4File()))
					//					{
					//						svFilename = _project.getSV4File();
					//					}
					//
					//					result = getOneSeries(svFilename, (mts2.getBPartAt(i) + "/" + mts2.getCPartAt(i)), model);
				}
			}
			return result;
		}
		catch(Exception ex)
		{
			LOGGER.log(Level.SEVERE, "Unable to get time series from.", ex);
		}
		return null;
	}

	/**
	 * Variant of getDifferenceSeriesWithMultipleTimeSeries to work with MTS (multiple time series)
	 *
	 * @param timeSeriesResults array of arrays of HEC TimeSeriesContainer objects, each
	 *                          representing a set of results for a scenario. Base is in
	 *                          position [0].
	 * @return array of arrays of HEC TimeSeriesContainer objects (size one less
	 * than timeSeriesResult. Position [0] contains difference [1]-[0],
	 * position [1] contains difference [2]-[0], ...
	 */
	public TimeSeriesContainer[][] getDifferenceSeriesWithMultipleTimeSeries(TimeSeriesContainer[][] timeSeriesResults)
	{

		try
		{
			TimeSeriesContainer[][] results = new TimeSeriesContainer[timeSeriesResults.length][_alternatives.size()];

			for(int tsi = 0; tsi < timeSeriesResults.length; tsi++)
			{

				for(int i = 0; i < _alternatives.size(); i++)
				{

					results[tsi][i] = (TimeSeriesContainer) timeSeriesResults[tsi][i + 1].clone();
					for(int j = 0; j < results[tsi][i].numberValues; j++)
					{
						results[tsi][i].values[j] = results[tsi][i].values[j] - timeSeriesResults[tsi][0].values[j];
					}
				}
			}
			return results;
		}
		catch(RuntimeException ex)
		{
			LOGGER.log(Level.SEVERE, "Unable to get time-series.", ex);
		}
		return timeSeriesResults;
	}

	/**
	 * Variant of CalcTAFforCFS to work with multiple time series
	 *
	 * @param primaryResults
	 */
	public void calcTAFforCFS(TimeSeriesContainer[][] primaryResults)
	{

		try
		{
			// Allocate and zero out

			int datasets = primaryResults.length;
			int scenarios = primaryResults[0].length;

			_annualTAFs = new double[datasets][scenarios][_endWY - _startWY + 2];

			for(int mtsi = 0; mtsi < datasets; mtsi++)
			{
				for(int i = 0; i < scenarios; i++)
				{
					for(int j = 0; j < _endWY - _startWY + 1; j++)
					{
						_annualTAFs[mtsi][i][j] = 0.0;
					}
				}
			}

			// Calculate

			if("CFS".equals(_originalUnits))
			{

				HecTime ht = new HecTime();
				Calendar calendar = Calendar.getInstance();

				// Primary series

				for(int mtsi = 0; mtsi < primaryResults.length; mtsi++)
				{
					for(int i = 0; i < scenarios; i++)
					{
						for(int j = 0; j < primaryResults[mtsi][i].numberValues; j++)
						{

							ht.set(primaryResults[mtsi][i].times[j]);
							calendar.set(ht.year(), ht.month() - 1, 1);
							double monthlyTAF = primaryResults[mtsi][i].values[j]
									* calendar.getActualMaximum(Calendar.DAY_OF_MONTH) * CFS_2_TAF_DAY;
							int wy = ((ht.month() < 10) ? ht.year() : ht.year() + 1) - _startWY;
							if(wy >= 0)
							{
								_annualTAFs[mtsi][i][wy] += monthlyTAF;
							}
							if(!_isCFS)
							{
								primaryResults[mtsi][i].values[j] = monthlyTAF;
							}
						}
						if(!_isCFS)
						{
							primaryResults[mtsi][i].units = "TAF per year";
						}
					}
				}
			}

			// Calculate differences if applicable (primary series only)

			if(primaryResults[0].length > 1)
			{
				_annualTAFsDiff = new double[datasets][scenarios - 1][_endWY - _startWY + 2];
				for(int mtsi = 0; mtsi < primaryResults.length - 1; mtsi++)
				{
					for(int i = 0; i < scenarios; i++)
					{
						for(int j = 0; j < _endWY - _startWY + 1; j++)
						{
							_annualTAFsDiff[mtsi][i][j] = _annualTAFs[mtsi + 1][i][j] - _annualTAFs[0][i][j];
						}
					}
				}
			}
		}
		catch(Exception ex)
		{
			LOGGER.log(Level.SEVERE, "Unable to calculate TAF.", ex);
		}

	}

	public double getAnnualTAF(int mtsi, int i, int wy)
	{

		return wy < _startWY ? -1 : _annualTAFs[mtsi][i][wy - _startWY];
	}

	public double getAnnualTAFDiff(int mtsi, int i, int wy)
	{

		return wy < _startWY ? -1 : _annualTAFsDiff[mtsi][i][wy - _startWY];
	}

	/**
	 * Variant of getExceedanceSeriesWithMultipleTimeSeries for mts
	 *
	 * @param timeSeriesResults
	 * @return
	 */
	public TimeSeriesContainer[][][] getExceedanceSeriesWithMultipleTimeSeries(
			TimeSeriesContainer[][] timeSeriesResults)
	{

		try
		{
			TimeSeriesContainer[][][] results;
			if(timeSeriesResults == null)
			{
				results = null;
			}
			else
			{
				int datasets = timeSeriesResults.length;
				results = new TimeSeriesContainer[14][datasets][_alternatives.size() + 1];
				for(int mtsI = 0; mtsI < datasets; mtsI++)
				{
					for(int month = 0; month < 14; month++)
					{

						HecTime ht = new HecTime();
						for(int i = 0; i < _alternatives.size() + 1; i++)
						{
							if(timeSeriesResults[mtsI][i] != null)
							{
								if(month == 13)
								{
									results[month][mtsI][i] = (TimeSeriesContainer) timeSeriesResults[mtsI][i].clone();
								}
								else
								{

									int n;
									int[] times2 = null;
									double[] values2 = null;

									results[month][mtsI][i] = new TimeSeriesContainer();

									if(month == 12)
									{

										// Annual totals - grab from annualTAFs
										n = _annualTAFs[i].length;
										times2 = new int[n];
										values2 = new double[n];
										for(int j = 0; j < n; j++)
										{
											ht.setYearMonthDay(j + _startWY, 11, 1, 0);
											times2[j] = ht.value();
											values2[j] = _annualTAFs[mtsI][i][j];
										}

									}
									else
									{

										int[] times = timeSeriesResults[mtsI][i].times;
										double[] values = timeSeriesResults[mtsI][i].values;
										n = 0;
										if(times != null)
										{
											for(final int time : times)
											{
												ht.set(time);
												if(ht.month() == month + 1)
												{
													n = n + 1;
												}
											}
											times2 = new int[n];
											values2 = new double[n];
											n = 0;
											for(int j = 0; j < times.length; j++)
											{
												ht.set(times[j]);
												if(ht.month() == month + 1)
												{
													times2[n] = times[j];
													values2[n] = values[j];
													n = n + 1;
												}
											}
										}
									}
									results[month][mtsI][i].times = times2;
									results[month][mtsI][i].values = values2;
									results[month][mtsI][i].numberValues = n;
									results[month][mtsI][i].units = timeSeriesResults[mtsI][i].units;
									results[month][mtsI][i].fullName = timeSeriesResults[mtsI][i].fullName;
									results[month][mtsI][i].fileName = timeSeriesResults[mtsI][i].fileName;
								}
							}
							if(results[month][mtsI][i] != null && results[month][mtsI][i].values != null)
							{
								double[] sortArray = results[month][mtsI][i].values;
								Arrays.sort(sortArray);
								results[month][mtsI][i].values = sortArray;
							}
						}
					}
				}
			}
			return results;
		}
		catch(Exception ex)
		{
			LOGGER.log(Level.SEVERE, "Unable to get time-series.", ex);
		}
		return null;
	}

	/**
	 * Variant of getExceedanceSeriesDWithMultipleTimeSeries that works with MTS files
	 * <p>
	 * Should be recombinable with other exceedance methods.
	 *
	 * @param timeSeriesResults
	 * @return
	 */
	public TimeSeriesContainer[][][] getExceedanceSeriesDWithMultipleTimeSeries(
			TimeSeriesContainer[][] timeSeriesResults)
	{

		try
		{
			TimeSeriesContainer[][][] results;
			if(timeSeriesResults == null)
			{
				results = null;
			}
			else
			{
				int datasets = timeSeriesResults.length;
				results = new TimeSeriesContainer[14][datasets][_alternatives.size()];
				for(int mtsI = 0; mtsI < datasets; mtsI++)
				{

					for(int month = 0; month < 14; month++)
					{

						HecTime ht = new HecTime();
						for(int i = 0; i < _alternatives.size(); i++)
						{

							if(month == 13)
							{

								results[month][mtsI][i] = (TimeSeriesContainer) timeSeriesResults[mtsI][i + 1].clone();
								for(int j = 0; j < results[month][mtsI][i].numberValues; j++)
								{
									results[month][mtsI][i].values[j] -= timeSeriesResults[mtsI][0].values[j];
								}

							}
							else
							{

								int n;
								int[] times2;
								double[] values2;

								results[month][mtsI][i] = new TimeSeriesContainer();

								if(month == 12)
								{

									// Annual totals - grab from annualTAFs
									n = _annualTAFs[mtsI][i + 1].length;
									times2 = new int[n];
									values2 = new double[n];
									for(int j = 0; j < n; j++)
									{
										ht.setYearMonthDay(j + _startWY, 11, 1, 0);
										times2[j] = ht.value();
										values2[j] = _annualTAFs[mtsI][i + 1][j] - _annualTAFs[mtsI][0][j];
									}

								}
								else
								{

									int[] times = timeSeriesResults[mtsI][i + 1].times;
									double[] values = timeSeriesResults[mtsI][i + 1].values;
									n = 0;
									for(int j = 0; j < times.length; j++)
									{
										ht.set(times[j]);
										if(ht.month() == month + 1)
										{
											n = n + 1;
										}
									}
									times2 = new int[n];
									values2 = new double[n];
									int nmax = n; // Added to trap Schematic
									// View
									// case where required flow
									// has
									// extra values
									n = 0;
									for(int j = 0; j < times.length; j++)
									{
										ht.set(times[j]);
										if((ht.month() == month + 1) && (n < nmax)
												&& (j < timeSeriesResults[0][mtsI].values.length))
										{
											times2[n] = times[j];
											values2[n] = values[j] - timeSeriesResults[0][mtsI].values[j];
											n = n + 1;
										}
									}
								}
								results[month][mtsI][i].times = times2;
								results[month][mtsI][i].values = values2;
								results[month][mtsI][i].numberValues = n;
								results[month][mtsI][i].units = timeSeriesResults[mtsI][i + 1].units;
								results[month][mtsI][i].fullName = timeSeriesResults[mtsI][i + 1].fullName;
								results[month][mtsI][i].fileName = timeSeriesResults[mtsI][i + 1].fileName;
							}
							if(results[month][mtsI][i].values != null)
							{
								double[] sortArray = results[month][mtsI][i].values;
								Arrays.sort(sortArray);
								results[month][mtsI][i].values = sortArray;
							}
						}
					}
				}
			}
			return results;
		}
		catch(Exception ex)
		{
			LOGGER.log(Level.SEVERE, "Unable to get time-series.", ex);
		}
		return null;
	}
}
