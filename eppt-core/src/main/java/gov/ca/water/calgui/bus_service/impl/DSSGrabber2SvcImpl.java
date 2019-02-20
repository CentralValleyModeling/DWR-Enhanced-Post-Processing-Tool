/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */

package gov.ca.water.calgui.bus_service.impl;

//! Variant on DSSGrabber1BO for MTS (multiple time series)

import calsim.app.DerivedTimeSeries;
import calsim.app.MultipleTimeSeries;
import gov.ca.water.calgui.bo.GUILinks3BO;
import gov.ca.water.calgui.bo.ResultUtilsBO;
import gov.ca.water.calgui.bus_service.ISeedDataSvc;
import gov.ca.water.calgui.constant.Constant;
import gov.ca.water.calgui.tech_service.IErrorHandlingSvc;
import gov.ca.water.calgui.tech_service.impl.ErrorHandlingSvcImpl;
import hec.heclib.util.HecTime;
import hec.io.TimeSeriesContainer;
import org.apache.log4j.Logger;
import org.swixml.SwingEngine;

import javax.swing.*;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Vector;

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
 * <li>setBase</li>
 * <li>setLocation</li>
 * <li>setDateRange</li>
 * <li>getPrimarySeries</li>
 * <li>getSecondarySeries</li>
 * <li>Other calculations</li>
 * </ul>
 */
public class DSSGrabber2SvcImpl extends DSSGrabber1SvcImpl
{

	static Logger LOG = Logger.getLogger(DSSGrabber2SvcImpl.class.getName());
	private final DerivedTimeSeries dts;
	private final MultipleTimeSeries mts;
	private ISeedDataSvc seedDataSvc = SeedDataSvcImpl.getSeedDataSvcImplInstance();
	private IErrorHandlingSvc errorHandlingSvc = new ErrorHandlingSvcImpl();
	private SwingEngine swingEngine = XMLParsingSvcImpl.getXMLParsingSvcImplInstance().getSwingEngine();
	private double[][][] _annualTAFs;
	private double[][][] _annualTAFsDiff;

	public DSSGrabber2SvcImpl(JList list, DerivedTimeSeries dts, MultipleTimeSeries mts)
	{

		super(list);
		this.dts = dts;
		this.mts = mts;
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
				primaryDSSName = locationName;
				secondaryDSSName = "";
				yLabel = "";
				sLabel = "";
				title = locationName;
			}
			else if(locationName.startsWith("/"))
			{
				// Handle names passed from WRIMS GUI
				String[] parts = locationName.split("/");
				title = locationName;
				primaryDSSName = parts[2] + "/" + parts[3];
				secondaryDSSName = "";
				yLabel = "";
				sLabel = "";
			}
			else
			{
				String lookupID = locationName;
				if(lookupID.startsWith(Constant.SCHEMATIC_PREFIX))
				// Strip off prefix for schematic view - NOT SURE IF WE
				// CAN'T
				// JUST ELIMINATE PREFIX?
				{
					lookupID = lookupID.substring(Constant.SCHEMATIC_PREFIX.length());
				}

				// Location name is otherwise assumed coded as "ckpbxxx"

				GUILinks3BO guiLinks3BO = seedDataSvc.getObjById(locationName);
				if(guiLinks3BO != null)
				{
					primaryDSSName = guiLinks3BO.getPrimary();
					secondaryDSSName = guiLinks3BO.getSecondary();
					yLabel = guiLinks3BO.getyTitle();
					title = guiLinks3BO.getTitle();
					sLabel = guiLinks3BO.getyTitle2();
				}
			}
		}
		catch(Exception e)
		{
			LOG.error(e.getMessage());
			String messageText = "Unable to set location.";
			errorHandlingSvc.businessErrorHandler(messageText, (JFrame) swingEngine.find(Constant.MAIN_FRAME_NAME), e);
		}
	}

	/**
	 * Reads the DSS results for the primary series for each scenario. Also
	 * stores for reference the units of measure for the primary series in the
	 * private variable originalUnits.
	 *
	 * @return Array of HEC TimeSeriesContainer - one TSC for each scenario
	 */
	public TimeSeriesContainer[] getPrimarySeries(String locationName)
	{

		try
		{
			TimeSeriesContainer[] results = null;

			if(checkReadiness() != null)
			{
				throw new NullPointerException(checkReadiness());
			}

			else
			{

				if(locationName.contains(Constant.SCHEMATIC_PREFIX) && primaryDSSName.contains(","))
				{

					// Special handling for DEMO of schematic view - treat
					// multiple
					// series as multiple scenarios
					// TODO: Longer-term approach is probably to add a rank to
					// arrays storing all series

					String[] dssNames = primaryDSSName.split(",");
					scenarios = dssNames.length;
					results = new TimeSeriesContainer[scenarios];
					for(int i = 0; i < scenarios; i++)
					{
						results[i] = getOneSeries(baseName, dssNames[i]);
					}

					originalUnits = results[0].units;

				}
				else
				{

					// Store number of scenarios

					scenarios = lstScenarios.getModel().getSize();
					results = new TimeSeriesContainer[scenarios];

					// Base first

					results[0] = getOneSeries_WRIMS(baseName, primaryDSSName, dts);
					originalUnits = results[0].units;

					// Then scenarios

					int j = 0;
					for(int i = 0; i < scenarios; i++)
					{
						String scenarioName;
						if(baseName.contains("_SV.DSS"))
						{
							// For SVars, use WRIMS GUI Project object to
							// determine
							// input files
							switch(i)
							{
								case 0:
									scenarioName = project.getSVFile();
									break;
								case 1:
									scenarioName = project.getSV2File();
									break;
								case 2:
									scenarioName = project.getSV3File();
									break;
								case 3:
									scenarioName = project.getSV4File();
									break;
								default:
									scenarioName = "";
									break;
							}
						}
						else
						{
							scenarioName = lstScenarios.getModel().getElementAt(i).toString();
						}
						if(!baseName.equals(scenarioName))
						{
							j = j + 1;
							results[j] = getOneSeries_WRIMS(scenarioName, primaryDSSName, dts);
						}
					}
				}
			}

			return results;
		}
		catch(Exception e)
		{
			LOG.error(e.getMessage());
			String messageText = "Unable to get time-series.";
			errorHandlingSvc.businessErrorHandler(messageText, (JFrame) swingEngine.find(Constant.MAIN_FRAME_NAME), e);
		}
		return null;
	}

	public TimeSeriesContainer[] getMultipleTimeSeries(int mtsI)
	{

		try
		{
			TimeSeriesContainer[] results = null;

			if(checkReadiness() != null)
			{
				throw new NullPointerException(checkReadiness());
			}

			else
			{

				// Store number of scenarios

				scenarios = lstScenarios.getModel().getSize();
				results = new TimeSeriesContainer[scenarios];

				// Base first

				results[0] = getOneSeries_WRIMS(baseName, mtsI, mts);
				if(results[0] != null)
				{
					originalUnits = results[0].units;
				}

				// Then scenarios

				int j = 0;
				for(int i = 0; i < scenarios; i++)
				{
					String scenarioName;
					if(baseName.contains("_SV.DSS"))
					{
						// For SVars, use WRIMS GUI Project object to determine
						// input files
						switch(i)
						{
							case 0:
								scenarioName = project.getSVFile();
								break;
							case 1:
								scenarioName = project.getSV2File();
								break;
							case 2:
								scenarioName = project.getSV3File();
								break;
							case 3:
								scenarioName = project.getSV4File();
								break;
							default:
								scenarioName = "";
								break;
						}
					}
					else
					{
						scenarioName = lstScenarios.getModel().getElementAt(i).toString();
					}
					if(!baseName.equals(scenarioName))
					{
						j = j + 1;
						results[j] = getOneSeries_WRIMS(scenarioName, mtsI, mts);
					}
				}
			}

			return results;
		}
		catch(Exception e)
		{
			LOG.error(e.getMessage());
			String messageText = "Unable to get time-series.";
			errorHandlingSvc.businessErrorHandler(messageText, (JFrame) swingEngine.find(Constant.MAIN_FRAME_NAME), e);
		}
		return null;
	}

	private TimeSeriesContainer getOneSeries_WRIMS(String dssFilename, String dssName, DerivedTimeSeries dts2)
	{

		try
		{
			Vector<?> dtsNames = dts2.getDtsNames();

			TimeSeriesContainer result = null;
			boolean first = true;
			for(int i = 0; i < dts2.getNumberOfDataReferences(); i++)
			{
				TimeSeriesContainer interimResult;
				if(!((String) dtsNames.get(i)).isEmpty())
				{
					// Operand is reference to another DTS
					DerivedTimeSeries adt = ResultUtilsBO.getResultUtilsInstance(null).getProject()
														 .getDTS((String) dtsNames.get(i));
					interimResult = getOneSeries_WRIMS(dssFilename, dssName, adt);
				}
				else
				{
					// Operand is a DSS time series
					primaryDSSName = (dts2.getBPartAt(i) + "/" + dts2.getCPartAt(i));
					if(dts2.getVarTypeAt(i).equals("DVAR"))
					{
						interimResult = getOneSeries(dssFilename,
								(dts2.getBPartAt(i) + "/" + dts2.getCPartAt(i) + "/LOOKUP"));
					}
					else
					{
						String svFilename = "";

						if(dssFilename.equals(project.getDVFile()))
						{
							svFilename = project.getSVFile();
						}
						else if(dssFilename.equals(project.getDV2File()))
						{
							svFilename = project.getSV2File();
						}
						else if(dssFilename.equals(project.getDV3File()))
						{
							svFilename = project.getSV3File();
						}
						else if(dssFilename.equals(project.getDV4File()))
						{
							svFilename = project.getSV4File();
						}

						interimResult = getOneSeries(svFilename,
								(dts2.getBPartAt(i) + "/" + dts2.getCPartAt(i) + "/LOOKUP"));
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
		catch(Exception e)
		{
			LOG.error(e.getMessage());
			String messageText = "Unable to get time-series.";
			errorHandlingSvc.businessErrorHandler(messageText, (JFrame) swingEngine.find(Constant.MAIN_FRAME_NAME), e);
		}
		return null;

	}

	private TimeSeriesContainer getOneSeries_WRIMS(String dssFilename, int i, MultipleTimeSeries mts2)
	{

		try
		{
			TimeSeriesContainer result = null;
			if(!mts2.getDTSNameAt(i).equals(""))
			{
				// Operand is reference to a DTS
				DerivedTimeSeries adt = ResultUtilsBO.getResultUtilsInstance(null).getProject()
													 .getDTS(mts.getDTSNameAt(i));
				result = getOneSeries_WRIMS(dssFilename, "", adt);
				primaryDSSName = mts.getDTSNameAt(i);

			}
			else
			{
				// Operand is a DSS time series
				primaryDSSName = (mts2.getBPartAt(i) + "//" + mts2.getCPartAt(i));
				if(mts2.getVarTypeAt(i).equals("DVAR"))
				{
					result = getOneSeries(dssFilename, (mts2.getBPartAt(i) + "/" + mts2.getCPartAt(i)));
				}
				else
				{
					String svFilename = "";

					if(dssFilename.equals(project.getDVFile()))
					{
						svFilename = project.getSVFile();
					}
					else if(dssFilename.equals(project.getDV2File()))
					{
						svFilename = project.getSV2File();
					}
					else if(dssFilename.equals(project.getDV3File()))
					{
						svFilename = project.getSV3File();
					}
					else if(dssFilename.equals(project.getDV4File()))
					{
						svFilename = project.getSV4File();
					}

					result = getOneSeries(svFilename, (mts2.getBPartAt(i) + "/" + mts2.getCPartAt(i)));
				}
			}
			return result;
		}
		catch(Exception e)
		{
			LOG.error(e.getMessage());
			String messageText = "Unable to get time series from.";
			errorHandlingSvc.businessErrorHandler(messageText, (JFrame) swingEngine.find(Constant.MAIN_FRAME_NAME), e);
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
			TimeSeriesContainer[][] results = new TimeSeriesContainer[timeSeriesResults.length][scenarios - 1];

			for(int tsi = 0; tsi < timeSeriesResults.length; tsi++)
			{

				for(int i = 0; i < scenarios - 1; i++)
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
		catch(Exception e)
		{
			LOG.error(e.getMessage());
			String messageText = "Unable to get time-series.";
			errorHandlingSvc.businessErrorHandler(messageText, (JFrame) swingEngine.find(Constant.MAIN_FRAME_NAME), e);
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

			_annualTAFs = new double[datasets][scenarios][endWY - startWY + 2];

			for(int mtsi = 0; mtsi < datasets; mtsi++)
			{
				for(int i = 0; i < scenarios; i++)
				{
					for(int j = 0; j < endWY - startWY + 1; j++)
					{
						_annualTAFs[mtsi][i][j] = 0.0;
					}
				}
			}

			// Calculate

			if(originalUnits.equals("CFS"))
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
							int wy = ((ht.month() < 10) ? ht.year() : ht.year() + 1) - startWY;
							if(wy >= 0)
							{
								_annualTAFs[mtsi][i][wy] += monthlyTAF;
							}
							if(!isCFS)
							{
								primaryResults[mtsi][i].values[j] = monthlyTAF;
							}
						}
						if(!isCFS)
						{
							primaryResults[mtsi][i].units = "TAF per year";
						}
					}
				}
			}

			// Calculate differences if applicable (primary series only)

			if(primaryResults[0].length > 1)
			{
				_annualTAFsDiff = new double[datasets][scenarios - 1][endWY - startWY + 2];
				for(int mtsi = 0; mtsi < primaryResults.length - 1; mtsi++)
				{
					for(int i = 0; i < scenarios; i++)
					{
						for(int j = 0; j < endWY - startWY + 1; j++)
						{
							_annualTAFsDiff[mtsi][i][j] = _annualTAFs[mtsi + 1][i][j] - _annualTAFs[0][i][j];
						}
					}
				}
			}
		}
		catch(Exception e)
		{
			LOG.error(e.getMessage());
			String messageText = "Unable to calculate TAF.";
			errorHandlingSvc.businessErrorHandler(messageText, (JFrame) swingEngine.find(Constant.MAIN_FRAME_NAME), e);
		}

	}

	public double getAnnualTAF(int mtsi, int i, int wy)
	{

		return wy < startWY ? -1 : _annualTAFs[mtsi][i][wy - startWY];
	}

	public double getAnnualTAFDiff(int mtsi, int i, int wy)
	{

		return wy < startWY ? -1 : _annualTAFsDiff[mtsi][i][wy - startWY];
	}

	/**
     * Variant of getExceedanceSeriesWithMultipleTimeSeries for mts
	 *
	 * @param timeSeriesResults
	 * @return
	 */
    public TimeSeriesContainer[][][] getExceedanceSeriesWithMultipleTimeSeries(TimeSeriesContainer[][] timeSeriesResults)
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
				results = new TimeSeriesContainer[14][datasets][scenarios];
				for(int mtsI = 0; mtsI < datasets; mtsI++)
				{
					for(int month = 0; month < 14; month++)
					{

						HecTime ht = new HecTime();
						for(int i = 0; i < scenarios; i++)
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
											ht.setYearMonthDay(j + startWY, 11, 1, 0);
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
		catch(Exception e)
		{
			LOG.error(e.getMessage());
			String messageText = "Unable to get time-series.";
			errorHandlingSvc.businessErrorHandler(messageText, (JFrame) swingEngine.find(Constant.MAIN_FRAME_NAME), e);
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
    public TimeSeriesContainer[][][] getExceedanceSeriesDWithMultipleTimeSeries(TimeSeriesContainer[][] timeSeriesResults)
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
				results = new TimeSeriesContainer[14][datasets][scenarios - 1];
				for(int mtsI = 0; mtsI < datasets; mtsI++)
				{

					for(int month = 0; month < 14; month++)
					{

						HecTime ht = new HecTime();
						for(int i = 0; i < scenarios - 1; i++)
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
										ht.setYearMonthDay(j + startWY, 11, 1, 0);
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
		catch(Exception e)
		{
			LOG.error(e.getMessage());
			String messageText = "Unable to get time-series.";
			errorHandlingSvc.businessErrorHandler(messageText, (JFrame) swingEngine.find(Constant.MAIN_FRAME_NAME), e);
		}
		return null;
	}
}
