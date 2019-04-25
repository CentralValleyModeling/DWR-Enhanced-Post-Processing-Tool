/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */

package gov.ca.water.calgui.busservice.impl;

//! Base DSS file access service

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.Scanner;
import java.util.Vector;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.swing.*;

import calsim.app.Project;
import gov.ca.water.calgui.bo.GUILinksAllModelsBO;
import gov.ca.water.calgui.bo.RBListItemBO;
import gov.ca.water.calgui.bo.ResultUtilsBO;
import gov.ca.water.calgui.busservice.IDSSGrabber1Svc;
import gov.ca.water.calgui.busservice.IGuiLinksSeedDataSvc;
import gov.ca.water.calgui.techservice.IDialogSvc;
import gov.ca.water.calgui.techservice.IErrorHandlingSvc;
import gov.ca.water.calgui.techservice.impl.DialogSvcImpl;
import gov.ca.water.calgui.techservice.impl.ErrorHandlingSvcImpl;

import hec.heclib.dss.HecDss;
import hec.heclib.util.HecTime;
import hec.hecmath.computation.r;
import hec.hecmath.functions.TimeSeriesFunctions;
import hec.io.TimeSeriesContainer;
import org.apache.commons.io.FilenameUtils;

/**
 * Class to grab (load) DSS time series for a set of scenarios passed in a
 * JList. Each scenario is a string that corresponds to a fully qualified (?)
 * DSS file name. The DSS_Grabber instance provides access to the following:
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
public class DSSGrabber1SvcImpl implements IDSSGrabber1Svc
{

	static final double CFS_2_TAF_DAY = 0.001983471;
	private static final double TAF_DAY_2_CFS = 504.166667;
	private static Logger LOGGER = Logger.getLogger(DSSGrabber1SvcImpl.class.getName());
	final List<RBListItemBO> _scenarios = new ArrayList<>();
	String _baseName;
	GUILinksAllModelsBO.Model _baseModel;
	final Map<GUILinksAllModelsBO.Model, String> _primaryDSSName = new HashMap<>();
	final Map<GUILinksAllModelsBO.Model, String> _secondaryDSSName = new HashMap<>();
	// Chart title
	String _plotTitle;
	// Y-axis label
	String _axisLabel;
	// Label for secondary time series
	String _legend;
	// Indicates whether "CFS" button was selected
	String _originalUnits;
	int _scenarioCount;
	boolean _isCFS;
	// USGS Water Year for start and end time.
	int _startWY;
	int _endWY;
	Project _project = ResultUtilsBO.getResultUtilsInstance().getProject();
	// Copy of original units
	// Start and end time of interest
	private int _startTime;
	private int _endTime;
	// Number of scenarios passed in list parameter
	private double[][] _annualTAFs;
	private double[][] _annualTAFsDiff;
	private IGuiLinksSeedDataSvc _seedDataSvc = GuiLinksSeedDataSvcImpl.getSeedDataSvcImplInstance();
	private IDialogSvc _dialogSvc = DialogSvcImpl.getDialogSvcInstance();
	private boolean _stopOnMissing;
	private final Map<GUILinksAllModelsBO.Model, List<String>> _missingDSSRecords = new HashMap<>();

	public DSSGrabber1SvcImpl(List<RBListItemBO> scenarios)
	{
		_scenarios.addAll(scenarios);
		String propertiesFile = "callite-gui.properties";
		try
		{
			Properties properties = new Properties();
			properties.load(ModelRunSvcImpl.class.getClassLoader().getResourceAsStream(propertiesFile));
			_stopOnMissing = Boolean.parseBoolean(properties.getProperty("stop.display.on.null"));
		}
		catch(IOException | RuntimeException ex)
		{
			LOGGER.log(Level.WARNING, "Unable to read properties file from: " + propertiesFile, ex);
			_stopOnMissing = true;
		}
		clearMissingList();
	}

	/**
	 * Clears list of DSS records that were not found in scenario DV.DSS files
	 */
	public void clearMissingList()
	{
		_missingDSSRecords.clear();
	}

	/**
	 * Provide access to list of DSS records not found during processing
	 *
	 * @return list, or null if not tracked due to property setting
	 */
	public Map<GUILinksAllModelsBO.Model, List<String>> getMissingList()
	{
		return _missingDSSRecords;
	}

	/**
	 * Provide access to stopOnMissing flag read from callite-gui.properties
	 *
	 * @return true = stop display task when missing a record, false = continue
	 * with task
	 */
	public boolean getStopOnMissing()
	{
		return _stopOnMissing;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see
	 * gov.ca.water.calgui.busservice.impl.IDSSGrabber1Svc#setIsCFS(boolean)
	 */
	@Override
	public void setIsCFS(boolean isCFS)
	{
		this._isCFS = isCFS;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see
	 * gov.ca.water.calgui.busservice.impl.IDSSGrabber1Svc#setDateRange(java.
	 * lang.String)
	 */
	@Override
	public void setDateRange(String dateRange)
	{

		try
		{
			HecTime ht = new HecTime();

			int m = ResultUtilsBO.getResultUtilsInstance().monthToInt(dateRange.substring(0, 3));
			int y = Integer.parseInt(dateRange.substring(3, 7));
			ht.setYearMonthDay(m == 12 ? y + 1 : y, m == 12 ? 1 : m + 1, 1, 0);
			_startTime = ht.value();
			_startWY = (m < 10) ? y : y + 1; // Water year

			m = ResultUtilsBO.getResultUtilsInstance().monthToInt(dateRange.substring(8, 11));
			y = new Integer(dateRange.substring(11, 15));
			ht.setYearMonthDay(m == 12 ? y + 1 : y, m == 12 ? 1 : m + 1, 1, 0);
			_endTime = ht.value();
			_endWY = (m < 10) ? y : y + 1;
		}
		catch(UnsatisfiedLinkError | NoClassDefFoundError ex)
		{
			LOGGER.log(Level.SEVERE, "Possible javaheclib.dll issue javaHecLib.dll may be the wrong version or missing.", ex);
		}
		catch(RuntimeException ex)
		{
			_startTime = -1;
			LOGGER.log(Level.WARNING, ex.getMessage(), ex);
		}

	}

	/*
	 * (non-Javadoc)
	 *
	 * @see gov.ca.water.calgui.busservice.impl.IDSSGrabber1Svc#getBase()
	 */
	@Override
	public String getBase()
	{

		String delimiter;

		// Windows
		if(_baseName.contains("\\"))
		{

			delimiter = "\\\\";
		}

		// The rest of the world
		else
		{
			delimiter = "/";
		}

		String[] pathParts = _baseName.split(delimiter);
		String fullFileName = pathParts[pathParts.length - 1];
		String fileName = fullFileName.substring(0, fullFileName.lastIndexOf("."));
		return fileName;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see
	 * gov.ca.water.calgui.busservice.impl.IDSSGrabber1Svc#setBase(java.lang.
	 * String)
	 */
	@Override
	public void setBase(String baseName, GUILinksAllModelsBO.Model model)
	{
		_baseName = baseName;
		_baseModel = model;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see
	 * gov.ca.water.calgui.busservice.impl.IDSSGrabber1Svc#setLocation(java.
	 * lang.String)
	 */
	@Override
	public void setLocation(String locationName, GUILinksAllModelsBO.Model model)
	{

		locationName = locationName.trim();

		if(locationName.startsWith("/"))
		{
			// Handle names passed from WRIMS GUI
			String[] parts = locationName.split("/");
			_plotTitle = locationName;
			_primaryDSSName.clear();
			_primaryDSSName.put(model, parts[2] + "/" + parts[3] + "/" + parts[6]);
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

	/*
	 * (non-Javadoc)
	 *
	 * @see gov.ca.water.calgui.busservice.impl.IDSSGrabber1Svc#getYLabel()
	 */
	@Override
	public String getYLabel()
	{
		return _axisLabel;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see gov.ca.water.calgui.busservice.impl.IDSSGrabber1Svc#getSLabel()
	 */
	@Override
	public String getSLabel()
	{
		return _legend;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see gov.ca.water.calgui.busservice.impl.IDSSGrabber1Svc#getPlotTitle()
	 */
	@Override
	public String getPlotTitle()
	{
		if(_plotTitle != null && !_plotTitle.isEmpty())
		{
			return _plotTitle;
		}
		else
		{
			List<String> titles = new ArrayList<>();
			for(Map.Entry<GUILinksAllModelsBO.Model, String> entry : _primaryDSSName.entrySet())
			{
				titles.add(entry.toString() + " (" + entry.getKey() + ")");
			}
			return String.join(",", titles);
		}
	}

	/**
	 * Determines if the cls (CalLite scenario) file associated with a given
	 * DV.DSS file shows dynamic SJR on
	 *
	 * @param dvFilename
	 * @return true if .cls shows dynamic SJR on, false otherwise
	 */
	private boolean clsIsDynamicSJR(String dvFilename)
	{
		String clsFileName = dvFilename.substring(0, dvFilename.length() - 7) + ".cls";
		File clsF = new File(clsFileName);
		String sjrState = "";
		try
		{
			Scanner scanner;
			scanner = new Scanner(new FileInputStream(clsF.getAbsolutePath()));
			while(scanner.hasNextLine() && sjrState.isEmpty())
			{
				String text = scanner.nextLine();
				if(text.startsWith("Dynamic_SJR|"))
				{

					String[] texts = text.split("[|]");
					sjrState = texts[1];
				}
			}
			scanner.close();

		}
		catch(IOException ex)
		{
			LOGGER.log(Level.FINE, clsF.getName() + " not openable - SJR assumed static", ex);
		}

		return Boolean.valueOf(sjrState);
	}

	/**
	 * Determines if the cls (CalLite scenario) file associated with a given
	 * DV.DSS file shows D1485 Fish and Wildlife (Antioch+Chipps) on
	 *
	 * @param dvFilename
	 * @return true if .cls shows Antioch/Chipps on, false otherwise
	 */
	// Logic to display error message if a user is trying access Antioch/Chipps
	// quick results and the selected scenario was not run
	// with Antioch/Chipps
	private boolean clsAntiochChipps(String dvFilename)
	{
		String clsFileName = dvFilename.substring(0, dvFilename.length() - 7) + ".cls";
		File clsF = new File(clsFileName);
		String anChstate = "";
		try
		{
			Scanner scanner;
			scanner = new Scanner(new FileInputStream(clsF.getAbsolutePath()));
			while(scanner.hasNextLine() && anChstate.isEmpty())
			{
				String text = scanner.nextLine();
				if(text.startsWith("CkbReg_AN|"))
				{

					String[] texts = text.split("[|]");
					anChstate = texts[1];
				}
			}
			scanner.close();

		}
		catch(IOException ex)
		{
			LOGGER.log(Level.FINE, clsF.getName() + " not openable - Antioch/Chipps assumed off", ex);
		}

		return Boolean.valueOf(anChstate);
	}

	// Logic to display error message if a user is trying access LVE quick
	// results and the selected scenario was not run with LVE RH
	// 10/2/2014

	/**
	 * Determines if the cls (CalLite scenario) file associated with a given
	 * DV.DSS file shows Los Vaqueros Enlargement on
	 *
	 * @param dvFilename
	 * @return true if .cls shows Antioch/Chipps on, false otherwise
	 */
	private boolean clsLVE(String dvFilename)
	{
		String clsFileName = dvFilename.substring(0, dvFilename.length() - 7) + ".cls";
		File clsF = new File(clsFileName);
		String lveState = "";
		try
		{
			Scanner scanner;
			scanner = new Scanner(new FileInputStream(clsF.getAbsolutePath()));
			while(scanner.hasNextLine() && lveState.isEmpty())
			{
				String text = scanner.nextLine();
				if(text.startsWith("fac_ckb3|"))
				{

					String[] texts = text.split("[|]");
					lveState = texts[1];
				}
			}
			scanner.close();

		}
		catch(IOException ex)
		{
			LOGGER.log(Level.FINE, clsF.getName() + " not openable - LVE assumed off", ex);
		}

		return Boolean.valueOf(lveState);
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see
	 * gov.ca.water.calgui.busservice.impl.IDSSGrabber1Svc#hasPower(java.lang.
	 * String)
	 */
	@Override
	public boolean hasPower(String dssFilename)
	{
		try
		{
			HecDss hD = HecDss.open(dssFilename);
			@SuppressWarnings("unchecked")

			Vector<String> aList = hD.getPathnameList();
			for(String path : aList)
			{
				String[] parts = path.split("/");
				if(parts[1].startsWith("HYDROPOWER"))
				{
					return true;
				}
			}
		}
		catch(Exception ex)
		{

			LOGGER.log(Level.FINE, ex.getMessage(), ex);

		}
		return false;
	}

	/**
	 * Reads a specified dataset from a specified HEC DSS file.
	 *
	 * @param dssFilename name of HEC DSS file from which to read results
	 * @param dssName     name(s) of dataset(s) to read from HEC DSS file. Multiple
	 *                    dataset names can be specified - separated by "+"; the dataset
	 *                    results will be read and summed. if the first data set has the
	 *                    suffix (-1), the results read will be shifted one month
	 *                    earlier.
	 * @return HEC TimeSeriesContainer with times, values, number of values, and
	 * file name.
	 */
	protected TimeSeriesContainer getOneSeries(String dssFilename, String dssName, GUILinksAllModelsBO.Model model)
	{

		HecDss hD = null;
		TimeSeriesContainer result = null;

		try
		{

			hD = HecDss.open(dssFilename);
			Vector<String> aList = hD.getPathnameList();
			if(!hD.isOpened())
			{
				throw new IllegalArgumentException("Unable to open DSS file: " + dssFilename);
			}
			// Determine A-part and F-part directly from file - 10/4/2011 -
			// assumes constant throughout
			Iterator<String> it = aList.iterator();
			if(it.hasNext())
			{

				String aPath = it.next();
				String[] temp = aPath.split("/");
				String hecAPart = temp[1];
				String hecFPart = temp[6] + "/";

				String delims = "[+]";
				String[] dssNames1 = dssName.split(delims);

				// Check for time shift (-1 at end of name)

				boolean doTimeShift = false;
				if(dssNames1[0].endsWith("(-1)"))
				{
					doTimeShift = true;
					dssNames1[0] = dssNames1[0].substring(0, dssNames1[0].length() - 4);
				}

				// Assign F-Part for each DSS - use default if not specified,
				// otherwise last part in supplied name.

				String[] dssNames = new String[dssNames1.length];
				String[] hecFParts = new String[dssNames1.length];
				for(int i = 0; i < dssNames1.length; i++)
				{
					String[] nameParts = dssNames1[i].split("/");
					if(nameParts.length < 2)
					{
						dssNames[i] = "MISSING_B/OR_CPART";
					}
					else
					{
						dssNames[i] = nameParts[0] + "/" + nameParts[1];
						if(nameParts.length == 2)
						{
							hecFParts[i] = hecFPart;
						}
						else
						{
							// TODO: Use nameParts UNLESS it's "LOOKUP", in which
							// case we should look up the value by matching B and C
							// parts

							hecFParts[i] = nameParts[nameParts.length - 1] + "/";
							if("LOOKUP/".equals(hecFParts[i]))
							{
								for(String path : aList)
								{
									String[] parts = path.split("/");
									if(dssNames[i].equals(parts[2] + "/" + parts[3]))
									{
										hecFParts[i] = parts[6] + "/";
									}

								}
							}
						}
					}
				}

				// TODO: Note hard-coded D- and E-PART
				String firstPath = "/" + hecAPart + "/" + dssNames[0] + "/01JAN1930/1MON/" + hecFParts[0];
				result = (TimeSeriesContainer) hD.get(firstPath, true);
				LOGGER.fine(String.format("/%s/%s/01JAN1930/1MON/%s", hecAPart, dssNames[0], hecFParts[0]));
				if((result == null) || (result.numberValues < 1))
				{

					String message;
					result = null;

					if(!clsIsDynamicSJR(dssFilename) && (("S_MELON/STORAGE".equals(dssNames[0]))
							|| ("S_PEDRO/STORAGE".equals(dssNames[0])) || ("S_MCLRE/STORAGE".equals(dssNames[0]))
							|| ("S_MLRTN/STORAGE".equals(dssNames[0])) || ("C_STANRIPN/FLOW-CHANNEL".equals(
							dssNames[0]))
							|| ("C_TUOL/FLOW-CHANNEL".equals(dssNames[0])) || ("C_MERCED2/FLOW-CHANNEL".equals(
							dssNames[0]))
							|| ("C_SJRMS/FLOW-CHANNEL".equals(dssNames[0]))
							|| ("D_STANRIPN/FLOW-DELIVERY".equals(dssNames[0]))
							|| ("D_STANGDWN/FLOW-DELIVERY".equals(dssNames[0]))
							|| ("D_TUOL/FLOW-DELIVERY".equals(dssNames[0]))
							|| ("D_TUOL1B/FLOW-DELIVERY".equals(dssNames[0]))
							|| ("D_TUOL2/FLOW-DELIVERY".equals(dssNames[0]))
							|| ("D_MERCED1/FLOW-DELIVERY".equals(dssNames[0]))
							|| ("D_MERCED2/FLOW-DELIVERY".equals(dssNames[0]))
							|| ("D_MDRCNL/FLOW-DELIVERY".equals(dssNames[0]))
							|| ("D_FKCNL/FLOW-DELIVERY".equals(dssNames[0]))))
					{

						message = " Could not find " + dssNames[0] + " in " + dssFilename
								+ ".\n The selected scenario was not run using dynamic SJR simulation;";

					}

					else if(!clsAntiochChipps(dssFilename)
							&& (("AN_EC_STD/SALINITY".equals(dssNames[0])) || ("CH_EC_STD/SALINITY".equals(
							dssNames[0]))))
					{

						message = " Could not find " + dssNames[0] + " in " + dssFilename
								+ ".\n The selected scenario was not run with D-1485 Fish and Wildlife (at Antioch and Chipps) regulations.";
					}

					else if(!clsLVE(dssFilename) && (("S422/STORAGE".equals(dssNames[0]))
							|| ("WQ408_OR_/SALINITY".equals(dssNames[0])) || ("WQ408_VC_/SALINITY".equals(dssNames[0]))
							|| ("WQ408_RS_/SALINITY".equals(dssNames[0]))
							|| ("C422_FILL_CC/FLOW-CHANNEL".equals(dssNames[0]))
							|| ("D420/FLOW-DELIVERY".equals(dssNames[0])) || ("D408_OR/FLOW-DELIVERY".equals(
							dssNames[0]))
							|| ("D408_VC/FLOW-DELIVERY".equals(dssNames[0]))
							|| ("D408_RS/FLOW-DELIVERY".equals(dssNames[0])) || ("WQ420/SALINITY".equals(dssNames[0]))))
					{

						message = "Could not find " + dssNames[0] + " in " + dssFilename
								+ ".\n The selected scenario was not run with Los Vaqueros Enlargement.";
					}

					else
					{
						message = "Could not find " + dssNames[0] + " in " + dssFilename + " - attempted to retrieve path: " + firstPath;
					}
					List<String> messages = _missingDSSRecords.computeIfAbsent(model, m -> new ArrayList<>());
					messages.add(message);
				}
				else
				{

					// If no error, add results from other datasets in dssNames
					// array.

					for(int i = 1; i < dssNames.length; i++)
					{

						// TODO: Note hard-coded D- and E-PART
						String pathName = "/" + hecAPart + "/" + dssNames[i] + "/01JAN2020/1MON/" + hecFParts[i];
						TimeSeriesContainer result2 = (TimeSeriesContainer) hD
								.get(pathName, true);
						if(result2 == null || result2.numberValues < 1)
						{
							result2 = null;
							String message = String.format("Could not find %s in %s - attempted to retrieve path: %s",
									dssNames[0], dssFilename, pathName);
							if(_stopOnMissing)
							{
								JOptionPane.showMessageDialog(null, message, "Error", JOptionPane.ERROR_MESSAGE);
							}
							else
							{
								List<String> messages = _missingDSSRecords.computeIfAbsent(model, m -> new ArrayList<>());
								messages.add(message);
							}
						}
						else
						{
							//combine result2 and result together and assign to result.
							result = TimeSeriesFunctions.add(
									Arrays.asList(new r(result), new r(result2)),
									(String) null);
						}
					}


				}
				// Trim to date range
				if(result != null)
				{
					// Find starting index
					int first = 0;
					for(int i = 0; (i < result.numberValues) && (result.times[i] < _startTime); i++)
					{
						first = i + 1;
					}
					// find ending index
					int last = result.numberValues - 1;
					for(int i = result.numberValues - 1; (i >= 0) && (result.times[i] >= _endTime); i--)
					{
						last = i;
					}
					// Shift results in array to start
					if(first != 0)
					{
						for(int i = 0; i <= (last - first); i++)
						{ // TODO: Think
							// through
							// change to
							// <= done 11/9
							result.times[i] = result.times[i + first];
							result.values[i] = result.values[i + first];
						}
					}

					result.numberValues = last - first + 1; // Adjust count of
					// results

					// Do time shift where indicated (when a dataset has suffix
					// "(-1)"

					if(doTimeShift)
					{
						if(result.numberValues - 1 - result.numberValues >= 0)
						{
							System.arraycopy(result.times, result.numberValues + 1, result.times, result.numberValues,
									result.numberValues - 1 - result.numberValues);
						}
						result.numberValues = result.numberValues - 1;
					}
				}

			}
		}
		catch(Exception ex)
		{
			String messageText;
			if(ex.getMessage() != null && ex.getMessage().contains("Unable to recognize record"))
			{
				messageText = "Could not find record - HEC message was '" + ex.getMessage() + "'";
				LOGGER.log(Level.FINE, messageText, ex);
				List<String> messages = _missingDSSRecords.computeIfAbsent(model, m -> new ArrayList<>());
				messages.add(messageText);
			}
			else
			{
				messageText = "Unable to get time series." + ex.getMessage();
				LOGGER.log(Level.SEVERE, messageText, ex);
			}
		}
		finally
		{
			if(hD != null)
			{
				hD.close();
			}
		}

		// Store name portion of DSS file in TimeSeriesContainer

		String shortFileName = new File(dssFilename).getName();
		if(result != null)
		{
			result.fileName = shortFileName;
		}

		return result;
	}

	void checkReadiness()
	{
		String result = null;
		if(_startTime == -1)
		{
			result = "Date range is not set in DSSGrabber.";
		}
		else if(_baseName == null)
		{
			result = "Base scenario is not set in DSSGrabber.";
		}
		else if(_primaryDSSName == null)
		{
			result = "Base scenario is not set in DSSGrabber.";
		}
		LOGGER.log(Level.FINE, result);
		if(result != null)
		{
			throw new IllegalStateException(result);
		}
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see
	 * gov.ca.water.calgui.busservice.impl.IDSSGrabber1Svc#getPrimarySeries(
	 * java.lang.String)
	 */
	@Override
	public TimeSeriesContainer[] getPrimarySeries(String locationName)
	{

		TimeSeriesContainer[] results = null;

		try
		{
			checkReadiness();

			// Store number of scenarios

			_scenarioCount = _scenarios.size();
			results = new TimeSeriesContainer[_scenarioCount];

			// Base first
			TimeSeriesContainer oneSeries = getOneTimeSeriesFromAllModels(_baseName, _baseModel, _primaryDSSName);
			results[0] = oneSeries;
			if(oneSeries != null)
			{
				oneSeries.fullName = FilenameUtils.getBaseName(_baseName) + " (" + _baseModel + ")";
			}
			if(results[0] != null)
			{
				_originalUnits = results[0].units;
			}
			else if(_stopOnMissing)
			{
				reportMissingTimeSeries(_baseName);
			}

			// Then scenarios

			int j = 0;
			for(int i = 0; i < _scenarioCount; i++)
			{
				String scenarioName;
				GUILinksAllModelsBO.Model model = _baseModel;
				if(_baseName.toUpperCase().contains("_SV.DSS"))
				{
					// For SVars, use WRIMS GUI Project object to
					// determine
					// input files
					switch(i)
					{
						case 0:
							scenarioName = _project.getSVFile();
							break;
						case 1:
							scenarioName = _project.getSV2File();
							break;
						case 2:
							scenarioName = _project.getSV3File();
							break;
						case 3:
							scenarioName = _project.getSV4File();
							break;
						default:
							scenarioName = "";
							break;
					}
				}
				else
				{
					RBListItemBO rbListItemBO = _scenarios.get(i);
					scenarioName = rbListItemBO.toString();
					model = rbListItemBO.getModel();
				}
				if(!_baseName.equals(scenarioName))
				{
					j = j + 1;
					TimeSeriesContainer tsc = getOneTimeSeriesFromAllModels(scenarioName, model, _primaryDSSName);
					results[j] = tsc;
					if(tsc != null)
					{
						tsc.fullName = FilenameUtils.getBaseName(scenarioName) + " (" + model + ")";
					}
				}
			}
		}
		catch(RuntimeException ex)
		{
			LOGGER.log(Level.SEVERE, "Unable to get time series.", ex);
		}

		return results;
	}

	private TimeSeriesContainer getOneTimeSeriesFromAllModels(String scenarioName, GUILinksAllModelsBO.Model model,
															  Map<GUILinksAllModelsBO.Model, String> dssNames)
	{
		TimeSeriesContainer oneSeries = null;
		String primaryTs = dssNames.get(model);
		if(primaryTs != null)
		{
			oneSeries = getOneSeries(scenarioName, primaryTs, model);
		}
		if(oneSeries == null)
		{
			LOGGER.log(Level.WARNING, "No matching GUI Links record in: " + scenarioName + " for Model: " + model + " with path: " + primaryTs);
			if(_stopOnMissing)
			{
				reportMissingTimeSeries(scenarioName);
			}
		}
		return oneSeries;
	}

	private void reportMissingTimeSeries(String scenarioName)
	{
		List<String> titles = new ArrayList<>();
		for(Map.Entry<GUILinksAllModelsBO.Model, String> entry : _primaryDSSName.entrySet())
		{
			titles.add(entry.toString() + " (" + entry.getKey() + ")");
		}
		_dialogSvc.getOK("Could not find " + String.join(",", titles) + " in " + scenarioName,
				JOptionPane.ERROR_MESSAGE);
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see
	 * gov.ca.water.calgui.busservice.impl.IDSSGrabber1Svc#getSecondarySeries()
	 */
	@Override
	public TimeSeriesContainer[] getSecondarySeries()
	{

		if(_secondaryDSSName.isEmpty())
		{
			return null;
		}
		else
		{

			_scenarioCount = _scenarios.size();
			TimeSeriesContainer[] results = new TimeSeriesContainer[_scenarioCount];

			// Base first

			TimeSeriesContainer oneSeries = getOneTimeSeriesFromAllModels(_baseName, _baseModel, _secondaryDSSName);
			results[0] = oneSeries;
			if(oneSeries != null)
			{
				oneSeries.fullName = FilenameUtils.getBaseName(_baseName) + " (" + _baseModel + ")";
			}
			// Then scenarios

			int j = 0;
			for(int i = 0; i < _scenarioCount; i++)
			{
				RBListItemBO rbListItemBO = _scenarios.get(i);
				String scenarioName = rbListItemBO.toString();
				GUILinksAllModelsBO.Model model = rbListItemBO.getModel();

				if(!_baseName.equals(scenarioName))
				{
					j = j + 1;
					TimeSeriesContainer tsc = getOneTimeSeriesFromAllModels(scenarioName, model, _secondaryDSSName);
					results[j] = tsc;
					if(tsc != null)
					{
						tsc.fullName = FilenameUtils.getBaseName(scenarioName) + " (" + _baseModel + ")";
					}
				}
			}
			return results;
		}
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see
	 * gov.ca.water.calgui.busservice.impl.IDSSGrabber1Svc#getDifferenceSeriesWithMultipleTimeSeries(
	 * hec.io.TimeSeriesContainer[])
	 */
	@Override
	public TimeSeriesContainer[] getDifferenceSeries(TimeSeriesContainer[] timeSeriesResults)
	{

		TimeSeriesContainer[] results = new TimeSeriesContainer[_scenarioCount - 1];
		if(timeSeriesResults[0] != null)
		{
			for(int i = 0; i < _scenarioCount - 1; i++)
			{
				TimeSeriesContainer timeSeriesResult = timeSeriesResults[i + 1];
				if(timeSeriesResult != null)
				{
					results[i] = (TimeSeriesContainer) timeSeriesResult.clone();
					for(int j = 0; j < results[i].numberValues; j++)
					{
						results[i].values[j] = results[i].values[j] - timeSeriesResults[0].values[j];
					}
				}
			}
		}
		return results;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see
	 * gov.ca.water.calgui.busservice.impl.IDSSGrabber1Svc#calcTAFforCFS(hec.io
	 * .TimeSeriesContainer[], hec.io.TimeSeriesContainer[])
	 */
	@Override
	public void calcTAFforCFS(TimeSeriesContainer[] primaryResults, TimeSeriesContainer[] secondaryResults)
	{

		try
		{
			// Allocate and zero out

			int datasets = primaryResults.length;
			if(secondaryResults != null)
			{
				datasets = datasets + secondaryResults.length;
			}

			_annualTAFs = new double[datasets][_endWY - _startWY + 2];

			for(int i = 0; i < datasets; i++)
			{
				for(int j = 0; j < _endWY - _startWY + 1; j++)
				{
					_annualTAFs[i][j] = 0.0;
				}
			}

			// Calculate

			if("CFS".equals(_originalUnits))
			{

				HecTime ht = new HecTime();
				Calendar calendar = Calendar.getInstance();

				// Primary series

				for(int i = 0; i < primaryResults.length; i++)
				{
					if(primaryResults[i] != null)
					{
						for(int j = 0; j < primaryResults[i].numberValues; j++)
						{

							ht.set(primaryResults[i].times[j]);
							calendar.set(ht.year(), ht.month() - 1, 1);
							double monthlyTAF = primaryResults[i].values[j]
									* calendar.getActualMaximum(Calendar.DAY_OF_MONTH) * CFS_2_TAF_DAY;
							int wy = ((ht.month() < 10) ? ht.year() : ht.year() + 1) - _startWY;
							if(wy >= 0)
							{
								_annualTAFs[i][wy] += monthlyTAF;
							}
							if(!_isCFS)
							{
								primaryResults[i].values[j] = monthlyTAF;
							}
						}
						if(!_isCFS)
						{
							primaryResults[i].units = "TAF per year";
						}
					}
				}

				// Calculate differences if applicable (primary series only)

				if(primaryResults.length > 1)
				{
					_annualTAFsDiff = new double[primaryResults.length - 1][_endWY - _startWY + 2];
					for(int i = 0; i < primaryResults.length - 1; i++)
					{
						for(int j = 0; j < _endWY - _startWY + 1; j++)
						{
							_annualTAFsDiff[i][j] = _annualTAFs[i + 1][j] - _annualTAFs[0][j];
						}
					}
				}

				if(secondaryResults != null)
				{

					// Secondary series

					for(int i = 0; i < secondaryResults.length; i++)
					{
						if(secondaryResults[i] != null)
						{
							for(int j = 0; j < secondaryResults[i].numberValues; j++)
							{

								ht.set(secondaryResults[i].times[j]);
								calendar.set(ht.year(), ht.month() - 1, 1);
								double monthlyTAF = secondaryResults[i].values[j]
										* calendar.getActualMaximum(Calendar.DAY_OF_MONTH) * CFS_2_TAF_DAY;
								int wy = ((ht.month() < 10) ? ht.year() : ht.year() + 1) - _startWY;
								_annualTAFs[i + primaryResults.length][wy] += monthlyTAF;
								if(!_isCFS)
								{
									secondaryResults[i].values[j] = monthlyTAF;
								}

							}
							if(!_isCFS)
							{
								secondaryResults[i].units = "TAF per year";
							}
						}
					}
				}
			}
		}
		catch(RuntimeException ex)
		{
			LOGGER.log(Level.SEVERE, "Unable to calculate TAF.", ex);
		}
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see
	 * gov.ca.water.calgui.busservice.impl.IDSSGrabber1Svc#getAnnualTAF(int,
	 * int)
	 */
	@Override
	public double getAnnualTAF(int i, int wy)
	{

		return wy < _startWY ? -1 : _annualTAFs[i][wy - _startWY];
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see
	 * gov.ca.water.calgui.busservice.impl.IDSSGrabber1Svc#getAnnualTAFDiff(
	 * int, int)
	 */
	@Override
	public double getAnnualTAFDiff(int i, int wy)
	{

		return wy < _startWY ? -1 : _annualTAFsDiff[i][wy - _startWY];
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see
	 * gov.ca.water.calgui.busservice.impl.IDSSGrabber1Svc#getExceedanceSeriesWithMultipleTimeSeries(
	 * hec.io.TimeSeriesContainer[])
	 */
	@Override
	public TimeSeriesContainer[][] getExceedanceSeries(TimeSeriesContainer[] timeSeriesResults)
	{

		TimeSeriesContainer[][] results;
		try
		{
			boolean valid = timeSeriesResults != null;
			if(valid)
			{
				for(TimeSeriesContainer tsc : timeSeriesResults)
				{
					valid = tsc != null && tsc.times != null;
					if(!valid)
					{
						break;
					}
				}
			}
			if(!valid)
			{
				results = null;
			}
			else
			{
				results = new TimeSeriesContainer[14][_scenarioCount];

				for(int month = 0; month < 14; month++)
				{

					HecTime ht = new HecTime();
					for(int i = 0; i < _scenarioCount; i++)
					{

						if(month == 13)
						{
							results[month][i] = (TimeSeriesContainer) timeSeriesResults[i].clone();
						}
						else
						{

							int n;
							int[] times2;
							double[] values2;

							results[month][i] = new TimeSeriesContainer();

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
									values2[j] = _annualTAFs[i][j];
								}

							}
							else
							{

								int[] times = timeSeriesResults[i].times;
								double[] values = timeSeriesResults[i].values;

								n = 0;
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
							results[month][i].times = times2;
							results[month][i].values = values2;
							results[month][i].numberValues = n;
							results[month][i].units = timeSeriesResults[i].units;
							results[month][i].fullName = timeSeriesResults[i].fullName;
							results[month][i].fileName = timeSeriesResults[i].fileName;
						}
						if(results[month][i].values != null)
						{
							double[] sortArray = results[month][i].values;
							Arrays.sort(sortArray);
							results[month][i].values = sortArray;
						}
					}
				}
			}
			return results;
		}
		catch(RuntimeException ex)
		{
			LOGGER.log(Level.SEVERE, "Unable to get time-series.", ex);
		}
		return null;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see
	 * gov.ca.water.calgui.busservice.impl.IDSSGrabber1Svc#getExceedanceSeriesDWithMultipleTimeSeries
	 * (hec.io.TimeSeriesContainer[])
	 */
	@Override
	public TimeSeriesContainer[][] getExceedanceSeriesD(TimeSeriesContainer[] timeSeriesResults)
	{

		/*
		 * Copy of getExceedanceSeriesWithMultipleTimeSeries to handle "exceedance of differences"
		 *
		 * Calculates difference of annual TAFs to get proper results for [12]
		 * should be recombined with getExceedanceSeriesWithMultipleTimeSeries
		 */

		try
		{
			TimeSeriesContainer[][] results;
			boolean valid = timeSeriesResults != null;
			if(valid)
			{
				for(TimeSeriesContainer tsc : timeSeriesResults)
				{
					valid = tsc != null && tsc.times != null;
					if(!valid)
					{
						break;
					}
				}
			}
			if(!valid)
			{
				results = null;
			}
			else
			{
				results = new TimeSeriesContainer[14][_scenarioCount - 1];

				for(int month = 0; month < 14; month++)
				{

					HecTime ht = new HecTime();
					for(int i = 0; i < _scenarioCount - 1; i++)
					{

						if(month == 13)
						{

							results[month][i] = (TimeSeriesContainer) timeSeriesResults[i + 1].clone();
							for(int j = 0; j < results[month][i].numberValues; j++)
							{
								results[month][i].values[j] -= timeSeriesResults[0].values[j];
							}

						}
						else
						{

							int n;
							int[] times2;
							double[] values2;

							results[month][i] = new TimeSeriesContainer();

							if(month == 12)
							{

								// Annual totals - grab from annualTAFs
								n = _annualTAFs[i + 1].length;
								times2 = new int[n];
								values2 = new double[n];
								for(int j = 0; j < n; j++)
								{
									ht.setYearMonthDay(j + _startWY, 11, 1, 0);
									times2[j] = ht.value();
									values2[j] = _annualTAFs[i + 1][j] - _annualTAFs[0][j];
								}

							}
							else
							{

								int[] times = timeSeriesResults[i + 1].times;
								double[] values = timeSeriesResults[i + 1].values;
								n = 0;
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
								int nmax = n; // Added to trap Schematic View
								// case
								// where required flow has extra
								// values
								n = 0;
								for(int j = 0; j < times.length; j++)
								{
									ht.set(times[j]);
									if((ht.month() == month + 1) && (n < nmax)
											&& (j < timeSeriesResults[0].values.length))
									{
										times2[n] = times[j];
										values2[n] = values[j] - timeSeriesResults[0].values[j];
										n = n + 1;
									}
								}
							}
							results[month][i].times = times2;
							results[month][i].values = values2;
							results[month][i].numberValues = n;
							results[month][i].units = timeSeriesResults[i + 1].units;
							results[month][i].fullName = timeSeriesResults[i + 1].fullName;
							results[month][i].fileName = timeSeriesResults[i + 1].fileName;
						}
						if(results[month][i].values != null)
						{
							double[] sortArray = results[month][i].values;
							Arrays.sort(sortArray);
							results[month][i].values = sortArray;
						}
					}
				}
			}
			return results;
		}
		catch(RuntimeException ex)
		{
			LOGGER.log(Level.SEVERE, "Unable to get time-series.", ex);
		}
		return null;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see
	 * gov.ca.water.calgui.busservice.impl.IDSSGrabber1Svc#getOriginalUnits()
	 */
	@Override
	public String getOriginalUnits()
	{
		return _originalUnits;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see
	 * gov.ca.water.calgui.busservice.impl.IDSSGrabber1Svc#getPrimaryDSSName()
	 */
	@Override
	public Map<GUILinksAllModelsBO.Model, String> getPrimaryDSSName()
	{
		return _primaryDSSName;
	}

}
