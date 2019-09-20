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

//! Base DSS file access service

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Properties;
import java.util.Scanner;
import java.util.Vector;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.swing.*;

import calsim.app.Project;
import gov.ca.water.calgui.bo.GUILinksAllModelsBO;
import gov.ca.water.calgui.bo.ResultUtilsBO;
import gov.ca.water.calgui.bo.ThresholdLinksBO;
import gov.ca.water.calgui.busservice.IDSSGrabber1Svc;
import gov.ca.water.calgui.busservice.IGuiLinksSeedDataSvc;
import gov.ca.water.calgui.project.EpptDssContainer;
import gov.ca.water.calgui.project.EpptScenarioRun;
import gov.ca.water.calgui.project.NamedDssPath;
import gov.ca.water.calgui.techservice.IDialogSvc;
import gov.ca.water.calgui.techservice.impl.DialogSvcImpl;
import org.jfree.data.time.Month;

import hec.heclib.dss.DSSPathname;
import hec.heclib.dss.HecDataManager;
import hec.heclib.dss.HecDss;
import hec.heclib.util.HecTime;
import hec.hecmath.DSS;
import hec.hecmath.computation.r;
import hec.hecmath.functions.TimeSeriesFunctions;
import hec.io.TimeSeriesContainer;

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
 * <li>setScenarioRuns</li>
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
	final Map<GUILinksAllModelsBO.Model, String> _primaryDSSName = new HashMap<>();
	final Map<GUILinksAllModelsBO.Model, String> _secondaryDSSName = new HashMap<>();
	private final Map<GUILinksAllModelsBO.Model, String> _thresholdDSSName = new HashMap<>();
	final List<EpptScenarioRun> _alternatives = new ArrayList<>();
	private final IGuiLinksSeedDataSvc _seedDataSvc = GuiLinksSeedDataSvcImpl.getSeedDataSvcImplInstance();
	private final ThresholdLinksSeedDataSvc _thresholdLinksSeedDataSvc = ThresholdLinksSeedDataSvc.getSeedDataSvcImplInstance();
	private final Map<GUILinksAllModelsBO.Model, List<String>> _missingDSSRecords = new HashMap<>();
	// Chart title
	String _plotTitle;
	// Y-axis label
	String _axisLabel;
	// Label for secondary time series
	String _legend;
	// Indicates whether "CFS" button was selected
	String _originalUnits;
	boolean _isCFS;
	// USGS Water Year for start and end time.
	int _startWY;
	int _endWY;
	Project _project = ResultUtilsBO.getResultUtilsInstance().getProject();
	EpptScenarioRun _baseScenarioRun;
	// Copy of original units
	// Start and end time of interest
	private int _startTime;
	private int _endTime;
	// Number of scenarios passed in list parameter
	private double[][] _annualTAFs;
	private double[][] _annualTAFsDiff;
	private boolean _stopOnMissing;
	private ThresholdLinksBO _threshold;

	public DSSGrabber1SvcImpl()
	{
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
	public void setDateRange(LocalDate startMonth, LocalDate endMonth)
	{

		try
		{
			HecTime ht = new HecTime();

			int m = startMonth.getMonth().getValue();
			int y = startMonth.getYear();
			ht.setYearMonthDay(m == 12 ? (y + 1) : y, m == 12 ? 1 : (m + 1), 1, 0);
			_startTime = ht.value();
			if(m < 10)
			{
				_startWY = y;
			}
			else
			{
				_startWY = y + 1;
			} // Water year

			m = endMonth.getMonth().getValue();
			y = endMonth.getYear();
			ht.setYearMonthDay(m == 12 ? (y + 1) : y, m == 12 ? 1 : (m + 1), 1, 0);
			_endTime = ht.value();
			_endWY = (m < 10) ? y : (y + 1);
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
	 * @see gov.ca.water.calgui.busservice.impl.IDSSGrabber1Svc#getBaseRunName()
	 */
	@Override
	public String getBaseRunName()
	{
		return _baseScenarioRun.getName();
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see
	 * gov.ca.water.calgui.busservice.impl.IDSSGrabber1Svc#setScenarioRuns(java.lang.
	 * String)
	 */
	@Override
	public void setScenarioRuns(EpptScenarioRun scenarioRun, List<EpptScenarioRun> alternatives)
	{
		_baseScenarioRun = scenarioRun;
		_alternatives.clear();
		_alternatives.addAll(alternatives);
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see
	 * gov.ca.water.calgui.busservice.impl.IDSSGrabber1Svc#setLocation(java.
	 * lang.String)
	 */
	@Override
	public void setLocation(String locationName)
	{

		locationName = locationName.trim();

		if(locationName.startsWith("/"))
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

			GUILinksAllModelsBO guiLinksAllModelsBO = _seedDataSvc.getObjById(locationName);
			if(guiLinksAllModelsBO != null)
			{
				_primaryDSSName.clear();
				_primaryDSSName.putAll(guiLinksAllModelsBO.getPrimary());
				_thresholdDSSName.clear();
				_secondaryDSSName.clear();
				_secondaryDSSName.putAll(guiLinksAllModelsBO.getSecondary());
				_axisLabel = guiLinksAllModelsBO.getPlotAxisLabel();
				_plotTitle = guiLinksAllModelsBO.getPlotTitle();
				_legend = guiLinksAllModelsBO.getLegend();
			}
		}
	}

	public void setThresholdId(int id)
	{

		ThresholdLinksBO objById = _thresholdLinksSeedDataSvc.getObjById(id);
		if(objById != null)
		{
			_threshold = objById;
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
	 * @param dssPath name of HEC DSS file from which to read results
	 * @param dssName name(s) of dataset(s) to read from HEC DSS file. Multiple
	 *                dataset names can be specified - separated by "+"; the dataset
	 *                results will be read and summed. if the first data set has the
	 *                suffix (-1), the results read will be shifted one month
	 *                earlier.
	 * @return HEC TimeSeriesContainer with times, values, number of values, and
	 * file name.
	 */
	protected TimeSeriesContainer getOneSeries(NamedDssPath dssPath, String dssName, GUILinksAllModelsBO.Model model)
	{

		TimeSeriesContainer result = null;

		HecDataManager hecDataManager = new HecDataManager();
		HecDss hecDss = null;
		try
		{
			hecDss = HecDss.open(dssPath.getDssPath().toString());
			hecDataManager.setDSSFileName(dssPath.getDssPath().toString());


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
			for(int i = 0; i < dssNames1.length; i++)
			{
				String[] nameParts = dssNames1[i].split("/");
				if(nameParts.length < 2)
				{
					dssNames[i] = "MISSING_B/OR_CPART";
				}
				else
				{
					dssNames[i] = dssNames1[i].trim();
				}
			}

			String hecAPart = dssPath.getAPart();
			String hecEPart = dssPath.getEPart();
			String hecFPart = dssPath.getFPart();
			String firstPath =  "/" + hecAPart + "/" + dssNames[0] + "//" + hecEPart + "/" + hecFPart + "/";

			result = (TimeSeriesContainer) hecDss.get(firstPath, true);
			LOGGER.fine(firstPath);
			if((result == null) || (result.numberValues < 1))
			{

				String message;
				result = null;

				if(!clsIsDynamicSJR(dssPath.getDssPath().toString()) && (("S_MELON/STORAGE".equals(dssNames[0]))
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

					message = " Could not find " + dssNames[0] + " in " + dssPath.getDssPath()
							+ ".\n The selected scenario was not run using dynamic SJR simulation;";

				}

				else if(!clsAntiochChipps(dssPath.getDssPath().toString())
						&& (("AN_EC_STD/SALINITY".equals(dssNames[0])) || ("CH_EC_STD/SALINITY".equals(
						dssNames[0]))))
				{

					message = " Could not find " + dssNames[0] + " in " + dssPath.getDssPath()
							+ ".\n The selected scenario was not run with D-1485 Fish and Wildlife (at Antioch and Chipps) regulations.";
				}

				else if(!clsLVE(dssPath.getDssPath().toString()) && (("S422/STORAGE".equals(dssNames[0]))
						|| ("WQ408_OR_/SALINITY".equals(dssNames[0])) || ("WQ408_VC_/SALINITY".equals(dssNames[0]))
						|| ("WQ408_RS_/SALINITY".equals(dssNames[0]))
						|| ("C422_FILL_CC/FLOW-CHANNEL".equals(dssNames[0]))
						|| ("D420/FLOW-DELIVERY".equals(dssNames[0])) || ("D408_OR/FLOW-DELIVERY".equals(
						dssNames[0]))
						|| ("D408_VC/FLOW-DELIVERY".equals(dssNames[0]))
						|| ("D408_RS/FLOW-DELIVERY".equals(dssNames[0])) || ("WQ420/SALINITY".equals(dssNames[0]))))
				{

					message = "Could not find " + dssNames[0] + " in " + dssPath.getDssPath()
							+ ".\n The selected scenario was not run with Los Vaqueros Enlargement.";
				}

				else
				{
					message = "Could not find " + dssNames[0] + " in " + dssPath.getDssPath() + " - attempted to retrieve path: " + firstPath;
				}
			}
			else
			{

				// If no error, add results from other datasets in dssNames
				// array.

				for(int i = 1; i < dssNames.length; i++)
				{

					// TODO: Note hard-coded D- and E-PART
					String pathName = "/" + hecAPart + "/" + dssNames[i] + "//" + dssPath.getEPart() + "/" + hecFPart + "/";
					TimeSeriesContainer result2 = (TimeSeriesContainer) hecDss
							.get(pathName, true);
					if(result2 == null || result2.numberValues < 1)
					{
						result2 = null;
						String message = String.format("Could not find %s in %s - attempted to retrieve path: %s",
								dssNames[0], dssPath.getDssPath(), pathName);
						if(_stopOnMissing)
						{
							JOptionPane.showMessageDialog(null, message, "Error", JOptionPane.ERROR_MESSAGE);
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
		catch(Exception ex)
		{
			String messageText;
			if(ex.getMessage() != null && ex.getMessage().contains("Unable to recognize record"))
			{
				messageText = "Could not find record - HEC message was '" + ex.getMessage() + "'";
				LOGGER.log(Level.FINE, messageText, ex);
			}
			else
			{
				messageText = "Unable to get time series." + ex.getMessage();
				LOGGER.log(Level.SEVERE, messageText, ex);
			}
		}
		finally
		{
			if(hecDss != null)
			{
//				hecDss.close();
			}
		}

		// Store name portion of DSS file in TimeSeriesContainer

		if(result != null)
		{
			result.fileName = dssPath.getAliasName() + " (" + model + ")";
			result.fullName = dssPath.getAliasName() + " (" + model + ")";
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
		else if(_baseScenarioRun == null)
		{
			result = "Base scenario is not set in DSSGrabber.";
		}
		else if(_primaryDSSName.isEmpty())
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
	public TimeSeriesContainer[] getPrimarySeries()
	{

		TimeSeriesContainer[] results = null;

		try
		{
			checkReadiness();

			// Store number of scenarios
			results = new TimeSeriesContainer[_alternatives.size() + 1];

			GUILinksAllModelsBO.Model baseModel = _baseScenarioRun.getModel();
			String baseDssPathName = _primaryDSSName.get(baseModel);
			if(baseDssPathName != null)
			{
				// Base first
				EpptDssContainer dssContainer = _baseScenarioRun.getDssContainer();
				TimeSeriesContainer oneSeries = getOneTimeSeriesFromAllModels(dssContainer,
						_baseScenarioRun.getModel(), baseDssPathName);
				results[0] = oneSeries;
				if(results[0] != null)
				{
					_originalUnits = results[0].units;
				}

				// Then scenarios

				for(int i = 0; i < _alternatives.size(); i++)
				{
					EpptScenarioRun epptScenarioRun = _alternatives.get(i);
					TimeSeriesContainer tsc = null;

					GUILinksAllModelsBO.Model altModel = epptScenarioRun.getModel();
					String altDssPathName = _primaryDSSName.get(altModel);
					if(altDssPathName != null)
					{
						tsc = getOneTimeSeriesFromAllModels(epptScenarioRun.getDssContainer(),
								epptScenarioRun.getModel(), altDssPathName);
						results[i + 1] = tsc;
					}
				}
			}
			else
			{
				LOGGER.log(Level.WARNING, "No matching DV GUI Links record in Model: {0} because the path is null",
						new Object[]{baseModel});
			}
		}
		catch(RuntimeException ex)
		{
			LOGGER.log(Level.SEVERE, "Unable to get time series.", ex);
		}

		return results;
	}

	private TimeSeriesContainer getOneTimeSeriesFromAllModels(EpptDssContainer dssContainer,
															  GUILinksAllModelsBO.Model model,
															  String dssPathName)
	{
		TimeSeriesContainer oneSeries = null;
		Optional<TimeSeriesContainer> timeSeriesContainerOptional = dssContainer.getAllDssFiles().stream()
																				.filter(Objects::nonNull)
																				.map(p -> getOneSeries(p, dssPathName,
																						model))
																				.filter(Objects::nonNull)
																				.findFirst();
		if(timeSeriesContainerOptional.isPresent())
		{
			oneSeries = timeSeriesContainerOptional.get();
		}
		else
		{
			String messageText = "Could not find record - " + dssPathName;
			List<String> messages = _missingDSSRecords.computeIfAbsent(model, m -> new ArrayList<>());
			messages.add(messageText);
		}

		return oneSeries;
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
		TimeSeriesContainer[] results = null;
		if(!_secondaryDSSName.isEmpty())
		{
			List<TimeSeriesContainer> timeSeriesContainers = new ArrayList<>();
			GUILinksAllModelsBO.Model baseModel = _baseScenarioRun.getModel();
			String baseDssPathName = _secondaryDSSName.get(baseModel);
			if(baseDssPathName != null)
			{
				for(String splitPathname : baseDssPathName.split("\\|"))
				{
					// Base first
					TimeSeriesContainer oneSeries = getOneTimeSeriesFromAllModels(_baseScenarioRun.getDssContainer(),
							baseModel, splitPathname);
					timeSeriesContainers.add(oneSeries);
					// Then scenarios

				}
				for(EpptScenarioRun epptScenarioRun : _alternatives)
				{
					GUILinksAllModelsBO.Model altModel = epptScenarioRun.getModel();
					String altDssPathName = _secondaryDSSName.get(altModel);
					if(altDssPathName != null)
					{
						for(String splitPathname : altDssPathName.split("\\|"))
						{
							TimeSeriesContainer tsc = getOneTimeSeriesFromAllModels(epptScenarioRun.getDssContainer(),
									epptScenarioRun.getModel(), splitPathname);
							timeSeriesContainers.add(tsc);
						}
					}
				}
				results = timeSeriesContainers.toArray(new TimeSeriesContainer[0]);
			}
			else
			{
				LOGGER.log(Level.WARNING, "No matching SV GUI Links record in for Model: {0} with path: {1}",
						new Object[]{baseModel, baseDssPathName});
			}
		}
		return results;
	}

	public TimeSeriesContainer[] getThresholdTimeSeries()
	{

		TimeSeriesContainer[] results = null;

		try
		{
			checkReadiness();

			// Store number of scenarios
			results = new TimeSeriesContainer[_alternatives.size() + 1];
			GUILinksAllModelsBO.Model baseModel = _baseScenarioRun.getModel();
			ThresholdLinksBO.ModelData modelData = _threshold.getModelData(baseModel);
			if(modelData != null)
			{
				String baseDssPathName = modelData.getPrimary();
				// Base first
				EpptDssContainer dssContainer = _baseScenarioRun.getDssContainer();
				TimeSeriesContainer oneSeries = getOneTimeSeriesFromAllModels(dssContainer,
						_baseScenarioRun.getModel(), baseDssPathName);
				results[0] = oneSeries;

				// Then scenarios

				for(int i = 0; i < _alternatives.size(); i++)
				{
					EpptScenarioRun epptScenarioRun = _alternatives.get(i);
					TimeSeriesContainer tsc = null;

					GUILinksAllModelsBO.Model altModel = epptScenarioRun.getModel();
					ThresholdLinksBO.ModelData altModelData = _threshold.getModelData(altModel);
					if(altModelData != null)
					{
						String altDssPathName = altModelData.getPrimary();

						tsc = getOneTimeSeriesFromAllModels(epptScenarioRun.getDssContainer(),
								epptScenarioRun.getModel(), altDssPathName);
					}
					results[i + 1] = tsc;
				}
			}
			else
			{
				LOGGER.log(Level.WARNING, "No matching Threshold Links record in for Model: {0} with path: {1}",
						new Object[]{baseModel, baseModel});
			}
		}
		catch(RuntimeException ex)
		{
			LOGGER.log(Level.SEVERE, "Unable to get time series.", ex);
		}

		return results;
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

		TimeSeriesContainer[] results = new TimeSeriesContainer[_alternatives.size()];
		if(timeSeriesResults[0] != null)
		{
			for(int i = 0; i < _alternatives.size(); i++)
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
			boolean valid = timeSeriesResults != null && isValid(timeSeriesResults);
			if(!valid)
			{
				results = null;
			}
			else
			{
				results = new TimeSeriesContainer[14][_alternatives.size() + 1];

				for(int month = 0; month < 14; month++)
				{

					HecTime ht = new HecTime();
					for(int i = 0; i < _alternatives.size() + 1; i++)
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

	private boolean isValid(TimeSeriesContainer[] timeSeriesResults)
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
		return valid;
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

		TimeSeriesContainer[][] results = null;
		try
		{
			boolean valid = timeSeriesResults != null && isValid(timeSeriesResults);
			if(valid)
			{
				results = new TimeSeriesContainer[14][_alternatives.size()];

				for(int month = 0; month < 14; month++)
				{

					HecTime ht = new HecTime();
					for(int i = 0; i < _alternatives.size(); i++)
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
		return results;
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
