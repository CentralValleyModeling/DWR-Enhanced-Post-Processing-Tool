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

package gov.ca.water.calgui.busservice.impl;

//! Base DSS file access service

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.time.LocalDate;
import java.time.Month;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Properties;
import java.util.Scanner;
import java.util.logging.Level;
import java.util.logging.Logger;

import gov.ca.water.calgui.bo.DetailedIssue;
import gov.ca.water.calgui.bo.GUILinksAllModelsBO;
import gov.ca.water.calgui.bo.ThresholdLinksBO;
import gov.ca.water.calgui.busservice.IDSSGrabber1Svc;
import gov.ca.water.calgui.constant.Constant;
import gov.ca.water.calgui.project.EpptDssContainer;
import gov.ca.water.calgui.project.EpptScenarioRun;
import gov.ca.water.calgui.project.NamedDssPath;

import hec.heclib.dss.DSSPathname;
import hec.heclib.dss.HecDataManager;
import hec.heclib.dss.HecDss;
import hec.heclib.util.HecTime;
import hec.heclib.util.HecTimeArray;
import hec.hecmath.computation.r;
import hec.hecmath.functions.TimeSeriesFunctions;
import hec.io.TimeSeriesContainer;
import rma.util.RMAConst;

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

	public static final double CFS_2_TAF_DAY = 0.001983471;
	private static Logger LOGGER = Logger.getLogger(DSSGrabber1SvcImpl.class.getName());
	final Map<GUILinksAllModelsBO.Model, String> _primaryDSSName = new HashMap<>();
	final Map<GUILinksAllModelsBO.Model, String> _secondaryDSSName = new HashMap<>();
	final List<EpptScenarioRun> _alternatives = new ArrayList<>();
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
	EpptScenarioRun _baseScenarioRun;
	// Copy of original units
	// Start and end time of interest
	private HecTime _startTime;
	private HecTime _endTime;

	public DSSGrabber1SvcImpl()
	{
		String propertiesFile = "callite-gui.properties";
		try
		{
			Properties properties = new Properties();
			properties.load(ModelRunSvcImpl.class.getClassLoader().getResourceAsStream(propertiesFile));
		}
		catch(IOException | RuntimeException ex)
		{
			LOGGER.log(Level.WARNING, "Unable to read properties file from: " + propertiesFile, ex);
		}
		clearMissingList();
		setDateRange(LocalDate.of(1850, java.time.Month.JANUARY, 1),
				LocalDate.of(2150, Month.JANUARY, 1));
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

			int month = startMonth.getMonth().getValue();
			int year = startMonth.getYear();
			int dayOfMonth = startMonth.getDayOfMonth();
			ht.setYearMonthDay(year, month, dayOfMonth, 0);
			_startTime = new HecTime(ht);
			month = endMonth.getMonth().getValue();
			year = endMonth.getYear();
			dayOfMonth = endMonth.getDayOfMonth();
			ht.setYearMonthDay(year, month, dayOfMonth, 0);
			_endTime = new HecTime(ht);
		}
		catch(RuntimeException ex)
		{
			LOGGER.log(Level.WARNING, ex.getMessage(), ex);
		}

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

	@Override
	public void setDtsLink(DetailedIssue dtsLink)
	{
		if(dtsLink != null)
		{
			_primaryDSSName.clear();
			GUILinksAllModelsBO.Model.values().forEach(m -> _primaryDSSName.put(m, dtsLink.getLinkedVar()));
			_secondaryDSSName.clear();
			_axisLabel = "";
			_plotTitle = dtsLink.getTitle();
			_legend = dtsLink.getLinkedVar();
		}
	}

	@Override
	public void setGuiLink(GUILinksAllModelsBO guiLinksAllModelsBO)
	{
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

	@Override
	public void setThresholdLink(ThresholdLinksBO objById)
	{
		if(objById != null)
		{
			_primaryDSSName.clear();
			GUILinksAllModelsBO.Model.values()
									 .stream()
									 .map(objById::getModelData)
									 .filter(Objects::nonNull)
									 .forEach(d -> _primaryDSSName.put(d.getModel(), d.getPrimary()));
			_secondaryDSSName.clear();
			_axisLabel = objById.getLabel();
			_plotTitle = "";
			_legend = "";
		}
	}



	public static TimeSeriesContainer diffSeries(TimeSeriesContainer baseSeries, TimeSeriesContainer primarySeries)
	{
		TimeSeriesContainer retval = null;
		if(baseSeries != null && primarySeries != null)
		{
			retval = new TimeSeriesContainer();
			baseSeries.clone(retval);
			HecTimeArray times = retval.getTimes();
			double[] values = new double[times.numberElements()];
			for(int i = 0;i < times.numberElements(); i++)
			{
				HecTime time = times.timeArray()[i];
				double baseValue = baseSeries.getValue(time);
				double altValue = primarySeries.getValue(time);
				if(Constant.isValidValue(altValue) && Constant.isValidValue(baseValue))
				{
					values[i] = altValue - baseValue;
				}
				else
				{
					values[i] = RMAConst.UNDEF_DOUBLE;
				}
			}
			retval.setValues(values);
			retval.setFullName("Diff " + primarySeries.getFullName() + " <br>from " + baseSeries.getFullName());
		}
		return retval;
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
			String file = dssPath.getDssPath().toString();
			if(HecDataManager.doesDSSFileExist(file))
			{
				hecDss = HecDss.open(file);
				hecDataManager.setDSSFileName(file);


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

				boolean missingBOrCPart = false;
				String[] dssNames = new String[dssNames1.length];
				for(int i = 0; i < dssNames1.length; i++)
				{
					String[] nameParts = dssNames1[i].split("/");
					if(nameParts.length < 2)
					{
						dssNames[i] = nameParts[0] + "/*";
						missingBOrCPart = true;
					}
					else
					{
						dssNames[i] = dssNames1[i].trim();
					}
				}
				String hecAPart = dssPath.getAPart();
				String hecEPart = dssPath.getEPart();
				String hecFPart = dssPath.getFPart();
				String firstPath = "/" + hecAPart + "/" + dssNames[0] + "//" + hecEPart + "/" + hecFPart + "/";

				if(!missingBOrCPart)
				{
					result = (TimeSeriesContainer) hecDss.get(firstPath, _startTime.toString(), _endTime.toString());
					DssPatternUpdater.updateTimeSeriesContainer(result);
				}
				String message = null;
				if((result == null) || (result.numberValues < 1))
				{

					result = null;

					if(!clsIsDynamicSJR(file) && (("S_MELON/STORAGE".equals(dssNames[0]))
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

					else if(!clsAntiochChipps(file)
							&& (("AN_EC_STD/SALINITY".equals(dssNames[0])) || ("CH_EC_STD/SALINITY".equals(
							dssNames[0]))))
					{

						message = " Could not find " + dssNames[0] + " in " + dssPath.getDssPath()
								+ ".\n The selected scenario was not run with D-1485 Fish and Wildlife (at Antioch and Chipps) regulations.";
					}

					else if(!clsLVE(file) && (("S422/STORAGE".equals(dssNames[0]))
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
						TimeSeriesContainer result2 = (TimeSeriesContainer) hecDss.get(pathName, true);
						DssPatternUpdater.updateTimeSeriesContainer(result2);
						if(result2 == null || result2.numberValues < 1)
						{
							message = String.format("Could not find %s in %s - attempted to retrieve path: %s",
									dssNames[0], dssPath.getDssPath(), pathName);
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
				if(message != null)
				{
					LOGGER.log(Level.FINER, message);
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

		// Store name portion of DSS file in TimeSeriesContainer

		if(result != null)
		{
			//Only for secondary values - Monthly table depends on this name
			if(dssName.equalsIgnoreCase(_secondaryDSSName.get(model)))
			{
				result.supplementalInfo = _legend;
			}
			result.fileName = dssPath.getAliasName() + " (" + model + ")";
			result.fullName = dssPath.getAliasName() + " (" + model + ")";
		}

		return result;
	}

	void checkReadiness()
	{
		String result = null;
		if(_startTime != null || _endTime != null)
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

		TimeSeriesContainer[] results = new TimeSeriesContainer[_alternatives.size() + 1];

		try
		{
			checkReadiness();

			GUILinksAllModelsBO.Model baseModel = _baseScenarioRun.getModel();
			String baseDssPathName = _primaryDSSName.get(baseModel);
			if(baseDssPathName != null)
			{
				// Base first
				TimeSeriesContainer oneSeries = getOneTimeSeriesFromAllModels(_baseScenarioRun, baseDssPathName);
				results[0] = oneSeries;
				if(results[0] != null)
				{
					_originalUnits = results[0].units;
				}

				// Then scenarios

				for(int i = 0; i < _alternatives.size(); i++)
				{
					EpptScenarioRun epptScenarioRun = _alternatives.get(i);

					GUILinksAllModelsBO.Model altModel = epptScenarioRun.getModel();
					String altDssPathName = _primaryDSSName.get(altModel);
					if(altDssPathName != null)
					{
						TimeSeriesContainer tsc = getOneTimeSeriesFromAllModels(epptScenarioRun, altDssPathName);
						results[i + 1] = tsc;
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

	private TimeSeriesContainer getOneTimeSeriesFromAllModels(EpptScenarioRun epptScenarioRun, String dssPathName)
	{
		GUILinksAllModelsBO.Model model = _baseScenarioRun.getModel();
		TimeSeriesContainer oneSeries = null;
		EpptDssContainer dssContainer = epptScenarioRun.getDssContainer();
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
			String messageText = "Could not find record for scenario run: " + epptScenarioRun + " - " + dssPathName;
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
		TimeSeriesContainer[] results = new TimeSeriesContainer[_alternatives.size() + 1];
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
					TimeSeriesContainer oneSeries = getOneTimeSeriesFromAllModels(_baseScenarioRun, splitPathname);
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
							TimeSeriesContainer tsc = getOneTimeSeriesFromAllModels(epptScenarioRun, splitPathname);
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

	public void setDssPathname(DSSPathname value)
	{
		_primaryDSSName.clear();
		GUILinksAllModelsBO.Model.values().forEach(m->_primaryDSSName.put(m, value.getBPart() + "/" + value.getCPart()));
		_secondaryDSSName.clear();
		_axisLabel = "";
		_plotTitle = "";
		_legend = "";
	}
}
