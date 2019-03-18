/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */

package gov.ca.hec;

import java.util.Vector;
import java.util.regex.Pattern;

import hec.heclib.dss.CondensedReference;
import hec.heclib.dss.DSSPathname;
import hec.heclib.dss.HecDss;
import hec.heclib.util.HecTime;
import hec.hecmath.HecMath;
import hec.hecmath.HecMathException;
import hec.hecmath.TimeSeriesMath;
import hec.io.DataContainer;
import hec.io.TimeSeriesContainer;

/**
 * A few utility functions to access HECDSSVue functionality
 * 
 * @author psandhu
 *
 */
public class HecUtils {
	/**
	 * opens a dss files only if it exists, else fails
	 * 
	 * @throws Exception
	 */
	public static HecDss openDSS(String dssfile) throws Exception {
		return HecDss.open(dssfile, true);
	}

	/**
	 * opens a dss files only if it exists, else fails
	 * 
	 * @throws Exception
	 */
	public static HecDss openDSS(String dssfile, boolean existing) throws Exception {
		return HecDss.open(dssfile, existing);
	}

	/**
	 * Takes a handle and closes it
	 * 
	 * @param dss
	 */
	public static void closeDSS(HecDss dss) {
		dss.close();
	}

	/**
	 * Takes a dss handle (previously opened by call to open_dss and a time
	 * window defined by strings in ddMMMyyyy HHmm format (start time and end
	 * time) Note: setting these windows before accessing dss file can lead to
	 * some odd truncation of data.
	 */
	public static void setWorkingTimewindow(HecDss dss, String stime, String etime) {
		dss.setTimeWindow(stime, etime);
	}

	/**
	 * Creates a pattern string for the get_matching function of the form [part
	 * letter A|B|C|D|E|F]=<string to match> [space] from a pathstr of the
	 * format [/[part string to match| empty] repeated for A-F E.g.
	 * /OBS/MTZ/STAGE/1HOUR//SENSOR-36/ would be transformed to A=OBS B=MTZ
	 * C=STAGE D=1HOUR F=SENSOR-36. Note that E part is skipped as it is empty
	 * 
	 * @return
	 */
	public static String createPathPattern(String pathstr) {
		String[] fields = pathstr.split("/");
		String[] parts = { "A", "B", "C", "D", "E", "F" };
		String pattern = "";
		for (int part = 0; part < parts.length; part++) {
			String partVal = fields[part + 1].trim();
			if (partVal.length() == 0)
				continue;
			pattern = pattern + parts[part] + "=" + partVal + " ";
		}
		return pattern.trim();
	}

	/**
	 * Takes a dss handle and a pattern in format of "([part letter
	 * (A|B|C|D|E|F)]=<string to match> [space])*" e.g. get_matching(obs,'A=OBS
	 * C=MTZ E=15MIN')
	 * 
	 * Fails by return None and printing message of failure.
	 * 
	 * @throws Exception
	 */
	public static DataContainer getMatching(HecDss dss, String pattern) throws Exception {
		@SuppressWarnings("unchecked")
		Vector<String> matches = dss.getCatalogedPathnames(pattern);
		if (matches.size() >= 1) {
			return dss.get(matches.get(0));
		} else {
			System.err.println("No match for: " + pattern + ", " + matches);
			return null;
		}
	}

	/**
	 * Calculate root mean square error between the two time series and return the value.
	 * @throws HecMathException
	 * 
	 */
	public static double calculateRMS(TimeSeriesContainer model, TimeSeriesContainer obs) throws HecMathException {
		TimeSeriesMath modelt = new TimeSeriesMath(model);
		TimeSeriesMath obst = new TimeSeriesMath(obs);
		HecMath diff = modelt.subtract(obst);
		return Math.sqrt(diff.multiply(diff).sum() / diff.numberValidValues());
	}

	/**
	 * Calculate Nash Sutcliffe efficiency between the two time series.
	 * <a href="https://en.wikipedia.org/wiki/Nash%E2%80%93Sutcliffe_model_efficiency_coefficient">Nash Sutcliffe Efficiency Coefficient</a>
	 * 
	 */
	public static double calculateNashSutcliffeEfficiency(TimeSeriesContainer model, TimeSeriesContainer obs)
			throws HecMathException {
		TimeSeriesMath modelt = new TimeSeriesMath(model);
		TimeSeriesMath obst = new TimeSeriesMath(obs);
		double tavg = obst.abs().sum() / obst.numberValidValues();
		HecMath diffModel = modelt.subtract(obst);
		HecMath diffObs = obst.subtract(tavg);
		return 1 - (diffModel.multiply(diffModel)).sum() / (diffObs.multiply(diffObs)).sum();
	}

	/**
	 *  For the open dss handle and dss path and time window string return the time series container for the request
	 */
	public static DataContainer getMatching(HecDss dss, String dsspath, String twStr) throws Exception {
		String dsspathRegEx = searchRegex(replaceAndEscape(dsspath), dss);
		if (dsspathRegEx == null) {
			return null;
		}
		String startTime = null;
		String endTime = null;
		if (twStr == null || twStr.length() == 0) {
			HecTime start = new HecTime();
			HecTime end = new HecTime();
			dss.getTimeSeriesExtents(dsspathRegEx, start, end);
			startTime = start.dateAndTime();
			endTime = end.dateAndTime();
		} else {
			String[] twFields = twStr.split(" ");
			startTime = twFields[0] + " " + twFields[1];
			endTime = twFields[2] + " " + twFields[3];
		}
		DataContainer dataContainer = dss.get(dsspathRegEx, startTime, endTime);
		return dataContainer;
	}

	/**
	 * parse time window from dss path. This is the format that the condensed catalog from HECDSS files of the form 
	 * 01JAN1990 2400-05JAN1992 0200 
	 */
	public static String parseTimeWindowFromPath(String dsspath) {
		DSSPathname pathname = new DSSPathname(dsspath);
		String[] fields = pathname.getDPart().split(" - ");
		return fields[0] + " 0000 " + fields[1] + " 0000";
	}

	/**
	 * Search for regular expression in the dss condensed catalog. 
	 * @ return first pathname that matches or null
	 */
	public static String searchRegex(String regpath, HecDss dss) {
		Vector<CondensedReference> condensedCatalog = dss.getCondensedCatalog();
		Pattern p = Pattern.compile(regpath, Pattern.CASE_INSENSITIVE);
		for (CondensedReference condensedReference : condensedCatalog) {
			String pathname = condensedReference.getNominalPathname();
			if (p.matcher(pathname).matches()) {
				return pathname;
			}
		}
		return null;
	}
	
	/**
	 * Escaping certain characters in a regular expression for the dss pathname and returning as a string
	 */
	public static String replaceAndEscape(String dsspath) {
		if (dsspath == null)
			return ".*";
		String[] fields = dsspath.split("/");
		StringBuilder sb = new StringBuilder();
		int slashCount = 0;
		for (String f : fields) {
			slashCount++;
			if (slashCount == 1) {
				sb.append("/");
				continue;
			}
			if (f.length() == 0) {
				sb.append(".*");
			} else {
				sb.append(Pattern.quote(f));
			}
			sb.append("/");
		}
		while (slashCount < 7) {
			sb.append(".*/");
			slashCount++;
		}
		return sb.toString();
	}

	/**
	 * Average the time series for the given interval (as a string) and return time series
	 * @param dataContainer
	 * @param intervalAsString
	 *            e.g., "1DAY" for daily averaging
	 * @return
	 * @throws HecMathException
	 */
	public static DataContainer average(DataContainer dataContainer, String intervalAsString) throws HecMathException {
		return new TimeSeriesMath((TimeSeriesContainer) dataContainer)
				.transformTimeSeries(intervalAsString, null, "AVE").getData();
	}

}
