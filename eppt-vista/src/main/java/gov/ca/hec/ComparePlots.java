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

package gov.ca.hec;

import java.io.FileReader;
import java.util.ArrayList;
import java.util.List;

import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;

import hec.gfx2d.G2dDialog;
import hec.heclib.dss.HecDss;
import hec.hecmath.HecMath;
import hec.hecmath.TimeSeriesMath;
import hec.io.DataContainer;
import hec.io.TimeSeriesContainer;
/**
 * Creates comparision plots using HEC DSSVue and instructions from a json file
 * <pre>
{
  "doDiffToFirst": "False",
  "doAverage": "False",
  "stime": "20JUL2009 0000",
  "etime": "22JUL2009 0000",
   "files": ["z:/somewhere/file1.dss","z:/somewhereelse/file2.dss","z:/anotherplace/file3.dss"],
   "plots":[ 
    { "title": "Nice 1", refs:["//BPLACE/SOMETYPE////", "//BPLACE/SOMETYPE////", "//BPLACE/SOMEOTHERTYPE////"]},
    .....<as many as you want plots
   ]
}
 * </pre>
 * @author psandhu
 *
 */
public class ComparePlots {

	public static void main(String[] args) throws Exception {
		if (args.length < 1){
			System.err.println("Usage: program file.json");
			System.exit(1);
		}
		JsonParser parser = new JsonParser();
		JsonObject top = parser.parse(new FileReader(args[0])).getAsJsonObject();
		// starttime and endtime in HEC Time format
		String stime = top.get("stime") == null ? null : top.get("stime").getAsString();
		String etime = top.get("etime") == null ? null : top.get("etime").getAsString();
		// Files corresponding to the pathnames in plotSet structure
		String[] files = convertToStringArray(top.get("files").getAsJsonArray());
		// a plot will be generated for each plotSet identified by a name and an
		// array of pathnames
		String[] titles = parseTitles(top.get("plots"));
		List<String[]> pathsArray = parsePathArray(top.get("plots"));
		boolean doAverage = top.get("doAverage").getAsBoolean();
		// set to True for differences and False for not
		boolean doDiffToFirst = top.get("doDiffToFirst").getAsBoolean();
		runCompare(files, pathsArray, titles, stime, etime, doAverage, doDiffToFirst);
	}

	private static List<String[]> parsePathArray(JsonElement jsonElement) {
		JsonArray plotsObj = jsonElement.getAsJsonArray();
		ArrayList<String[]> pathsArray = new ArrayList<String[]>();
		for(JsonElement e: plotsObj){
			if (!e.isJsonObject()){
				continue;
			}
			JsonArray jarray = e.getAsJsonObject().get("refs").getAsJsonArray();
			pathsArray.add(convertToStringArray(jarray));
		}
		return pathsArray;
	}

	private static String[] parseTitles(JsonElement jsonElement) {
		JsonArray plotsObj = jsonElement.getAsJsonArray();
		ArrayList<String> titles = new ArrayList<String>();
		for(JsonElement e: plotsObj){
			if (!e.isJsonObject()){
				continue;
			}
			titles.add(e.getAsJsonObject().get("title").getAsString());
		}
		String[] array = new String[titles.size()];
		return titles.toArray(array);
	}

	public static String[] convertToStringArray(JsonArray jArray) {
		ArrayList<String> list = new ArrayList<String>();
		for(JsonElement e: jArray){
			list.add(e.getAsString());
		}
		String[] array = new String[list.size()];
		return list.toArray(array);
	}

	public static G2dDialog doCompare(String[] paths, HecDss[] dssfiles, String twStr, String title, boolean doAverage,
			boolean diffToFirst) throws Exception {
		DataContainer[] data = new DataContainer[paths.length];
		for (int i = 0; i < paths.length; i++) {
			DataContainer d = HecUtils.getMatching(dssfiles[i], paths[i], twStr);
			if (doAverage) {
				d = HecUtils.average(d, "1DAY");
			}
			data[i] = d;
		}
		if (diffToFirst) {
			for (int i = 0; i < paths.length; i++) {
				HecMath diff = new TimeSeriesMath((TimeSeriesContainer) data[i])
						.subtract(new TimeSeriesMath((TimeSeriesContainer) data[0]));
				diff.getData().location = data[i].location + "-DIFF";
				data[i] = diff.getData();
			}
		}
		G2dDialog eplot = HecPlotUtils.plot(data, title);
		//eplot.maximize();
		return eplot;
	}

	public static void runCompare(String[] files, List<String[]> pathsArray, String[] titles, String stime, String etime,
			boolean doAverage, boolean doDiffToFirst) throws Exception {
		HecDss[] dssfiles = new HecDss[files.length];
		int count = 0;
		for (String f : files) {
			HecDss d = HecUtils.openDSS(f);
			// Do this after data retrieval is better? See doCompare function.
			// d.setTimeWindow(stime, etime);
			dssfiles[count++] = d;
		}
		String twStr = (stime == null || etime == null) ? null : stime + " " + etime;
		for(int i=0; i < titles.length; i++){
			String title = titles[i];
			String[] paths = pathsArray.get(i);
			doCompare(paths, dssfiles, twStr, title, doAverage, doDiffToFirst);
		}
		for (HecDss d : dssfiles) {
			HecUtils.closeDSS(d);
		}
		System.out.println("END");
	}

}
