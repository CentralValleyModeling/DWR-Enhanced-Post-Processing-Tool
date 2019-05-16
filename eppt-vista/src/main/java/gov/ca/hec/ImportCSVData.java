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

import java.io.File;
import java.io.FileReader;
import java.io.FilenameFilter;
import java.io.LineNumberReader;
import java.util.ArrayList;

import hec.heclib.dss.HecDss;
import hec.heclib.util.HecTime;
import hec.io.TimeSeriesContainer;

/**
 * Import data from d:/wk-ann/anns from *.csv files into dss
 *
 * @author psandhu
 */
public class ImportCSVData
{
	public static void main(String[] args) throws Exception
	{
		String dir = "d:/wk-ann/anns/";
		String dssFilename = dir + "import_csv.dss";
		HecDss hdss = HecDss.open(dssFilename);
		File[] csvFiles = new File(dir).listFiles(new FilenameFilter()
		{

			@Override
			public boolean accept(File dir, String name)
			{
				return name.toLowerCase().endsWith(".csv");
			}
		});
		for(File f : csvFiles)
		{
			TimeSeriesContainer[] atsc = readTimeSeries(f);
			// 1st time series is target, 2nd is prediction
			if(atsc.length > 1)
			{
				TimeSeriesContainer tsc = atsc[1];
				tsc.fileName = dssFilename;
				String fpart = f.getName().split("\\.")[0];
				tsc.fullName = "/RNN/RSAC054/EC//1DAY/" + fpart + "/";
				//tsc.type="PER-AVER";
				tsc.type = "INST-VAL";
				tsc.interval = 24 * 60; // 1DAY
				String startTimeString = guessFromFilename(f.getName());
				tsc.startTime = new HecTime(startTimeString, "2400").value();
				tsc.numberValues = tsc.values.length;
				tsc.times = new int[tsc.numberValues];
				for(int i = 0; i < tsc.times.length; i++)
				{
					tsc.times[i] = tsc.startTime + (i * tsc.interval);
				}
				tsc.units = "UMHOS/CM";
				hdss.write(tsc);
			}
			else
			{
				System.err.println("No predictions found, 2nd series is missing in csv file: " + f.getAbsolutePath());
			}
		}

	}

	/**
	 * Guess start time from filename. If it ends with train -> 03MAR1990 if it ends with test -> 28NOV2011
	 *
	 * @param name
	 * @return
	 */
	private static String guessFromFilename(String name)
	{
		String basename = name.split(".csv")[0];
		basename = basename.toLowerCase();
		if(basename.endsWith("train") || basename.startsWith("train"))
		{
			return "03MAR1990";
		}
		else if(basename.endsWith("test") || basename.startsWith("test"))
		{
			return "28NOV2011";
		}
		else
		{
			System.err.println("Filename: " + name + " does not end with train or test! Unknown start time? Guessing 03MAR1990");
			return "03MAR1990";
		}
	}

	/**
	 * Reads csv file of column data values (separated by ",") into HEC TimeSeriesContainers. No assumptions of time interval, start time etc.
	 * Those values would be filled after these are returned from this method by the calling method.
	 *
	 * @param f
	 * @return
	 * @throws Exception
	 */
	public static TimeSeriesContainer[] readTimeSeries(File f) throws Exception
	{
		@SuppressWarnings("resource")
		LineNumberReader lnr = new LineNumberReader(new FileReader(f));
		String line = null;
		ArrayList<ArrayList<Double>> values = new ArrayList<ArrayList<Double>>();
		while((line = lnr.readLine()) != null)
		{
			String[] fields = line.split(",");
			if(values.size() < fields.length)
			{
				for(int i = values.size(); i < fields.length; i++)
				{
					values.add(new ArrayList<Double>());
				}
			}
			for(int i = 0; i < fields.length; i++)
			{
				values.get(i).add(new Double(fields[i]));
			}
		}
		TimeSeriesContainer[] atsc = new TimeSeriesContainer[values.size()];
		for(int i = 0; i < atsc.length; i++)
		{
			TimeSeriesContainer tsc = new TimeSeriesContainer();
			tsc.values = toPrimitiveDoubles(values.get(i));
			atsc[i] = tsc;
		}
		return atsc;
	}

	/**
	 * Fast way to convert Double array list to double[] primitive
	 *
	 * @param doubles
	 * @return
	 */
	public static double[] toPrimitiveDoubles(ArrayList<Double> doubles)
	{
		double[] target = new double[doubles.size()];
		for(int i = 0; i < target.length; i++)
		{
			target[i] = doubles.get(i).doubleValue();
		}
		return target;
	}
}
