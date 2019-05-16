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

package gov.ca.water.reportengine.filechanges;

import java.io.BufferedReader;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.HashSet;
import java.util.Set;
import java.util.Vector;
import java.util.logging.Level;

import com.google.common.flogger.FluentLogger;
import gov.ca.water.reportengine.EpptReportException;

import hec.heclib.dss.DSSPathname;
import hec.heclib.dss.HecDss;
import hec.io.TimeSeriesContainer;

public class AssumptionChangesDataProcessor
{
	private static final FluentLogger LOGGER = FluentLogger.forEnclosingClass();

	private static final int B_PART = 0;
	private static final int C_PART = 1;

	private final Set<DSSPathname> _csvMasterPathList;
	private final double _tolerance;


	public AssumptionChangesDataProcessor(Path csvPath, double tolerance) throws EpptReportException
	{
		_csvMasterPathList = loadMasterDSSPathList(csvPath);
		_tolerance = tolerance;
	}

	public AssumptionChangesStatistics processAssumptionChanges(Path basePath, Path altPath) throws EpptReportException
	{

		LOGGER.at(Level.INFO).log("Assumption Changes in base path: %s compared to: %s", basePath, altPath);
		LOGGER.at(Level.INFO).log("DSS Compare tolderance is: %s", _tolerance);
		HecDss baseFile = null;
		HecDss altFile = null;
		try
		{
			baseFile = HecDss.open(basePath.toString());
			altFile = HecDss.open(altPath.toString());

			return processAssumptionChangesStatistics(baseFile, altFile);


		}
		catch(EpptReportException epptException)
		{
			throw epptException;
		}
		catch(Exception e)
		{
			throw new EpptReportException("Error opening DSS file in assumption changes processor: " + "\n" + baseFile + "\n" + altFile, e);
		}
		finally
		{
			if(baseFile != null)
			{
				baseFile.close();
			}
			if(altFile != null)
			{
				altFile.close();
			}
		}


	}

	private AssumptionChangesStatistics processAssumptionChangesStatistics(HecDss baseFile, HecDss altFile) throws EpptReportException
	{
		Set<String> changes = new HashSet<>();
		Set<String> baseRecordsOnly = new HashSet<>();
		Set<String> altRecordsOnly = new HashSet<>();

		baseFile.getPathnameList();
		altFile.getPathnameList();
		for(DSSPathname pathName : _csvMasterPathList)
		{
			processDSSDifferences(changes, baseRecordsOnly, altRecordsOnly, baseFile, altFile, pathName);
		}

		return new AssumptionChangesStatistics(baseRecordsOnly, altRecordsOnly, changes);


	}

	private void processDSSDifferences(Set<String> changes, Set<String> baseRecordsOnly, Set<String> altRecordsOnly,
									   HecDss baseFile, HecDss altFile, DSSPathname pathFromMaster) throws EpptReportException
	{
		Vector baseCatalog = baseFile.getCatalogedPathnames(pathFromMaster.toString());
		boolean baseHasFile = !baseCatalog.isEmpty();

		Vector altCatalog = altFile.getCatalogedPathnames(pathFromMaster.toString());
		boolean altHasFile = !altCatalog.isEmpty();

		//if both have the file, then check if the are identical
		if(baseHasFile && altHasFile)
		{
			String baseDssPathname = baseCatalog.get(0).toString();
			if(!compareDSSData(baseFile, altFile, baseDssPathname))
			{
				changes.add(baseDssPathname);
			}
		}
		//add unique base files
		else if(baseHasFile && !altHasFile)
		{
			baseRecordsOnly.add(baseCatalog.get(0).toString());
		}
		//add unique alt files
		else if(altHasFile && !baseHasFile)
		{
			altRecordsOnly.add(altCatalog.get(0).toString());
		}
		else
		{
			LOGGER.at(Level.FINE).log("The base and the alternative did not contain the dss file: %s", pathFromMaster);
		}
	}


	private boolean compareDSSData(HecDss base, HecDss alt, String pathFromMaster) throws EpptReportException
	{
		boolean retval = false;
		try
		{

			TimeSeriesContainer baseContainer = (TimeSeriesContainer) base.get(pathFromMaster, true);
			double[] baseValues = baseContainer.values;
			int[] baseTimes = baseContainer.times;

			TimeSeriesContainer altContainer = (TimeSeriesContainer) alt.get(pathFromMaster, true);
			double[] altValues = altContainer.values;
			int[] altTimes = altContainer.times;

			if(baseValues.length == altValues.length && baseTimes.length == altTimes.length)
			{
				CompareDSSValues compare = new CompareDSSValues(_tolerance);
				if(compare.compareValues(baseValues, altValues))
				{
					if(compare.compareTimes(baseTimes, altTimes))
					{
						retval = true;
					}
				}
			}
		}
		catch(Exception e)
		{
			throw new EpptReportException("Error reading DSS record: " + pathFromMaster + ".\n" +
					"In files:" + "\n" + base.getFilename() + "\n" + alt.getFilename(), e);
		}

		return retval;
	}


	private Set<DSSPathname> loadMasterDSSPathList(Path dssCSVPath) throws EpptReportException
	{
		String line;
		String csvSplitBy = ",";
		Set<DSSPathname> dssPaths = new HashSet<>();

		try(BufferedReader br = Files.newBufferedReader(dssCSVPath))
		{

			//skip first line
			int i = 0;
			while((line = br.readLine()) != null)
			{
				if(i == 0)
				{
					i++;
					continue;
				}
				DSSPathname pathName = new DSSPathname();
				// use comma as separator
				String[] row = line.split(csvSplitBy);
				if(row.length < 2)
				{
					continue;
				}
				pathName.setAPart("*");
				pathName.setBPart(row[B_PART]);
				pathName.setCPart(row[C_PART]);
				pathName.setDPart("*");
				pathName.setEPart("*");
				pathName.setFPart("*");
				dssPaths.add(pathName);
			}
		}
		catch(IOException e)
		{
			throw new EpptReportException("Error generating assumption changes. Error reading the csv file: " + dssCSVPath, e);
		}
		return dssPaths;
	}
}
