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

import java.io.IOException;
import java.nio.file.AccessDeniedException;
import java.nio.file.Files;
import java.nio.file.NoSuchFileException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.attribute.FileTime;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import gov.ca.water.calgui.busservice.IMonitorSvc;
import gov.ca.water.calgui.constant.Constant;

/**
 * This class is used for getting the String for Monitor the process which is
 * done behind the seen.
 *
 * @author Mohan
 */
public final class MonitorSvcImpl implements IMonitorSvc
{

	private static final Logger LOG = Logger.getLogger(MonitorSvcImpl.class.getName());

	@Override
	public String save(Path scenarioName)
	{
		Path runDir = getRunDirectory(scenarioName);
		return scenarioName + " - Saving - " + lastLine(
				runDir.resolve(Constant.SAVE_FILE + Constant.TXT_EXT));

	}

	@Override
	public String runWSIDI(String scenarioName)
	{
		String scenPROGRESSFile = Constant.RUN_DETAILS_DIR + scenarioName + Constant.RUN_DIR + "//PROGRESS.txt";
		String scenWRESLCheckWSIDIFile = Constant.RUN_DETAILS_DIR + scenarioName + Constant.RUN_DIR
				+ "//=WreslCheck_main_wsidi=.log";
		String scenWSIDIIterationFile = Constant.RUN_DETAILS_DIR + scenarioName + Constant.RUN_DIR
				+ "//wsidi_iteration.log";
		String infoWSIDI;
		String line;
		if(Paths.get(scenWSIDIIterationFile).toFile().exists())
		{
			String lastLineForI = lastLine(Paths.get(scenWSIDIIterationFile));
			infoWSIDI = "(wsidi " + lastLineForI + ") ";
		}
		else
		{
			infoWSIDI = "(wsidi) ";
		}
		FileTime fileTime = null;
		try
		{
			fileTime = Files.getLastModifiedTime(Paths.get(scenPROGRESSFile));
		}
		catch(IOException ex)
		{
			LOG.log(Level.FINER, "Progress file does not yet exist", ex);
		}
		if(fileTime != null)
		{
			boolean progressFileExists = Paths.get(scenPROGRESSFile).toFile().exists();
			boolean timeoutNotExceeded = System.currentTimeMillis() - fileTime.toMillis() < 30000;
			if(progressFileExists
					&& timeoutNotExceeded)
			{
				line = lastLine(Paths.get(scenPROGRESSFile));
				line = progressString(line);
				return scenarioName + " " + infoWSIDI + " - " + line;
			}
		}

		line = lastLine(Paths.get(scenWRESLCheckWSIDIFile));
		if(line.endsWith("===================="))
		{
			line = lastButOneLine(Paths.get(scenWRESLCheckWSIDIFile));
		}
		line = parsingString(line);
		return scenarioName + " " + infoWSIDI + " - " + line;

	}

	@Override
	public String runModel(Path scenarioName)
	{
		Path runDir = getRunDirectory(scenarioName);
		Path scenPROGRESSFile = runDir.resolve("PROGRESS");
		Path scenWRESLCHECKFile = runDir.resolve("=WreslCheck_main=.log");
		String line;
		FileTime fileTime = null;
		try
		{
			fileTime = Files.getLastModifiedTime(scenPROGRESSFile);
		}
		catch(IOException ex)
		{
			// no need to handle because the file is not yet there.
			LOG.log(Level.FINER, "Progress file does not yet exist", ex);
		}
		if(fileTime != null)
		{
			if(scenPROGRESSFile.toFile().exists()
					&& (System.currentTimeMillis() - fileTime.toMillis() < 300_000L))
			{
				line = lastLine(scenPROGRESSFile);
				line = progressString(line);

				return scenarioName + " - " + line;
			}
		}
		else if(scenWRESLCHECKFile.toFile().exists())
		{
			line = lastLine(scenWRESLCHECKFile);
			if(line.endsWith("===================="))
			{
				line = lastButOneLine(scenWRESLCHECKFile);
			}
			line = parsingString(line);
			return scenarioName + " - " + line;
		}

		return scenarioName + " - " + "PENDING";

	}

	private Path getRunDirectory(Path scenarioName)
	{
		return scenarioName.getParent().resolve(Constant.RUN_DETAILS_DIR).resolve(
				scenarioName.getFileName().toString()).resolve(Constant.RUN_DIR);
	}

	/**
	 * This method will convert the batch string from the progrtss.txt file into
	 * the detail Message to display.
	 *
	 * @param value The string value.
	 * @return Will convert the batch string from the progrtss.txt file into the
	 * detail Message to display.
	 */
	private String progressString(String value)
	{
		if(value.contains("unopenable!"))
		{
			return "RUNNING - unable to read progress.txt";
		}
		if(value.contains("Empty!"))
		{
			return "RUNNING - run starting";
		}
		if(value.toUpperCase().contains("RUN COMPLETED"))
		{
			return "DONE - run completed";
		}
		if(value.contains("Run failed."))
		{
			return "DONE - run failed.";
		}
		else
		{
			String[] parts = value.split(" ");
			if(parts.length == 4)
			{
				try
				{
					int totalMonths = 12 * (Integer.parseInt(parts[1]) - Integer.parseInt(parts[0]));
					int months = Math.min(totalMonths, Integer.parseInt(parts[3])
							+ 12 * (Integer.parseInt(parts[2]) - Integer.parseInt(parts[0])));
					value = parts[3] + "/" + parts[2] + " (" + (100 * months / totalMonths) + "%)";
				}
				catch(NumberFormatException e)
				{
					value = "There is a error in formating the numbers.";
				}
			}
			return "RUNNING - " + value;
		}
	}

	/**
	 * This method will convert the batch string into the detail Message to
	 * display.
	 *
	 * @param value The string value.
	 * @return Will convert the batch string into the detail Message to display.
	 */
	private String parsingString(String value)
	{
		if(value.contains("unopenable!"))
		{
			return "PARSING - unable to read parsing log";
		}
		if(value.contains("Empty!"))
		{
			return "PARSING - parsing started";
		}
		if(!value.contains("Total errors:"))
		{
			return "PARSING - " + value;
		}
		else
		{
			return "PARSING - Parsing complete - " + value;
		}
	}

	/**
	 * This will open the file and read the last line and return it.
	 *
	 * @param fileName file name with whole path.
	 * @return Will return the last line.
	 */
	private String lastLine(Path fileName)
	{
		String value;
		try(Stream<String> stream = Files.lines(fileName))
		{
			List<String> list = stream.collect(Collectors.toList());
			value = list.get(list.size() - 1);
		}
		catch(NoSuchFileException ex)
		{
			value = "Waiting ... no progress file available";
			LOG.log(Level.FINE, value, ex);
		}
		catch(AccessDeniedException ex)
		{
			value = "Access denied for file " + fileName;
			LOG.log(Level.FINE, value, ex);
		}
		catch(IOException ex)
		{
			value = ex.getMessage();
			LOG.log(Level.FINE, value, ex);
		}
		return value;
	}

	/**
	 * This will open the file and read the last but one line and return it.
	 *
	 * @param fileName file name with whole path.
	 * @return This will return the last but one line.
	 */
	private String lastButOneLine(Path fileName)
	{
		String value;
		try(Stream<String> stream = Files.lines(fileName))
		{
			List<String> list = stream.collect(Collectors.toList());
			value = list.get(list.size() - 2);
		}
		catch(NoSuchFileException ex)
		{
			value = "Loading log file....";
			LOG.log(Level.FINE, value, ex);
		}
		catch(AccessDeniedException ex)
		{
			value = "The Access is denied for this file " + fileName;
			LOG.log(Level.FINE, value, ex);
		}
		catch(IOException ex)
		{
			value = ex.getMessage();
			LOG.log(Level.FINE, value, ex);
		}
		return value;
	}
}
