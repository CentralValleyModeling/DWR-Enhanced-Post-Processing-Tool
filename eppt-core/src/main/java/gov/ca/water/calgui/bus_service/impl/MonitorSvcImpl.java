/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */

package gov.ca.water.calgui.bus_service.impl;

import gov.ca.water.calgui.bus_service.IMonitorSvc;
import gov.ca.water.calgui.constant.Constant;
import org.apache.log4j.Logger;

import java.io.IOException;
import java.nio.file.AccessDeniedException;
import java.nio.file.Files;
import java.nio.file.NoSuchFileException;
import java.nio.file.Paths;
import java.nio.file.attribute.FileTime;
import java.util.List;
import java.util.Properties;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * This class is used for getting the String for Monitor the process which is
 * done behind the seen.
 *
 * @author Mohan
 */
public final class MonitorSvcImpl implements IMonitorSvc
{

    private static final Logger LOG = Logger.getLogger(MonitorSvcImpl.class.getName());
    private Properties properties = new Properties();

    @Override
    public String save(String scenarioName)
    {
        return scenarioName + " - Saving - " + lastLine(
                Constant.RUN_DETAILS_DIR + scenarioName + Constant.RUN_DIR + Constant.SAVE_FILE + Constant.TXT_EXT);

    }

    @Override
    public String runWSIDI(String scenarioName)
    {
        String scenPROGRESSFile = Constant.RUN_DETAILS_DIR + scenarioName + Constant.RUN_DIR + "//PROGRESS.txt";
        String scenWRESLCheckWSIDIFile = Constant.RUN_DETAILS_DIR + scenarioName + Constant.RUN_DIR
                + "//=WreslCheck_main_wsidi=.log";
        String scenWSIDIIterationFile = Constant.RUN_DETAILS_DIR + scenarioName + Constant.RUN_DIR
                + "//wsidi_iteration.log";
        String infoWSIDI = "";
        String line = "";
        if (Paths.get(scenWSIDIIterationFile).toFile().exists())
        {
            String lastLineForI = lastLine(scenWSIDIIterationFile);
            if (lastLineForI.equalsIgnoreCase("iteration " + properties.getProperty("wsidi.iterations") + "/"
                    + properties.getProperty("wsidi.iterations")))
            {
                try
                {
                    Thread.sleep(30000);
                }
                catch (InterruptedException ex)
                {
                    LOG.error(ex);
                    Thread.currentThread().interrupt();
                }
            }
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
        catch (IOException e)
        {
            // no need to handle because the file is not yet there.
        }
        if (fileTime != null)
        {

            if (Paths.get(scenPROGRESSFile).toFile().exists()
                    && (System.currentTimeMillis() - fileTime.toMillis() < 30000))
            {
                line = lastLine(scenPROGRESSFile);
                line = progressString(line);
                return scenarioName + " " + infoWSIDI + " - " + line;
            }
        }

        line = lastLine(scenWRESLCheckWSIDIFile);
        if (line.endsWith("===================="))
        {
            line = lastButOneLine(scenWRESLCheckWSIDIFile);
        }
        line = parsingString(line);
        return scenarioName + " " + infoWSIDI + " - " + line;

    }

    @Override
    public String runModel(String scenarioName)
    {
        String scenPROGRESSFile = Constant.RUN_DETAILS_DIR + scenarioName + Constant.RUN_DIR + "//PROGRESS.txt";
        String scenWRESLCHECKFile = Constant.RUN_DETAILS_DIR + scenarioName + Constant.RUN_DIR
                + "//=WreslCheck_main=.log";
        String line = "";
        FileTime fileTime = null;
        try
        {
            fileTime = Files.getLastModifiedTime(Paths.get(scenPROGRESSFile));
        }
        catch (IOException ex)
        {
            // no need to handle because the file is not yet there.
        }
        if (fileTime != null)
        {
            if (Paths.get(scenPROGRESSFile).toFile().exists()
                    && (System.currentTimeMillis() - fileTime.toMillis() < 300000))
            {
                line = lastLine(scenPROGRESSFile);
                line = progressString(line);

                return scenarioName + " - " + line;
            }
        }
        else if (Paths.get(scenWRESLCHECKFile).toFile().exists())
        {
            line = lastLine(scenWRESLCHECKFile);
            if (line.endsWith("===================="))
            {
                line = lastButOneLine(scenWRESLCHECKFile);
            }
            line = parsingString(line);
            return scenarioName + " - " + line;
        }

        return scenarioName + " - " + "PENDING";

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
        if (value.contains("unopenable!"))
        {
            return "RUNNING - unable to read progress.txt";
        }
        if (value.contains("Empty!"))
        {
            return "RUNNING - run starting";
        }
        if (value.toUpperCase().contains("RUN COMPLETED"))
        {
            return "DONE - run completed";
        }
        if (value.contains("Run failed."))
        {
            return "DONE - run failed.";
        }
        else
        {
            String[] parts = value.split(" ");
            if (parts.length == 4)
            {
                try
                {
                    int totalMonths = 12 * (Integer.parseInt(parts[1]) - Integer.parseInt(parts[0]));
                    int months = Math.min(totalMonths, Integer.parseInt(parts[3])
                            + 12 * (Integer.parseInt(parts[2]) - Integer.parseInt(parts[0])));
                    value = parts[3] + "/" + parts[2] + " (" + (100 * months / totalMonths) + "%)";
                }
                catch (NumberFormatException e)
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
        if (value.contains("unopenable!"))
        {
            return "PARSING - unable to read parsing log";
        }
        if (value.contains("Empty!"))
        {
            return "PARSING - parsing started";
        }
        if (!value.contains("Total errors:"))
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
    public String lastLine(String fileName)
    {
        String value = "";
        try (Stream<String> stream = Files.lines(Paths.get(fileName)))
        {
            List<String> list = stream.collect(Collectors.toList());
            value = list.get(list.size() - 1);
        }
        catch (NoSuchFileException ex)
        {
            value = "Waiting ... no progress file available";
            LOG.debug(value);
        }
        catch (AccessDeniedException ex)
        {
            value = "Access denied for file " + fileName;
            LOG.debug(value);
        }
        catch (IOException ex)
        {
            value = ex.getMessage();
            LOG.debug(ex.getMessage());
        }
        return value;
    }

    /**
     * This will open the file and read the last but one line and return it.
     *
     * @param fileName file name with whole path.
     * @return This will return the last but one line.
     */
    public String lastButOneLine(String fileName)
    {
        String value = "";
        try (Stream<String> stream = Files.lines(Paths.get(fileName)))
        {
            List<String> list = stream.collect(Collectors.toList());
            value = list.get(list.size() - 2);
        }
        catch (NoSuchFileException ex)
        {
            value = "Loading log file....";
            LOG.debug(value);
        }
        catch (AccessDeniedException ex)
        {
            value = "The Access is denied for this file " + fileName;
            LOG.debug(value);
        }
        catch (IOException ex)
        {
            value = ex.getMessage();
            LOG.debug(ex.getMessage());
        }
        return value;
    }
}
