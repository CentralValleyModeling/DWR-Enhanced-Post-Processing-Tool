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

package gov.ca.water.reportengine.reportreaders;

import gov.ca.water.reportengine.EpptReportException;
import gov.ca.water.reportengine.detailedissues.DetailedIssue;
import gov.ca.water.reportengine.executivereport.ExecutiveReportException;
import hec.lang.Const;

import java.io.BufferedReader;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;

public class DetailedIssuesReader
{

    private static final int MODULE_ID = 0;
    private static final int LINKED_VAR = 1;
    private static final int GUI_LINK = 2;
    private static final int THRESHOLD_LINK = 3;

    private final Path _detailedIssuesCSVPath;

    public DetailedIssuesReader(Path detailedIssuesCSVPath)
    {

        _detailedIssuesCSVPath = detailedIssuesCSVPath;
    }

    public List<DetailedIssue> read() throws EpptReportException
    {
        String line = "";
        String csvSplitBy = ",";
        List<DetailedIssue> retval = new ArrayList<>();
        try (BufferedReader br = Files.newBufferedReader(_detailedIssuesCSVPath))
        {
            //skip first line
            int i = 0;
            while ((line = br.readLine()) != null)
            {
                if (i == 0)
                {
                    i++;
                    continue;
                }
                String[] row = line.split(csvSplitBy);
                int subModuleID = Integer.parseInt(row[MODULE_ID]);
                String linkedVar = row[LINKED_VAR];

                int guiLink = Const.UNDEFINED_INT;
                int thresholdLink  = Const.UNDEFINED_INT;
                if(row.length>2)
                {
                    if (!row[GUI_LINK].equals(""))
                    {
                        guiLink = Integer.parseInt(row[GUI_LINK]);
                    }
                }
                if(row.length>3)
                {
                    if (!row[THRESHOLD_LINK].equals(""))
                    {
                        thresholdLink = Integer.parseInt(row[THRESHOLD_LINK]);
                    }
                }

                retval.add(new DetailedIssue(subModuleID, linkedVar, guiLink, thresholdLink));

            }
        }
        catch (IOException e)
        {
            throw new EpptReportException("Error reading the csv file: " + _detailedIssuesCSVPath, e);
        }
        return retval;
    }

}
