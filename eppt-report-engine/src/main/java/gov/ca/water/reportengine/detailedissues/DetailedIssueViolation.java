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

package gov.ca.water.reportengine.detailedissues;

import hec.io.TimeSeriesContainer;

import java.util.ArrayList;
import java.util.List;

public class DetailedIssueViolation
{

    private final List<Integer> _times;
    private final String _title;
    private final List<Issue> _issues = new ArrayList<>();

    DetailedIssueViolation(List<Integer> times, String title, TimeSeriesContainer valueContainer, TimeSeriesContainer thresholdContainer, String waterYearType)
    {

        _times = times;
        _title = title;

        for(Integer time : times)
        {
            String date = "testTime";
            String waterYear = "testWaterYear";
            String value = "value";
            String standard = "standard";
            Issue issue = new Issue(date, waterYear, value, standard);
            _issues.add(issue);
        }
    }

    public List<Issue> getIssues()
    {
        return _issues;
    }

    static class Issue
    {

        private final String _time;

        Issue(String time, String waterYearType, String value, String standard)
        {

            _time = time;
        }

    }

}
