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

import hec.heclib.util.HecTime;
import hec.io.TimeSeriesContainer;
import org.apache.log4j.Logger;

import java.text.SimpleDateFormat;
import java.time.ZoneId;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Map;

public class DetailedIssueViolation
{
    private static final Logger LOGGER = Logger.getLogger(DetailedIssueViolation.class.getName());

    private final List<HecTime> _times;
    private final String _title;
    private final List<Issue> _issues = new ArrayList<>();

    DetailedIssueViolation(List<HecTime> times, String title, Map<HecTime, Double> actualValues,
                           Map<HecTime, Double> thresholdValues, Map<HecTime, String> waterYearType)
    {

        _times = times;
        _title = title;

        for(HecTime time : times)
        {

            String waterYear = waterYearType.get(time);
            Double value = actualValues.get(time);
            Double standard = thresholdValues.get(time);


            if(waterYear != null && standard != null)
            {
                Issue issue = new Issue(time, waterYear, value, standard);
                _issues.add(issue);
            }
            else
            {
                LOGGER.warn("Tried to create a detailed issue for time " + time.toString() + " but the water year or the standard was null.");
            }
        }
    }

    public String getTitle()
    {
        return _title;
    }

    public List<Issue> getIssues()
    {
        return _issues;
    }

    static class Issue
    {

        private final HecTime _time;
        private final String _waterYearType;
        private final Double _value;
        private final Double _standard;

        Issue(HecTime time, String waterYearType, Double value, Double standard)
        {

            _time = time;
            _waterYearType = waterYearType;
            _value = value;
            _standard = standard;
        }

        @Override
        public String toString()
        {

            String actualValue = "N/A";
            if(_value != null)
            {
                 actualValue = String.format("%.2f", _value);
            }

            String standardValue = String.format("%.2f", _standard);

            String formattedTime = _time.month()  +"/"+ _time.year();
            return formattedTime + " " + _waterYearType + " @ " + actualValue + " | " + standardValue;
        }
    }

}
