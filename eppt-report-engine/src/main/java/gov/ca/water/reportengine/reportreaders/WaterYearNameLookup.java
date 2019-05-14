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

import java.util.List;
import java.util.Map;

public class WaterYearNameLookup
{


    private final Map<String, List<String>> _colHeadersToValues;

    public WaterYearNameLookup(Map<String, List<String>> colHeadersToValues)
    {

        _colHeadersToValues = colHeadersToValues;
    }

    public String getWaterYearType(int waterYearTypeNum, String columnHeader)
    {
        String retval = "Undefined";
        if(_colHeadersToValues.containsKey(columnHeader))
        {
            List<String> values = _colHeadersToValues.get(columnHeader);
            if(values.size()>waterYearTypeNum)
            {
                retval = values.get(waterYearTypeNum);
            }
        }
        return retval;
    }

}
