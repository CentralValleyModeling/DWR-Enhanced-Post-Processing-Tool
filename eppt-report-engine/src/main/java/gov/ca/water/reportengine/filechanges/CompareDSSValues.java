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

import rma.lang.RmaMath;

public class CompareDSSValues
{

    private final double _tolerance;

    public CompareDSSValues(double tolerance)
    {

        _tolerance = tolerance;
    }

     boolean compareTimes(int[] baseTimes, int[] altTimes)
    {
        for (int i = 0; i < baseTimes.length; i++)
        {
            if (baseTimes[i] != altTimes[i])
            {
                return false;
            }
        }
        return true;
    }

     boolean compareValues( double[] baseValues, double[] altValues)
    {
        for (int i = 0; i < baseValues.length; i++)
        {
            if(!RmaMath.equals(baseValues[i] , altValues[i] , _tolerance))
            {
                return false;
            }

        }
        return true;
    }

}
