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

package gov.ca.water.reportengine.executivereport;

class FlagViolation
{


    private  int _time;
    private  double _maxValue;

    FlagViolation(int time)
    {
        _time = time;
    }

    FlagViolation(double maxValue)
    {
        _maxValue = maxValue;
    }

    int getTime()
    {
        return _time;
    }

    double getMaxValue()
    {
        return _maxValue;
    }

}
