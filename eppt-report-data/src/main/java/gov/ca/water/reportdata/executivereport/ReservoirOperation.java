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

package gov.ca.water.reportdata.executivereport;

public class ReservoirOperation
{

    private final int _incidentsBelowDP;
    private final int _incidentsAboveFlood;
    private final int _incidentsSpill;

    public ReservoirOperation(int incidentsBelowDP, int incidentsAboveFlood, int incidentsSpill)
    {
        _incidentsBelowDP = incidentsBelowDP;
        _incidentsAboveFlood = incidentsAboveFlood;
        _incidentsSpill = incidentsSpill;
    }

    public int getIncidentsBelowDP()
    {
        return _incidentsBelowDP;
    }
    public int getIncidentsAboveFlood()
    {
        return _incidentsAboveFlood;
    }
    public int getIncidentsSpill()
    {
        return _incidentsSpill;
    }

}
