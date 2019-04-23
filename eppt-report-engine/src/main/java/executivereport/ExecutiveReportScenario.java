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

package executivereport;

import gov.ca.water.reportdata.executivereport.ReservoirOperation;
import gov.ca.water.reportdata.executivereport.UpstreamMIFReq;

import java.util.List;

public class ExecutiveReportScenario
{

    private ReservoirOperation _reservoirOperation;
    private UpstreamMIFReq _upstreamMIFReq;

    public ExecutiveReportScenario(List<Module> modules)
    {
        for(Module mod : modules)
        {
            if(mod.getName().equalsIgnoreCase("Reservoir Operations"))
            {
                createReservoirOperations(mod);
            }
        }
    }


    private void createReservoirOperations(Module mod)
    {
        List<SubModule> subModules = mod.getSubModules();
        if(subModules.size()<3)
        {
            return;
        }
        int incidentsBelowDP = subModules.get(0).getViolations().size();
        int incidentsAboveFlood = subModules.get(1).getViolations().size();
        int numberOfSpill = subModules.get(2).getViolations().size();

        _reservoirOperation =  new ReservoirOperation(incidentsBelowDP,incidentsAboveFlood,numberOfSpill);
    }

    private void createUpstreamMinimumFlow(Module mod)
    {
        _upstreamMIFReq = new UpstreamMIFReq(mod.getSubModules().get(0).getViolations().size());
    }

}
