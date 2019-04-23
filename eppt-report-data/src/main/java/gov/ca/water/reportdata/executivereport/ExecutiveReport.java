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

import java.util.List;

public class ExecutiveReport
{

    private final List<ModelInputs> _modelInputs;
    private final ExecutiveReportHeader _executiveReportHeader;
    private final List<ReservoirOperation> _resOps;
    private final List<UpstreamMIFReq> _upstreamMIFReqs;
    private final List<NodWeirsOperations> _nodWeirs;
    private final List<NodGroundwaterPumping> _nodPumpings;
    private final List<DeltaOperation> _deltas;
    private final List<COA> _coas;
    private final List<AllocationsDeliveries> _allocationDel;
    private final List<MassBalance> _massBalances;

    public ExecutiveReport( ExecutiveReportHeader erHeader, List<ModelInputs> mis, List<ReservoirOperation> resOps, List<UpstreamMIFReq> mifs,
                            List<NodWeirsOperations> nodWeirs, List<NodGroundwaterPumping> nodPumpings, List<DeltaOperation> deltas,
                            List<COA> coas, List<AllocationsDeliveries> allocationsDeliveries, List<MassBalance> massBalances)
    {
        _massBalances = massBalances;
        _allocationDel = allocationsDeliveries;
        _coas = coas;
        _deltas = deltas;
        _nodPumpings = nodPumpings;
        _nodWeirs = nodWeirs;
        _upstreamMIFReqs = mifs;
        _resOps = resOps;
        _modelInputs = mis;
        _executiveReportHeader = erHeader;

    }

    public List<MassBalance> getMassBalances()
    {
        return _massBalances;
    }

    public List<AllocationsDeliveries> getAllocationDeliveries()
    {
        return _allocationDel;
    }
    public List<COA> getCOAs()
    {
        return _coas;
    }

    public List<DeltaOperation> getDeltaOperations()
    {
        return _deltas;
    }

    public List<NodGroundwaterPumping> getNodGroundwaterPumpings()
    {
        return _nodPumpings;
    }
    public List<NodWeirsOperations> getNodWeirsOperations()
    {
        return _nodWeirs;
    }

    public List<UpstreamMIFReq> getUpstreamMIFReqs()
    {
        return _upstreamMIFReqs;
    }

    public List<ReservoirOperation> getReservoirOperations()
    {
        return _resOps;
    }
    public ExecutiveReportHeader getExecutiveReportHeader()
    {
        return _executiveReportHeader;
    }

    public List<ModelInputs> getModelInputs()
    {
        return _modelInputs;
    }

    public int getNumberOfAlternatives()
    {
        return 1;
    }

//    public int getIncidentsOfViolationInBase()
//    {
//
//    }
//    public int getIncidentsOfViolationInAlternative()
//    {
//
//    }
//
//    public double getPercentChangeInAlternativeStudy()
//    {
//
//    }






}
