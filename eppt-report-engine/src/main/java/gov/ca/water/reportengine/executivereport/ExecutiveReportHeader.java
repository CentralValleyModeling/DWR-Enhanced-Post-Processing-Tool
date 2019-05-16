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

public class ExecutiveReportHeader
{


	private final int _baseViolations;
	private final int _altViolations;
	private final String _pctChangeViolations;
	private final double _avgAnnualExportPrev;
	private final double _avgAnnualExportCur;
	private final String _annualExportIncDec;
	private final double _avgCVPNodStoragePrev;
	private final double _avgCVPNodStorageCur;
	private final double _avgSWPNodStorPrev;
	private final double _avgSWPNodStorCur;
	private final String _swpNodStorIncDec;
	private final String _cvpNodStorIncDec;

	public ExecutiveReportHeader(int baseViolations, int altViolations, String pctChangeViolations, double avgAnnualExportPrev,
								 double avgAnnualExportCur, String annualExportIncDec, double avgCVPNodStoragePrev,
								 double avgCVPNodStorageCur, String cvpNodStorIncDec, double avgSWPNodStorPrev, double avgSWPNodStorCur,
								 String swpNodStorIncDec)
	{
		_cvpNodStorIncDec = cvpNodStorIncDec;
		_swpNodStorIncDec = swpNodStorIncDec;
		_avgSWPNodStorCur = avgSWPNodStorCur;
		_avgSWPNodStorPrev = avgSWPNodStorPrev;
		_avgCVPNodStorageCur = avgCVPNodStorageCur;
		_avgCVPNodStoragePrev = avgCVPNodStoragePrev;
		_annualExportIncDec = annualExportIncDec;
		_avgAnnualExportPrev = avgAnnualExportPrev;
		_avgAnnualExportCur = avgAnnualExportCur;
		_pctChangeViolations = pctChangeViolations;
		_altViolations = altViolations;
		_baseViolations = baseViolations;
	}

	public String getCvpNodStorIncDec()
	{
		return _cvpNodStorIncDec;
	}

	public String getSwpNodStorIncDec()
	{
		return _swpNodStorIncDec;
	}

	public double getAvgSWPNodStorCur()
	{
		return _avgSWPNodStorCur;
	}

	public double getAvgSWPNodStorPrev()
	{
		return _avgSWPNodStorPrev;
	}

	public double getAvgCVPNodStorageCur()
	{
		return _avgCVPNodStorageCur;
	}

	public double getAvgCVPNodStoragePrev()
	{
		return _avgCVPNodStoragePrev;
	}

	public String getAnnualExportIncDec()
	{
		return _annualExportIncDec;
	}

	public double getAvgAnnualExportPrev()
	{
		return _avgAnnualExportPrev;
	}

	public double getAvgAnnualExportCur()
	{
		return _avgAnnualExportCur;
	}

	public String getPctChangeViolations()
	{
		return _pctChangeViolations;
	}

	public int getBaseViolations()
	{
		return _baseViolations;
	}

	public int getAltViolations()
	{
		return _altViolations;
	}

}
