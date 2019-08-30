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

package gov.ca.water.reportengine;

import java.util.List;

import gov.ca.water.reportengine.standardsummary.SummaryReportParameters;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 08-30-2019
 */
public class ReportParameters
{
	private final double _tolerance;
	private final String _author;
	private final String _subtitle;
	private final SummaryReportParameters _summaryReportParameters;
	private final List<String> _disabledModules;

	public ReportParameters(double tolerance, String author, String subtitle, SummaryReportParameters summaryReportParameters,List<String> disabledModules)
	{
		_tolerance = tolerance;
		_author = author;
		_subtitle = subtitle;
		_summaryReportParameters = summaryReportParameters;
		_disabledModules = disabledModules;
	}

	public SummaryReportParameters getSummaryReportParameters()
	{
		return _summaryReportParameters;
	}

	public String getAuthor()
	{
		return _author;
	}

	public String getSubtitle()
	{
		return _subtitle;
	}

	public double getTolerance()
	{
		return _tolerance;
	}

	public List<String> getDisabledReportModules()
	{
		return _disabledModules;
	}
}
