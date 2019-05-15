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

package gov.ca.water.quickresults.ui.report;

import java.nio.file.Path;

import gov.ca.water.calgui.ProcessRunner;
import gov.ca.water.calgui.wresl.ProcessOutputConsumer;
import gov.ca.water.eppt.jasperengine.JasperReportRunner;
import gov.ca.water.reportengine.QAQCReportException;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 05-13-2019
 */
public class QAQCProcessRunner extends ProcessRunner
{
	private final Path _jrxml;
	private final Path _output;

	public QAQCProcessRunner(Path jrxml, Path output, ProcessOutputConsumer consumer)
	{
		super(consumer);
		_jrxml = jrxml;
		_output = output;
	}


	@Override
	public void processStarted(Process process)
	{
		getOutputStreamConsumer().runStarted(null, process);
	}

	@Override
	protected String[] getExtraArgs()
	{
		return new String[]{"\"" + _jrxml.toString() + "\"", "\"" + _output.toString() + "\""};
	}

	@Override
	protected void processExitValue(Process process) throws QAQCReportException
	{
		getOutputStreamConsumer().runFinished(process);
		int exitValue = process.exitValue();
		if(exitValue != 0)
		{
			throw new QAQCReportException("Error processing Report");
		}
	}


	protected Class<?> getMain()
	{
		return JasperReportRunner.class;
	}
}
