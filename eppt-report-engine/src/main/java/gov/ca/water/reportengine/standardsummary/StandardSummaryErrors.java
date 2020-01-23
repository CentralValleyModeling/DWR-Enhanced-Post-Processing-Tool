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

package gov.ca.water.reportengine.standardsummary;

import java.util.ArrayList;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import com.google.common.flogger.FluentLogger;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 10-10-2019
 */
public class StandardSummaryErrors
{
	private static final Logger LOGGER = Logger.getLogger(StandardSummaryErrors.class.getName());
	private final List<Runnable> _loggingStatements = new ArrayList<>();

	public void addError(Logger logger, String message)
	{
		_loggingStatements.add(()->logger.log(Level.WARNING, message));
	}

	public void addError(Logger logger, String message, Exception exception)
	{
		_loggingStatements.add(()->logger.log(Level.WARNING, message, exception));
	}

	public void addError(FluentLogger logger, String message)
	{
		_loggingStatements.add(()->logger.at(Level.WARNING).log(message));
	}

	public void addError(FluentLogger logger, String message, Exception exception)
	{
		_loggingStatements.add(()->logger.at(Level.WARNING).withCause(exception).log(message));
	}

	public void log()
	{
		if(!_loggingStatements.isEmpty())
		{
			LOGGER.log(Level.WARNING, "The QA/QC Report ran with the following errors:");
			_loggingStatements.forEach(Runnable::run);
			_loggingStatements.clear();
		}
	}
}
