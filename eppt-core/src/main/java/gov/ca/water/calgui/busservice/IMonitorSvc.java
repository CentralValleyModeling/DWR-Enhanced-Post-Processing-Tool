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

package gov.ca.water.calgui.busservice;

import java.nio.file.Path;

/**
 * This is the interface for Monitor the process which is done behind the seen.
 *
 * @author Mohan
 */
public interface IMonitorSvc
{

	/**
	 * This method will return the status of the save process.
	 *
	 * @param scenarioName Just the scenario name.
	 * @return Will return the string to be displayed.
	 */
	String save(Path scenarioName);

	/**
	 * This method will return the status of the batch run process.
	 *
	 * @param scenarioName Just the scenario name.
	 * @return Will return the string to be displayed.
	 */
	String runModel(Path scenarioName);

	/**
	 * This method will return the status of the batch run process for WSIDI.
	 *
	 * @param scenarioName Just the scenario name.
	 * @return Will return the string to be displayed.
	 */
	String runWSIDI(String scenarioName);
}
