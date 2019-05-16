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
import java.util.List;

/**
 * This is the interface for Batch Run.
 *
 * @author Mohan
 */
public interface IModelRunSvc
{
	/**
	 * This method will generate the batch file and run it.
	 *
	 * @param scenarioNamesList The list of scenario names to run batch on.
	 * @param isWsidi           The flag which says whether this run is WSIDI of not.
	 */
	void doBatch(List<Path> scenarioNamesList, boolean isWsidi);
}
