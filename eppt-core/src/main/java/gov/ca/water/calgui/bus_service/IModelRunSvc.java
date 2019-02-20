/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */

package gov.ca.water.calgui.bus_service;

import java.util.List;

import org.swixml.SwingEngine;

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
	 * @param swingEngine       The {@link SwingEngine} Object.
	 * @param isWsidi           The flag which says whether this run is WSIDI of not.
	 */
	void doBatch(List<String> scenarioNamesList, SwingEngine swingEngine, boolean isWsidi);
}
