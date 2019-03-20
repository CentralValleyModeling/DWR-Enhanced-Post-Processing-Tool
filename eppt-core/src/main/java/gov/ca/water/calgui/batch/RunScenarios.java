/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */

package gov.ca.water.calgui.batch;
//! Creates and executes batch files using multiple threads

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import gov.ca.water.calgui.busservice.impl.ModelRunSvcImpl;
import org.apache.log4j.Logger;

/**
 * This class will run the multiple batch files created when running on multiple
 * threads.
 */
public class RunScenarios
{
    private static final Logger LOG = Logger.getLogger(ModelRunSvcImpl.class.getName());

	public RunScenarios()
	{
	}

	public static void main(String[] args)
	{
		List<String> scenarioList = new ArrayList<>();
		scenarioList.add("test1");
		scenarioList.add("test2");
		RunScenarios rs = new RunScenarios();
		try
		{
			rs.runParallel(scenarioList);
		}
		catch(Exception ex)
		{
            LOG.error(ex.getMessage());
		}
	}

	public void runParallel(List<String> scenarioList) throws IOException, InterruptedException
	{
		for(String sc : scenarioList)
		{
			String fn = "run_" + sc + ".bat";
			Runtime rt = Runtime.getRuntime();
			Process proc = rt.exec("cmd /c start /min " + System.getProperty("user.dir") + "\\" + fn);
			proc.waitFor();
		}
	}
}
