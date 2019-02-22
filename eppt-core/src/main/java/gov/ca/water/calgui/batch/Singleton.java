/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */

package gov.ca.water.calgui.batch;
//! Creates and executes a single batch file for one run

import org.antlr.runtime.RecognitionException;
import org.apache.log4j.Logger;
import wrimsv2.commondata.wresldata.StudyDataSet;
import wrimsv2.components.ControllerBatch;
import wrimsv2.components.Error;
import wrimsv2.components.PreRunModel;
import wrimsv2.evaluator.PreEvaluator;
import wrimsv2.wreslparser.elements.StudyUtils;

import java.io.IOException;
import java.util.Calendar;
import java.util.concurrent.TimeUnit;

/**
 * This class is used to run a single study of the wrimsv2 class.
 */
public class Singleton
{
	private static final Logger LOG = Logger.getLogger(Singleton.class.getName());

    private Singleton(String[] args)
	{
		runStudy(args);
	}

	public static void main(String[] args)
	{
		new Singleton(args);
	}

	private void runStudy(String[] args)
	{
		ControllerBatch cb = new ControllerBatch();
		cb.enableProgressLog = true;
		long startTimeInMillis = Calendar.getInstance().getTimeInMillis();
		try
		{
			cb.processArgs(args);
			StudyDataSet sds = cb.parse();
			LOG.error(Integer.toString(StudyUtils.total_errors) + Error.getTotalError() + "*****");
			if(StudyUtils.total_errors == 0 && Error.getTotalError() == 0)
			{
				new PreEvaluator(sds);
				new PreRunModel(sds);
				cb.generateStudyFile();
				cb.runModel(sds);
				long endTimeInMillis = Calendar.getInstance().getTimeInMillis();
				int runPeriod = (int) (endTimeInMillis - startTimeInMillis);
				LOG.debug("=================Run Time is " + runPeriod / 60000 + "min"
						+ ((runPeriod/1000) % 60)+ "sec====");
			}
			else
			{
				LOG.error("=================Run ends with errors (" + StudyUtils.total_errors + ","
						+ Error.getTotalError() + " =================");
			}
		}
		catch(RecognitionException ex)
		{
            LOG.error(ex.getMessage());
		}
		catch(IOException ex)
		{
            LOG.error(ex.getMessage());
		}
		System.exit(0);
	}
}