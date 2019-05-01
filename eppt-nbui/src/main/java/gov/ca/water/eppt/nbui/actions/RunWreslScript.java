/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */

package gov.ca.water.eppt.nbui.actions;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.IOException;
import java.util.Calendar;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.antlr.runtime.RecognitionException;
import org.openide.awt.ActionID;
import org.openide.awt.ActionReference;
import org.openide.awt.ActionReferences;
import org.openide.awt.ActionRegistration;
import org.openide.util.NbBundle;
import wrimsv2.commondata.wresldata.StudyDataSet;
import wrimsv2.components.ControllerBatch;
import wrimsv2.components.Error;
import wrimsv2.components.PreRunModel;
import wrimsv2.evaluator.PreEvaluator;
import wrimsv2.wreslparser.elements.StudyUtils;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 03-18-2019
 */
@ActionID(
		category = "EPPT",
		id = "gov.ca.water.eppt.nbui.actions.RunWreslScript"
)
@ActionRegistration(
		iconBase = "gov/ca/water/eppt/nbui/actions/run.png",
		displayName = "Run WRESL Script"
)
@ActionReferences(
		{
				@ActionReference(path = "Menu/Tools", position = 0)
				,
				@ActionReference(path = "Toolbars/EPPT", position = 666)
		})
@NbBundle.Messages("CTL_RunWreslScript=Run WRESL Script")
public class RunWreslScript implements ActionListener
{
	private static final Logger LOGGER = Logger.getLogger(RunWreslScript.class.getName());

	@Override
	public void actionPerformed(ActionEvent e)
	{
		System.getenv("temp_wrims2");
		initReport(new String[]{"-config=J:\\DWR\\CalLiteBuild\\CalLiteGUI_P3\\Scenarios\\Run_Details\\DEFAULT\\DEFAULT.config"});
	}

	void initReport(String[] args)
	{
		ControllerBatch cb = new ControllerBatch();
		cb.enableProgressLog = true;
		long startTimeInMillis = Calendar.getInstance().getTimeInMillis();
		try
		{

			cb.processArgs(args);
			StudyDataSet sds = cb.parse();
			String errorString = Integer.toString(StudyUtils.total_errors) + Error.getTotalError() + "*****";
			if(StudyUtils.total_errors == 0 && Error.getTotalError() == 0)
			{
				LOGGER.info(errorString);
				new PreEvaluator(sds);
				new PreRunModel(sds);
//				cb.generateStudyFile();
				cb.runModelXA(sds);
				long endTimeInMillis = Calendar.getInstance().getTimeInMillis();
				int runPeriod = (int) (endTimeInMillis - startTimeInMillis);
				LOGGER.log(Level.CONFIG, "=================Run Time is {0}min {1}sec====",
						new Object[]{runPeriod / 60000, ((runPeriod / 1000) % 60)});
			}
			else
			{
				LOGGER.log(Level.SEVERE, "=================Run ends with errors ({0},{1})=================",
						new Object[]{StudyUtils.total_errors, Error.getTotalError()});
			}
		}
		catch(RecognitionException | IOException ex)
		{
			LOGGER.log(Level.SEVERE, "Error running WRESL Script", ex);
		}
	}

}
