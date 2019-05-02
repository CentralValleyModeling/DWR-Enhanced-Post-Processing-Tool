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

package gov.ca.water.calgui.wresl;

import java.io.IOException;
import java.nio.file.Path;
import java.time.LocalDate;
import java.util.Calendar;
import java.util.logging.Level;
import java.util.logging.Logger;

import gov.ca.water.calgui.project.EpptScenarioRun;
import org.antlr.runtime.RecognitionException;
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
 * @since 05-01-2019
 */
public class WreslScriptRunner
{
	private static final Logger LOGGER = Logger.getLogger(WreslScriptRunner.class.getName());
	private final EpptScenarioRun _scenarioRun;

	public WreslScriptRunner(EpptScenarioRun scenarioRun)
	{
		_scenarioRun = scenarioRun;
	}

	public static void main(String[] args) throws WreslScriptException
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
			throw new WreslScriptException("WRESL Script Execution error", ex);
		}
	}

	public void run(LocalDate start, LocalDate end) throws WreslScriptException
	{
		try
		{
			Path configPath = new WreslConfigWriter(_scenarioRun)
					.withStartDate(start)
					.withEndDate(end)
					.write()
					.toAbsolutePath();
			String separator = System.getProperty("file.separator");
			String classpath = "\"" + System.getProperty("java.class.path") + "\"";
			String path = "\"" + System.getProperty("java.home")
					+ separator + "bin" + separator + "java" + "\"";
			String[] args = new String[]{path, "-cp",
					classpath, WreslScriptRunner.class.getName(), "-config=\"" + configPath.toString() + "\""};
			String commandLine = String.join(" ", args);
			ProcessBuilder processBuilder = new ProcessBuilder()
					.command(commandLine)
					.inheritIO();
			LOGGER.log(Level.INFO, "Running process: {0}", commandLine);
			Process process = processBuilder.start();
			process.waitFor();
			int exitValue = process.exitValue();
			if(exitValue != 0)
			{
				throw new WreslScriptException(
						"Error running wresl script: " + _scenarioRun.getWreslMain() + ". Return code was: " + exitValue);
			}
		}
		catch(IOException | InterruptedException ex)
		{
			LOGGER.log(Level.SEVERE, "Error starting WRESL script JVM", ex);
		}
	}
}
