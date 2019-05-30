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

import java.io.File;
import java.io.IOException;
import java.nio.file.Path;
import java.time.Duration;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.temporal.ChronoUnit;
import java.util.logging.Level;
import java.util.logging.Logger;

import gov.ca.water.calgui.constant.EpptPreferences;
import gov.ca.water.calgui.project.EpptScenarioRun;
import org.antlr.runtime.RecognitionException;
import wrimsv2.commondata.wresldata.StudyDataSet;
import wrimsv2.components.ControllerBatch;
import wrimsv2.components.Error;
import wrimsv2.components.PreRunModel;
import wrimsv2.evaluator.PreEvaluator;
import wrimsv2.wreslparser.elements.StudyUtils;

import hec.heclib.dss.HecDSSFileAccess;

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
	private final ProcessOutputConsumer _outputStreamConsumer;

	public WreslScriptRunner(EpptScenarioRun scenarioRun, ProcessOutputConsumer outputStreamConsumer)
	{
		_scenarioRun = scenarioRun;
		_outputStreamConsumer = outputStreamConsumer;
	}

	public static void main(String[] args)
	{
		LocalDateTime start = LocalDateTime.now();
		LOGGER.log(Level.INFO, "============= Starting Run: {0} =============", start);
		try
		{
			HecDSSFileAccess.setMessageLevel(HecDSSFileAccess.MESS_LEVEL_GENERAL);
			ControllerBatch cb = new ControllerBatch();
			cb.enableProgressLog = true;
			cb.processArgs(args);
			StudyDataSet sds = cb.parse();
			String errorString = Integer.toString(StudyUtils.total_errors) + Error.getTotalError() + "*****";
			if(StudyUtils.total_errors == 0 && Error.getTotalError() == 0)
			{
				LOGGER.info(errorString);
				new PreEvaluator(sds);
				new PreRunModel(sds);
//				cb.runModelXA(sds);
				cb.runModel(sds);
			}
			else
			{
				LOGGER.log(Level.SEVERE, "=================Run ends with errors ({0},{1})=================",
						new Object[]{StudyUtils.total_errors, Error.getTotalError()});
			}
		}
		catch(RuntimeException | IOException | RecognitionException ex)
		{
			LOGGER.log(Level.SEVERE, "Error during WRESL Run", ex);
			System.exit(-1);
		}
		finally
		{
			LocalDateTime end = LocalDateTime.now();
			LOGGER.log(Level.INFO, "============= Run Finished: {0} =============", end);
			long minutes = ChronoUnit.MINUTES.between(start, end);
			long seconds = Duration.between(start, end).minus(minutes, ChronoUnit.MINUTES).getSeconds();
			LOGGER.log(Level.INFO, "============= Run Took: {0}min {1}sec =============", new Object[]{minutes, seconds});
		}
		System.exit(0);
	}

	public void run(LocalDate start, LocalDate end) throws WreslScriptException
	{
		Process process = null;
		try
		{
			Path configPath = new WreslConfigWriter(_scenarioRun)
					.withStartDate(start)
					.withEndDate(end)
					.write()
					.toAbsolutePath();
			Path wrimsDir = EpptPreferences.getWrimsPath();
			String javaExe = wrimsDir.resolve("jre").resolve("bin").resolve("java.exe").toString();
			Path wrimsLib = wrimsDir.resolve("lib");
			Path wrimsSys = wrimsLib.resolve("sys");
			String javaLibraryPath = "-Djava.library.path=" + wrimsLib.toString();
			String classpath = wrimsLib + File.separator + "*;" + wrimsSys + File.separator + "*";

			String[] args = new String[]{javaExe, "-Xmx4096m", "-Xss1024K", javaLibraryPath, "-cp", classpath, ControllerBatch.class.getName(), "-config=" + configPath.toString()};
			String commandLine = String.join(" ", args);
			LOGGER.log(Level.INFO, "Running process: {0}", commandLine);
			process = new ProcessBuilder(args).start();
			_outputStreamConsumer.runStarted(_scenarioRun, process);
			process.waitFor();
			int exitValue = process.exitValue();
			LOGGER.log(Level.WARNING,  "{0} WRESL ERROR Return Code: {1}", new Object[]{_scenarioRun.getWreslMain(), exitValue});
		}
		catch(IOException | InterruptedException ex)
		{
			LOGGER.log(Level.SEVERE, "Error starting WRESL script JVM", ex);
			Thread.currentThread().interrupt();
		}
		finally
		{
			if(process != null)
			{
				process.destroyForcibly();
				_outputStreamConsumer.runFinished(process);
			}
		}
	}
}
