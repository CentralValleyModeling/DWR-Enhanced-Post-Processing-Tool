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

import java.io.BufferedWriter;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.time.Duration;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.temporal.ChronoUnit;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.stream.Collectors;
import java.util.stream.Stream;

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
	private final WreslOutputConsumer _outputStreamConsumer;

	public WreslScriptRunner(EpptScenarioRun scenarioRun, WreslOutputConsumer outputStreamConsumer)
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
				cb.runModelXA(sds);
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
			String separator = System.getProperty("file.separator");
			String javaLibraryPath = "-Djava.library.path=\"" + Paths.get("dwr_eppt/modules/lib").toAbsolutePath() + "\"";
			String path = "\"" + System.getProperty("java.home")
					+ separator + "bin" + separator + "java" + "\"";
			String classpath =  "echo off \n";
			Path epptDir = Paths.get("dwr_eppt");
			Path modulesDir = epptDir.resolve("modules");
			try(Stream<Path> walk = Files.walk(modulesDir, 3))
			{
				classpath += walk.filter(p -> p.toFile().isDirectory())
																	   .filter(p -> !p.toString().endsWith("jar"))
																	   .map(Object::toString)
																	   .map(p -> "set classpath=%classpath%;" + p + "/*")
																	   .collect(Collectors.joining("\n")) + "echo on\n";
			}


			String[] args = new String[]{path, "-Xmx1472m -Xss1280K", javaLibraryPath, WreslScriptRunner.class.getName(), "-config=\"" + configPath.toString() + "\""};
			String commandLine = String.join(" ", args);
			Path outputBat = Paths.get("output.bat");
			try(BufferedWriter bufferedWriter = Files.newBufferedWriter(outputBat))
			{
				bufferedWriter.newLine();
				bufferedWriter.newLine();
				bufferedWriter.write(classpath);
				bufferedWriter.newLine();
				bufferedWriter.write(commandLine);
				bufferedWriter.flush();
			}
			ProcessBuilder processBuilder = new ProcessBuilder()
					.command(outputBat.toString());
			LOGGER.log(Level.INFO, "Running process: {0}", commandLine);
			process = processBuilder.start();
			InputStream errorStream = process.getErrorStream();
			InputStream outputStream = process.getInputStream();
			_outputStreamConsumer.runStarted(_scenarioRun, process, outputStream, errorStream);
			process.waitFor();
			int exitValue = process.exitValue();
			if(exitValue != 0)
			{
				throw new WreslScriptException(_scenarioRun.getWreslMain() + " " +
						"WRESL ERROR Return Code: " + exitValue);
			}
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
				process.destroy();
			}
		}
	}
}
