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

package gov.ca.water.calgui;

import java.io.BufferedWriter;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Date;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import gov.ca.water.calgui.wresl.ProcessOutputConsumer;
import gov.ca.water.calgui.wresl.WreslScriptRunner;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 05-13-2019
 */
public abstract class ProcessRunner
{
	private static final Logger LOGGER = Logger.getLogger(WreslScriptRunner.class.getName());
	private final ProcessOutputConsumer _outputStreamConsumer;

	public ProcessRunner(ProcessOutputConsumer outputStreamConsumer)
	{
		_outputStreamConsumer = outputStreamConsumer;
	}

	public void run()
	{
		Process process = null;
		try
		{
			String separator = System.getProperty("file.separator");
			String javaLibraryPath = "-Djava.library.path=\"" + Paths.get("dwr_eppt/modules/lib").toAbsolutePath() + "\"";
			String path = "\"" + System.getProperty("java.home")
					+ separator + "bin" + separator + "java" + "\"";
			String classpath = "echo off \n";
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


			String[] args = new String[]{path, "-Xmx1472m -Xss1280K", javaLibraryPath, getMain().getName()};
			String commandLine = String.join(" ", args);
			commandLine += " " + String.join(" ", getExtraArgs());
			Path outputBat = Paths.get("output" + new Date().getTime() + ".bat");
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
			processStarted(process);
			process.waitFor();
			processExitValue(process);
		}
		catch(Exception ex)
		{
			LOGGER.log(Level.SEVERE, "Error running external process", ex);
			Thread.currentThread().interrupt();
		}
		finally
		{
			if(process != null)
			{
				process.destroyForcibly();
			}
		}
	}

	protected ProcessOutputConsumer getOutputStreamConsumer()
	{
		return _outputStreamConsumer;
	}

	abstract protected void processStarted(Process process);

	abstract protected Class<?> getMain();

	abstract protected String[] getExtraArgs();

	abstract protected void processExitValue(Process process) throws Exception;
}
