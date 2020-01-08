/*
 * Enhanced Post Processing Tool (EPPT) Copyright (c) 2020.
 *
 * EPPT is copyrighted by the State of California, Department of Water Resources. It is licensed
 * under the GNU General Public License, version 2. This means it can be
 * copied, distributed, and modified freely, but you may not restrict others
 * in their ability to copy, distribute, and modify it. See the license below
 * for more details.
 *
 * GNU General Public License
 */

package gov.ca.water.quickresults.ui.report;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.stream.Stream;

import gov.ca.water.calgui.constant.Constant;
import gov.ca.water.calgui.constant.EpptPreferences;
import gov.ca.water.calgui.project.EpptScenarioRun;

import static java.nio.file.StandardCopyOption.REPLACE_EXISTING;
import static java.util.stream.Collectors.toList;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 01-08-2020
 */
final class QaQcFileUtils
{
	private static final Logger LOGGER = Logger.getLogger(QaQcFileUtils.class.getName());

	private QaQcFileUtils()
	{
		throw new AssertionError("Utility class");
	}

	static Path copyJasperPaths() throws IOException
	{
		String jasperDir = Constant.JASPER_DIR;

		Path lastProjectConfiguration = EpptPreferences.getLastProjectConfiguration();
		Path reports = lastProjectConfiguration.getParent().resolve("Reports");
		QaQcFileUtils.copyFolder(Paths.get(jasperDir), reports);
		try(Stream<Path> walk = Files.walk(reports, 7))
		{
			List<Path> jasper = walk.filter(p -> p.getFileName().toString().endsWith("jasper"))
									.collect(toList());
			for(Path path : jasper)
			{
				Files.deleteIfExists(path);
			}
		}
		return reports;
	}

	static Path createWreslMain(EpptScenarioRun scenarioRun, boolean force)
	{
		Path retval = Paths.get("");
		if(scenarioRun != null)
		{
			Path projectDir = EpptPreferences.getLastProjectConfiguration().getParent();
			Path wreslDir = projectDir.resolve(scenarioRun.getName() + "_WRESL");
			Path wreslMainFile = wreslDir.resolve(Constant.WRESL_MAIN);
			if(!force && wreslMainFile.toFile().exists())
			{
				retval = wreslMainFile;
			}
			else
			{
				try
				{
					copyWreslDirectory(scenarioRun, wreslDir);
					copyLookupDirectory(scenarioRun, wreslDir);
					retval = wreslMainFile;
				}
				catch(IOException e)
				{
					LOGGER.log(Level.SEVERE, "Unable to create WRESL directory: " + wreslDir, e);
				}
			}
		}
		return retval;
	}

	private static void copyWreslDirectory(EpptScenarioRun scenarioRun, Path wreslDir) throws IOException
	{
		QaQcFileUtils.copyFolder(scenarioRun.getWreslDirectory(), wreslDir);
	}

	private static void copyLookupDirectory(EpptScenarioRun scenarioRun, Path wreslDir) throws IOException
	{
		QaQcFileUtils.copyFolder(scenarioRun.getLookupDirectory(), wreslDir.resolve("lookup"));
	}

	private static void copyFolder(Path src, Path dest) throws IOException
	{
		try(Stream<Path> walk = Files.walk(src))
		{
			walk.forEach(source -> copy(source, dest.resolve(src.relativize(source))));
		}
	}

	private static void copy(Path source, Path dest)
	{
		try
		{
			if(!dest.toFile().exists())
			{
				boolean mkdirs = dest.toFile().mkdirs();
				if(!mkdirs)
				{
					throw new IOException("Unable to create directory for: " + dest);
				}
			}
			if(source.toFile().isFile())
			{
				Files.copy(source, dest, REPLACE_EXISTING);
			}
		}
		catch(IOException e)
		{
			throw new IllegalStateException("Error copying file: " + source, e);
		}
	}
}
