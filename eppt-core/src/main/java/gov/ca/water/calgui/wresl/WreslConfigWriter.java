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
import java.io.PrintWriter;
import java.io.UncheckedIOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.time.LocalDate;
import java.util.logging.Level;
import java.util.logging.Logger;

import gov.ca.water.calgui.constant.Constant;
import gov.ca.water.calgui.project.EpptDssContainer;
import gov.ca.water.calgui.project.EpptScenarioRun;
import gov.ca.water.calgui.project.NamedDssPath;

import hec.heclib.dss.HecDss;
import hec.heclib.util.Heclib;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 05-02-2019
 */
class WreslConfigWriter
{
	private static final Logger LOGGER = Logger.getLogger(WreslConfigWriter.class.getName());
	private final EpptScenarioRun _scenarioRun;
	private final Path _wreslMainFile;
	private LocalDate _start;
	private LocalDate _end;

	WreslConfigWriter(EpptScenarioRun scenarioRun, Path wreslMainFile)
	{
		_scenarioRun = scenarioRun;
		_wreslMainFile = wreslMainFile;
	}

	WreslConfigWriter withStartDate(LocalDate start)
	{
		_start = start;
		return this;
	}

	WreslConfigWriter withEndDate(LocalDate end)
	{
		_end = end;
		return this;
	}

	Path write() throws WreslScriptException
	{
		if(_start == null)
		{
			throw new WreslScriptException("Undefined start date");
		}
		else if(_end == null)
		{
			throw new WreslScriptException("Undefined end date");
		}
		return new WreslConfig(_start, _end).write();
	}

	private class WreslConfig
	{

		private final LocalDate _startDate;
		private final LocalDate _endDate;

		private WreslConfig(LocalDate startDate, LocalDate endDate)
		{
			_startDate = startDate;
			_endDate = endDate;
		}

		private Path write() throws WreslScriptException
		{
			if(_scenarioRun == null)
			{
				throw new WreslScriptException("Error running WRESL Script. Please ensure the Scenario Run is setup correctly");
			}

			String configText = wrimsv2.wreslparser.elements.Tools
					.readFileAsString(Constant.WRESL_DIR + "\\config.template");
			EpptDssContainer dssContainer = _scenarioRun.getDssContainer();

			NamedDssPath svDssFile = dssContainer.getSvDssFile();
			if(svDssFile != null)
			{
				configText = configText.replace("{SvAPart}", svDssFile.getAPart());
				configText = configText.replace("{SvFPart}", svDssFile.getFPart());
				configText = configText.replace("{SvFile}", "\"" + svDssFile.getDssPath().toString() + "\"");
				deleteCorruptCatalogFile(svDssFile);
			}
			NamedDssPath ivDssFile = dssContainer.getIvDssFile();
			NamedDssPath dvDssFile = dssContainer.getDvDssFile();
			if(ivDssFile != null)
			{
				configText = configText.replace("{IvFPart}", ivDssFile.getFPart());
				configText = configText.replace("{IvFile}", "\"" + ivDssFile.getDssPath().toString() + "\"");
				deleteCorruptCatalogFile(ivDssFile);
			}
			else if(dvDssFile != null)
			{
				configText = configText.replace("{IvFPart}", dvDssFile.getFPart());
				configText = configText.replace("{IvFile}", "\"" + dvDssFile.getDssPath().toString() + "\"");
			}
			if(dvDssFile != null)
			{
				configText = configText.replace("{DvFile}", "\"" + dvDssFile.getDssPath().toString() + "\"");
				deleteCorruptCatalogFile(dvDssFile);
			}

			if(_wreslMainFile != null)
			{
				configText = configText.replace("{MainFile}", "\"" + _wreslMainFile.toAbsolutePath().toString() + "\"");
			}
			else
			{
				throw new WreslScriptException("No WRESL Main file defined for the Scenario Run");
			}
			Path postProcessDss = _scenarioRun.getPostProcessDss();
			if(postProcessDss != null)
			{
				deleteCorruptCatalogFile(_scenarioRun.getDssContainer().getDtsDssFile());
				configText = configText.replace("{PostProcessDss}", "\"" + postProcessDss.toString() + "\"");
			}


			configText = configText.replace("{StartYear}", Integer.toString(_startDate.getYear()));
			configText = configText.replace("{StartMonth}", Integer.toString(_startDate.getMonthValue()));
			configText = configText.replace("{StartDay}", Integer.toString(_startDate.getDayOfMonth()));
			configText = configText.replace("{EndYear}", Integer.toString(_endDate.getYear()));
			configText = configText.replace("{EndMonth}", Integer.toString(_endDate.getMonthValue()));
			configText = configText.replace("{EndDay}", Integer.toString(_endDate.getDayOfMonth()));

			String name = _scenarioRun.getName();
			name = name.replaceAll("[^a-zA-Z0-9.\\-]", "_");
			Path configPath = _wreslMainFile.toAbsolutePath().getParent().resolve("WRESL_" + name + ".config");
			LOGGER.log(Level.INFO, "Writing WRESL config: {0}", configPath);
			configPath.getParent().toFile().mkdirs();
			try(BufferedWriter bufferedWriter = Files.newBufferedWriter(configPath);
				PrintWriter configFilePW = new PrintWriter(bufferedWriter))
			{
				configFilePW.print(configText);
				configFilePW.flush();
			}
			catch(IOException ex)
			{
				throw new WreslScriptException("There is a error when building the config file for wsidi", ex);
			}
			return configPath;
		}

	}

	private void deleteCorruptCatalogFile(NamedDssPath namedDssPath)
	{
		if(namedDssPath != null)
		{
			Path dssPath = namedDssPath.getDssPath();
			if(dssPath != null && dssPath.toFile().exists())
			{
				deleteEmptyDss7Files(dssPath);
				Path path = Paths.get(dssPath.toString().toLowerCase().replace(".dss", ".dsc"));
				if(path.toFile().exists())
				{
					try
					{
						long size = Files.size(path);
						if(size == 0)
						{
							Files.deleteIfExists(path);
						}
					}
					catch(IOException | UncheckedIOException e)
					{
						LOGGER.log(Level.WARNING, "Unable to clear empty DSS catalog file: " + path + " Manual deletion may be necessary.", e);
					}
				}
			}
		}
	}

	private void deleteEmptyDss7Files(Path dssPath)
	{
		int version = Heclib.Hec_zgetFileVersion(dssPath.toString());
		if(version == 7)
		{
			HecDss open = null;
			try
			{
				open = HecDss.open(dssPath.toString());
				if(open.getCatalogedPathnames().isEmpty())
				{
					open.close();
					Files.deleteIfExists(dssPath);
				}
			}
			catch(Exception e)
			{
				LOGGER.log(Level.FINE, "Unable to open dss file: " + dssPath, e);
			}
			finally
			{
				if(open != null)
				{
					open.close();
				}
			}
		}
	}
}
