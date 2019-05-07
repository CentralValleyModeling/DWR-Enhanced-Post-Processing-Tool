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
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.LocalDate;

import gov.ca.water.calgui.constant.Constant;
import gov.ca.water.calgui.project.EpptDssContainer;
import gov.ca.water.calgui.project.EpptScenarioRun;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 05-02-2019
 */
class WreslConfigWriter
{
	private final EpptScenarioRun _scenarioRun;
	private LocalDate _start;
	private LocalDate _end;

	WreslConfigWriter(EpptScenarioRun scenarioRun)
	{
		_scenarioRun = scenarioRun;
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

		//		private final String _svFPart;
		//		private final String _ivFPart;
		private final LocalDate _startDate;
		private final LocalDate _endDate;

		private WreslConfig(/*String svFPart, String ivFPart, */LocalDate startDate,
																LocalDate endDate)
		{
			//			_svFPart = svFPart;
			//			_ivFPart = ivFPart;
			_startDate = startDate;
			_endDate = endDate;
		}

		private Path write() throws WreslScriptException
		{
			String configText = wrimsv2.wreslparser.elements.Tools
					.readFileAsString(Constant.WRESL_DIR + "\\config.template");
			EpptDssContainer dssContainer = _scenarioRun.getDssContainer();
			configText = configText.replace("{SvarFile}", dssContainer.getSvDssFile().getDssPath().toString());
			configText = configText.replace("{InitFile}", dssContainer.getIvDssFile().getDssPath().toString());
			configText = configText.replace("{DvarFile}", dssContainer.getDvDssFile().getDssPath().toString());
			configText = configText.replace("{SvarFPart}", "2020D09E");
			configText = configText.replace("{InitFPart}", "2020D09E");
			configText = configText.replace("{StartYear}", Integer.toString(_startDate.getYear()));
			configText = configText.replace("{StartMonth}", Integer.toString(_startDate.getMonthValue()));
			configText = configText.replace("{StartDay}", Integer.toString(_startDate.getDayOfMonth()));
			configText = configText.replace("{EndYear}", Integer.toString(_endDate.getYear()));
			configText = configText.replace("{EndMonth}", Integer.toString(_endDate.getMonthValue()));
			configText = configText.replace("{EndDay}", Integer.toString(_endDate.getDayOfMonth()));
			configText = configText.replace("{MainFile}", _scenarioRun.getWreslMain().toAbsolutePath().toString());

			Path configPath = _scenarioRun.getOutputPath().resolve("WRESL.config");
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
}
