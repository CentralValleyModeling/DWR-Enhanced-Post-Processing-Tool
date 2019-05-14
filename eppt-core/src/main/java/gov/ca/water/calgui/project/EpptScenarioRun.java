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

package gov.ca.water.calgui.project;

import java.nio.file.Path;

import gov.ca.water.calgui.bo.GUILinksAllModelsBO;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 03-22-2019
 */
public class EpptScenarioRun
{
	private final String _name;
	private final String _description;
	private final GUILinksAllModelsBO.Model _model;
	private final Path _outputPath;
	private final Path _wreslMain;
	private final EpptDssContainer _dssContainer;

	/**
	 * @param name         name descriptor meta data for scenario run
	 * @param description  description meta data for
	 * @param model        model used for scenario run
	 * @param outputPath   output path for DSS files. Will be used for relative paths to the EpptDssContainer
	 * @param wreslMain    main file for running WRESL Post Processing Script
	 * @param dssContainer
	 */
	public EpptScenarioRun(String name, String description, GUILinksAllModelsBO.Model model, Path outputPath,
						   Path wreslMain, EpptDssContainer dssContainer)
	{
		_name = name;
		_description = description;
		_model = model;
		_outputPath = outputPath;
		_wreslMain = wreslMain;
		_dssContainer = dssContainer;
	}

	/**
	 * @return path to the main script for running WRESL Post Processor
	 */
	public Path getWreslMain()
	{
		return _wreslMain;
	}

	/**
	 * @return output directory containing the output from the WRESL script run. Used to hold relative paths for DSS files
	 */
	public Path getOutputPath()
	{
		return _outputPath;
	}

	/**
	 * @return name of the scenario run
	 */
	public String getName()
	{
		return _name;
	}

	/**
	 * @return description of the Scenario Run
	 */
	public String getDescription()
	{
		return _description;
	}

	/**
	 * @return model for the WRESL script and DSS paths
	 */
	public GUILinksAllModelsBO.Model getModel()
	{
		return _model;
	}

	public Path getPostProcessDss()
	{
		NamedDssPath dvDssFile = getDssContainer().getDvDssFile();
		if(dvDssFile != null)
		{
			return dvDssFile.getDssPath().getParent().resolve(getName() + "_PostProc.dss");
		}
		else
		{
			return getWreslMain().getParent().resolve(getName() + "_PostProc.dss");
		}
	}

	/**
	 * @return container with relevant DSS file paths
	 */
	public EpptDssContainer getDssContainer()
	{
		return _dssContainer;
	}

	@Override
	public String toString()
	{
		return getName();
	}
}
