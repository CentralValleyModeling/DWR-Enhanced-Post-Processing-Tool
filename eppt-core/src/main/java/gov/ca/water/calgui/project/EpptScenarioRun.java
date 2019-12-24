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
import java.util.Objects;

import gov.ca.water.calgui.bo.GUILinksAllModelsBO;
import javafx.scene.paint.Color;

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
	private final Path _waterYearTable;
	private final EpptDssContainer _dssContainer;
	private final Color _color;
	private boolean _baseSelected;
	private boolean _altSelected;

	/**
	 * @param name         name descriptor meta data for scenario run
	 * @param description  description meta data for
	 * @param model        model used for scenario run
	 * @param outputPath   output path for DSS files. Will be used for relative paths to the EpptDssContainer
	 * @param wreslMain    main file for running WRESL Post Processing Script
	 * @param dssContainer
	 */
	public EpptScenarioRun(String name, String description, GUILinksAllModelsBO.Model model, Path outputPath,
						   Path wreslMain, Path waterYearTable, EpptDssContainer dssContainer, Color color)
	{
		_name = name;
		_description = description;
		_model = model;
		_outputPath = outputPath;
		_wreslMain = wreslMain;
		_waterYearTable = waterYearTable;
		_dssContainer = dssContainer;
		_color = color;
	}

	/**
	 * Copy ctor
	 *
	 * @param name         name descriptor meta data for scenario run
	 * @param description  description meta data for
	 * @param epptScenarioRun scenario run to copy
	 */
	public EpptScenarioRun(String name, String description, EpptScenarioRun epptScenarioRun)
	{
		_name = name;
		_description = description;
		_model = epptScenarioRun.getModel();
		_outputPath = epptScenarioRun.getOutputPath();
		_wreslMain = epptScenarioRun.getWreslMain();
		_waterYearTable = epptScenarioRun.getWaterYearTable();
		_dssContainer = new EpptDssContainer(epptScenarioRun.getDssContainer());
		_color = epptScenarioRun.getColor();
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
		NamedDssPath dtsDssFile = getDssContainer().getDtsDssFile();
		if(dtsDssFile != null)
		{
			return dtsDssFile.getDssPath();
		}
		else
		{
			return null;
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

	public Path getWaterYearTable()
	{
		return _waterYearTable;
	}

	public Color getColor()
	{
		return _color;
	}

	@Override
	public boolean equals(Object o)
	{
		if(this == o)
		{
			return true;
		}
		if(o == null || getClass() != o.getClass())
		{
			return false;
		}
		final EpptScenarioRun that = (EpptScenarioRun) o;
		return Objects.equals(getName(), that.getName()) &&
				Objects.equals(getDescription(), that.getDescription()) &&
				Objects.equals(getModel(), that.getModel()) &&
				Objects.equals(getOutputPath(), that.getOutputPath()) &&
				Objects.equals(getWreslMain(), that.getWreslMain()) &&
				Objects.equals(getWaterYearTable(), that.getWaterYearTable()) &&
				Objects.equals(getDssContainer(), that.getDssContainer()) &&
				Objects.equals(getColor(), that.getColor());
	}

	@Override
	public int hashCode()
	{
		return Objects.hash(getName(), getDescription(), getModel(), getOutputPath(), getWreslMain(), getWaterYearTable(), getDssContainer(),
				getColor());
	}

	public void setBaseSelected(boolean baseSelected)
	{
		_baseSelected = baseSelected;
	}

	public void setAltSelected(boolean altSelected)
	{
		_altSelected = altSelected;
	}

	public boolean isBaseSelected()
	{
		return _baseSelected;
	}

	public boolean isAltSelected()
	{
		return _altSelected;
	}
}
