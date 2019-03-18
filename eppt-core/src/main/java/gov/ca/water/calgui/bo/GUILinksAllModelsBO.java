/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */

package gov.ca.water.calgui.bo;

import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;

import static java.util.stream.Collectors.toList;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 03-08-2019
 */
public class GUILinksAllModelsBO
{
	private final String _checkboxId;
	private final Map<Model, ModelData> _modelMapping = new HashMap<>();
	private final String _plotAxisLabel;
	private final String _plotTitle;
	private final String _legend;

	public GUILinksAllModelsBO(String checkboxId, String plotAxisLabel, String plotTitle, String legend)
	{
		_checkboxId = checkboxId;
		_plotAxisLabel = plotAxisLabel;
		_plotTitle = plotTitle;
		_legend = legend;
	}

	public String getCheckboxId()
	{
		return _checkboxId;
	}

	public String getPlotAxisLabel()
	{
		return _plotAxisLabel;
	}

	public String getPlotTitle()
	{
		return _plotTitle;
	}

	public String getLegend()
	{
		return _legend;
	}

	public Optional<ModelData> getModelData(Model model)
	{
		return Optional.ofNullable(_modelMapping.get(model));
	}

	public void addModelMapping(Model model, String primary, String secondary)
	{
		if(model == null)
		{
			throw new IllegalArgumentException("Unable to add null model");
		}
		_modelMapping.put(model, new ModelData(primary, secondary));
	}

	public List<String> getPrimary()
	{
		return Arrays.stream(Model.values())
					 .map(this::getModelData)
					 .filter(Optional::isPresent)
					 .map(Optional::get)
					 .map(ModelData::getPrimary)
					 .filter(Objects::nonNull)
					 .collect(toList());
	}

	public List<String> getSecondary()
	{
		return Arrays.stream(Model.values())
					 .map(this::getModelData)
					 .filter(Optional::isPresent)
					 .map(Optional::get)
					 .map(ModelData::getSecondary)
					 .filter(Objects::nonNull)
					 .collect(toList());
	}

	public enum Model
	{
		CAL_LITE("CalLite"), CAL_SIM_2("CalSim2"), CAL_SIM_3("CalSim3");

		private final String _name;

		Model(String name)
		{
			_name = name;
		}

		public static Model findModel(String model)
		{
			Model retval = null;
			for(Model m : Model.values())
			{
				if(model.equalsIgnoreCase(m._name))
				{
					retval = m;
					break;
				}
			}
			return retval;
		}
	}

	public class ModelData
	{
		private final String _primary;
		private final String _secondary;

		private ModelData(String primary, String seconday)
		{
			_primary = primary;
			_secondary = seconday;
		}

		public String getPrimary()
		{
			return _primary;
		}

		public String getSecondary()
		{
			return _secondary;
		}
	}
}
