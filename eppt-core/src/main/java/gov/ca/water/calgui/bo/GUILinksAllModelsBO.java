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

package gov.ca.water.calgui.bo;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;

import static java.util.stream.Collectors.toMap;

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

	private Optional<ModelData> getModelData(Model model)
	{
		return Optional.ofNullable(_modelMapping.get(model));
	}

	public void addModelMapping(String modelString, String primary, String secondary)
	{
		if(modelString == null)
		{
			throw new IllegalArgumentException("Unable to add null model");
		}
		Model model = new Model(modelString);
		Model.addModel(model);
		_modelMapping.put(model, new ModelData(primary, secondary, Model.findModel(modelString)));
	}

	public Map<Model, String> getPrimary()
	{
		return Model.values().stream()
					 .map(this::getModelData)
					 .filter(Optional::isPresent)
					 .map(Optional::get)
					 .filter(data->data.getPrimary() != null)
					 .collect(toMap(ModelData::getModel, ModelData::getPrimary));
	}

	public Map<Model, String> getSecondary()
	{
		return Model.values().stream()
					 .map(this::getModelData)
					 .filter(Optional::isPresent)
					 .map(Optional::get)
					 .filter(data->data.getSecondary() != null)
					 .filter(data->!data.getSecondary().isEmpty())
					 .collect(toMap(ModelData::getModel, ModelData::getSecondary));
	}

	public static class Model
	{
		private static final Set<Model> MODELS = new HashSet<>();

		private final String _name;

		Model(String name)
		{
			Objects.requireNonNull(name, "Model name connot be null");
			_name = name;
		}

		public static Model findModel(String model)
		{
			Model retval = null;
			for(Model m : Model.values())
			{
				if(m._name.equalsIgnoreCase(model))
				{
					retval = m;
					break;
				}
			}
			if(retval == null)
			{
				retval = new Model("CalLite");
			}
			return retval;
		}

		private static void addModel(Model models)
		{
			MODELS.add(models);
		}

		public static List<Model> values()
		{
			return new ArrayList<>(MODELS);
		}

		@Override
		public String toString()
		{
			return _name;
		}

		@Override
		public boolean equals(Object o)
		{
			if(this == o)
			{
				return true;
			}
			if(!(o instanceof Model))
			{
				return false;
			}
			final Model model = (Model) o;
			return _name.equals(model._name);
		}

		@Override
		public int hashCode()
		{
			return Objects.hash(_name);
		}
	}

	public static class ModelData
	{
		private final String _primary;
		private final String _secondary;
		private final Model _model;

		private ModelData(String primary, String seconday, Model model)
		{
			_primary = primary;
			_secondary = seconday;
			_model = model;
		}

		public Model getModel()
		{
			return _model;
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
