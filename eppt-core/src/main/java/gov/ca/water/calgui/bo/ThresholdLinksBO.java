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

import java.util.HashMap;
import java.util.Map;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 05-11-2019
 */
public class ThresholdLinksBO
{
	private final int _id;
	private final String _label;
	private final Map<GUILinksAllModelsBO.Model, ModelData> _modelMapping = new HashMap<>();

	public ThresholdLinksBO(int id, String label)
	{
		_id = id;
		_label = label;
	}

	public int getId()
	{
		return _id;
	}

	public String getLabel()
	{
		return _label;
	}

	public ModelData getModelData(GUILinksAllModelsBO.Model model)
	{
		return _modelMapping.get(model);
	}

	public void addModelMapping(String modelString, String primary)
	{
		if(modelString == null)
		{
			throw new IllegalArgumentException("Unable to add null model");
		}
		GUILinksAllModelsBO.Model model = GUILinksAllModelsBO.Model.findModel(modelString);
		_modelMapping.put(model, new ModelData(primary, model));
	}

	public static class ModelData
	{
		private final String _primary;
		private final GUILinksAllModelsBO.Model _model;

		private ModelData(String primary, GUILinksAllModelsBO.Model model)
		{
			_primary = primary;
			_model = model;
		}

		public GUILinksAllModelsBO.Model getModel()
		{
			return _model;
		}

		public String getPrimary()
		{
			return _primary;
		}

	}
}
