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
//! Special class combining string and radio button in a list

/**
 * RBListItem is a helper for the FileDialog class. It holds DV and SV path information and selection (base) status of scenarios
 * listed in the results control panel.
 *
 * @author tslawecki
 */
public class RBListItemBO
{

	private final String _label;
	private final String _dssPath;
	private final GUILinksAllModelsBO.Model _model;
	private boolean _isSelected = false;

	public RBListItemBO(String label, String dssPath, GUILinksAllModelsBO.Model model)
	{
		_label = label;
		_dssPath = dssPath;
		_model = model;
	}

	public GUILinksAllModelsBO.Model getModel()
	{
		return _model;
	}

	public boolean isSelected()
	{
		return _isSelected;
	}

	public void setSelected(boolean isSelected)
	{
		this._isSelected = isSelected;
	}

	@Override
	public String toString()
	{
		return _dssPath;
	}

	public String getLabel()
	{
		return _label;
	}
}
