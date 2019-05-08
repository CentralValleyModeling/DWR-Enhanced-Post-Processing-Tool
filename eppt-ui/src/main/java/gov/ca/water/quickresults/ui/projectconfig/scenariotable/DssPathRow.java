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

package gov.ca.water.quickresults.ui.projectconfig.scenariotable;

import gov.ca.water.calgui.project.NamedDssPath;
import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.value.ObservableValue;

import com.rma.javafx.treetable.columns.specs.TreeTableColumnSpec;

import static gov.ca.water.quickresults.ui.projectconfig.scenariotable.ScenarioTableModel.DSS_PATH_COL_SPEC;
import static gov.ca.water.quickresults.ui.projectconfig.scenariotable.ScenarioTableModel.NAME_COL_SPEC;
import static gov.ca.water.quickresults.ui.projectconfig.scenariotable.ScenarioTableModel.TYPE_COL_SPEC;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 05-07-2019
 */
class DssPathRow extends ParentRowModel
{

	private final SimpleObjectProperty<String> _nameProperty;
	private final SimpleObjectProperty<String> _dssPathProperty;
	private final SimpleObjectProperty<String> _typeProperty;

	DssPathRow(ParentRowModel parent, NamedDssPath namedDssPath, String type)
	{
		super(parent);
		_nameProperty = new SimpleObjectProperty<>(namedDssPath.getAliasName());
		_dssPathProperty = new SimpleObjectProperty<>(namedDssPath.getDssPath().toString());
		_typeProperty = new SimpleObjectProperty<>(type);
	}

	@Override
	public ObservableValue<?> getObservableValue(TreeTableColumnSpec spec)
	{
		ObservableValue<?> retval = null;
		if(spec == NAME_COL_SPEC)
		{
			retval = _nameProperty;
		}
		else if(spec == DSS_PATH_COL_SPEC)
		{
			retval = _dssPathProperty;
		}
		else if(spec == TYPE_COL_SPEC)
		{
			retval = _typeProperty;
		}
		return retval;
	}

	@Override
	public boolean isEditable(TreeTableColumnSpec columnSpec)
	{
		return false;
	}
}
