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

package gov.ca.water.quickresults.ui.projectconfig;

import javax.swing.tree.DefaultMutableTreeNode;

import gov.ca.water.calgui.project.NamedDssPath;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 04-09-2019
 */
class DssPathNode extends DefaultMutableTreeNode
{
	private final NamedDssPath _dssPath;
	private final String _suffix;

	DssPathNode(NamedDssPath dssPath, String suffix)
	{
		_dssPath = dssPath;
		_suffix = suffix;
	}

	DssPathNode(NamedDssPath dssPath)
	{
		this(dssPath, null);
	}

	NamedDssPath getDssPath()
	{
		return _dssPath;
	}

	@Override
	public String toString()
	{
		return _dssPath.getAliasName() + " (" + _suffix + ")";
	}
}
