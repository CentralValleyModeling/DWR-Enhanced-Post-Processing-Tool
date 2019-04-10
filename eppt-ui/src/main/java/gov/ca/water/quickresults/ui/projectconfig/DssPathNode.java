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

import java.nio.file.Path;
import javax.swing.tree.DefaultMutableTreeNode;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 04-09-2019
 */
class DssPathNode extends DefaultMutableTreeNode
{
	private final Path _dssPath;
	private final String _suffix;

	DssPathNode(Path dssPath, String suffix)
	{
		_dssPath = dssPath;
		_suffix = suffix;
	}

	DssPathNode(Path dssPath)
	{
		this(dssPath, null);
	}

	public Path getDssPath()
	{
		return _dssPath;
	}

	@Override
	public String toString()
	{
		return _dssPath.getFileName() + " (" + _suffix + ")";
	}
}
