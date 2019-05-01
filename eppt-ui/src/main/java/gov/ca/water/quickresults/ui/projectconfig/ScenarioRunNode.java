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

import gov.ca.water.calgui.project.EpptDssContainer;
import gov.ca.water.calgui.project.EpptScenarioRun;
import gov.ca.water.calgui.project.NamedDssPath;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 04-09-2019
 */
class ScenarioRunNode extends DefaultMutableTreeNode
{
	private final EpptScenarioRun _scenarioRun;
	private boolean _base;

	ScenarioRunNode(EpptScenarioRun scenarioRun)
	{
		_scenarioRun = scenarioRun;
		EpptDssContainer dssContainer = scenarioRun.getDssContainer();
		NamedDssPath dvDssFile = dssContainer.getDvDssFile();
		if(dvDssFile != null)
		{
			add(new DssPathNode(dvDssFile, "DV"));
		}
		NamedDssPath svDssFile = dssContainer.getSvDssFile();
		if(svDssFile != null)
		{
			add(new DssPathNode(svDssFile, "SV"));
		}
		NamedDssPath ivDssFile = dssContainer.getIvDssFile();
		if(ivDssFile != null)
		{
			add(new DssPathNode(ivDssFile, "IV"));
		}
		dssContainer.getExtraDssFiles().forEach(p -> add(new DssPathNode(p)));
	}

	boolean isBase()
	{
		return _base;
	}

	void setBase(boolean base)
	{
		_base = base;
	}


	EpptScenarioRun getScenarioRun()
	{
		return _scenarioRun;
	}

	@Override
	public String toString()
	{
		return _scenarioRun.getName() + " (" + _scenarioRun.getModel() + ")";
	}
}
