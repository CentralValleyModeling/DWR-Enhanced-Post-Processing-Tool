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

package calsim.gui;

import calsim.app.AppUtils;
import calsim.app.Project;

public final class CalLiteGUIHelper
{

	Project project = new Project();

	public CalLiteGUIHelper()
	{
		AppUtils.setCurrentProject(project);

		project.setDVFile("D:\\workspace\\CalLite\\Scenarios\\DEFAULT_DV.dss");
		AppUtils.baseOn = true;
	}

}
