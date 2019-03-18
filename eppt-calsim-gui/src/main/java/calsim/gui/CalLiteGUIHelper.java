/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
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
