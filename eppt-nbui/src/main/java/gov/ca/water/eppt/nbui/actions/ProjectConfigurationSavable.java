/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */

package gov.ca.water.eppt.nbui.actions;

import java.io.IOException;

import gov.ca.water.eppt.nbui.ProjectConfigurationTopComponent;
import org.netbeans.spi.actions.AbstractSavable;
import org.openide.filesystems.FileObject;
import org.openide.loaders.SaveAsCapable;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 03-11-2019
 */
public class ProjectConfigurationSavable extends AbstractSavable implements SaveAsCapable
{
	private final ProjectConfigurationTopComponent _tc;

	public ProjectConfigurationSavable(ProjectConfigurationTopComponent tc)
	{
		_tc = tc;
		_tc.topComponentNameModified();
		register();
	}

	@Override
	protected String findDisplayName()
	{
		return "Project Configuration";
	}

	@Override
	protected void handleSave() throws IOException
	{
		new SaveProjectConfiguration().saveCurrentConfiguration();
	}

	void removeFromLookup()
	{
		_tc.removeContent(this);
		unregister();
		_tc.topComponentNameUnmodified();
	}

	@Override
	public boolean equals(Object obj)
	{
		if(obj instanceof ProjectConfigurationSavable)
		{
			ProjectConfigurationSavable m = (ProjectConfigurationSavable) obj;
			return _tc == m._tc;
		}
		return false;
	}

	@Override
	public int hashCode()
	{
		return _tc.hashCode();
	}

	@Override
	public void saveAs(FileObject fileObject, String s) throws IOException
	{
		new NewProjectConfiguration().saveAs();
		_tc.removeContent(this);
		unregister();
	}
}
