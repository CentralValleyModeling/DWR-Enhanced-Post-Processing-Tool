/*
 * Copyright (c) 2020
 * United States Army Corps of Engineers - Hydrologic Engineering Center (USACE/HEC)
 * All Rights Reserved.  USACE PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from HEC
 */

package gov.ca.water.calgui.techservice.impl;

import java.io.File;
import java.io.IOException;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.swing.filechooser.FileFilter;

import org.apache.commons.io.FileUtils;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 02-18-2020
 */
public class NonSymlinkFilter extends FileFilter
{
	private static final Logger LOGGER = Logger.getLogger(NonSymlinkFilter.class.getName());

	@Override
	public boolean accept(File f)
	{
		try
		{
			return !FileUtils.isSymlink(f);
		}
		catch(IOException e)
		{
			LOGGER.log(Level.FINE, "Unable to determine if file is symlink: " + f, e);
			return true;
		}
	}

	@Override
	public String getDescription()
	{
		return "Select Directory";
	}
}