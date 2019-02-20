/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package vista.gui;

import java.io.File;
import javax.swing.filechooser.FileFilter;
import javax.swing.filechooser.FileView;

public class ExtensionFilter extends FileFilter
{
	private String extension, description;

	/**
	 *
	 */
	public ExtensionFilter(String extension, String description)
	{
		this.extension = extension.toLowerCase();
		this.description = description;
	}

	/**
	 * Whether the given file is accepted by this filter.
	 */
	public boolean accept(File f)
	{
		if(f.isDirectory())
		{
			return true;
		}
		// make both lower case for uniformity
		String fname = f.getName().trim().toLowerCase();
		// check to see if it ends in the correct extension
		return fname.endsWith(extension);
	}

	/**
	 * The description of this filter. For example: "JPG and GIF Images"
	 *
	 * @see FileView#getName
	 */
	public String getDescription()
	{
		return description;
	}
}
