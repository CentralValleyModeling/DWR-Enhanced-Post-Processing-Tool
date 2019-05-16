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
//! Custom file filter builder for file choosers

import java.io.File;

/**
 * Custom file filter class for use in CalLite GUI
 *
 * @author tslawecki
 */
public class SimpleFileFilter extends javax.swing.filechooser.FileFilter
{
	private final String _fileExt;
	private final String _desc;

	/**
	 * Creates a FileFilter for use with file choosers. Automatically builds
	 * description string from extension
	 *
	 * @param aFileExt
	 */
	public SimpleFileFilter(String aFileExt)
	{
		_fileExt = aFileExt.toLowerCase();
		_desc = aFileExt.toUpperCase() + " File (*." + aFileExt.toLowerCase() + ")";
	}

	/**
	 * Creates a FileFilter for use with file choosers, assigning description
	 * explicitly.
	 *
	 * @param aFileExt
	 * @param aDesc
	 */
	public SimpleFileFilter(String aFileExt, String aDesc)
	{
		_fileExt = aFileExt.toLowerCase();
		_desc = aDesc;
	}

	@Override
	public boolean accept(File file)
	{
		// Convert to lower case before checking extension
		return (file.getName().toLowerCase().endsWith("." + _fileExt) || file.isDirectory());
	}

	@Override
	public String getDescription()
	{
		return _desc;
	}
}
