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

package calsim.debug;

public class DebugSetting
{
	public static boolean DEBUG_FILEINPUT = false;
	private static String _DEBUG_OPTION = "none";

	public static String getDebugOption()
	{
		return _DEBUG_OPTION;

	}

	public static void setDebugOption(String var)
	{
		_DEBUG_OPTION = var;

	}

}

