/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
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

