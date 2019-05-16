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
package vista.db.dss;

import java.io.BufferedInputStream;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.Properties;

/**
 * @author Nicky Sandhu
 * @version $Id: ServerProperties.java,v 1.1 2003/10/02 20:48:46 redwood Exp $
 */
public class ServerProperties
{
	public static Properties props = new Properties();

	static
	{
		String fs = System.getProperty("file.separator");
		InputStream is = null;
		try
		{
			is = new FileInputStream(System.getProperty("user.dir") + fs
					+ ".vistaServer");
		}
		catch(Exception e)
		{
			// System.err.println("Server properties file "+".vistaServer "+
			// "not found in current or home directory!\n using defaults");
		}
		if(is != null)
		{
			initialize(is);
		}
		else
		{
			initialize();
		}
	}

	/**
	 * initialize to default
	 */
	private static void initialize()
	{
		props.put("logfile", "");
		props.put("password", ""); // blank password... accept all
	}

	/**
	 * initialize from stream
	 */
	private static void initialize(InputStream is)
	{
		initialize();
		if(is == null)
		{
			return;
		}
		// end default properties
		try
		{
			props.load(new BufferedInputStream(is));
		}
		catch(IOException ioe)
		{
			System.out.println(ioe.getMessage());
		}
	}

	/**
	 *
	 */
	public static String getProperty(String key)
	{
		return props.getProperty(key);
	}
}
