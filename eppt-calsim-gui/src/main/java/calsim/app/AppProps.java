/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */

package calsim.app;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * Properties for the application. These properties are stored in the
 * app.props file in the [user.dir]/.calsim directory. These directories
 * & files are created if they do not exist.
 *
 * @author Nicky Sandhu
 * @version $Id: AppProps.java,v 1.1.2.3 2000/12/20 20:02:06 amunevar Exp $
 */
public final class AppProps
{
	private static final Logger LOGGER = Logger.getLogger(AppProps.class.getName());
	private static final Properties PROPERTIES = new Properties();

	static
	{
		initializeProps();
	}

	private AppProps()
	{
		throw new AssertionError("Static utility class");
	}

	private static void initializeProps()
	{
		File appPropsDir = checkForPropsDir();
		File appPropsFile = new File(appPropsDir.getPath() + File.separator + "app.props");
		if(!appPropsFile.exists())
		{
			// then look in jar file for defaults & save to props file
			String filename = "J:\\DWR\\RCP_ProofOfConcept\\EPPT_ANT\\Scenario/calsim/app/app.props";
			try
			{
				PROPERTIES.load(new FileInputStream(filename));
				save();
			}
			catch(IOException ioe)
			{
				LOGGER.log(Level.SEVERE, "Error loading properties from file: " + filename, ioe);
			}
		}
		else
		{
			// read from app.props file
			try
			{
				// always load defaults and then user customized properties
				PROPERTIES.load(new FileInputStream("J:\\DWR\\RCP_ProofOfConcept\\EPPT_ANT\\Scenario/calsim/app/app.props"));
				PROPERTIES.load(new FileInputStream(appPropsFile));
				// make sure the version is matched or exists. If not load from
				// jar file first and then from props file again
				Properties jarprops = new Properties();
				jarprops.load(
						new FileInputStream("J:\\DWR\\RCP_ProofOfConcept\\EPPT_ANT\\Scenario/calsim/app/app.props"));
				String propVersion = PROPERTIES.getProperty("AppProps.version");
				String jarPropVersion = jarprops.getProperty("AppProps.version");
				if(propVersion == null || Integer.parseInt(propVersion) < Integer.parseInt(jarPropVersion))
				{
					// load the latest
					String filename = "J:\\DWR\\RCP_ProofOfConcept\\EPPT_ANT\\Scenario/calsim/app/app.props";
					PROPERTIES.load(new FileInputStream(filename));
					// override with users properties
					PROPERTIES.load(new FileInputStream(appPropsFile));
					// finally override the versionid for app props
					PROPERTIES.put("AppProps.version", jarPropVersion);
					// finally save all this work
					save();
				}
			}
			catch(IOException ioe)
			{
				LOGGER.log(Level.SEVERE, "Error loading properties from file", ioe);
			}
		}
	}

	private static File checkForPropsDir()
	{
		// first look in user.home for .calsim directory
		String appDir = System.getProperty("user.home") + File.separator + ".calsim";
		File appPropsDir = new File(appDir);
		if(!appPropsDir.exists())
		{
			// create this directory...
			boolean success = appPropsDir.mkdir();
			if(!success)
			{
				LOGGER.log(Level.SEVERE, "Unable to create directory: {0}", appPropsDir);
			}
		}
		return appPropsDir;
	}

	/**
	 *
	 */
	public static void save()
	{
		// look for .calsim directory in user.home & a file called app.props
		File appPropsDir = checkForPropsDir();
		File appPropsFile = new File(appPropsDir.getPath() + File.separator + "app.props");
		// then look in jar file for defaults & save to props file
		try
		{
			PROPERTIES.store(new FileOutputStream(appPropsFile), "application user properties");
		}
		catch(IOException ioe)
		{
			LOGGER.log(Level.SEVERE, "Error loading properties from file: " + appPropsFile, ioe);
		}
	}

	/**
	 *
	 */
	public static String getProperty(String key)
	{
		return PROPERTIES.getProperty(key);
	}

	/**
	 *
	 */
	public static void setProperty(String key, String val)
	{
		PROPERTIES.put(key, val);
	}
}
