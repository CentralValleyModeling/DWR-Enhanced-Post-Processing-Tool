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

package gov.ca.water.calgui.techservice.impl;

import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.util.Properties;
import javax.help.HelpSet;
import javax.help.JHelp;
import javax.swing.*;

import org.apache.log4j.Logger;

/**
 * Generates help window allowing search of Cal-Lite user's manual.
 *
 * @author Mohan
 */
public class CalLiteHelp
{
	private static final Logger LOG = Logger.getLogger(CalLiteHelp.class.getName());
	private static Properties properties = new Properties();

	/**
	 * This method will show the help dialg box for which ever label that is
	 * passed in. Ex Run Settings, Hydroclimate..
	 *
	 * @param label Takes calling dashboard's title which is mapped to help html
	 *              docs via docs/map.xml
	 */
	public void showHelp(String label)
	{
		try
		{
			properties.load(CalLiteHelp.class.getClassLoader().getResourceAsStream("callite-gui.properties"));
		}
		catch(IOException e1)
		{
			LOG.debug(e1);
		}
		String path = new File(properties.getProperty("help.dir")).getAbsolutePath();
		String versionId = properties.getProperty("version.id");
		try
		{
			URL url = new URL("file:///" + path);
			JHelp helpViewer = new JHelp(new HelpSet(null, url));
			JFrame help = new JFrame("CalLite " + versionId + " GUI Help");
			help.getContentPane().add(helpViewer);
			helpViewer.setCurrentID(label);
			help.pack();
			help.setVisible(true);
		}
		catch(Exception e)
		{
			LOG.debug("Helpset not found at: " + path + ". " + e);
		}
	}
}
