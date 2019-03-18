/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */

package gov.ca.water.quickresults.ui;

import java.awt.Component;
import java.awt.Container;
import java.awt.event.ActionListener;
import java.net.URL;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.swing.*;

import org.swixml.SwingEngine;

import rma.swing.RmaJPanel;

import static gov.ca.water.calgui.constant.Constant.CONFIG_DIR;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 02-28-2019
 */
public abstract class EpptPanel extends RmaJPanel
{
	private static final Logger LOGGER = Logger.getLogger(EpptPanel.class.getName());
	private final SwingEngine _swingEngine;

	public EpptPanel()
	{
		_swingEngine = new SwingEngine();
	}

	public SwingEngine getSwingEngine()
	{
		return _swingEngine;
	}

	public abstract String getJavaHelpId();

	public void setActionListener(ActionListener actionListener)
	{
		getSwingEngine().setActionListener(this, actionListener);
	}

	protected Container renderSwixml(String fileName) throws Exception
	{
		Container retval;
		try
		{
			String configPath = "file:" + CONFIG_DIR + fileName;
			URL url = new URL(configPath);
			retval = getSwingEngine().render(url);
		}
		catch(Exception ex)
		{
			LOGGER.log(Level.FINE, "Unable to render from URL, trying test file", ex);
			retval = getSwingEngine().render("ui/" + fileName);
		}
		return retval;
	}


	public static void setCheckboxesSelectedRecusive(boolean b, Container container)
	{
		Component[] components = container.getComponents();
		for(final Component component : components)
		{
			if(component instanceof JCheckBox)
			{
				JCheckBox c = (JCheckBox) component;
				c.setSelected(b);
			}
			else if(component instanceof Container)
			{
				setCheckboxesSelectedRecusive(b, (Container) component);
			}
		}
	}
}
