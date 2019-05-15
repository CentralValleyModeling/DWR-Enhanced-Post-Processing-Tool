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
	private ActionListener _actionListener;

	public EpptPanel()
	{
		_swingEngine = new SwingEngine();
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

	public SwingEngine getSwingEngine()
	{
		return _swingEngine;
	}

	public abstract String getJavaHelpId();

	protected ActionListener getActionListener()
	{
		return _actionListener;
	}

	public void setActionListener(ActionListener actionListener)
	{
		_actionListener = actionListener;
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
}
