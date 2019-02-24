/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */

package gov.ca.water.quickresults.ui;

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.Container;
import java.util.ArrayList;
import java.util.List;
import javax.swing.*;

import gov.ca.water.calgui.bo.RBListItemBO;
import org.swixml.SwingEngine;

import rma.swing.RmaJPanel;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 02-21-2019
 */
public class ScenarioConfigurationPanel extends RmaJPanel
{
	private static final String SCENARIO_CONFIGURATION_XML_PATH = "ui/Scenario_Configuration.xml";
	private final SwingEngine _engine;

	public ScenarioConfigurationPanel()
	{
		_engine = new SwingEngine();
		try
		{
			super.setLayout(new BorderLayout());
			Container swixmlScenarioConfigurationPanel = _engine.render(SCENARIO_CONFIGURATION_XML_PATH);
			super.add(swixmlScenarioConfigurationPanel);
		}
		catch(Exception e)
		{
			throw new IllegalStateException(e);
		}
	}

	SwingEngine getSwingEngine()
	{
		return _engine;
	}

	Component getSelectedList()
	{
		return _engine.find("SelectedList");
	}

	JPanel getControls2()
	{
		return (JPanel) _engine.find("controls2");
	}

	List<RBListItemBO> getScenarios()
	{
		List<RBListItemBO> retval = new ArrayList<>();
		Component component = getSelectedList();
		if(component instanceof JList)
		{
			JList<RBListItemBO> lstScenarios = (JList<RBListItemBO>) component;
			ListModel<RBListItemBO> model = lstScenarios.getModel();
			for(int i = 0; i < model.getSize(); i++)
			{
				retval.add(model.getElementAt(i));
			}
		}
		return retval;
	}
}
