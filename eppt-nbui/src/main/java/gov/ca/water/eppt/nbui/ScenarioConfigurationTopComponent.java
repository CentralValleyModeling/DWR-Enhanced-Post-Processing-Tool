/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package gov.ca.water.eppt.nbui;

import java.awt.BorderLayout;
import java.util.Objects;
import javax.swing.*;

import gov.ca.water.eppt.nbui.actions.ScenarioConfigurationSavable;
import gov.ca.water.quickresults.ui.scenarioconfig.ScenarioConfigurationListener;
import gov.ca.water.quickresults.ui.scenarioconfig.ScenarioConfigurationPanel;
import org.openide.awt.ActionID;
import org.openide.awt.ActionReference;
import org.openide.util.NbBundle.Messages;
import org.openide.util.lookup.AbstractLookup;
import org.openide.util.lookup.InstanceContent;
import org.openide.windows.TopComponent;

import rma.swing.RmaJPanel;

/**
 * Top component which displays something.
 */
@TopComponent.Description(
		preferredID = "ScenarioConfigurationTopComponent"
)
@TopComponent.Registration(mode = "explorer", openAtStartup = true, position = 1111)
@ActionID(category = "Window", id = "gov.ca.water.eppt.nbui.ScenarioConfigurationTopComponent")
@ActionReference(path = "Menu/Window", position = 1111)
@TopComponent.OpenActionRegistration(
		displayName = "Scenario Configuration",
		preferredID = "ScenarioConfigurationTopComponent"
)
@Messages(
		{
				"CTL_ScenarioConfigurationAction=Scenario Configuration",
				"CTL_ScenarioConfigurationTopComponent=Scenario Configuration Window",
				"HINT_ScenarioConfigurationTopComponent=This is the Scenario Configuration window"
		})
public final class ScenarioConfigurationTopComponent extends EpptTopComponent
{
	private static final String TOP_COMPONENT_NAME = "Scenario Configuration";
	private final InstanceContent _instanceContent = new InstanceContent();
	private String _lastQuickState = null;

	/**
	 *
	 */
	public ScenarioConfigurationTopComponent()
	{
		setName(TOP_COMPONENT_NAME);
		associateLookup(new AbstractLookup(_instanceContent));
		RmaJPanel rmaJPanel = new RmaJPanel()
		{
			@Override
			public void setModified(boolean b)
			{
				if(b)
				{
					String newQuickState = ScenarioConfigurationPanel.getScenarioConfigurationPanel().quickState();
					if(!Objects.equals(_lastQuickState, newQuickState))
					{
						_lastQuickState = newQuickState;
						topComponentNameModified();
						ScenarioConfigurationSavable savable = getLookup().lookup(ScenarioConfigurationSavable.class);
						if(savable == null)
						{
							_instanceContent.add(
									new ScenarioConfigurationSavable(ScenarioConfigurationTopComponent.this));
						}
					}
				}
				else
				{
					setName(TOP_COMPONENT_NAME);
				}
			}
		};
		ScenarioConfigurationPanel scenarioConfigurationPanel = ScenarioConfigurationPanel.getScenarioConfigurationPanel();
		ScenarioConfigurationListener scenarioConfigurationListener = new ScenarioConfigurationListener(
				scenarioConfigurationPanel);
		scenarioConfigurationPanel.setActionListener(scenarioConfigurationListener);
		rmaJPanel.setLayout(new BorderLayout());
		rmaJPanel.add(scenarioConfigurationPanel, BorderLayout.CENTER);
		JScrollPane scrollPane = new JScrollPane(rmaJPanel);
		setLayout(new BorderLayout());
		add(scrollPane, BorderLayout.CENTER);
	}

	/**
	 * @param content
	 */
	public void removeContent(Object content)
	{
		_instanceContent.remove(content);
	}

	/**
	 *
	 */
	public void topComponentNameModified()
	{
		setDisplayName(TOP_COMPONENT_NAME + "*");
	}

	/**
	 *
	 */
	public void topComponentNameUnmodified()
	{
		setDisplayName(TOP_COMPONENT_NAME);
	}

	@Override
	public String getJavaHelpId()
	{
		return null;
	}
}
