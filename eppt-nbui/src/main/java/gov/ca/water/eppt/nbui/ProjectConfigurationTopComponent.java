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
package gov.ca.water.eppt.nbui;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.util.Collection;
import javax.swing.*;

import gov.ca.water.calgui.project.EpptConfigurationController;
import gov.ca.water.eppt.nbui.actions.ProjectConfigurationSavable;
import gov.ca.water.quickresults.ui.global.EpptConfigurationPane;
import javafx.application.Platform;
import javafx.embed.swing.JFXPanel;
import javafx.scene.Scene;
import org.netbeans.api.actions.Savable;
import org.openide.awt.ActionID;
import org.openide.awt.ActionReference;
import org.openide.awt.ActionReferences;
import org.openide.util.NbBundle.Messages;
import org.openide.util.lookup.AbstractLookup;
import org.openide.util.lookup.InstanceContent;
import org.openide.windows.TopComponent;
import org.openide.windows.WindowManager;

/**
 * Top component which displays something.
 */
@TopComponent.Description(
		preferredID = "ProjectConfigurationTopComponent",
		iconBase = "gov/ca/water/eppt/nbui/ProjectConfiguration.png"
)
@TopComponent.Registration(mode = "explorer", openAtStartup = true, position = 1111)
@ActionID(category = "Window", id = "gov.ca.water.eppt.nbui.ProjectConfigurationTopComponent")
@ActionReferences({
		@ActionReference(path = "Menu/Window", position = 1111),
		@ActionReference(path = "Toolbars/Window", position = 1111)
})
@TopComponent.OpenActionRegistration(
		displayName = "Project Configuration",
		preferredID = "ProjectConfigurationTopComponent"
)
@Messages(
		{
				"CTL_ProjectConfigurationAction=Project Configuration",
				"CTL_ProjectConfigurationTopComponent=Project Configuration Window",
				"HINT_ProjectConfigurationTopComponent=This is the Project Configuration window"
		})
public final class ProjectConfigurationTopComponent extends EpptTopComponent
{
	private static final String TOP_COMPONENT_NAME = "Project Configuration";
	private final InstanceContent _instanceContent = new InstanceContent();

	public ProjectConfigurationTopComponent()
	{
		setName(TOP_COMPONENT_NAME);
		associateLookup(new AbstractLookup(_instanceContent));
		initComponents();
	}

	public void removeContent(Object content)
	{
		_instanceContent.remove(content);
	}

	public void topComponentNameModified()
	{
		SwingUtilities.invokeLater(()->setDisplayName(TOP_COMPONENT_NAME + "*"));
	}

	public void topComponentNameUnmodified()
	{
		setDisplayName(TOP_COMPONENT_NAME);
	}

	@Override
	public String getJavaHelpId()
	{
		return "3.1_ProjectConfiguration.htm";
	}

	private void initComponents()
	{
		EpptConfigurationController epptConfigurationController = EpptControllerProvider.getEpptConfigurationController();
		epptConfigurationController.modifiedProperty().addListener((e,o,n)->
		{
			Collection<? extends ProjectConfigurationSavable> projectConfigurationSavables = Savable.REGISTRY.lookupAll(
					ProjectConfigurationSavable.class);
			if(n && projectConfigurationSavables.isEmpty())
			{
				new ProjectConfigurationSavable(this);
			}
		});
		JFXPanel jfxPanel = new JFXPanel();
		Platform.runLater(() ->
		{
			EpptConfigurationPane epptConfigurationPane = new EpptConfigurationPane(epptConfigurationController);
			epptConfigurationPane.setPrefHeight(900);
			EpptControllerProvider.addListener(()->Platform.runLater(epptConfigurationPane::reloadProject));
			jfxPanel.setScene(new Scene(epptConfigurationPane));
			epptConfigurationPane.reloadProject();
		});
		JScrollPane scrollPane = new JScrollPane(jfxPanel);
		setLayout(new BorderLayout());
		add(scrollPane, BorderLayout.CENTER);
	}
}
