/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */

package gov.ca.water.quickresults.ui.quickresults;

import java.awt.Component;
import java.util.List;
import javax.swing.*;

import gov.ca.water.calgui.EpptInitializationException;
import gov.ca.water.calgui.bo.GUILinksAllModelsBO;
import gov.ca.water.calgui.bo.RBListItemBO;
import gov.ca.water.quickresults.ui.EpptPanel;
import gov.ca.water.quickresults.ui.EpptScaffold;
import gov.ca.water.quickresults.ui.projectconfig.ProjectConfigurationPanel;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 02-23-2019
 */
public class QuickResultsScaffold extends EpptScaffold
{

	public static void main(String[] args) throws EpptInitializationException
	{
		new QuickResultsScaffold().initScaffold();
	}


	private static void addScenarios()
	{
		ProjectConfigurationPanel projectConfigurationPanel = ProjectConfigurationPanel.getProjectConfigurationPanel();
		JRadioButton radioButton = (JRadioButton) projectConfigurationPanel.getSwingEngine().find("rdbp001");
		radioButton.setSelected(true);
		Component component = projectConfigurationPanel.getSwingEngine().find("SelectedList");
		if(component instanceof JList)
		{
			JList<RBListItemBO> lstScenarios = (JList<RBListItemBO>) component;
			List<RBListItemBO> currentScenarios = projectConfigurationPanel.getScenarios();
			String baseFile = Thread.currentThread().getContextClassLoader().getResource(
					"Base.dss").getFile().substring(1);
			RBListItemBO base = new RBListItemBO(baseFile, "Base.dss", GUILinksAllModelsBO.Model.CAL_LITE);
			base.setSelected(true);
			currentScenarios.add(base);
			String altFile = Thread.currentThread().getContextClassLoader().getResource(
					"Alternative.dss").getFile().substring(1);
			currentScenarios.add(new RBListItemBO(altFile, "Alternative.dss", GUILinksAllModelsBO.Model.CAL_LITE));
			String cs2 = Thread.currentThread().getContextClassLoader().getResource(
					"CSII_DCR2017_Base_DV.dss").getFile().substring(1);
			currentScenarios.add(new RBListItemBO(cs2, "CSII_DCR2017_Base_DV.dss", GUILinksAllModelsBO.Model.CAL_SIM_2));
			DefaultListModel<RBListItemBO> defaultModel = new DefaultListModel<>();
			for(RBListItemBO item : currentScenarios)
			{
				defaultModel.addElement(item);
			}
			lstScenarios.setModel(defaultModel);
		}
	}

	@Override
	protected EpptPanel buildEpptPanel()
	{
		addScenarios();
		QuickResultsPanel quickResultsPanel = new QuickResultsPanel();
		QuickResultsListener quickResultsListener = new QuickResultsListener(quickResultsPanel);
		quickResultsPanel.getSwingEngine().setActionListener(quickResultsPanel, quickResultsListener);
		return quickResultsPanel;
	}
}
