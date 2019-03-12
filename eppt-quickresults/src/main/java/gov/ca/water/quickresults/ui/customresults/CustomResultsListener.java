/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */

package gov.ca.water.quickresults.ui.customresults;

import java.awt.Window;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.*;

import calsim.app.Project;
import calsim.gui.DtsTreeModel;
import calsim.gui.DtsTreePanel;
import calsim.gui.GuiUtils;
import gov.ca.water.calgui.bo.ResultUtilsBO;
import org.apache.log4j.Logger;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 02-26-2019
 */
public class CustomResultsListener implements ActionListener
{
	private static final Logger LOGGER = Logger.getLogger(CustomResultsListener.class.getName());
	private final CustomResultsPanel _customResultsPanel;

	public CustomResultsListener(CustomResultsPanel customResultsPanel)
	{
		_customResultsPanel = customResultsPanel;
	}

	@Override
	public void actionPerformed(ActionEvent e)
	{

		switch(e.getActionCommand())
		{
			case "CR_LoadList":
				loadList();
				break;
			case "CR_SaveList":
				saveList();
				break;
			case "CR_ClearTree":
				clearTree();
				break;
			default:
				LOGGER.fatal("Testing action events " + e.getActionCommand());
		}
	}

	private void loadList()
	{
		Window window = SwingUtilities.windowForComponent(_customResultsPanel);
		ResultUtilsBO.getResultUtilsInstance(_customResultsPanel.getSwingEngine()).readCGR((JFrame) window);
	}

	private void saveList()
	{
		Window window = SwingUtilities.windowForComponent(_customResultsPanel);
		ResultUtilsBO.getResultUtilsInstance(_customResultsPanel.getSwingEngine()).writeCGR((JFrame) window);
	}

	private void clearTree()
	{
		Project p = ResultUtilsBO.getResultUtilsInstance(_customResultsPanel.getSwingEngine()).getProject();
		p.clearMTSList();
		p.clearDTSList();
		DtsTreeModel dtm = DtsTreePanel.getCurrentModel();
		DtsTreeModel.clearVectors();
		dtm.createTreeFromPrj(null, null, "");
		GuiUtils.getCLGPanel().repaint();
	}
}
