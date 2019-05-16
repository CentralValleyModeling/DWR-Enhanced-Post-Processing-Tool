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

package gov.ca.water.quickresults.ui.customresults;

import java.awt.Window;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.swing.*;

import calsim.app.Project;
import calsim.gui.DtsTreeModel;
import calsim.gui.DtsTreePanel;
import calsim.gui.GuiUtils;
import gov.ca.water.calgui.bo.ResultUtilsBO;

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
				LOGGER.log(Level.FINE, "Testing action events {0}", e.getActionCommand());
		}
	}

	private void loadList()
	{
		Window window = SwingUtilities.windowForComponent(_customResultsPanel);
		ResultUtilsBO.getResultUtilsInstance().readCGR((JFrame) window);
	}

	private void saveList()
	{
		Window window = SwingUtilities.windowForComponent(_customResultsPanel);
		ResultUtilsBO.getResultUtilsInstance().writeCGR((JFrame) window, null);
	}

	private void clearTree()
	{
		Project p = ResultUtilsBO.getResultUtilsInstance().getProject();
		p.clearMTSList();
		p.clearDTSList();
		DtsTreeModel dtm = DtsTreePanel.getCurrentModel();
		DtsTreeModel.clearVectors();
		dtm.createTreeFromPrj(null, null, "");
		GuiUtils.getCLGPanel().repaint();
	}
}
