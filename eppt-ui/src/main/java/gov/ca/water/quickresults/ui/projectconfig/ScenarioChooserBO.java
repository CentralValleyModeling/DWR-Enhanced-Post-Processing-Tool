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

package gov.ca.water.quickresults.ui.projectconfig;

import java.awt.Component;
import java.awt.Window;
import java.io.File;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Logger;
import javax.swing.*;

import gov.ca.water.calgui.bo.GUILinksAllModelsBO;
import gov.ca.water.calgui.bo.RBListItemBO;
import gov.ca.water.calgui.bo.SimpleFileFilter;
import gov.ca.water.calgui.constant.EpptPreferences;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 03-21-2019
 */
public class ScenarioChooserBO
{
	private static final Logger LOGGER = Logger.getLogger(ScenarioChooserBO.class.getName());
	private final DefaultListModel _lmScenNames;
	private final Component _parentComponent;
	private final JFileChooser _fc = new JFileChooser();

	/**
	 * Constructor used for DSS files, result is appended to list, radiobuttons
	 * are enabled when list length greater than 1, button is enabled when list
	 * is not empty;
	 */
	public ScenarioChooserBO(DefaultListModel<RBListItemBO> lmScenNames, Component mainFrame)
	{
		_parentComponent = mainFrame;
		_lmScenNames = lmScenNames;
		_fc.setFileFilter(new SimpleFileFilter("DSS"));
		_fc.setCurrentDirectory(EpptPreferences.getScenariosPaths().toFile());
		_fc.setMultiSelectionEnabled(false);
		_fc.setDialogTitle("Select Scenario");
	}

	public DefaultListModel getLmScenNames()
	{
		return _lmScenNames;
	}


	private GUILinksAllModelsBO.Model chooseModel()
	{
		Object[] objects = GUILinksAllModelsBO.Model.values().toArray();
		Window window = SwingUtilities.windowForComponent(_parentComponent);
		return (GUILinksAllModelsBO.Model) JOptionPane.showInputDialog(window, "EPPT Model",
				"Choose Model:", JOptionPane.QUESTION_MESSAGE, null,
				objects, objects[0]);

	}


	List<RBListItemBO> createScenario()
	{
		List<RBListItemBO> retval = new ArrayList<>();
		GUILinksAllModelsBO.Model model = chooseModel();
		_fc.setFileSelectionMode(JFileChooser.FILES_ONLY);
		int dialogRC = _fc.showDialog(_parentComponent, "Select");
		if(_lmScenNames != null && dialogRC != 1)
		{
			for(File file : _fc.getSelectedFiles())
			{
				retval.add(new RBListItemBO(file.getPath(), file.getName(), model));
			}
		}
		return retval;
	}
}
