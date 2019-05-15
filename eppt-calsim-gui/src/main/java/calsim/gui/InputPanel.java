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

package calsim.gui;

import javax.swing.*;

import calsim.app.DefaultTableData;
import calsim.app.Study;
//import java.util.*;

/**
 * Panel containing the system tables
 *
 * @author Nicky Sandhu
 * @version $Id: InputPanel.java,v 1.1.2.11 2001/07/12 01:59:41 amunevar Exp $
 */
public class InputPanel extends JTabbedPane
{
	String[] tabNames = {"Connectivity", "Reservoir", "Channel", "Delivery", "Return", "Inflow", "Weight"};
	String[] stdFiles = {
			"connectivity-table.csv",
			"reservoir-node-table.csv",
			"channel-arc-table.csv",
			"delivery-arc-table.csv",
			"return-flow-arc-table.csv",
			"inflow-arc-table.csv",
			"weights-table.csv"
	};
	String[][] headers = {
			new String[]{"Node", "Arcs IN", "Arcs OUT", "Storage", "Description"},
			new String[]{"Node", "# of Levels", "Level 1", "Level 2", "Level 3", "Level 4", "Level 5", "Level 6", "Level 7", "Level 8", "Level 9", "Level 10", "Units", "Discharge Arc", "Description"},
			new String[]{"Arc", "Minimum Flow", "Units", "Maximum Flow", "Units", "Minimum Instream Flow", "Units", "Description"},
			new String[]{"Arc", "Demand", "Units", "Description"},
			new String[]{"Arc", "Assoc. Delivery Arc", "Return Fraction", "Description"},
			new String[]{"Arc", "Inflow", "Units", "Description"},
			new String[]{"Dvar", "Weight", "Dvar Units", "Description"}
	};

	/**
	 *
	 */
	public InputPanel()
	{
		for(int i = 0; i < tabNames.length; i++)
		{
			addTab(tabNames[i], new MTab(new InputDataUI(new DefaultTableData(tabNames[i], headers[i]))));
		}
	}

	/**
	 *
	 */
	public InputPanel(Study study)
	{
		for(int i = 0; i < tabNames.length; i++)
		{
			addTab(tabNames[i], new MTab(new InputDataUI(new DefaultTableData(tabNames[i], headers[i]))));
		}
	}

	/**
	 *
	 */
	public void clearData()
	{
		MTab mt;
		InputDataUI mp;
		for(int i = 0; i < tabNames.length; i++)
		{
			mt = (MTab) getComponentAt(i);
			mp = (InputDataUI) mt.getMPanel();
			mp.resetModel();
		}
	}

	/**
	 * returns the filename to be saved to for the given tab
	 */
	public String getFilenameForTab(String tab)
	{
		//    boolean found = false;
		for(int i = 0; i < tabNames.length; i++)
		{
			if(tab.equalsIgnoreCase(tabNames[i]))
			{
				return stdFiles[i];
			}
		}
		return null;
	}
}
