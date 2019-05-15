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

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.GridLayout;
import javax.swing.*;
import javax.swing.tree.DefaultMutableTreeNode;

import calsim.app.DerivedTimeSeries;
import calsim.app.MultipleTimeSeries;


/**
 * Panel that holds the DTS Tree on the Output side of the GUI
 *
 * @author Joel Fenolio
 * @author Clay Booher - one correction
 * @version $Id: DtsTreePanel.java,v 1.1.2.2 2001/07/12 01:59:36 amunevar Exp $
 */


public class DtsTreePanel extends JPanel
{

	private static final DefaultMutableTreeNode DUMBY_ROOT = new DefaultMutableTreeNode("Root");
	private static final String[] tags = {".dts", ".DTS", ".mts", ".MTS"};
	private static DtsTreeModel DTS_TREE_MODEL;
	private final JSplitPane _holder = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT);
	private final CalsimTree _tree;
	private final DTSTable _table;
	private DerivedTimeSeries _dts;

	public DtsTreePanel()
	{
		DTS_TREE_MODEL = new DtsTreeModel(DUMBY_ROOT, tags, null, this);
		_tree = new CalsimTree(DTS_TREE_MODEL);
		DTS_TREE_MODEL.setTree(_tree);
		setLayout(new BorderLayout());
		_holder.setLeftComponent(createTreeHolder());
		DerivedTimeSeries dts = new DerivedTimeSeries(" ");
		setDTS(dts);
		_table = new DTSTable(dts);
		//CB THIS FIXED IT SO THE TABLE SHOWS!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
		_holder.setRightComponent(_table);
		_holder.setDividerLocation(200);
		add(_holder, BorderLayout.CENTER);
		setVisible(true);
		//CB NO VISIBLE CHANGE
		this.setBackground(Color.YELLOW);
	}

	public static DtsTreeModel getCurrentModel()
	{
		return DTS_TREE_MODEL;
	}

	private JPanel createTreeHolder()
	{
		JPanel treeholder = new JPanel(new GridLayout(1, 1));
		JScrollPane scrollPane = new JScrollPane(_tree);
		treeholder.add(scrollPane);
		return treeholder;
	}

	void setDTSTable(DerivedTimeSeries dts, MultipleTimeSeries mts)
	{
		_table.setTableModel(dts, mts);
	}

	public DerivedTimeSeries getDTS()
	{

		return _dts;
	}

	public void setDTS(DerivedTimeSeries dts)
	{
		_dts = dts;
	}

	public DTSTable getTable()
	{
		return _table;
	}
}





