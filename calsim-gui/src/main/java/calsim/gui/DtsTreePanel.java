/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */

package calsim.gui;

import java.awt.*;
import javax.swing.*;
import javax.swing.tree.DefaultMutableTreeNode;

import calsim.app.DerivedTimeSeries;
import calsim.app.MultipleTimeSeries;
//import java.io.*;
//import javax.swing.table.TableColumnModel;
//import javax.swing.event.TableModelEvent;

/**
 * Panel that holds the DTS Tree on the Output side of the GUI
 *
 * @author Joel Fenolio
 * @author Clay Booher - one correction
 * @version $Id: DtsTreePanel.java,v 1.1.2.2 2001/07/12 01:59:36 amunevar Exp $
 */


public class DtsTreePanel extends JPanel
{

	static DefaultMutableTreeNode dumbyRoot = new DefaultMutableTreeNode("Root");
	static String[] tags = {".dts", ".DTS", ".mts", ".MTS"};
	//static String pics[] = {"d:\\Calsim1\\calsim\\gui\\sphere01.gif","d:\\Calsim1\\calsim\\gui\\sphere01.gif","d:\\Calsim1\\calsim\\gui\\smdi_or.gif","d:\\Calsim1\\calsim\\gui\\smdi_or.gif"};
	private static DtsTreeModel _dtm;
	JSplitPane holder = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT);
	CalsimTree _tree;
	DTSTable _table;
	DefaultInternalFrame _fr;
	DerivedTimeSeries _dts;
	public DtsTreePanel()
	{
		_dtm = new DtsTreeModel(dumbyRoot, tags, null, this);
		_tree = new CalsimTree(_dtm);
		_dtm.setTree(_tree);
		dumbyRoot = null;
		//_fileholder = createFileHolder();
		setLayout(new BorderLayout());
		//add(_fileholder,BorderLayout.NORTH);
		holder.setLeftComponent(createTreeHolder());
		DerivedTimeSeries dts = new DerivedTimeSeries(" ");
		setDTS(dts);
		_table = new DTSTable(dts);
		//CB    _fr = new DefaultInternalFrame(_table);
		//CB    holder.setRightComponent(_fr);  //CB table is NOT visible with this original line of code (for Java 5 and 6)
		holder.setRightComponent(
				_table);  //CB THIS FIXED IT SO THE TABLE SHOWS!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
		holder.setDividerLocation(200);
		add(holder, BorderLayout.CENTER);
		setVisible(true);
		this.setBackground(Color.YELLOW);    //CB NO VISIBLE CHANGE
	}

	public static DtsTreeModel getCurrentModel()
	{
		return _dtm;
	}

	public JPanel createTreeHolder()
	{
		JPanel treeholder = new JPanel(new GridLayout(1, 1));
		JScrollPane holder = new JScrollPane(_tree);
		treeholder.add(holder);
		return treeholder;
	}

	public void setDTSTable(DerivedTimeSeries dts, MultipleTimeSeries mts)
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





