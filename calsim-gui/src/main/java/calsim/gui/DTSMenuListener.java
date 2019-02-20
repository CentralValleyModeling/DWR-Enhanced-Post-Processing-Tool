/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */

package calsim.gui;

import java.awt.event.ActionListener;
import java.util.Arrays;
import java.util.Comparator;
import javax.swing.*;
import javax.swing.event.MenuEvent;
import javax.swing.event.MenuListener;

import calsim.app.AppUtils;
import calsim.app.DerivedTimeSeries;
import calsim.app.Project;

/**
 * Creates the DTS menu list dynamically from the current project's list
 *
 * @author Nicky Sandhu
 * @version $Id: DTSMenuListener.java,v 1.1.2.5 2000/12/20 20:07:03 amunevar Exp $
 */
public class DTSMenuListener implements MenuListener
{
	ActionListener _al;
	Comparator _comp;

	/**
	 *
	 */
	public DTSMenuListener(ActionListener al)
	{
		_al = al;
		_comp = new DTSComparator();
	}

	/**
	 * Invoked when a menu item is selected.
	 *
	 * @param e a MenuEvent object
	 */
	public void menuSelected(MenuEvent e)
	{
		JMenu menu = (JMenu) e.getSource();
		menu.removeAll();
		Project prj = AppUtils.getCurrentProject();
		DerivedTimeSeries[] dtsList = prj.getDTSList();
		if(dtsList == null)
		{
			return;
		}
		Arrays.sort(dtsList, _comp);
		for(int i = 0; i < dtsList.length; i++)
		{
			JMenuItem mi = new JMenuItem(dtsList[i].getName());
			mi.addActionListener(_al);
			menu.add(mi);
		}
	}

	/**
	 * Invoked when the menu selection changes.
	 *
	 * @param e a MenuEvent object
	 */
	public void menuDeselected(MenuEvent e)
	{
	}

	/**
	 * Invoked when the menu selection is canceled.
	 *
	 * @param e a MenuEvent object
	 */
	public void menuCanceled(MenuEvent e)
	{
	}
}
