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
import calsim.app.MultipleTimeSeries;
import calsim.app.Project;

/**
 * Creates the MTS menu list dynamically from the current project's list
 *
 * @author Nicky Sandhu
 * @version $Id: MTSMenuListener.java,v 1.1.2.6 2000/12/20 20:07:18 amunevar Exp $
 */
public class MTSMenuListener implements MenuListener
{
	private ActionListener _al;
	private Comparator _comp;

	/**
	 *
	 */
	public MTSMenuListener(ActionListener al)
	{
		_al = al;
		_comp = new MTSComparator();
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
		MultipleTimeSeries[] mtsList = prj.getMTSList();
		if(mtsList == null)
		{
			return;
		}
		Arrays.sort(mtsList, _comp);
		for(int i = 0; i < mtsList.length; i++)
		{
			JMenuItem mi = new JMenuItem(mtsList[i].getName());
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
