/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */

package calsim.gui;

import javax.swing.*;

/**
 * This a panel which adapts an MPanel to be a tabbed
 * component in a JTabbedPane
 *
 * @author Nicky Sandhu
 * @version $Id: MTab.java,v 1.1.2.3 2000/12/20 20:07:20 amunevar Exp $
 */
public class MTab extends JPanel
{
	private MPanel _panel;

	/**
	 *
	 */
	public MTab(MPanel panel)
	{
		setName(panel.getFrameTitle());
		// add menu bar
		JPanel mbp = new JPanel();
		mbp.setLayout(new BoxLayout(mbp, BoxLayout.X_AXIS));
		mbp.add(panel.getJMenuBar());
		mbp.add(Box.createVerticalStrut(15));
		mbp.add(Box.createHorizontalGlue());
		mbp.setBorder(BorderFactory.createRaisedBevelBorder());
		//
		setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));
		add(mbp);
		add(panel);
		_panel = panel;
	}

	/**
	 *
	 */
	public MPanel getMPanel()
	{
		return _panel;
	}
}
