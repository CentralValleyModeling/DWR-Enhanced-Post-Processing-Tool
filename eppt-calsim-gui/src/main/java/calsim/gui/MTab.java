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
