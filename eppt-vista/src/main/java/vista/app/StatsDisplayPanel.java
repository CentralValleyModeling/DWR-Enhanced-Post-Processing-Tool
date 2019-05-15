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
package vista.app;

import java.awt.GridLayout;
import javax.swing.*;

import vista.set.DataSet;
import vista.set.SetUtils;

/**
 * 
 * 
 * @author Nicky Sandhu
 * @version $Id: StatsDisplayPanel.java,v 1.1 2003/10/02 20:48:42 redwood Exp $
 */
public class StatsDisplayPanel extends JPanel {
	public StatsDisplayPanel(DataSet ds) {
		if (ds == null)
			throw new IllegalArgumentException(
					"Null data set cannot have stats");
		setLayout(new GridLayout(2, 1));
		JTextArea ta1, ta2;
		add(ta2 = new JTextArea(SetUtils.getHeader(ds).toString()));
		add(ta1 = new JTextArea(SetUtils.getStats(ds)));
		ta1.setEditable(false);
		ta2.setEditable(false);
	}
}
