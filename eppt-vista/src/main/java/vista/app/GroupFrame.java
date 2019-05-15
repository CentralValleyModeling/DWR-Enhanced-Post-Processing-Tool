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

import java.awt.BorderLayout;
import java.awt.Container;
import java.awt.Toolkit;
import javax.swing.*;

import vista.gui.VistaUtils;
import vista.set.Group;

/**
 * A frame containing the view of a group and its associated menu items.
 *
 * @author Nicky Sandhu
 * @version $Id: GroupFrame.java,v 1.1 2003/10/02 20:48:31 redwood Exp $
 */
public class GroupFrame extends DefaultFrame
{
	/**
	 *
	 */
	private GroupTable _groupTablePanel;

	/**
	 *
	 */
	public GroupFrame(Group g)
	{
		setIconImage(Toolkit.getDefaultToolkit().createImage(
				VistaUtils.getImageAsBytes("/vista/OWE.gif")));
		_groupTablePanel = new GroupTable(g);
		JMenuBar mbar = new JMenuBar();
		_groupTablePanel.addMenus(mbar);
		// mbar.setHelpMenu( new JMenu("Help") );
		getRootPane().setJMenuBar(mbar);
		// add components...
		Container contentPane = getContentPane();
		contentPane.setLayout(new BorderLayout());
		contentPane.add(_groupTablePanel, BorderLayout.CENTER);
		pack();
		this.setTitle(g.getName());
		// set size to default
		// int width = new
		// Integer(_mainProps.getProperty("MainGUI.width")).intValue();
		// int height = new
		// Integer(_mainProps.getProperty("MainGUI.height")).intValue();
		// setSize( width, height );
		setSize(750, 900);
		setVisible(true);

	}

	/**
	 *
	 */
	public GroupTable getGroupTable()
	{
		return _groupTablePanel;
	}
}
