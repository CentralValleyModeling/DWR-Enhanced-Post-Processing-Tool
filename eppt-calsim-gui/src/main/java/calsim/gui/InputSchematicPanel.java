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
import javax.swing.*;

import calsim.schematic.CalsimSchematic;
import calsim.schematic.CalsimSchematicCanvas;
import calsim.schematic.input.InputHandler;

/**
 * A panel to manage display of schematic canvas
 *
 * @author Nicky Sandhu
 * @version $Id: InputSchematicPanel.java,v 1.1.2.4 2000/12/20 20:07:14 amunevar Exp $
 */
public class InputSchematicPanel extends JPanel
{
	private CalsimSchematicCanvas _csc;
	private JScrollPane _scrollPane;

	/**
	 *
	 */
	public InputSchematicPanel(CalsimSchematic sch)
	{
		setLayout(new BorderLayout());
		_scrollPane = new JScrollPane();
		setSchematic(sch);
		add(_scrollPane, BorderLayout.CENTER);
		setBorder(BorderFactory.
									   createTitledBorder(
											   BorderFactory.createMatteBorder(1, 1, 1, 1, Color.blue),
											   "Schematic")
		);
	}

	/**
	 *
	 */
	public void setSchematic(CalsimSchematic sch)
	{
		if(sch == null)
		{
			return;
		}
		_csc = new CalsimSchematicCanvas(sch);
		//_csc.add(new PopupMenuHandler());
		//    _csc.add(new InputPopupMenuHandler());
		_csc.add(new InputHandler());
		_csc.setSize(sch.getPreferredSize());
		_scrollPane.setViewportView(_csc);
		_scrollPane.setViewportBorder(BorderFactory.createRaisedBevelBorder());
		int vinc = (int) Math.round(Math.max(0.002 * _csc.getSize().height, 5));
		int hinc = (int) Math.round(Math.max(0.002 * _csc.getSize().width, 5));
		_scrollPane.getVerticalScrollBar().setUnitIncrement(vinc);
		_scrollPane.getHorizontalScrollBar().setUnitIncrement(hinc);
		_scrollPane.getViewport().putClientProperty("EnableWindowBlit", Boolean.TRUE);
		_scrollPane.getViewport().setScrollMode(JViewport.BACKINGSTORE_SCROLL_MODE);
	}

	/**
	 *
	 */
	public void removeSchematic()
	{
		_scrollPane.setViewportView(new JPanel());
		_scrollPane.revalidate();
	}
}
