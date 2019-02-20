/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package calsim.schematic;
//import vista.graph.*;
//import vista.set.*;

import javax.swing.*;
//import java.awt.*;
//import java.awt.image.*;
//import java.awt.event.*;
//import java.util.*;

/**
 * @author Nicky Sandhu
 * @version $Id: CalsimSchematicFrame.java,v 1.1.2.3 1999/05/06 16:28:06 nsandhu Exp $
 */
public class CalsimSchematicFrame extends JFrame
{
	private CalsimSchematicCanvas _canvas;

	/**
	 *
	 */
	public CalsimSchematicFrame(CalsimSchematicCanvas sc)
	{
		super("CALSIM Schematic");
		_canvas = sc;
		JScrollPane jsp = new JScrollPane();
		jsp.setViewportView(_canvas);
		getContentPane().add(jsp);
		//
		setSize(_canvas.getPreferredSize());
		setLocation(100, 100);
		setVisible(true);
	}
}
