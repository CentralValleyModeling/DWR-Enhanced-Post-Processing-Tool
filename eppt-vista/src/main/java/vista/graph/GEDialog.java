/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package vista.graph;

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.Container;
import java.awt.Frame;
import javax.swing.*;

import vista.gui.Changeable;
import vista.gui.DialogButtonPanel;

/**
 * A dialog class for handling attribute change requests.
 */
public class GEDialog extends JDialog implements Changeable {
	/**
	 * constructor
	 */
	public GEDialog(Frame parent, GEDialogPanel panel) {
		super(parent, "GE Dialog");
		setModal(false);
		Container pane = getContentPane();
		pane.setLayout(new BorderLayout());
		setDialogPanel(panel);
		pane.add(new DialogButtonPanel(this), BorderLayout.SOUTH);
		pack();
	}

	/**
   *
   */
	void setDialogPanel(GEDialogPanel panel) {
		Container pane = getContentPane();
		if (_panel != null)
			pane.remove(_panel);
		_panel = panel;
		pane.add(_panel, BorderLayout.CENTER);
	}

	/**
    *
    */
	GEDialogPanel getDialogPanel() {
		return _panel;
	}

	/**
	 * Apply the changes (OK/Apply button pressed)
	 */
	public void applyChanges() {
		_panel.applyChanges();
		Component comp = getParent();
		if (comp instanceof GraphFrameInterface) {
			GraphFrameInterface gf = (GraphFrameInterface) comp;
			GECanvas gc = gf.getCanvas();
			gc.redoNextPaint();
			gc.update(gc.getGraphics());
		}
	}

	/**
	 * Done with making changes (OK/Cancel button pressed)
	 */
	public void doneChanges() {
		setVisible(false);
		dispose();
	}

	/**
	 * the dialog panel
	 */
	private GEDialogPanel _panel;
}
