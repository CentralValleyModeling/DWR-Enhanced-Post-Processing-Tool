/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package vista.graph;

import java.awt.BorderLayout;
import java.awt.FlowLayout;
import java.awt.Insets;
import javax.swing.*;
import javax.swing.border.Border;

/**
 * A panel to edit insets. If invalid value is entered in the text field a
 * message is displayed with the error and previous values are used.
 * 
 * @author Nicky Sandhu
 * @version $Id: InsetsEditPanel.java,v 1.1 2003/10/02 20:49:02 redwood Exp $
 */
public class InsetsEditPanel extends JPanel {
	/**
	 * Create a edit panel with given insets
	 */
	public InsetsEditPanel(Insets i) {
		//
		top = i.top;
		left = i.left;
		bottom = i.bottom;
		right = i.right;
		//
		topField = new JTextField(new Integer(top).toString(), 6);
		topField.setBorder(BorderFactory.createTitledBorder("Top"));
		leftField = new JTextField(new Integer(left).toString(), 6);
		leftField.setBorder(BorderFactory.createTitledBorder("Left"));
		bottomField = new JTextField(new Integer(bottom).toString(), 6);
		bottomField.setBorder(BorderFactory.createTitledBorder("Bottom"));
		rightField = new JTextField(new Integer(right).toString(), 6);
		rightField.setBorder(BorderFactory.createTitledBorder("Right"));
		//
		JPanel iPanel = new JPanel();
		iPanel.setLayout(new FlowLayout());
		iPanel.add(topField);
		iPanel.add(leftField);
		iPanel.add(bottomField);
		iPanel.add(rightField);
		//
		JPanel mainPanel = this;
		mainPanel.setLayout(new BorderLayout());
		mainPanel.add(iPanel, BorderLayout.CENTER);
		//
		Border eBorder = BorderFactory.createEtchedBorder();
		Border mainBorder = BorderFactory.createTitledBorder(eBorder, "Insets");
		iPanel.setBorder(mainBorder);
	}

	/**
	 * return an insets object with the set values if possible
	 */
	public Insets getInsets() {
		try {
			top = new Integer(topField.getText()).intValue();
			left = new Integer(leftField.getText()).intValue();
			bottom = new Integer(bottomField.getText()).intValue();
			right = new Integer(rightField.getText()).intValue();
		} catch (NumberFormatException e) {
			JOptionPane.showMessageDialog(this, e);
		}
		return new Insets(top, left, bottom, right);
	}

	/**
	 * inset values
	 */
	private int top, left, bottom, right;
	/**
	 * inset fields
	 */
	private JTextField topField, leftField, bottomField, rightField;
}
