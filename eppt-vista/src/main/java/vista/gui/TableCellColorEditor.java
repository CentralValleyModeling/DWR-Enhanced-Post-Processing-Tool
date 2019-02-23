/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package vista.gui;

import java.awt.Color;
import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.*;

public class TableCellColorEditor extends DefaultCellEditor {
	Color currentColor = null;

	public TableCellColorEditor(JButton b) {
		super(new JCheckBox()); // Unfortunately, the constructor
		// expects a check box, combo box,
		// or text field.
		editorComponent = b;
		setClickCountToStart(1); // This is usually 1 or 2.

		// Must do this so that editing stops when appropriate.
		b.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				fireEditingStopped();
			}
		});
	}

	protected void fireEditingStopped() {
		super.fireEditingStopped();
	}

	public Object getCellEditorValue() {
		return currentColor;
	}

	public Component getTableCellEditorComponent(JTable table, Object value,
			boolean isSelected, int row, int column) {
		((JButton) editorComponent).setText(value.toString());
		currentColor = (Color) value;
		return editorComponent;
	}
}
