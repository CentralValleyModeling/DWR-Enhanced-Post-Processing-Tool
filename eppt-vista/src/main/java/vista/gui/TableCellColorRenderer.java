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
package vista.gui;

import java.awt.Color;
import java.awt.Component;
import javax.swing.*;
import javax.swing.border.Border;
import javax.swing.table.TableCellRenderer;

/**
 * 
 * 
 * @author Nicky Sandhu ,Armin Munevar
 * @version $Id: TableCellColorRenderer.java,v 1.2 2000/03/21 18:16:31 nsandhu
 *          Exp $
 */
public class TableCellColorRenderer extends JLabel implements TableCellRenderer {
	Border unselectedBorder = null;
	Border selectedBorder = null;
	boolean isBordered = true;

	public TableCellColorRenderer(boolean isBordered) {
		super();
		this.isBordered = isBordered;
		setOpaque(true); // MUST do this for background to show up.
	}

	public Component getTableCellRendererComponent(JTable table, Object color,
			boolean isSelected, boolean hasFocus, int row, int column) {
		setBackground((Color) color);
		if (isBordered) {
			if (isSelected) {
				if (selectedBorder == null) {
					selectedBorder = BorderFactory.createMatteBorder(2, 5, 2,
							5, table.getSelectionBackground());
				}
				setBorder(selectedBorder);
			} else {
				if (unselectedBorder == null) {
					unselectedBorder = BorderFactory.createMatteBorder(2, 5, 2,
							5, table.getBackground());
				}
				setBorder(unselectedBorder);
			}
		}
		return this;
	}
}
