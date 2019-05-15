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
import javax.swing.*;

import vista.gui.Changeable;
import vista.gui.DialogButtonPanel;
import vista.set.DataReference;
import vista.set.Pathname;

@SuppressWarnings("serial")
public class PathnameEditor extends JDialog implements Changeable{


	private DataReference ref;
	private JTable table;
	private DataTableFrame dataTable;

	public PathnameEditor(DataTableFrame dataTable, DataReference ref){
		if (ref==null || ref.getPathname()==null){
			throw new IllegalArgumentException("Reference is null or pathname is null!");
		}
		this.ref = ref;
		this.dataTable = dataTable;
		Pathname p = ref.getPathname();
		String[][] rowData = new String[1][4];
		rowData[0][0] = p.getPart(Pathname.A_PART);
		rowData[0][1] = p.getPart(Pathname.B_PART);
		rowData[0][2] = p.getPart(Pathname.C_PART);
		rowData[0][3] = p.getPart(Pathname.F_PART);
		table = new JTable(rowData, new String[]{"A PART", "B PART", "C PART", "F PART"});
		table.setShowGrid(true);
		this.getContentPane().setLayout(new BorderLayout());
		this.getContentPane().add(table.getTableHeader(), BorderLayout.NORTH);
		this.getContentPane().add(table, BorderLayout.CENTER);
		this.getContentPane().add(new DialogButtonPanel(this), BorderLayout.SOUTH);
		this.pack();
		this.setVisible(true);
	}

	@Override
	public void applyChanges() {
		if (table.isEditing()){
			table.getCellEditor().stopCellEditing();
		}
		Pathname p = ref.getPathname();
		p.setPart(Pathname.A_PART, (String) table.getModel().getValueAt(0, 0));
		p.setPart(Pathname.B_PART, (String) table.getModel().getValueAt(0, 1));
		p.setPart(Pathname.C_PART, (String) table.getModel().getValueAt(0, 2));
		p.setPart(Pathname.F_PART, (String) table.getModel().getValueAt(0, 3));
		ref.setPathname(p);
		dataTable.updatePathnameLabel();
	}

	@Override
	public void doneChanges() {
		this.dispose();
	}
}
