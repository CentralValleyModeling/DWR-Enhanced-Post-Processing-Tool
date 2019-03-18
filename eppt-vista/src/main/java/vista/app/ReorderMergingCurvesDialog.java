/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */

package vista.app;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.EventObject;
import javax.swing.*;
import javax.swing.table.DefaultTableModel;
import javax.swing.table.TableCellRenderer;
import javax.swing.table.TableModel;

import vista.graph.Curve;
import vista.graph.GECanvas;
import vista.graph.LegendItem;
import vista.gui.Changeable;
import vista.gui.DialogButtonPanel;
import vista.set.DataReference;

@SuppressWarnings("serial")
public class ReorderMergingCurvesDialog extends JDialog implements Changeable{

	private JTable table;
	private boolean reordered;

	public ReorderMergingCurvesDialog(Component comp,Curve[] curves){
		reordered=false;
		Object[][] data = new Object[curves.length][1];
		for(int i=0; i < curves.length; i++){
			data[i][0] = curves[i];
		}
		table = new JTable(new DefaultTableModel(data, new String[]{"Data"}));
		table.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
		table.setRowSelectionAllowed(true);
		table.getColumnModel().getColumn(0).setCellRenderer(new CurveCellRenderer());
		table.getColumnModel().getColumn(0).setCellEditor(new CurveCellEditor(new JTextField()));
		JPanel mainPanel = new JPanel();
		mainPanel.setLayout(new BorderLayout());
		JPanel buttonPanel = new JPanel();
		mainPanel.add(buttonPanel, BorderLayout.NORTH);
		buttonPanel.add(new JLabel("Select row and click on up/down button"));
		JButton upBtn = new JButton("Up");
		buttonPanel.add(upBtn);
		JButton downBtn = new JButton("Down");
		buttonPanel.add(downBtn);
		ActionListener orderListener = new OrderChangeActionListener();
		upBtn.addActionListener(orderListener);
		downBtn.addActionListener(orderListener);
		
		JPanel tablePanel = new JPanel();
		mainPanel.add(tablePanel, BorderLayout.CENTER);
		tablePanel.setLayout(new BorderLayout());
		tablePanel.add(table.getTableHeader(), BorderLayout.NORTH);
		tablePanel.add(table,BorderLayout.CENTER);
		
		
		getContentPane().setLayout(new BorderLayout());
		getContentPane().add(mainPanel, BorderLayout.CENTER);
		getContentPane().add(new DialogButtonPanel(this, false), BorderLayout.SOUTH);
		this.setLocationRelativeTo(comp);
		pack();
		setModal(true);
		setVisible(true);
		
	}
	
	
	@Override
	public void applyChanges() {
		reordered = true;
	}

	@Override
	public void doneChanges() {
		this.dispose();
	}

	public class OrderChangeActionListener implements ActionListener{
		public static final int UP = 100;
		public static final int DOWN = 200;
		@Override
		public void actionPerformed(ActionEvent evt) {
			JButton button = (JButton) evt.getSource();
			if (button.getText().equals("Up")){
				moveAndSelect(UP);
			} else {
				moveAndSelect(DOWN);
			}
		}
		
		private void moveAndSelect(int direction) {
			int selectedRow = table.getSelectedRow();
			if (selectedRow == -1){
				return;
			}
			if (selectedRow ==0 && direction == UP){
				return;
			}
			if (selectedRow == table.getRowCount()-1 && direction == DOWN){
				return;
			}
			int swapRow = direction == UP ? selectedRow - 1 : selectedRow + 1;
			TableModel model = table.getModel();
			Object valueAt = model.getValueAt(selectedRow, 0);
			model.setValueAt(model.getValueAt(swapRow, 0), selectedRow, 0);
			model.setValueAt(valueAt, swapRow, 0);
			table.setRowSelectionInterval(swapRow, swapRow);
		}
		
	}
	
	public class CurveCellEditor extends DefaultCellEditor{

		public CurveCellEditor(JTextField textField) {
			super(textField);
		}

		@Override
		public boolean isCellEditable(EventObject anEvent) {
			return false;
		}
		
		
	}
	
	public class CurveCellRenderer implements TableCellRenderer{

		@Override
		public Component getTableCellRendererComponent(JTable table,
				Object value, boolean isSelected, boolean hasFocus, int row,
				int column) {
			Curve curve = (Curve) value;
			DataReference ref = (DataReference) curve.getModel().getReferenceObject();
			LegendItem item = new LegendItem(curve);
			GECanvas canvas = new GECanvas(item);
			canvas.setToolTipText(ref.getPathname().toString());
			if (isSelected){
				item.setBackgroundColor(Color.GRAY);
			}
			return canvas;
		}
		
	}

	public Curve[] getCurves() {
		if (!reordered){
			return null;
		}
		TableModel model = table.getModel();
		int n = model.getRowCount();
		Curve[] curves = new Curve[n];
		for(int i=0; i < n; i++){
			curves[i] = (Curve) model.getValueAt(i, 0);
		}
		return curves;
	}
}
