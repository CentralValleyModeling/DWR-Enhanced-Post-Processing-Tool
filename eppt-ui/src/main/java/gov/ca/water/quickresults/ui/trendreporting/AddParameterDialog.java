/*
 * Enhanced Post Processing Tool (EPPT) Copyright (c) 2020.
 *
 * EPPT is copyrighted by the State of California, Department of Water Resources. It is licensed
 * under the GNU General Public License, version 2. This means it can be
 * copied, distributed, and modified freely, but you may not restrict others
 * in their ability to copy, distribute, and modify it. See the license below
 * for more details.
 *
 * GNU General Public License
 */

package gov.ca.water.quickresults.ui.trendreporting;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Frame;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.KeyEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.util.HashMap;
import java.util.Map;
import javax.swing.*;
import javax.swing.table.AbstractTableModel;

import gov.ca.water.calgui.bo.GUILinksAllModelsBO;

import rma.swing.RmaJTable;

public class AddParameterDialog extends JDialog
{
	private final TableModel _tableModel;
	private JPanel _contentPane;
	private JButton _buttonOK;
	private JButton _buttonCancel;
	private JTextField _typeField;
	private JTextField _parameterField;
	private JTable _table1;
	private boolean _canceled = true;

	public AddParameterDialog(Frame frame)
	{
		super(frame, "New Parameter", true);
		$$$setupUI$$$();
		setContentPane(_contentPane);
		setModal(true);
		getRootPane().setDefaultButton(_buttonOK);
		_buttonOK.addActionListener(e -> onOK());
		_buttonCancel.addActionListener(e -> onCancel());
		_tableModel = new TableModel();
		for(int i = 0; i < _tableModel.getRowCount(); i++)
		{
			_tableModel.setValueAt("B-PART/C-PART", i, 1);
		}
		_table1.setModel(_tableModel);
		// call onCancel() when cross is clicked
		setDefaultCloseOperation(DO_NOTHING_ON_CLOSE);
		addWindowListener(new WindowAdapter()
		{
			@Override
			public void windowClosing(WindowEvent e)
			{
				onCancel();
			}
		});

		// call onCancel() on ESCAPE
		_contentPane.registerKeyboardAction(e -> onCancel(), KeyStroke.getKeyStroke(KeyEvent.VK_ESCAPE, 0),
				JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT);
		pack();
		setLocationRelativeTo(frame);
	}

	public boolean isCanceled()
	{
		return _canceled;
	}

	private void onOK()
	{
		_canceled = false;
		// add your code here
		dispose();
	}

	private void onCancel()
	{
		// add your code here if necessary
		dispose();
	}

	public String getDataType()
	{
		return _typeField.getText();
	}

	public String getParameter()
	{
		return _parameterField.getText();
	}

	public Map<GUILinksAllModelsBO.Model, String> getBAndCParts()
	{
		return _tableModel._bAndCParts;
	}

	/**
	 * Method generated by IntelliJ IDEA GUI Designer
	 * >>> IMPORTANT!! <<<
	 * DO NOT edit this method OR call it in your code!
	 *
	 * @noinspection ALL
	 */
	private void $$$setupUI$$$()
	{
		createUIComponents();
		_contentPane = new JPanel();
		_contentPane.setLayout(new BorderLayout(5, 5));
		_contentPane.setBorder(BorderFactory.createTitledBorder(BorderFactory.createEmptyBorder(5, 5, 0, 5), null));
		final JPanel panel1 = new JPanel();
		panel1.setLayout(new BorderLayout(0, 0));
		_contentPane.add(panel1, BorderLayout.SOUTH);
		final JPanel panel2 = new JPanel();
		panel2.setLayout(new FlowLayout(FlowLayout.RIGHT, 5, 5));
		panel1.add(panel2, BorderLayout.CENTER);
		_buttonOK = new JButton();
		_buttonOK.setMaximumSize(new Dimension(70, 25));
		_buttonOK.setMinimumSize(new Dimension(70, 25));
		_buttonOK.setPreferredSize(new Dimension(70, 25));
		_buttonOK.setText("OK");
		panel2.add(_buttonOK);
		_buttonCancel = new JButton();
		_buttonCancel.setMaximumSize(new Dimension(70, 25));
		_buttonCancel.setMinimumSize(new Dimension(70, 25));
		_buttonCancel.setPreferredSize(new Dimension(70, 25));
		_buttonCancel.setText("Cancel");
		panel2.add(_buttonCancel);
		final JPanel panel3 = new JPanel();
		panel3.setLayout(new BorderLayout(10, 10));
		_contentPane.add(panel3, BorderLayout.CENTER);
		final JPanel panel4 = new JPanel();
		panel4.setLayout(new GridBagLayout());
		panel4.setPreferredSize(new Dimension(250, 108));
		panel3.add(panel4, BorderLayout.NORTH);
		_parameterField = new JTextField();
		_parameterField.setHorizontalAlignment(2);
		_parameterField.setPreferredSize(new Dimension(400, 26));
		_parameterField.setText("");
		GridBagConstraints gbc;
		gbc = new GridBagConstraints();
		gbc.gridx = 1;
		gbc.gridy = 1;
		gbc.weightx = 1.0;
		gbc.anchor = GridBagConstraints.WEST;
		gbc.fill = GridBagConstraints.HORIZONTAL;
		gbc.insets = new Insets(5, 5, 5, 5);
		panel4.add(_parameterField, gbc);
		final JLabel label1 = new JLabel();
		label1.setHorizontalAlignment(2);
		label1.setHorizontalTextPosition(2);
		label1.setText("Parameter Name:");
		gbc = new GridBagConstraints();
		gbc.gridx = 0;
		gbc.gridy = 1;
		gbc.weightx = 0.2;
		gbc.anchor = GridBagConstraints.WEST;
		gbc.insets = new Insets(0, 0, 5, 5);
		panel4.add(label1, gbc);
		final JLabel label2 = new JLabel();
		label2.setHorizontalAlignment(2);
		label2.setHorizontalTextPosition(2);
		label2.setText("Type Name:");
		gbc = new GridBagConstraints();
		gbc.gridx = 0;
		gbc.gridy = 0;
		gbc.weightx = 0.2;
		gbc.anchor = GridBagConstraints.WEST;
		gbc.insets = new Insets(0, 0, 5, 5);
		panel4.add(label2, gbc);
		_typeField = new JTextField();
		_typeField.setHorizontalAlignment(2);
		_typeField.setPreferredSize(new Dimension(400, 26));
		_typeField.setText("");
		gbc = new GridBagConstraints();
		gbc.gridx = 1;
		gbc.gridy = 0;
		gbc.weightx = 1.0;
		gbc.anchor = GridBagConstraints.WEST;
		gbc.fill = GridBagConstraints.HORIZONTAL;
		gbc.insets = new Insets(5, 5, 5, 5);
		panel4.add(_typeField, gbc);
		final JScrollPane scrollPane1 = new JScrollPane();
		scrollPane1.setPreferredSize(new Dimension(300, 100));
		panel3.add(scrollPane1, BorderLayout.CENTER);
		_table1.setPreferredScrollableViewportSize(new Dimension(300, 50));
		_table1.setPreferredSize(new Dimension(300, 70));
		scrollPane1.setViewportView(_table1);
	}

	/**
	 * @noinspection ALL
	 */
	public JComponent $$$getRootComponent$$$()
	{
		return _contentPane;
	}

	private void createUIComponents()
	{
		_table1 = new RmaJTable();
	}

	private static class TableModel extends AbstractTableModel
	{

		private final Map<GUILinksAllModelsBO.Model, String> _bAndCParts = new HashMap<>();

		@Override
		public int getRowCount()
		{
			return GUILinksAllModelsBO.Model.values().size();
		}

		@Override
		public int getColumnCount()
		{
			return 2;
		}

		@Override
		public Object getValueAt(int rowIndex, int columnIndex)
		{
			if(columnIndex == 0)
			{
				return GUILinksAllModelsBO.Model.values().get(rowIndex);
			}
			else
			{
				Object model = getValueAt(rowIndex, 0);
				return _bAndCParts.getOrDefault((GUILinksAllModelsBO.Model) model, "");
			}
		}

		@Override
		public void setValueAt(Object aValue, int rowIndex, int columnIndex)
		{
			if(columnIndex == 1)
			{
				Object model = getValueAt(rowIndex, 0);
				if(model instanceof GUILinksAllModelsBO.Model && aValue != null)
				{
					_bAndCParts.put((GUILinksAllModelsBO.Model) model, aValue.toString());
				}
			}
		}

		@Override
		public boolean isCellEditable(int rowIndex, int columnIndex)
		{
			return columnIndex != 0;
		}

		@Override
		public String getColumnName(int columnIndex)
		{
			if(columnIndex == 0)
			{
				return "Model";
			}
			else
			{
				return "B and C Part";
			}
		}
	}
}
