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

package gov.ca.water.bo;

import javax.swing.*;

import gov.ca.water.calgui.bo.DataTableModel;
import gov.ca.water.calgui.constant.Constant;
import org.apache.log4j.Logger;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 02-27-2019
 */
public class WsiDataTableModel extends DataTableModel
{
	private static final Logger LOG = Logger.getLogger(WsiDataTableModel.class.getName());
	private final JLabel _statusLabel;

	/**
	 * The object of the GUI.
	 *
	 * @param tableName
	 * @param columnName
	 * @param data
	 * @param isCellEditable
	 * @param statusLabel
	 */
	public WsiDataTableModel(String tableName, String[] columnName, Object[][] data, boolean isCellEditable,
							 JLabel statusLabel)
	{
		super(tableName, columnName, data, isCellEditable);
		_statusLabel = statusLabel;
	}


	@Override
	public Object clone()
	{
		String tableName = getTableName();
		String[] colNames = new String[getColumnNames().length];
		for(int i = 0; i < colNames.length; i++)
		{
			colNames[i] = getColumnNames()[i];
		}
		Object[][] data1 = new Object[getData().length][getData()[0].length];
		for(int i = 0; i < data1.length; i++)
		{
			for(int j = 0; j < data1[0].length; j++)
			{
				data1[i][j] = getData()[i][j];
			}
		}
		return new DataTableModel(tableName, colNames, data1, isCellEditable());
	}


	@Override
	public void setValueAt(Object value, int row, int col)
	{
		if(getTableName().toLowerCase().startsWith("wsi") || getTableName().equals(Constant.USER_DEFINED))
		{
			updateStatusText();
		}
		super.setValueAt(value, row, col);
	}

	private void updateStatusText()
	{
		try
		{
			String oldValue = _statusLabel.getText();
			if(oldValue.endsWith(Constant.UNEDITED_FORLABEL))
			{
				String val = oldValue.replace(Constant.UNEDITED_FORLABEL, Constant.EDITED_FORLABEL);
				_statusLabel.setText(val);
			}
			else if(oldValue.endsWith("iterations)"))
			{
				String[] parts = oldValue.split("\\(");
				String val = parts[0] + Constant.EDITED_FORLABEL;
				_statusLabel.setText(val);
			}
		}
		catch(RuntimeException ex)
		{
			LOG.error(ex);
		}
	}
}
