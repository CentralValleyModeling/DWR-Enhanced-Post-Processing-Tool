/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */

package calsim.gui;

import java.awt.BorderLayout;
import java.awt.FileDialog;
import java.awt.event.ActionEvent;
import javax.swing.*;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.event.TableModelEvent;

import calsim.app.InputTableData;
import calsim.app.LookupTableData;
import vista.gui.VistaUtils;

//import calsim.app.Wresler;
//import vista.gui.TableHeaderAction;
//import java.io.File;

/**
 * A display panel for InputTableData as its model
 *
 * @author Nicky Sandhu ,Armin Munevar
 * @version $Id: LookupDataUI.java,v 1.1.2.4 2001/07/12 01:59:43 amunevar Exp $
 */
public class LookupDataUI extends MPanel
{
	private JLabel _tableNameLabel;
	private JTable _table;
	private JTextArea _textArea;
	private InputTableData _model;

	/**
	 *
	 */
	public LookupDataUI()
	{
		_tableNameLabel = new JLabel();
		_tableNameLabel.setBorder(BorderFactory.createTitledBorder("Table Name"));
		_table = new JTable();
		_textArea = new JTextArea(5, 30);
		_textArea.setBorder(BorderFactory.createTitledBorder("Comments"));
		_textArea.getDocument().addDocumentListener(new DocumentListener()
		{
			public void insertUpdate(DocumentEvent evt)
			{
				getModel().setComment(_textArea.getText());
			}

			public void removeUpdate(DocumentEvent evt)
			{
				getModel().setComment(_textArea.getText());
			}

			public void changedUpdate(DocumentEvent evt)
			{
			}
		});
		//
		setLayout(new BorderLayout());
		add(_tableNameLabel, BorderLayout.NORTH);
		add(new JScrollPane(_table), BorderLayout.CENTER);
		add(new JScrollPane(_textArea), BorderLayout.SOUTH);
	}

	/**
	 *
	 */
	public LookupDataUI(InputTableData model)
	{
		this();
		setModel(model);
	}

	/**
	 *
	 */
	public InputTableData getModel()
	{
		return _model;
	}

	/**
	 *
	 */
	public void setModel(InputTableData model)
	{
		_model = model;
		_table.setModel(model);
		_tableNameLabel.setText(model.getTableName());
		_textArea.setText(model.getComment());
	}

	/**
	 * returns a menu bar associated with this panels components
	 */
	public JMenuBar getJMenuBar()
	{
		Action loadAction = new AbstractAction("Load...")
		{
			public void actionPerformed(ActionEvent evt)
			{
				String fname = VistaUtils.getFilenameFromDialog(_table, FileDialog.LOAD, "table", "Tables");
				if(fname == null)
				{
					return;
				}
				try
				{
					InputTableData model = new LookupTableData(fname);
					setModel(model);
					_table.sizeColumnsToFit(-1);
				}
				catch(Exception e)
				{
					JOptionPane.showMessageDialog(null, e.getMessage(), "Error", JOptionPane.ERROR_MESSAGE);
				}
			}
		};
		Action saveAction = new AbstractAction("Save")
		{
			public void actionPerformed(ActionEvent evt)
			{
				saveTo(getModel().getInputFile());
			}
		};
		Action addRowAction = new AbstractAction("Add Row")
		{
			public void actionPerformed(ActionEvent evt)
			{
				getModel().addRow(createDefaultRow());
				_table.tableChanged(new TableModelEvent(_table.getModel()));
			}
		};
		Action insertRowAction = new AbstractAction("Insert Row")
		{
			public void actionPerformed(ActionEvent evt)
			{
				int[] indices = getSelectedRows();
				if(indices == null || indices.length == 0)
				{
					return;
				}
				int numberDeleted = 0;
				for(int i = 0; i < indices.length; i++)
				{
					int currentIndex = indices[i] - numberDeleted;
					if(currentIndex >= getModel().getNumberOfRows())
					{
						continue;
					}
					getModel().insertRow(currentIndex, createDefaultRow());
					numberDeleted++;
				}
				_table.tableChanged(new TableModelEvent(_table.getModel()));
			}
		};
		Action deleteRowAction = new AbstractAction("Delete Row")
		{
			public void actionPerformed(ActionEvent evt)
			{
				int[] indices = getSelectedRows();
				if(indices == null || indices.length == 0)
				{
					return;
				}
				int numberDeleted = 0;
				for(int i = 0; i < indices.length; i++)
				{
					int currentIndex = indices[i] - numberDeleted;
					if(currentIndex >= getModel().getNumberOfRows())
					{
						continue;
					}
					getModel().removeRow(currentIndex);
					numberDeleted++;
				}
				_table.tableChanged(new TableModelEvent(_table.getModel()));
			}
		};
		// installs header listener action
/*    ActionListener al = new TableHeaderAction(_table){
      public void actionPerformed(ActionEvent evt){
	    int col = getColumn();
	    if ( col == -1 ) return;
        MouseEvent me = getMouseEvent(); // disabled for now
	    getModel().sort(col);
        // if shift reverse sort...
        if ( me.isShiftDown() ) getModel().reverse();
      }
    }; */

		JMenu fmenu = new JMenu("File");
		fmenu.add(loadAction);
		fmenu.add(saveAction);
		//
		JMenu eMenu = new JMenu("Edit");
		eMenu.add(addRowAction);
		eMenu.add(insertRowAction);
		eMenu.add(deleteRowAction);
		//
		JMenuBar mbar = new JMenuBar();
		mbar.add(fmenu);
		mbar.add(eMenu);
		return mbar;
	}

	/**
	 *
	 */
	void saveTo(String file)
	{
		//
		getModel().setComment(_textArea.getText());
		getModel().save(file);
	}

	/**
	 * returns the title of the frame containing this panel. This could
	 * be used to identify this panel by name as well.
	 */
	public String getFrameTitle()
	{
		return getModel().getTableName();
	}

	/**
	 *
	 */
	int[] getSelectedRows()
	{
		int[] ri = _table.getSelectedRows();
		if(ri == null || ri.length == 0)
		{
			JOptionPane.showMessageDialog(this, "Message",
					"Select a row first!",
					JOptionPane.PLAIN_MESSAGE);
			return null;
		}
		return ri;
	}

	/**
	 *
	 */
	String[] createDefaultRow()
	{
		String[] headers = getModel().getHeaders();
		String[] rowData = new String[headers.length];
		for(int i = 0; i < rowData.length; i++)
		{
			rowData[i] = "0";
		}
		return rowData;
	}
}
