/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */

package calsim.gui;

import java.awt.BorderLayout;
import java.awt.FlowLayout;
import javax.swing.*;
import javax.swing.event.ChangeEvent;
import javax.swing.event.TableModelEvent;
import javax.swing.table.TableModel;

import calsim.app.WsiDiTableModel;
import vista.gui.CursorChangeListener;

//import java.io.*;
//import java.awt.event.*;

/**
 * JTable for Wsi and Di input data
 *
 * @author Armin Munevar
 * @version $Id: WsiDiTable.java,v 1.1.2.3 2001/07/12 02:00:03 amunevar Exp $
 */
public class WsiDiTable extends MPanel
{
	public static boolean DEBUG = false;
	/**
	 *
	 */
/*	public static void main( String args[] ) {
    JDialog jd = new JDialog();
		jd.setModal(true);
    jd.getContentPane().setLayout(new BorderLayout());
		WsiDiTable table = new WsiDiTable();
		TableModel model = table.getModel();
    jd.setJMenuBar(table.getJMenuBar());
    jd.setTitle(table.getFrameTitle());
    jd.getContentPane().add(table);
    jd.setSize(500,250);
    jd.show();
		System.out.println("all done");
  }*/
	/**
	 *
	 */
	private JTable _table;
	private WsiDiTableModel _model;
	private JButton _okButton, _cancelButton;
	private JMenuBar _mbar;

	/**
	 *
	 */
	public WsiDiTable()
	{
		setLayout(new BorderLayout());
		_model = new WsiDiTableModel();
		_table = new JTable(_model);
		_okButton = createOkButton();
		_cancelButton = createCancelButton();
		JPanel bpanel = new JPanel();
		bpanel.setLayout(new FlowLayout());
		bpanel.add(_okButton);
		bpanel.add(_cancelButton);
		add(new JScrollPane(_table), BorderLayout.CENTER);
		add(bpanel, BorderLayout.SOUTH);

		int uw = 100;
		_table.getColumnModel().getColumn(0).setPreferredWidth(2 * uw);
		_table.getColumnModel().getColumn(1).setPreferredWidth(4 * uw);
		_table.getColumnModel().getColumn(2).setPreferredWidth(4 * uw);
		_table.getColumnModel().getColumn(3).setPreferredWidth(3 * uw);
	}

	/**
	 *
	 */
	public String getFrameTitle()
	{
		return "Water Supply Index - Demand Index Setup";
	}

	/**
	 *
	 */
	public JMenuBar getJMenuBar()
	{
		if(_mbar == null)
		{
			_mbar = createJMenuBar();
		}
		return _mbar;
	}

	/**
	 *
	 */
	public TableModel getModel()
	{
		return _table.getModel();
	}

	/**
	 *
	 */
	public JMenuBar createJMenuBar()
	{
		JMenuItem deleteItem = new JMenuItem("Delete Row");
		deleteItem.addActionListener(new CursorChangeListener()
		{
			public void doWork()
			{
				delete();
			}
		});
		JMenuItem addRowItem = new JMenuItem("Add Row");
		addRowItem.addActionListener(new CursorChangeListener()
		{
			public void doWork()
			{
				add();
			}
		});
		JMenuItem insertItem = new JMenuItem("Insert Row");
		insertItem.addActionListener(new CursorChangeListener()
		{
			public void doWork()
			{
				insert();
			}
		});
		//
		JMenu editMenu = new JMenu("Edit");
		editMenu.add(addRowItem);
		editMenu.add(insertItem);
		editMenu.add(deleteItem);
		//
		_mbar = new JMenuBar();
		_mbar.add(editMenu);
		return _mbar;
	}

	/**
	 *
	 */
	public JButton createOkButton()
	{
		JButton b = new JButton("OK");
		b.addActionListener(new CursorChangeListener()
		{
			public void doWork()
			{
				finish();
			}
		});
		return b;
	}

	/**
	 *
	 */
	public JButton createCancelButton()
	{
		JButton b = new JButton("Cancel");
		b.addActionListener(new CursorChangeListener()
		{
			public void doWork()
			{
				quit();
			}
		});
		return b;
	}

	/**
	 * delete rows
	 */
	void delete()
	{
		if(DEBUG)
		{
			System.out.println("Delete");
		}
		// get user selected rows
		stopEditing();
		int[] ri = _table.getSelectedRows();
		if(ri == null || ri.length == 0)
		{
			JOptionPane.showMessageDialog(this, "Message",
					"Select a row first!",
					JOptionPane.PLAIN_MESSAGE);
			return;
		}
		int numberDeleted = 0;
		for(int i = 0; i < ri.length; i++)
		{
			int currentIndex = ri[i] - numberDeleted;
			_model.removeRow(currentIndex);
			numberDeleted++;
		}
		_table.tableChanged(new TableModelEvent(_table.getModel()));
	}

	/**
	 *
	 */
	void add()
	{
		if(DEBUG)
		{
			System.out.println("Add");
		}
		stopEditing();
		_model.addRow(_model.getDefaultData());
		_table.tableChanged(new TableModelEvent(_table.getModel()));
	}

	/**
	 *
	 */
	void insert()
	{
		if(DEBUG)
		{
			System.out.println("Insert");
		}
		// get user selected row
		stopEditing();
		int ri = _table.getSelectedRow();
		if(ri == -1)
		{
			JOptionPane.showMessageDialog(this, "Message",
					"Select a row first!",
					JOptionPane.PLAIN_MESSAGE);
			return;
		}
		_model.insertRow(ri, _model.getDefaultData());
		_table.tableChanged(new TableModelEvent(_table.getModel()));
	}

	/**
	 *
	 */
	void finish()
	{
		if(DEBUG)
		{
			System.out.println("Finish");
		}
		stopEditing();
		_table.tableChanged(new TableModelEvent(_table.getModel()));
		JOptionPane.getFrameForComponent(this).dispose();
	}

	/**
	 *
	 */
	void quit()
	{
		if(DEBUG)
		{
			System.out.println("Quit");
		}
		stopEditing();
		_model.setQuitMode(true);
		JOptionPane.getFrameForComponent(this).dispose();
	}

	/**
	 *
	 */
	void stopEditing()
	{
		_table.editingStopped(new ChangeEvent(_table));
	}
}
