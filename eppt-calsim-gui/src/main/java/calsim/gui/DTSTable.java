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

package calsim.gui;

import java.awt.BorderLayout;
import java.awt.FileDialog;
import java.awt.FlowLayout;
import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import java.util.Map;
import javax.swing.*;
import javax.swing.event.ChangeEvent;
import javax.swing.event.TableModelEvent;

import calsim.app.AppUtils;
import calsim.app.DTSTableModel;
import calsim.app.DerivedTimeSeries;
import calsim.app.MTSTableModel;
import calsim.app.MultipleTimeSeries;
import calsim.app.Project;
import vista.gui.CursorChangeListener;
import vista.gui.VistaUtils;
import vista.set.Pathname;

/**
 * Derived Timeseries table
 *
 * @author Nicky Sandhu
 * @version $Id: DTSTable.java,v 1.1.4.45 2001/10/23 16:28:35 jfenolio Exp $
 */
public class DTSTable extends MPanel
{
	public static final boolean DEBUG = false;
	private static final String[] itemText =
			{
					"Print",
					"Load",
					"Save",
					"Delete Row",
					"Add Row",
					"Insert Row",
					"Quit",
					"Display"

			};
	private static final char[] ITEM_CHARS =
			{
					'p', 'l', 's', 'd', 'a', 'i', 'q', 'r'
			};
	private static final String[] TOOL_TIP_TEXT =
			{
					"Prints table",
					"Loads table from file",
					"Saves table to file",
					"Deletes selected row",
					"Adds row",
					"Inserts row at current selection",
					"Closes frame",
					"Retrieves and Calculates Data"
			};
	private static final int[] ITEM_KEYS = {KeyEvent.VK_P,
			KeyEvent.VK_L,
			KeyEvent.VK_S,
			KeyEvent.VK_D,
			KeyEvent.VK_A,
			KeyEvent.VK_I,
			KeyEvent.VK_Q,
			KeyEvent.VK_R
	};
	private JComboBox _dtsEditor = new JComboBox();
	private JTextField _bpartEditor = new JTextField();
	private JTextField _cpartEditor = new JTextField();
	private JComboBox<String> _varEditor = new JComboBox<>();
	private DTSTableModel _dtm;
	private MTSTableModel _mtm;
	private JTable _table;
	private JTextField _nameField;
	private DerivedTimeSeries _dts;
	private MultipleTimeSeries _mts;
	private boolean _modified;
	private JMenuBar _mbar;
	private boolean _start = true;
	private JButton _opencurrent;

	/**
	 *
	 */
	public DTSTable(DerivedTimeSeries dts)
	{
		setLayout(new BorderLayout());
		_table = new JTable(new DTSTableModel(dts));
		_table.registerKeyboardAction(new CursorChangeListener()
		{
			public void doWork()
			{
				stopEditing();
			}
		}, null, KeyStroke.getKeyStroke(KeyEvent.VK_ENTER, 0, true), JComponent.WHEN_IN_FOCUSED_WINDOW);
		int uw = 100;
		_table.getColumnModel().getColumn(0).setPreferredWidth(1 * uw);
		_table.getColumnModel().getColumn(1).setPreferredWidth(5 * uw);
		_table.getColumnModel().getColumn(2).setPreferredWidth(1 * uw);
		_table.getColumnModel().getColumn(3).setPreferredWidth(5 * uw);
		_table.getColumnModel().getColumn(4).setPreferredWidth(5 * uw);
		_table.sizeColumnsToFit(-1);  //CB TODO  use doLayout method instead
		_nameField = new JTextField(dts.getName(), 25);
		_nameField.setEditable(false);
		_nameField.addActionListener(new CursorChangeListener()
		{
			public void doWork()
			{
				changeNameToField();
			}
		});

		_table.setPreferredScrollableViewportSize(_table.getPreferredSize());  //CB added - but still CANNOT SEE TABLE

		JLabel nameLabel = new JLabel("Derived Time Series: ");
		JPanel namePanel = new JPanel();
		namePanel.setLayout(new BorderLayout());
		namePanel.add(nameLabel, BorderLayout.WEST);
		namePanel.add(_nameField, BorderLayout.CENTER);
		add(namePanel, BorderLayout.NORTH);
		add(new JScrollPane(_table), BorderLayout.CENTER);
		add(createButtonPanel(), BorderLayout.SOUTH);
		setDTS(dts, null);
	}

	/**
	 * Provided for CalLite GUI access to _dts. Added by Tad Slawecki/LimnoTech 12/2013
	 *
	 * @return
	 */
	public DerivedTimeSeries getDTS()
	{
		return _dts;
	}

	/**
	 * Provided for CalLite GUI access to _mts. Added by Tad Slawecki/LimnoTech 12/2013
	 *
	 * @return
	 */
	public MultipleTimeSeries getMTS()
	{
		return _mts;
	}

	public JButton getOpenCurrentButton()
	{
		return _opencurrent;
	}

	public JPanel createButtonPanel()
	{
		JPanel holder = new JPanel(new FlowLayout(FlowLayout.CENTER, 10, 5));
		JButton addrow = new JButton("Add");
		JButton insertrow = new JButton("Insert");
		JButton deleterow = new JButton("Delete");
		_opencurrent = new JButton("Display");
		addrow.addActionListener(new CursorChangeListener()
		{
			public void doWork()
			{
				add();
			}
		});
		insertrow.addActionListener(new CursorChangeListener()
		{
			public void doWork()
			{
				insert();
			}
		});
		deleterow.addActionListener(new CursorChangeListener()
		{
			public void doWork()
			{
				delete();
			}
		});
		_opencurrent.addActionListener(new GuiTaskListener("Retrieving...")
		{
			public void doWork()
			{
				retrieve();
			}
		});
		holder.add(addrow);
		holder.add(insertrow);
		holder.add(deleterow);
		holder.add(_opencurrent);
		return holder;
	}

	/**
	 *
	 */
	public String getFrameTitle()
	{
		return "Derived Time Series";
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

	private JMenuBar createJMenuBar()
	{
		JMenuItem deleteItem = new JMenuItem(itemText[3]);
		deleteItem.setToolTipText(TOOL_TIP_TEXT[3]);
		deleteItem.setAccelerator(KeyStroke.getKeyStroke(ITEM_KEYS[3], KeyEvent.CTRL_MASK));
		deleteItem.setMnemonic(ITEM_CHARS[3]);
		deleteItem.addActionListener(new CursorChangeListener()
		{
			public void doWork()
			{
				delete();
			}
		});
		//
		JMenuItem addRowItem = new JMenuItem(itemText[4]);
		addRowItem.setToolTipText(TOOL_TIP_TEXT[4]);
		addRowItem.setAccelerator(KeyStroke.getKeyStroke(ITEM_KEYS[4], KeyEvent.CTRL_MASK));
		addRowItem.setMnemonic(ITEM_CHARS[4]);
		addRowItem.addActionListener(new CursorChangeListener()
		{
			public void doWork()
			{
				add();
			}
		});
		//
		JMenuItem insertItem = new JMenuItem(itemText[5]);
		insertItem.setToolTipText(TOOL_TIP_TEXT[5]);
		insertItem.setAccelerator(KeyStroke.getKeyStroke(ITEM_KEYS[5], KeyEvent.CTRL_MASK));
		insertItem.setMnemonic(ITEM_CHARS[5]);
		insertItem.addActionListener(new CursorChangeListener()
		{
			public void doWork()
			{
				insert();
			}
		});
		//
		int index = 7;
		JMenuItem retrieveItem = new JMenuItem(itemText[index]);
		retrieveItem.setToolTipText(TOOL_TIP_TEXT[index]);
		retrieveItem.setAccelerator(KeyStroke.getKeyStroke(ITEM_KEYS[index], KeyEvent.CTRL_MASK));
		retrieveItem.setMnemonic(ITEM_CHARS[index]);
		retrieveItem.addActionListener(new CursorChangeListener()
		{
			public void doWork()
			{
				retrieve();
			}
		});
		JMenu editMenu = new JMenu("Edit");
		editMenu.add(addRowItem);
		editMenu.add(insertItem);
		editMenu.add(deleteItem);
		editMenu.setMnemonic('e');
		_mbar = new JMenuBar();
		return _mbar;
	}

	/**
	 *
	 */
	void print()
	{
		if(DEBUG)
		{
			System.out.println("Print");
		}
		stopEditing();
		GuiUtils.print(this);
	}

	/**
	 * sets DTS in the table to null
	 */
	public void setDTS()
	{
		_dts = null;
		_nameField.setText("");

	}

	/**
	 * sets the DTS displayed in the table
	 */
	public void setDTS(DerivedTimeSeries dts, MultipleTimeSeries mts)
	{
		if(dts != null)
		{
			_dtm = new DTSTableModel(dts);
			_mtm = null;
			_table.setModel(_dtm);
		}
		else
		{
			_mtm = new MTSTableModel(mts);
			_dtm = null;
			_table.setModel(new MTSTableModel(mts));
		}
		_table.tableChanged(new TableModelEvent(_table.getModel()));
		if(dts != null)
		{
			_nameField.setText(dts.getName());
		}
		else
		{
			_nameField.setText(mts.getName());
		}
		_dts = dts;
		_mts = mts;
		_modified = false;
		//  set operation editor
		JComboBox opEditor = new JComboBox();
		if(dts != null)
		{
			opEditor.addItem("+");
			opEditor.addItem("-");
			opEditor.addItem("*");
			opEditor.addItem("/");
		}
		else
		{
			opEditor.setEnabled(false);
		}
		// register tab as editing stopped
		_table.registerKeyboardAction(new AbstractAction("editingStopped")
		{
			public void actionPerformed(ActionEvent evt)
			{
				stopEditing();
			}
		}, KeyStroke.getKeyStroke(KeyEvent.VK_TAB, 0), JComponent.WHEN_FOCUSED);
		// set derived time series editor

		JComboBox dtsEditor;
		String[] dtsArray = null;
		dtsArray = AppUtils.getCurrentProject().getDTSNames();
		if(dtsArray != null)
		{
			if(AppUtils.getCurrentProject().getDTSMod())
			{
				dtsArray = GuiUtils.sortArray(dtsArray);
				dtsEditor = new JComboBox(dtsArray);
				_dtsEditor = dtsEditor;
			}
		}
		else
		{
			dtsEditor = new JComboBox();
			_dtsEditor = dtsEditor;
		}
		AppUtils.getCurrentProject().setDTSMod(false);
		//make var type editor
		_varEditor.addItem(AppUtils.SVAR);
		_varEditor.addItem(AppUtils.DVAR);
		_bpartEditor.setEditable(true);
		_cpartEditor.setEditable(true);
		_bpartEditor.addActionListener(e ->
		{
			int row = _table.getSelectedRow();
			if(!_start)
			{
				findSetCPart(_dtm, _mtm, row);
			}
		});
		_start = false;
		_varEditor.removeAllItems();
		_varEditor.addItem(AppUtils.SVAR);
		_varEditor.addItem(AppUtils.DVAR);
		_varEditor.setSelectedIndex(1);

		//

		if(dts != null)
		{
			_table.getColumn("Derived Time Series").setCellEditor(new DefaultCellEditor(_dtsEditor));
			_table.getColumn("Operator").setCellEditor(new DefaultCellEditor(opEditor));
			_table.getColumn("Dvar/Svar").setCellEditor(new DefaultCellEditor(_varEditor));
			_table.getColumn("B part").setCellEditor(new DefaultCellEditor(_bpartEditor));
			_table.getColumn("C part").setCellEditor(new DefaultCellEditor(_cpartEditor));
		}
		else
		{
			_table.getColumn("Derived Time Series").setCellEditor(new DefaultCellEditor(_dtsEditor));
			_table.getColumn("Dvar/Svar").setCellEditor(new DefaultCellEditor(_varEditor));
			_table.getColumn("B part").setCellEditor(new DefaultCellEditor(_bpartEditor));
			_table.getColumn("C part").setCellEditor(new DefaultCellEditor(_cpartEditor));
		}
		_table.selectAll();
	}

	/**
	 * gets the cpart associated with the chosen bpart in a dts or mts table
	 */
	private void findSetCPart(DTSTableModel dtm, MTSTableModel mtm, int r)
	{
		String b = _bpartEditor.getText().toUpperCase();
		String var = (String) _varEditor.getSelectedItem();
		final Project prj = AppUtils.getCurrentProject();
		Map svList = prj.getSVHashtable();
		Map dvList = prj.getDVHashtable();
		if(svList == null || dvList == null)
		{
			return;
		}
		if(AppUtils.SVAR.equals(var))
		{
			String c = (String) svList.get(b.toUpperCase());
			if(dtm != null)
			{
				dtm.setValueAt(c, r, 4);
			}
			else
			{
				mtm.setValueAt(c, r, 3);
			}
		}
		else if(AppUtils.DVAR.equals(var))
		{
			String c = (String) dvList.get(b.toUpperCase());
			if(dtm != null)
			{
				dtm.setValueAt(c, r, 4);
			}
			else
			{
				mtm.setValueAt(c, r, 3);
			}
		}
	}

	/**
	 *
	 */
	void load()
	{
		if(DEBUG)
		{
			System.out.println("Load");
		}
		stopEditing();
		try
		{
			String dtsfile = VistaUtils.getFilenameFromDialog(this, FileDialog.LOAD,
					"dts", "DTS File");
			if(dtsfile == null)
			{
				return;
			}
			DerivedTimeSeries dts = DerivedTimeSeries.load(dtsfile);
			if(DEBUG)
			{
				System.out.println("LOADED: " + dts.getName());
			}
			GuiUtils.checkAndAddToProject(this, dts);
			if(DEBUG)
			{
				System.out.println("Added to project: " + dts.getName());
			}
			if(_modified)
			{
				int opt = JOptionPane.showConfirmDialog
						(this,
								"Current table has been modified! Do you want to save to file?",
								"Warning", JOptionPane.YES_NO_OPTION, JOptionPane.WARNING_MESSAGE);
				if(opt == JOptionPane.YES_OPTION)
				{
					save();
					if(!_modified)
					{
						setDTS(dts, null);
					}
				}
				else
				{
					setDTS(dts, null);
				}
			}
		}
		catch(Exception e)
		{
			VistaUtils.displayException(this, e);
		}
	}

	/**
	 *
	 */
	void export()
	{
		stopEditing();
		String pathname = (String)
				JOptionPane.showInputDialog(this, "Pathname : ", "Input Pathname",
						JOptionPane.PLAIN_MESSAGE,
						null, null,
						_dts.getPathname().toString());
		Pathname path = Pathname.createPathname(pathname);
		if(path == null)
		{
			return;
		}
		String dssfile = VistaUtils.getFilenameFromDialog(this, FileDialog.SAVE,
				"dss", "DSS File");
		if(dssfile == null)
		{
			return;
		}
		AppUtils.exportToDSS(_dts, dssfile, pathname);
	}

	/**
	 *
	 */

	void save()
	{
		DtsTreePanel.getCurrentModel().setNodeName(_dts.getName());
	}

	/**
	 * delete rows
	 */
	void delete()
	{
		if(_dts != null)
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
				return;
			}
			int numberDeleted = 0;
			for(int i = 0; i < ri.length; i++)
			{
				int currentIndex = ri[i] - numberDeleted;
				if(currentIndex >= _dts.getNumberOfDataReferences())
				{
					continue;
				}
				_dts.remove(currentIndex);
				numberDeleted++;
			}
			_modified = true;
			_table.tableChanged(new TableModelEvent(_table.getModel()));
		}
		else
		{
			if(DEBUG)
			{
				System.out.println("Delete");
			}
			stopEditing();
			// get user selected rows
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
				if(currentIndex >= _mts.getNumberOfDataReferences())
				{
					continue;
				}
				_mts.remove(currentIndex);
				numberDeleted++;
			}
			_modified = true;
			_table.tableChanged(new TableModelEvent(_table.getModel()));
		}
	}

	/**
	 *
	 */
	void add()
	{
		if(_dts != null)
		{
			if(DEBUG)
			{
				System.out.println("Add");
			}
			stopEditing();
			int i = _dts.getNumberOfDataReferences();
			_dts.setVarTypeAt(i, AppUtils.DVAR);
			_modified = true;
			_table.tableChanged(new TableModelEvent(_table.getModel()));
		}
		else
		{
			if(DEBUG)
			{
				System.out.println("Add");
			}
			stopEditing();
			int index = _mts.getNumberOfDataReferences();
			_mts.setVarTypeAt(index, AppUtils.DVAR);
			_modified = true;
			_table.tableChanged(new TableModelEvent(_table.getModel()));
		}
	}

	/**
	 *
	 */
	void insert()
	{
		if(_dts != null)
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
				return;
			}
			_dts.insertAt(ri);
			_modified = true;
			_table.tableChanged(new TableModelEvent(_table.getModel()));
		}
		else
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
						"Select a few rows first!",
						JOptionPane.PLAIN_MESSAGE);
				return;
			}
			_mts.insertAt(ri);
			_mts.setVarTypeAt(ri, AppUtils.DVAR);
			_modified = true;
			_table.tableChanged(new TableModelEvent(_table.getModel()));
		}
	}

	/**
	 *
	 */
	void retrieve()
	{
		stopEditing();
		try
		{
			if(_dts != null)
			{
				System.out.println(_dts.getName());
				GuiUtils.displayDTS(_dts);
			}
			else if(_mts != null)
			{
				GuiUtils.displayMTS(_mts);
			}
		}
		catch(Exception e)
		{
			VistaUtils.displayException(this, e);
		}
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
		if(_modified)
		{
			int opt = JOptionPane.showConfirmDialog
					(this,
							"Current table has been modified! Do you want to save to file?",
							"Warning", JOptionPane.YES_NO_OPTION, JOptionPane.WARNING_MESSAGE);
			if(opt == JOptionPane.YES_OPTION)
			{
				save();
				if(!_modified)
				{
					JOptionPane.getFrameForComponent(this).dispose();
				}
			}
			else
			{
				JOptionPane.getFrameForComponent(this).dispose();
			}
		}
		else
		{
			JOptionPane.getFrameForComponent(this).dispose();
		}
	}

	/**
	 *
	 */
	void changeNameToField()
	{
		String name = _nameField.getText();
		if(DEBUG)
		{
			System.out.println("Name: " + name);
		}
		try
		{
			name = name.trim();
			if(name == null || name.isEmpty())
			{
				return;
			}
			if(_dts != null)
			{
				_dts.setName(_nameField.getText());
			}
			_modified = true;
		}
		catch(Exception e)
		{
			VistaUtils.displayException(this, e);
		}
	}

	/**
	 *
	 */
	public void stopEditing()
	{
		changeNameToField();
		_modified = true;
		_table.editingStopped(new ChangeEvent(_table));
	}

	/**
	 *
	 */
	public JTable getTable()
	{
		return _table;
	}

	public void setTableModel(DerivedTimeSeries dts, MultipleTimeSeries mts)
	{
		setDTS(dts, mts);
		if(dts != null)
		{
			_nameField.setText(dts.getName());
		}
		else
		{
			_nameField.setText(mts.getName());
		}
		_dts = dts;
		_mts = mts;
	}

	public void setNameField(String name)
	{
		_nameField.setText(name);
	}

}
