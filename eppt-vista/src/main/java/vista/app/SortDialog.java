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
import java.awt.FileDialog;
import java.awt.Point;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Vector;
import javax.swing.*;

import vista.gui.Changeable;
import vista.gui.DialogButtonPanel;
import vista.gui.VistaUtils;
import vista.set.ListDirectedPredicate;

/**
 * A modal dialog to establish connection with server
 *
 * @author Nicky Sandhu
 * @version $Id: SortDialog.java,v 1.1 2003/10/02 20:48:42 redwood Exp $
 */
public class SortDialog extends JDialog implements Changeable, RowMovable
{
	/**
	 *
	 */
	private static final boolean DEBUG = false;
	private JList _sortList;
	private GroupTable _table;
	private Vector _listV;

	/**
	 * sets up a blocking dialog displaying server and directory names
	 */
	public SortDialog(JFrame parent, GroupTable table)
	{
		super(parent, true);
		_table = table;
		//
		JMenuBar mbar = new JMenuBar();
		JMenu menu1 = new JMenu("List");
		JMenuItem loadFileMenu = new JMenuItem("Load from file...");
		loadFileMenu.addActionListener(new LoadFileListener());
		JMenuItem removeMenu = new JMenuItem("Remove Selected");
		removeMenu.addActionListener(new RemoveFileListener());
		menu1.add(loadFileMenu);
		menu1.add(removeMenu);
		mbar.add(menu1);
		setJMenuBar(mbar);
		//
		_listV = new Vector();
		_sortList = new JList(_listV);
		// set up connection panel
		JPanel mainPanel = new JPanel();
		mainPanel.setLayout(new BorderLayout());
		mainPanel.add(new JScrollPane(_sortList), BorderLayout.CENTER);
		// add main panel
		super.getContentPane().setLayout(new BorderLayout());
		super.getContentPane().add(mainPanel, BorderLayout.CENTER);
		super.getContentPane().add(new DialogButtonPanel(this),
				BorderLayout.SOUTH);
		setLocationRelativeTo(_table);
		addMouseListener(new RowMoveListener(this));
		pack();
	}

	/**
	 * adds str to end of list
	 */
	void addToSortList(String str)
	{
		_listV.addElement(str);
		_sortList.setListData(_listV);
		_sortList.repaint();
	}

	/**
	 *
	 */
	void removeSelected()
	{
		Object[] obj = _sortList.getSelectedValues();
		if(obj == null)
		{
			return;
		}
		for(int i = 0; i < obj.length; i++)
		{
			_listV.removeElement(obj[i]);
		}
		_sortList.setListData(_listV);
		_sortList.repaint();
	}

	/**
	 * Apply the changes (OK/Apply button pressed)
	 */
	public void applyChanges()
	{
		String[] list = new String[_listV.size()];
		_listV.copyInto(list);
		if(list != null && list.length > 1)
		{
			throw new RuntimeException("If this functionality is needed. it needs to be fixed first!");
		}
		//FIXME:
		ListDirectedPredicate ldp = new ListDirectedPredicate(list[0]);
		_table.getGroup().sortBy(ldp);
		_table.repaint();
	}

	/**
	 * Done with making changes (OK/Cancel button pressed)
	 */
	public void doneChanges()
	{
		this.dispose();
	}

	/**
	 * returns the row number at point p
	 */
	public int rowAtPoint(Point p)
	{
		return _sortList.locationToIndex(p);
	}

	/**
	 * moves row at oldPosition to newPosition
	 */
	public void moveRow(int oldPosition, int newPosition)
	{
		Object obj = _listV.elementAt(oldPosition);
		_listV.removeElement(obj);
		_listV.insertElementAt(obj, newPosition);
		_sortList.setListData(_listV);
		_sortList.repaint();
	}

	/**
	 * @author Nicky Sandhu
	 * @version $Id: SortDialog.java,v 1.1 2003/10/02 20:48:42 redwood Exp $
	 */
	public class LoadFileListener implements ActionListener
	{
		public void actionPerformed(ActionEvent evt)
		{
			String filename = VistaUtils.getFilenameFromDialog(SortDialog.this,
					FileDialog.LOAD, ".sort", "Sorting Order List");
			if(filename == null)
			{
				return;
			}
			SortDialog.this.addToSortList(filename);
		}
	} // end of load file listener

	/**
	 * @author Nicky Sandhu
	 * @version $Id: SortDialog.java,v 1.1 2003/10/02 20:48:42 redwood Exp $
	 */
	public class RemoveFileListener implements ActionListener
	{
		public void actionPerformed(ActionEvent evt)
		{
			removeSelected();
		}
	} // end of load file listener
}
