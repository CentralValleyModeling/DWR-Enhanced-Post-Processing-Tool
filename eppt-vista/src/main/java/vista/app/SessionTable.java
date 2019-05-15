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
import java.awt.Color;
import java.awt.GridLayout;
import java.awt.Point;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.InputEvent;
import java.awt.event.KeyEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import javax.swing.*;
import javax.swing.table.TableColumnModel;
import javax.swing.table.TableModel;

import vista.app.commands.AddNewGroupCommand;
import vista.app.commands.CloneGroupCommand;
import vista.app.commands.MoveGroupCommand;
import vista.app.commands.OpenGroupCommand;
import vista.app.commands.RemoveGroupCommand;
import vista.app.commands.SetSessionNameCommand;
import vista.app.commands.SortSessionCommand;
import vista.app.commands.UnionGroupCommand;
import vista.gui.VistaUtils;
import vista.set.Group;
import vista.set.GroupSortMechanism;
import vista.set.Session;
import vista.set.SessionTableModel;

/**
 * This class displays the session information as a table.
 * 
 * @author Nicky Sandhu
 * @version $Id: SessionTable.java,v 1.1 2003/10/02 20:48:41 redwood Exp $
 */
public class SessionTable extends SessionView implements RowMovable, View {
	private JLabel _nRefLabel;
	private Session _session;
	private JTextField _sessionNameText;
	private JPanel _infoPanel;
	private GroupFrame _groupTable;
	private JTable _table;

	/**
	 * Construct a table
	 */
	public SessionTable() {
		updateView();
	}

	/**
	 * updates view
	 */
	public void updateView() {
		Session cs = MainGUI.getContext().getCurrentSession();
		// if ( _session == null || (! _session.equals(cs)) ) {
		_session = cs;
		removeAll();
		createView();
		// }
		//
		Group cg = MainGUI.getContext().getCurrentGroup();
		if (cg != null) {
			if (_groupTable != null) {
				if (!_groupTable.isVisible())
					_groupTable = new GroupFrame(cg);
				else
					_groupTable.getGroupTable().setGroup(cg);
			} else {
				_groupTable = new GroupFrame(cg);
			}
		}
		//
		updateInfoPanel();
		if (this.isVisible()) {
			if (this.getGraphics() != null)
				this.paintAll(this.getGraphics());
		}
	}

	/**
	 * returns the context of this view => the application data
	 */
	public SessionContext getContext() {
		return MainGUI.getContext();
	}

	/**
	 * sets the session
	 */
	public void setSession(Session s) {
		_session = s;
	}

	/**
	 * returns the session
	 */
	public Session getSession() {
		return _session;
	}

	/**
	 * updates information
	 */
	protected void updateInfoPanel() {
		Session s = getSession();
		if (DEBUG)
			System.out.println("Session Name:" + s.getName());
		_sessionNameText.setText(s.getName());
		_nRefLabel.setText("NUMBER OF GROUPS: " + s.getNumberOfGroups());
	}

	/**
	 * returns the menu for this session
	 */
	public JMenu getMenu() {
		// set up group menus
		JMenu groupMenu = new JMenu("Group");
		JMenuItem newGroup = new JMenuItem("New");
		JMenuItem cloneGroup = new JMenuItem("Clone");
		JMenuItem openGroup = new JMenuItem("Open");
		JMenuItem openGroupTree = new JMenuItem("Display As Tree");
		JMenuItem deleteGroup = new JMenuItem("Delete");
		JMenuItem unionGroup = new JMenuItem("Union");
		JMenuItem intersectionGroup = new JMenuItem("Intersection");
		JMenuItem createGroup = new JMenuItem("Create Group from Template");
		groupMenu.add(openGroup);
		groupMenu.addSeparator();
		groupMenu.add(newGroup);
		groupMenu.add(cloneGroup);
		groupMenu.add(deleteGroup);
		groupMenu.add(unionGroup);
		groupMenu.add(intersectionGroup);
		// groupMenu.add( createGroup );
		//
		newGroup.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent evt) {
				createNewGroup(evt);
			}
		});
		cloneGroup.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent evt) {
				cloneGroup(evt);
			}
		});
		openGroup.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent evt) {
				openGroup(evt);
			}
		});
		deleteGroup.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent evt) {
				deleteGroup(evt);
			}
		});
		unionGroup.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent evt) {
				unionGroup(evt);
			}
		});
		intersectionGroup.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent evt) {
				intersectionGroup(evt);
			}
		});
		createGroup.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent evt) {
				createGroup(evt);
			}
		});
		return groupMenu;
	}

	/**
	 * gets the complete table after construction.
	 */
	public JTable getTable() {
		return _table;
	}

	/**
	 * creates a new group and adds it to the session
	 */
	private void createNewGroup(ActionEvent evt) {
		Executor.execute(new AddNewGroupCommand(getSession()), this);
	}

	/**
	 * clone the group as a table.
	 */
	private void cloneGroup(ActionEvent evt) {
		int[] rows = _table.getSelectedRows();
		Executor.execute(new CloneGroupCommand(getSession(), rows), this);
	}

	/**
	 * open the group as a table.
	 */
	public void openGroup(ActionEvent evt) {
		// !PENDING: ShowGroupInTable(Session s, int groupNumber)
		int[] rows = _table.getSelectedRows();
		Executor.execute(new OpenGroupCommand(MainGUI.getContext(), MainGUI
				.getContext().getCurrentSession(), rows), this);
	}

	/**
	 * deletes the selected group or groups
	 */
	public void deleteGroup(ActionEvent evt) {
		int[] rows = _table.getSelectedRows();
		Executor.execute(new RemoveGroupCommand(getSession(), rows), this);
	}

	/**
	 * takes the union of selected groups and adds it to the session.
	 */
	void unionGroup(ActionEvent evt) {
		int[] rows = _table.getSelectedRows();
		Executor.execute(new UnionGroupCommand(getSession(), rows), this);
	}

	/**
	 * takes the intersection of the selected groups and adds it to the session.
	 */
	void intersectionGroup(ActionEvent evt) {
		int[] rows = _table.getSelectedRows();
		Executor
				.execute(new IntersectionGroupCommand(getSession(), rows), this);
	}

	/**
	 * takes the intersection of the selected groups and adds it to the session.
	 */
	void createGroup(ActionEvent evt) {
		int[] rows = _table.getSelectedRows();
		Session s = getSession();
		Group g = s.getGroup(rows[0]);

		// Executor.execute( new IntersectionGroupCommand( getSession(), rows )
		// , this);
	}

	/**
	 * sets name of session
	 */
	public void changeSessionName(ActionEvent evt) {
		String name = _sessionNameText.getText();
		Executor.execute(new SetSessionNameCommand(getSession(), name), this);
	}

	/**
	 * creates table
	 */
	protected void createView() {
		Session s = getSession();
		TableModel sessionModel = new SessionTableModel(s);
		_table = new JTable(sessionModel);
		// add listeners... CTRL-key...
		// delete key
		VistaUtils.addKeyListener(_table, KeyEvent.VK_D, InputEvent.CTRL_MASK,
				this, "deleteGroup");
		// get data key
		VistaUtils.addKeyListener(_table, KeyEvent.VK_G, InputEvent.CTRL_MASK,
				this, "openGroup");
		// down
		VistaUtils.addKeyListener(_table, KeyEvent.VK_DOWN,
				InputEvent.CTRL_MASK, this, "ctrlDownPressed");
		// up
		VistaUtils.addKeyListener(_table, KeyEvent.VK_UP, InputEvent.CTRL_MASK,
				this, "ctrlUpPressed");
		//
		if (DEBUG) {
			for (int i = 0; i < sessionModel.getColumnCount(); i++) {
				System.out.println(_table.getColumnModel().getColumn(i)
						.getIdentifier());
			}
		}
		// set cell editor
		// DefaultCellEditor cellEditor = new DefaultCellEditor(new
		// JTextField(""));
		// cellEditor.setClickCountToStart(2);
		// _table.getColumnModel().getColumn(1).setCellEditor( cellEditor );
		_table.getColumnModel().getColumn(0).setMaxWidth(25);
		_table.getColumnModel().getColumn(0).setMinWidth(25);
		_table.setAutoResizeMode(JTable.AUTO_RESIZE_ALL_COLUMNS);
		// set table attributes
		_table.setRowSelectionAllowed(true);
		// _table.setMultipleSelectionAllowed(true);
		_table.setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
		_table.setGridColor(Color.red);
		_table.setVisible(true);
		_table.sizeColumnsToFit(JTable.AUTO_RESIZE_ALL_COLUMNS);
		_table.getTableHeader().setReorderingAllowed(false);
		_table.getTableHeader()
				.addMouseListener(new TableHeaderMouseListener());
		_table.addMouseListener(new RowMoveListener(this));
		_table.addMouseListener(new TableMouseListener(this));
		//
		this.setLayout(new BorderLayout());

		JPanel sessionNamePanel = new JPanel();
		sessionNamePanel.setLayout(new BorderLayout());
		sessionNamePanel.add(new JLabel("SESSION: ", JLabel.LEFT),
				BorderLayout.WEST);
		sessionNamePanel.add(_sessionNameText = new JTextField(40),
				BorderLayout.CENTER);
		VistaUtils.addKeyListener(_sessionNameText, KeyEvent.VK_ENTER,
				InputEvent.CTRL_MASK, this, "changeSessionName");
		_infoPanel = new JPanel();
		_infoPanel.setLayout(new GridLayout(2, 1));
		_infoPanel.add(sessionNamePanel);
		_infoPanel.add(_nRefLabel = new JLabel("NUMBER OF GROUPS: "
				+ getSession().getNumberOfGroups()));
		JPanel tablePanel = new JPanel();
		tablePanel.setLayout(new BorderLayout());
		tablePanel.add(new JScrollPane(_table));

		this.add(_infoPanel, BorderLayout.NORTH);
		this.add(tablePanel, BorderLayout.CENTER);
	}

	/**
	 * returns the row number at point p
	 */
	public int rowAtPoint(Point p) {
		return _table.rowAtPoint(p);
	}

	/**
	 * moves row at oldPosition to newPosition
	 */
	public void moveRow(int oldPosition, int newPosition) {
		Executor.execute(new MoveGroupCommand(getSession(), oldPosition,
				newPosition), this);
	}

	/**
   * 
   */
	public void ctrlDownPressed(ActionEvent evt) {
		int nRows = _table.getRowCount();
		int selectIndex = _table.getSelectedRow();
		int newIndex = (selectIndex + 1) % nRows;
		_table.setRowSelectionInterval(newIndex, newIndex);
		_table.repaint();
	}

	/**
   *
   */
	public void ctrlUpPressed(ActionEvent evt) {
		int nRows = _table.getRowCount();
		int selectIndex = _table.getSelectedRow();
		int newIndex = Math.abs((nRows + selectIndex - 1) % nRows);
		_table.setRowSelectionInterval(newIndex, newIndex);
		_table.repaint();
	}

	/**
	 * Sorts session
	 * 
	 * @author Nicky Sandhu
	 * @version $Id: SessionTable.java,v 1.1 2003/10/02 20:48:41 redwood Exp $
	 */
	private class TableHeaderMouseListener extends MouseAdapter {
		public void mouseClicked(MouseEvent e) {
			TableColumnModel columnModel = _table.getColumnModel();
			int viewColumn = columnModel.getColumnIndexAtX(e.getX());
			int column = _table.convertColumnIndexToModel(viewColumn);
			if (e.getClickCount() == 1 && column != -1) {
				if (ascending) {
					Executor.execute(new SortSessionCommand(MainGUI
							.getContext(), getSession(), new GroupSortMechanism(
							GroupSortMechanism.INCREASING)), SessionTable.this);
				} else {
					Executor.execute(new SortSessionCommand(MainGUI
							.getContext(), getSession(), new GroupSortMechanism(
							GroupSortMechanism.DECREASING)), SessionTable.this);
				}
				ascending = !ascending;
			}
		}
	} // end of mouse clicked class

	/**
	 * Opens group on double click.
	 * 
	 * @author Nicky Sandhu
	 * @version $Id: SessionTable.java,v 1.1 2003/10/02 20:48:41 redwood Exp $
	 */
	private class TableMouseListener extends MouseAdapter {
		private SessionTable _st;

		public TableMouseListener(SessionTable st) {
			_st = st;
		}

		public void mouseClicked(MouseEvent e) {
			if (e.getClickCount() < 2)
				return;
			int rowId = _st.getTable()
					.rowAtPoint(new Point(e.getX(), e.getY()));
			if (rowId == -1)
				return;
			int[] rows = new int[] { rowId };
			Executor.execute(new OpenGroupCommand(MainGUI.getContext(), MainGUI
					.getContext().getCurrentSession(), rows), _st);
		}
	} // end of mouse clicked class

	private static boolean ascending = true;
	/**
   *
   */
	private static final boolean DEBUG = false;
}
