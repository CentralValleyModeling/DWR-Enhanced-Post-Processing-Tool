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
import java.awt.FileDialog;
import java.awt.GridBagLayout;
import java.awt.GridLayout;
import java.awt.Point;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.InputEvent;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import javax.swing.*;
import javax.swing.event.TableModelEvent;
import javax.swing.table.AbstractTableModel;
import javax.swing.table.JTableHeader;
import javax.swing.table.TableColumnModel;
import javax.swing.table.TableModel;

import vista.app.commands.AddReferenceCloneCommand;
import vista.app.commands.AddReferenceCommand;
import vista.app.commands.CreateTWReferencesCommand;
import vista.app.commands.DeleteReferencesCommand;
import vista.app.commands.ExportDataCommand;
import vista.app.commands.ExportDataToDSSCommand;
import vista.app.commands.GraphDataCommand;
import vista.app.commands.InsertReferenceAtCommand;
import vista.app.commands.MonthlyAverageCommand;
import vista.app.commands.MoveReferenceCommand;
import vista.app.commands.ScatterGraphCommand;
import vista.app.commands.SortReferencesCommand;
import vista.app.commands.TabulateDataCommand;
import vista.app.commands.UpdateDataCommand;
import vista.graph.Graph;
import vista.gui.VistaUtils;
import vista.set.DataReference;
import vista.set.DataSet;
import vista.set.Group;
import vista.set.GroupTableModel;
import vista.set.PartNamePredicate;
import vista.set.Pathname;
import vista.set.RegularTimeSeries;
import vista.set.SetUtils;
import vista.set.SortMechanism;

/**
 * This class displays the group information as a table by implementing the
 * TableModel. The table columns are the parts of the pathname and the ordering
 * can be shuffled.
 * 
 * @author Nicky Sandhu
 * @version $Id: GroupTable.java,v 1.1 2003/10/02 20:48:31 redwood Exp $
 */
@SuppressWarnings("serial")
public class GroupTable extends JPanel implements RowMovable, View {
	private MathOperationsListener _mathListener;
	private JLabel _groupLabel, _nRefLabel;
	private JCheckBoxMenuItem _showFilterPanel, _showMathPanel, _showInfoPanel;
	private PathnameFilterPanel _filterPanel;
	private JPanel _infoPanel, _mathPanel;
	public JTable _table;
	private Group _group;
	private JTextField _timeWindowField;
	private static final boolean DEBUG = false;
	private GraphBuilder _graphBuilder;
	private static JProgressBar _progressBar;
	private boolean _initShowMath = true, _initShowFilter = true;

	/**
	 * Construct a table
	 */
	public GroupTable(Group group) {
		super();
		createTable(group);
		_graphBuilder = new DefaultGraphBuilder();
		MainGUI.getContext().setCurrentGroup(group);
	}

	/**
	 * the context for this gui
	 */
	public SessionContext getContext() {
		return MainGUI.getContext();
	}

	/**
	 * updates the view of the current group object.
	 */
	public void updateView() {
		Group g = MainGUI.getContext().getCurrentGroup();
		if (_group == null || (!_group.equals(g))) {
			this.removeAll();
			_group = g;
			createTable(g);
		}
		updateInfoPanel();
		if (this.isVisible()) {
			if (this.getGraphics() != null){
				this.repaint();
			}
		}
	}

	/**
	 * updates information
	 */
	public void updateInfoPanel() {
		if (DEBUG)
			System.out.println("Group Name:" + _group.getName());
		_groupLabel.setText("GROUP: " + _group.getName());
		_nRefLabel.setText("NUMBER OF DATA REFERENCES: "
				+ _group.getNumberOfDataReferences());
		((AbstractTableModel) _table.getModel())
				.fireTableChanged(new TableModelEvent(_table.getModel()));
		_table.repaint();
	}

	/**
   *
   */
	public void addMenus(JMenuBar mbar) {
		//
		JMenuItem cloneDataItem = new JMenuItem("Clone");
		JMenuItem deleteDataItem = new JMenuItem("Delete");
		JMenu exportToItem = new JMenu("Export Data to...");
		JMenuItem dssExport = new JMenuItem("DSS");
		JMenuItem dssExportWithoutFlags = new JMenuItem("DSS w/o Flags");
		JMenu txtMenu = new JMenu("Text");
		JMenuItem txtExport = new JMenuItem("DSS Format");
		JMenuItem txtNormalExport = new JMenuItem("Generic Format");
		txtMenu.add(txtExport);
		txtMenu.add(txtNormalExport);
		exportToItem.add(dssExport);
		exportToItem.add(dssExportWithoutFlags);
		exportToItem.add(txtMenu);
		//
		JMenu importFromItem = new JMenu("Import Data from...");
		Action importDSSTSAction = new AbstractAction("RTS format") {
			public void actionPerformed(ActionEvent evt) {
				importDSSTSAction(true);
			}
		};
		Action importDSSITSAction = new AbstractAction("ITS format") {
			public void actionPerformed(ActionEvent evt) {
				importDSSTSAction(false);
			}
		};
		importFromItem.add(importDSSTSAction);
		importFromItem.add(importDSSITSAction);
		//
		JMenuItem updateItem = new JMenuItem("Write to server");
		JMenuItem reloadItem = new JMenuItem("Reload Group");
		JMenuItem quitItem = new JMenuItem("Quit Window");
		//
		JMenu showMenu = new JMenu("Show as...");
		JMenuItem showMonthlyAverage = new JMenuItem("Monthly Average");
		JMenuItem showAsTableItem = new JMenuItem("Table");
		JMenu showAsGraph = new JMenu("Graph");
		JMenuItem showTSItem = new JMenuItem("Time Series");
		JMenuItem showScatterItem = new JMenuItem("Paired");
		showAsGraph.add(showTSItem);
		showAsGraph.add(showScatterItem);
		showMenu.add(showMonthlyAverage);
		showMenu.add(showAsTableItem);
		showMenu.add(showAsGraph);
		//
		JMenu dataMenu = new JMenu("Data");
		dataMenu.add(showMenu);
		dataMenu.addSeparator();
		// dataMenu.add(cloneDataItem);
		dataMenu.add(updateItem);
		dataMenu.add(exportToItem);
		dataMenu.add(importFromItem);
		dataMenu.addSeparator();
		dataMenu.add(deleteDataItem);
		dataMenu.addSeparator();
		dataMenu.add(reloadItem);
		dataMenu.add(quitItem);
		// dataMenu.add(exportToItem);
		// add listeners...
		cloneDataItem.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent evt) {
				cloneData(evt);
			}
		});
		deleteDataItem.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent evt) {
				ctrlDPressed(evt);
			}
		});
		updateItem.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent evt) {
				updateData(evt);
			}
		});
		dssExport.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent evt) {
				exportDataToDSS(evt,true);
			}
		});
		dssExportWithoutFlags.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent evt) {
				exportDataToDSS(evt,false);
			}
		});
		txtExport.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent evt) {
				exportData(evt);
			}
		});
		txtNormalExport.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent evt) {
				exportDataGeneric(evt);
			}
		});
		showMonthlyAverage.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent evt) {
				monthlyAverage(evt);
			}
		});
		showAsTableItem.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent evt) {
				ctrlTPressed(evt);
			}
		});
		showTSItem.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent evt) {
				ctrlGPressed(evt);
			}
		});
		showScatterItem.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent evt) {
				showScatter(evt);
			}
		});
		reloadItem.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent evt) {
				reloadGroup(evt);
			}
		});
		quitItem.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent evt) {
				quitWindow(evt);
			}
		});
		//
		//
		JMenu animationMenu = new JMenu("Animation");
		JMenuItem profileItem = new JMenuItem("Profile Animation");
		JMenuItem pieItem = new JMenuItem("Pie Chart Animation");
		profileItem.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent evt) {
				DataReference[] refs = getSelectedReferences();
				new ProfileAnimationDialog(refs);
			}
		});
		pieItem.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent evt) {
				DataReference[] refs = getSelectedReferences();
				if (refs == null || refs.length == 0)
					throw new IllegalArgumentException("No references selected");
				RegularTimeSeries[] rts = new RegularTimeSeries[refs.length];
				for (int i = 0; i < refs.length; i++) {
					DataSet ds = refs[i].getData();
					if (ds == null)
						throw new IllegalArgumentException(
								"Could not get data for: " + refs[i]);
					if (ds instanceof RegularTimeSeries)
						rts[i] = (RegularTimeSeries) ds;
					else
						throw new IllegalArgumentException(
								"only regular time series"
										+ " allowed in pie chart"
										+ " animations");
				}
				new PieChartAnimator(rts);
			}
		});
		animationMenu.add(profileItem);
		animationMenu.add(pieItem);
		//
		mbar.add(dataMenu);
		mbar.add(animationMenu);
	}

	/**
   *
   */
	public void showFilterPanel(ActionEvent evt) {
		boolean show = _showFilterPanel.isSelected();
		if (show) {
			_infoPanel.add(_filterPanel);
			_infoPanel.setLayout(new GridLayout(_infoPanel.getComponentCount(),
					1));
		} else {
			_infoPanel.remove(_filterPanel);
			_infoPanel.setLayout(new GridLayout(_infoPanel.getComponentCount(),
					1));
		}
		this.paintAll(this.getGraphics());
	}

	/**
   *
   */
	public void showSortDialog(ActionEvent evt) {
		new SortDialog((JFrame) JOptionPane.getFrameForComponent(this), this)
				.show();
	}

	/**
   *
   */
	public void showGroupTree(ActionEvent evt) {
		new GroupTreeFrame(_group);
	}

	/**
   *
   */
	public void showMathPanel(ActionEvent evt) {
		boolean show = _showMathPanel.getState();
		if (show) {
			_infoPanel.add(_mathPanel);
			_infoPanel.setLayout(new GridLayout(_infoPanel.getComponentCount(),
					1));
		} else {
			_infoPanel.remove(_mathPanel);
			_infoPanel.setLayout(new GridLayout(_infoPanel.getComponentCount(),
					1));
		}
		this.paintAll(this.getGraphics());
	}

	/**
   *
   */
	public void showInfoPanel(ActionEvent evt) {
		boolean show = _showInfoPanel.getState();
		if (show) {
			this.add(_infoPanel, BorderLayout.NORTH);
			_showFilterPanel.setEnabled(true);
			_showMathPanel.setEnabled(true);
		} else {
			this.remove(_infoPanel);
			_showFilterPanel.setEnabled(false);
			_showMathPanel.setEnabled(false);
		}
		this.paintAll(this.getGraphics());
	}

	/**
	 * quits this window
	 */
	public void reloadGroup(ActionEvent evt) {
		_group.reload();
		MainGUI.getContext().setCurrentGroup(_group);
		updateView();
	}

	/**
	 * quits this window
	 */
	public void quitWindow(ActionEvent evt) {
		JOptionPane.getFrameForComponent(this).dispose();
		MainGUI.getContext().setCurrentGroup(null);
	}

	/**
	 * adds reference to group and updates view
	 */
	public void addReference(DataReference ref) {
		Executor.execute(new AddReferenceCommand(getContext(), ref), this);
	}

	/**
	 * adds reference at currently selected item
	 */
	public void addReferenceAtCurrentSelection(DataReference ref) {
		int row = _table.getSelectedRow();
		Executor.execute(new InsertReferenceAtCommand(getContext(), ref,
				row + 1), this);
	}

	/**
	 * adds clone of references to group
	 */
	public void cloneData(ActionEvent evt) {
		DataReference[] refs = getSelectedReferences();
		if (refs == null || refs.length == 0)
			return;
		Executor
				.execute(new AddReferenceCloneCommand(getContext(), refs), this);
	}

	/**
	 * updates data on the server if allowed to do so.
	 */
	public void updateData(ActionEvent evt) {
		DataReference[] refs = getSelectedReferences();
		if (refs == null || refs.length == 0)
			return;
		Executor.execute(new UpdateDataCommand(refs), this);
	}

	/**
   *
   */
	public void exportData(ActionEvent evt) {
		// get filename from dialog...
		String saveFilename = VistaUtils.getFilenameFromDialog(this,
				FileDialog.SAVE, "txt", "Text Format");
		if (saveFilename == null)
			return;
		Executor.execute(new ExportDataCommand(getGroup(), _table
				.getSelectedRows(), saveFilename), this);
	}

	/**
   *
   */
	public void exportDataGeneric(ActionEvent evt) {
		// get filename from dialog...
		int[] rows = _table.getSelectedRows();
		if (rows == null || rows.length == 0)
			return;
		//
		String saveFilename = VistaUtils.getFilenameFromDialog(this,
				FileDialog.SAVE, "txt", "Text Format");
		if (saveFilename == null)
			return;
		//
		for (int i = 0; i < rows.length; i++) {
			DataReference ref = _group.getDataReference(rows[i]);
			SetUtils.write(ref.getData(), saveFilename, ref.getData()
					.isFlagged());
		}
	}

	/**
   *
   */
	public void exportDataToDSS(ActionEvent evt, boolean withFlags) {
		// get filename from dialog...
		String saveFilename = VistaUtils.getFilenameFromDialog(this,
				FileDialog.SAVE, "dss", "DSS Files");
		if (saveFilename == null)
			return;
		saveFilename = VistaUtils.setExtension(saveFilename, "dss");
		Executor.execute(new ExportDataToDSSCommand(getGroup(), _table
				.getSelectedRows(), saveFilename, withFlags), this);
	}

	/**
	 * sets new group for table.
	 */
	public void setGroup(Group g) {
		this.removeAll();
		createTable(g);
		if (this.isVisible()) {
			this.paintAll(this.getGraphics());
		}
	}

	/**
	 * group for this table
	 */
	public Group getGroup() {
		return _group;
	}

	/**
	 * returns selection
	 */
	public Object getSelectedValue() {
		int row = _table.getSelectedRow();
		return _group.getDataReference(row);
	}

	/**
	 * @return an array of selected value or array of size 0 if no values are
	 *         selected.
	 */
	public DataReference[] getSelectedReferences() {
		int rowCount = _table.getSelectedRowCount();
		int[] rows = _table.getSelectedRows();
		DataReference[] array = new DataReference[rowCount];
		for (int i = 0; i < rowCount; i++)
			array[i] = _group.getDataReference(rows[i]);
		return array;
	}

	/**
	 * gets the complete table after construction.
	 */
	public JTable getTable() {
		return _table;
	}

	/**
	 * CTRL - d pressed
	 */
	public void ctrlDPressed(ActionEvent evt) {
		int[] rows = _table.getSelectedRows();
		if (rows == null || rows.length == 0)
			return;
		Executor.execute(new DeleteReferencesCommand(getGroup(), rows), this);
	}

	/**
   *
   */
	public static void setMonitorMaximum(int value) {
		_progressBar.setMaximum(value);
	}

	public static void setMonitorMinimum(int value) {
		_progressBar.setMinimum(value);
	}

	/**
   *
   */
	public static void setMonitorValue(int value) {
		try {
			_progressBar.setValue(value);
			_progressBar.update(_progressBar.getGraphics());
		} catch (IllegalArgumentException e) {
		}
	}

	/**
   *
   */
	public static void incrementMonitorValue(int value) {
		try {
			_progressBar.setValue(_progressBar.getValue() + value);
			_progressBar.update(_progressBar.getGraphics());
		} catch (IllegalArgumentException e) {
		}
	}

	/**
   *
   */
	public void ctrlGPressed(ActionEvent evt) {
		Executor.execute(new GraphDataCommand(getGroup(), _table
				.getSelectedRows()), this);
	}

	/**
   *
   */
	public void showScatter(ActionEvent evt) {
		Executor.execute(new ScatterGraphCommand(getGroup(), _table
				.getSelectedRows()), this);
	}

	/**
   *
   */
	public void graphData() {
		Runnable task = new Runnable() {
			public void run() {
				try {
					int[] rows = _table.getSelectedRows();
					if (rows == null || rows.length == 0)
						return;
					setMonitorMinimum(0);
					setMonitorMaximum(rows.length);
					for (int i = 0; i < rows.length; i++) {
						try {
							_group.getDataReference(rows[i]).getData();
						} catch (Exception e) {
							VistaUtils.displayException(GroupTable.this, e);
						}
						incrementMonitorValue(1);
					}
					_graphBuilder = new DefaultGraphBuilder();
					for (int i = 0; i < rows.length; i++)
						_graphBuilder.addData(_group.getDataReference(rows[i]));
					Graph[] graphs = _graphBuilder.createGraphs();
					if (graphs != null && graphs.length > 0) {
						for (int i = 0; i < graphs.length; i++) {
							new DataGraphFrame(graphs[i], "Graph").setVisible(true);
						}
					}
				} catch (Exception e) {
					VistaUtils.displayException(GroupTable.this, e);
				} finally {
					setMonitorValue(0);
				}
			}
		};
		new Thread(task).start();
		// task.run();
	}

	/**
	 * Monthly Average
	 */

	public void monthlyAverage(ActionEvent evt) {
		try {
			Executor.execute(new MonthlyAverageCommand(getGroup(), _table
					.getSelectedRows()), this);
		} catch (Exception e) {
			JOptionPane.showMessageDialog(this, e);
		} finally {
		}
	}

	/**
	 * CTRL - t pressed
	 */
	public void ctrlTPressed(ActionEvent evt) {
		try {
			Executor.execute(new TabulateDataCommand(getGroup(), _table
					.getSelectedRows()), this);
			// this.setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR) );
			// int row = _table.getSelectedRow();
			// if ( row < 0 ) return;
			// try {
			// DataReference ref = _group.getDataReference(row);
			// if ( _dataTable == null )
			// _dataTable = new DataTable(_group.getDataReference( row ));
			// else
			// _dataTable = new DataTable(_group.getDataReference( row ));
		} catch (Exception e) {
			JOptionPane.showMessageDialog(this, e);
		} finally {
			// this.setCursor( Cursor.getDefaultCursor() );
		}
	}

	public void importDSSTSAction(boolean isRegular) {
		try {
			// get filename from dialog
			String filename = VistaUtils.getFilenameFromDialog(this,
					FileDialog.LOAD, ".txt", "Text files");
			if (filename == null)
				return;
			// use utility function to import data into data references into
			// current group. The dss file in the mentioned import is created
			// and then all the said references are merged with the current
			// group
			DataReference[] refs = SetUtils.importDataFromText(filename,
					isRegular);
			if (refs != null) {
				for (int i = 0; i < refs.length; i++) {
					DataReference ref = refs[i];
					if (ref == null)
						continue;
					_group.addDataReference(ref);
				}
			}
		} catch (Exception e) {
			VistaUtils.displayException(this, e);
		}
		updateView();
	}

	/**
	 * creates table
	 */
	private void createTable(Group group) {
		_group = group;
		TableModel groupModel = new GroupTableModel(_group);
		_table = new JTable(groupModel);
		TableColumnModel columnModel = _table.getColumnModel();
		// add listeners... CTRL-key...
		// delete key
		VistaUtils.addKeyListener(this, KeyEvent.VK_D, InputEvent.CTRL_MASK,
				this, "ctrlDPressed");
		// get data key
		VistaUtils.addKeyListener(this, KeyEvent.VK_G, InputEvent.CTRL_MASK,
				this, "ctrlGPressed");
		// get data key
		VistaUtils.addKeyListener(this, KeyEvent.VK_T, InputEvent.CTRL_MASK,
				this, "ctrlTPressed");
		//
		if (DEBUG) {
			for (int i = 0; i < groupModel.getColumnCount(); i++) {
				System.out.println(columnModel.getColumn(i).getIdentifier());
			}
		}
		// set column margins
		if (DEBUG) {
			System.out.println(columnModel.getColumn(Pathname.A_PART)
					.getWidth());
			System.out.println(columnModel.getColumn(Pathname.B_PART)
					.getWidth());
			System.out.println(columnModel.getColumn(Pathname.C_PART)
					.getWidth());
			System.out.println(columnModel.getColumn(Pathname.D_PART)
					.getWidth());
			System.out.println(columnModel.getColumn(Pathname.E_PART)
					.getWidth());
			System.out.println(columnModel.getColumn(Pathname.F_PART)
					.getWidth());
		}
		_table.setAutoResizeMode(JTable.AUTO_RESIZE_ALL_COLUMNS);
		columnModel.getColumn(0).setMaxWidth(30);
		columnModel.getColumn(0).setMinWidth(30);
		columnModel.getColumn(Pathname.A_PART + 1).setPreferredWidth(50);
		columnModel.getColumn(Pathname.B_PART + 1).setPreferredWidth(75);
		columnModel.getColumn(Pathname.C_PART + 1).setPreferredWidth(75);
		columnModel.getColumn(Pathname.D_PART + 1).setPreferredWidth(240);
		columnModel.getColumn(Pathname.E_PART + 1).setPreferredWidth(75);
		columnModel.getColumn(Pathname.F_PART + 1).setPreferredWidth(150);

		// _aFilterHeader = new FilterHeader("A part");
		// columnModel.getColumn(Pathname.A_PART).setHeaderRenderer(new
		// FilterHeader(");
		// columnModel.getColumn(Pathname.D_PART).
		// setCellEditor(new DefaultCellEditor(new JTextField(16)));
		// columnModel.addColumnModelListener( new GroupColumnListener() );
		// set table attributes
		_table.setRowSelectionAllowed(true);
		_table.setColumnSelectionAllowed(false);
		_table.setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
		_table.setGridColor(Color.blue);
		_table.setVisible(true);
		_table.sizeColumnsToFit(JTable.AUTO_RESIZE_ALL_COLUMNS);
		_table.getTableHeader().setReorderingAllowed(true);
		_table.addMouseListener(new RowMoveListener(this));
		//
		JTableHeader header = _table.getTableHeader();
		header.setUpdateTableInRealTime(false);
		header.addMouseListener(new TableHeaderMouseListener());
		// add math operation listeners
		_mathListener = new MathOperationsListener(this);
		// add info panel
		Box glbox = new Box(BoxLayout.X_AXIS);
		// glbox.add(Box.createVerticalStrut(20));
		glbox.add(_groupLabel = new JLabel("GROUP: " + _group.getName()));
		Box refbox = new Box(BoxLayout.X_AXIS);
		// refbox.add(Box.createVerticalStrut(20));
		refbox.add(_nRefLabel = new JLabel("NUMBER OF DATA REFERENCES: "
				+ _group.getNumberOfDataReferences()));
		JPanel iPanel = new JPanel();
		iPanel.setLayout(new GridLayout(2, 1));
		iPanel.add(glbox);
		iPanel.add(refbox);
		//
		_mathPanel = new MathOperationsPanel(this);
		_mathPanel.setBorder(BorderFactory.createTitledBorder(BorderFactory
				.createEtchedBorder(), "Math"));
		//
		_filterPanel = new PathnameFilterPanel(this);
		_filterPanel.setBorder(BorderFactory.createTitledBorder(BorderFactory
				.createEtchedBorder(), "Filter"));
		//
		_infoPanel = new JPanel();
		// _infoPanel.setLayout( new GridLayout(3,1) );
		_infoPanel.setLayout(new BoxLayout(_infoPanel, BoxLayout.Y_AXIS));
		Box hb1 = new Box(BoxLayout.X_AXIS);
		hb1.add(iPanel);
		hb1.add(Box.createVerticalStrut(25));
		_infoPanel.add(hb1);
		if (_initShowMath) {
			Box hb2 = new Box(BoxLayout.X_AXIS);
			hb2.add(_mathPanel);
			hb2.add(Box.createVerticalStrut(25));
			_infoPanel.add(hb2);
		}
		if (_initShowFilter) {
			Box hb3 = new Box(BoxLayout.X_AXIS);
			hb3.add(_filterPanel);
			hb3.add(Box.createVerticalStrut(40));
			_infoPanel.add(hb3);
		}
		JPanel tablePanel = new JPanel();
		tablePanel.setLayout(new BorderLayout());
		tablePanel.add(new JScrollPane(_table));
		// set up time window field
		JPanel twPanel = new JPanel();
		twPanel.setLayout(new BorderLayout());
		JButton twButton = new JButton("Set TimeWindow: ");
		twPanel.add(twButton, BorderLayout.WEST);
		twPanel.add(_timeWindowField = new JTextField(20), BorderLayout.CENTER);
		_timeWindowField.addKeyListener(new TimeWindowListener());
		twButton.addActionListener(new TimeWindowListener());
		twPanel.add(_progressBar = new JProgressBar(), BorderLayout.EAST);
		//
		JPanel mainPanel = new JPanel();
		mainPanel.setLayout(new BorderLayout());
		mainPanel.add(_infoPanel, BorderLayout.NORTH);
		mainPanel.add(tablePanel, BorderLayout.CENTER);
		mainPanel.add(twPanel, BorderLayout.SOUTH);
		// create and add tool bar
		JToolBar tb = new JToolBar();
		tb.setFloatable(true);
		ImageIcon gIcon = new ImageIcon(VistaUtils
				.getImageAsBytes("/vista/graph.gif"));
		JButton graphButton = new JButton(gIcon);
		ImageIcon tIcon = new ImageIcon(VistaUtils
				.getImageAsBytes("/vista/table.gif"));
		JButton tableButton = new JButton(tIcon);
		JButton deleteButton = new JButton("Delete");
		tableButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent evt) {
				ctrlTPressed(evt);
			}
		});
		graphButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent evt) {
				ctrlGPressed(evt);
			}
		});
		deleteButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent evt) {
				ctrlDPressed(evt);
			}
		});
		tb.add(graphButton);
		tb.add(tableButton);
		tb.add(deleteButton);
		//
		this.setLayout(new BorderLayout());
		this.add(tb, BorderLayout.NORTH);
		this.add(mainPanel, BorderLayout.CENTER);
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
		Executor.execute(new MoveReferenceCommand(getGroup(), oldPosition,
				newPosition), this);
	}

	/**
	 * 
	 * 
	 * @author Nicky Sandhu
	 * @version $Id: GroupTable.java,v 1.1 2003/10/02 20:48:31 redwood Exp $
	 */
	private class TimeWindowListener implements KeyListener, ActionListener {
		/**
		 * filters on pressing enter key on any field...
		 */
		public void keyPressed(KeyEvent evt) {
			if (evt.getKeyCode() != KeyEvent.VK_ENTER)
				return;
			createTimeWindowedReferences();
		}

		/**
		 * filters on clicking on that button
		 */
		public void actionPerformed(ActionEvent evt) {
			createTimeWindowedReferences();
		}

		/**
		 * creates time windowed references
		 */
		private void createTimeWindowedReferences() {
			String timeText = _timeWindowField.getText().trim();
			Executor.execute(new CreateTWReferencesCommand(getGroup(), _table
					.getSelectedRows(), timeText), GroupTable.this);
		}

		public void keyTyped(KeyEvent evt) {
		}

		public void keyReleased(KeyEvent evt) {
		}
	}// endof TimeWindowListener class

	//
	private static boolean[] ascVals = new boolean[Pathname.MAX_PARTS];

	// private static SortMechanism prevSm = null;
	/**
	 * 
	 * 
	 * @author Nicky Sandhu
	 * @version $Id: GroupTable.java,v 1.1 2003/10/02 20:48:31 redwood Exp $
	 */
	private class TableHeaderMouseListener extends MouseAdapter {

		public TableHeaderMouseListener() {
			for (int i = 0; i < ascVals.length; i++)
				ascVals[i] = true;
		}

		/**
   *
   */
		public void mouseClicked(MouseEvent e) {
			TableColumnModel columnModel = _table.getColumnModel();
			int viewColumn = columnModel.getColumnIndexAtX(e.getX());
			int column = _table.convertColumnIndexToModel(viewColumn);
			column = column - 1; // no sorting on first column
			// if ( e.isControlDown() ){
			// prevSm = null;
			// }
			if (e.getClickCount() == 1 && column != -1) {
				SortMechanism<DataReference> sortMechanism = null;
				// if ( prevSm != null && ((PartNamePredicate)
				// prevSm).getPartId() == column){
				// sortMechanism = prevSm;
				// } else {
				sortMechanism = new PartNamePredicate(column,
						SortMechanism.INCREASING);
				// }
				sortMechanism.setAscendingOrder(ascVals[column]);
				Executor.execute(new SortReferencesCommand(getGroup(),
						sortMechanism), GroupTable.this);
				ascVals[column] = !ascVals[column];
				// prevSm = sortMechanism;
			}
		}
	} // end of mouse clicked class

	// private static boolean ascending = true;
	public GridBagLayout gbl;
}
