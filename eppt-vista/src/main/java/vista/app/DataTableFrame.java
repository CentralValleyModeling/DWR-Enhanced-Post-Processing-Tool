/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package vista.app;

import java.awt.BorderLayout;
import java.awt.Container;
import java.awt.FileDialog;
import java.awt.GridLayout;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.AdjustmentEvent;
import java.awt.event.AdjustmentListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.io.IOException;
import java.util.regex.Pattern;
import javax.swing.*;

import vista.db.dss.DSSUtil;
import vista.graph.Graph;
import vista.gui.VistaUtils;
import vista.set.DataReference;
import vista.set.DataRetrievalException;
import vista.set.DataSet;
import vista.set.FlagUtils;
import vista.set.SetUtils;
import vista.set.TimeSeries;

/**
 * This is a table view on the DataSet.
 *
 * @author Nicky Sandhu
 * @version $Id: DataTable.java,v 1.1 2003/10/02 20:48:27 redwood Exp $
 */
public class DataTableFrame extends DefaultFrame
{
	private JLabel pathnameLabel;
	private JMenuBar mbar;
	/**
	 * the scroll pane
	 */
	private JScrollPane _tableScrollPane;
	/**
	 * the table
	 */
	private DataSetTable _table;
	/**
	 * the graph fram
	 */
	private DataGraphFrame _graphFrame;
	/**
	 * the data reference containing the data
	 */
	private DataReference _ref;
	/**
	 * the goto line number field
	 */
	private JTextField _lineNumberField;

	/**
	 *
	 */
	public DataTableFrame(DataReference ref)
	{
		this(ref, true);
	}

	/**
	 * Construct a table
	 */
	public DataTableFrame(DataReference ref, boolean visibleOnStart)
	{
		super();
		setIconImage(Toolkit.getDefaultToolkit().createImage(
				VistaUtils.getImageAsBytes("/vista/planning.gif")));
		_ref = ref;
		_table = new DataSetTable(ref.getData());
		//
		JPanel infoPanel = new JPanel();
		infoPanel.setLayout(new GridLayout(5, 1));
		infoPanel.add(new JLabel(ref.getServername()));
		infoPanel.add(new JLabel(ref.getFilename()));
		infoPanel.add(pathnameLabel = new JLabel(ref.getPathname().toString()));
		infoPanel.add(new JLabel("Number of data points: "
				+ _table.getRowCount()));
		if(ref.getTimeWindow() != null)
		{
			infoPanel.add(new JLabel(ref.getTimeWindow().toString()));
		}
		//
		JPanel gotoPanel = new JPanel();
		gotoPanel.setLayout(new BorderLayout());
		gotoPanel.add(new JLabel("Goto row: "), BorderLayout.WEST);
		_lineNumberField = new JTextField(40);
		gotoPanel.add(_lineNumberField, BorderLayout.CENTER);
		GotoListener l1 = new GotoListener();
		_lineNumberField.addKeyListener(l1);
		//
		JPanel tablePanel = new JPanel();
		tablePanel.setLayout(new BorderLayout());
		tablePanel.add(_tableScrollPane = new JScrollPane(_table));
		_tableScrollPane.getVerticalScrollBar().addAdjustmentListener(l1);
		// add components...
		Container contentPane = getContentPane();
		contentPane.setLayout(new BorderLayout());
		contentPane.add(infoPanel, BorderLayout.NORTH);
		contentPane.add(tablePanel, BorderLayout.CENTER);
		contentPane.add(gotoPanel, BorderLayout.SOUTH);
		//
		mbar = new JMenuBar();
		mbar.add(createDataMenu());
		if(isFlagged(_ref))
		{
			mbar.add(createFlagMenu());
			// editor for flags
		}
		getRootPane().setJMenuBar(mbar);
		//
		this.setTitle(ref.getPathname().toString());
		// set size according to flag/ no flag display
		int columnCount = _table.getColumnCount();
		setSize(170 * columnCount, 750);
		//
		this.setVisible(visibleOnStart);
	}

	/**
	 * gets the complete table after construction.
	 */
	public JTable getTable()
	{
		return _table.getTable();
	}

	/**
	 *
	 */
	private final boolean isFlagged(DataReference ref)
	{
		boolean isFlagged = false;
		try
		{
			DataSet data = ref.getData();
			isFlagged = data.isFlagged();
		}
		catch(DataRetrievalException dre)
		{
			VistaUtils.displayException(this._table, dre);
		}
		return isFlagged;
	}

	/**
	 *
	 */
	private JMenu createFlagMenu()
	{
		//
		JMenu flagMenu = new JMenu("Flag");
		JMenuItem markAsMissing = new JMenuItem("Mark selected as missing");
		JMenuItem markAsQuestionable = new JMenuItem(
				"Mark selected as questionable");
		JMenuItem markAsReject = new JMenuItem("Mark selected as reject");
		JMenuItem markAsOK = new JMenuItem("Mark selected as ok");
		JMenuItem markAsUS = new JMenuItem("Unmark selected");
		JMenuItem flagOverride = null;
		flagOverride = new JCheckBoxMenuItem("Override flags ?", _table
				.isFlagOverride());
		flagMenu.add(markAsOK);
		flagMenu.add(markAsMissing);
		flagMenu.add(markAsQuestionable);
		flagMenu.add(markAsReject);
		flagMenu.add(markAsUS);
		if(flagOverride != null)
		{
			flagMenu.addSeparator();
			flagMenu.add(flagOverride);
		}
		// add listeners
		markAsMissing.addActionListener(new ActionListener()
		{
			public void actionPerformed(ActionEvent evt)
			{
				markAsMissing(evt);
			}
		});
		markAsQuestionable.addActionListener(new ActionListener()
		{
			public void actionPerformed(ActionEvent evt)
			{
				markAsQuestionable(evt);
			}
		});
		markAsReject.addActionListener(new ActionListener()
		{
			public void actionPerformed(ActionEvent evt)
			{
				markAsReject(evt);
			}
		});
		markAsOK.addActionListener(new ActionListener()
		{
			public void actionPerformed(ActionEvent evt)
			{
				markAsOK(evt);
			}
		});
		markAsUS.addActionListener(new ActionListener()
		{
			public void actionPerformed(ActionEvent evt)
			{
				markAsUS(evt);
			}
		});
		if(flagOverride != null)
		{
			flagOverride.addActionListener(new ActionListener()
			{
				public void actionPerformed(ActionEvent evt)
				{
					setFlagOverride(evt);
				}
			});
		}
		return flagMenu;
	}

	/**
	 *
	 */
	private JMenu createDataMenu()
	{
		final JMenu dataMenu = new JMenu("Data");
		JMenuItem showAsGraphItem = new JMenuItem("Show As Graph");
		JMenuItem showFlagsItem = new JCheckBoxMenuItem("Show Flags",
				isFlagged(_ref));
		final JMenuItem addFlagsItem = new JMenuItem("Add Flags");
		JMenuItem showStatsItem = new JMenuItem("Show Attributes & Stats");
		JMenuItem showDetailedInfo = new JMenuItem("Show Detailed Info");
		JMenuItem editAttrItem = new JMenuItem("Edit Attributes");
		JMenuItem editPathnameItem = new JMenuItem("Edit Pathname");
		JMenu exportDataItem = new JMenu("Export Data to...");
		JMenuItem dssExport = new JMenuItem("DSS");
		JMenuItem dssExportWithoutFlags = new JMenuItem("DSS w/o flags");
		JMenu txtMenu = new JMenu("Text");
		JMenuItem txtExport = new JMenuItem("DSS Format");
		JMenuItem txtNormalExport = new JMenuItem("Generic Format");
		JMenuItem txtTableExport = new JMenuItem("Table Format");
		txtMenu.add(txtExport);
		txtMenu.add(txtNormalExport);
		txtMenu.add(txtTableExport);
		exportDataItem.add(dssExport);
		exportDataItem.add(dssExportWithoutFlags);
		exportDataItem.add(txtMenu);
		JMenuItem reloadItem = new JMenuItem("Reload Data");
		JMenuItem quitItem = new JMenuItem("Quit Window");
		dataMenu.add(showAsGraphItem);
		dataMenu.add(showFlagsItem);
		if(!isFlagged(_ref))
		{
			dataMenu.add(addFlagsItem);
		}
		dataMenu.add(showStatsItem);
		dataMenu.add(showDetailedInfo);
		dataMenu.add(editAttrItem);
		dataMenu.add(editPathnameItem);
		dataMenu.add(exportDataItem);
		dataMenu.addSeparator();
		dataMenu.add(reloadItem);
		dataMenu.add(quitItem);
		// add listeners
		showAsGraphItem.addActionListener(new ActionListener()
		{
			public void actionPerformed(ActionEvent evt)
			{
				showGraph(evt);
			}
		});
		showFlagsItem.addActionListener(new ActionListener()
		{
			public void actionPerformed(ActionEvent evt)
			{
				toggleFlagDisplay(evt);
			}
		});
		addFlagsItem.addActionListener(new ActionListener()
		{

			@Override
			public void actionPerformed(ActionEvent e)
			{
				addFlags(e);
				dataMenu.remove(addFlagsItem);
				mbar.add(createFlagMenu());
			}
		});
		showStatsItem.addActionListener(new ActionListener()
		{
			public void actionPerformed(ActionEvent evt)
			{
				showStatsDisplay(evt);
			}
		});
		showDetailedInfo.addActionListener(new ActionListener()
		{
			public void actionPerformed(ActionEvent evt)
			{
				showDetailedInfo(evt);
			}
		});
		editAttrItem.addActionListener(new ActionListener()
		{
			public void actionPerformed(ActionEvent evt)
			{
				editAttr(evt);
			}
		});
		editPathnameItem.addActionListener(new ActionListener()
		{
			public void actionPerformed(ActionEvent evt)
			{
				editPathname(evt);
			}
		});
		dssExport.addActionListener(new ActionListener()
		{
			public void actionPerformed(ActionEvent evt)
			{
				exportDataToDSS(evt, true);
			}
		});
		dssExportWithoutFlags.addActionListener(new ActionListener()
		{
			public void actionPerformed(ActionEvent evt)
			{
				exportDataToDSS(evt, false);
			}
		});
		txtExport.addActionListener(new ActionListener()
		{
			public void actionPerformed(ActionEvent evt)
			{
				exportData(evt);
			}
		});
		txtNormalExport.addActionListener(new ActionListener()
		{
			public void actionPerformed(ActionEvent evt)
			{
				exportDataGeneric(evt);
			}
		});
		txtTableExport.addActionListener(new ActionListener()
		{
			public void actionPerformed(ActionEvent evt)
			{
				exportDataTable(evt);
			}
		});
		reloadItem.addActionListener(new ActionListener()
		{
			public void actionPerformed(ActionEvent evt)
			{
				reloadData(evt);
			}
		});
		quitItem.addActionListener(new ActionListener()
		{
			public void actionPerformed(ActionEvent evt)
			{
				quitWindow(evt);
			}
		});

		return dataMenu;
	}

	/**
	 *
	 */
	public void setFlagOverride(ActionEvent evt)
	{
		if(evt.getSource() instanceof JCheckBoxMenuItem)
		{
			JCheckBoxMenuItem mi = (JCheckBoxMenuItem) evt.getSource();
			_table.setFlagOverride(mi.isSelected());
		}
		else
		{
			return;
		}
	}

	/**
	 *
	 */
	public void markAsUS(ActionEvent evt)
	{
		_table.markAs(FlagUtils.UNSCREENED_FLAG);
	}

	/**
	 *
	 */
	public void markAsOK(ActionEvent evt)
	{
		_table.markAs(FlagUtils.OK_FLAG);
	}

	/**
	 *
	 */
	public void markAsMissing(ActionEvent evt)
	{
		_table.markAs(FlagUtils.MISSING_FLAG);
	}

	/**
	 *
	 */
	public void markAsQuestionable(ActionEvent evt)
	{
		_table.markAs(FlagUtils.QUESTIONABLE_FLAG);
	}

	/**
	 *
	 */
	public void markAsReject(ActionEvent evt)
	{
		_table.markAs(FlagUtils.REJECT_FLAG);
	}

	/**
	 * quit window
	 */
	public void reloadData(ActionEvent evt)
	{
		_ref.reloadData();
		try
		{
			_table.updateTable(0, _ref.getData().size());
		}
		catch(DataRetrievalException dre)
		{
			VistaUtils.displayException(this._table, dre);
		}
	}

	/**
	 * quit window
	 */
	public void quitWindow(ActionEvent evt)
	{
		this.setVisible(false);
		this._ref = null;
		this.dispose();
	}

	/**
	 * show graph
	 */
	public void showGraph(ActionEvent evt)
	{
		if(_graphFrame == null || (!_graphFrame.isVisible()))
		{
			GraphBuilder gb = new DefaultGraphBuilder();
			gb.addData(_ref);
			Graph[] graphs = gb.createGraphs();
			_graphFrame = new DataGraphFrame(graphs[0], _ref.getName());
		}
	}

	/**
	 * show flag
	 */
	public void toggleFlagDisplay(ActionEvent evt)
	{
		_table.toggleFlagDisplay();
	}

	public void addFlags(ActionEvent e)
	{
		_ref.getData().addFlags();
		toggleFlagDisplay(e);
	}

	/**
	 *
	 */
	public void editAttr(ActionEvent evt)
	{
		new DataSetAttrEditor(_ref.getData());
	}

	protected void editPathname(ActionEvent evt)
	{
		new PathnameEditor(this, _ref);
	}

	/**
	 * show stats
	 */
	public void showStatsDisplay(ActionEvent evt)
	{
		final JDialog dialog = new JDialog(this);
		JButton btn;
		dialog.getContentPane().setLayout(new BorderLayout());
		dialog.getContentPane().add(new StatsDisplayPanel(_ref.getData()),
				BorderLayout.CENTER);
		dialog.getContentPane()
			  .add(btn = new JButton("OK"), BorderLayout.SOUTH);
		btn.addActionListener(new ActionListener()
		{
			public void actionPerformed(ActionEvent evt)
			{
				dialog.dispose();
			}
		});
		dialog.setModal(false);
		dialog.pack();
		dialog.show();
	}

	/**
	 * show detailed info like zofset etc.
	 */
	public void showDetailedInfo(ActionEvent evt)
	{
		final JDialog dialog = new JDialog(this);
		JButton btn;
		dialog.getContentPane().setLayout(new BorderLayout());
		dialog.getContentPane().add(new DetailedInfoPanel(_ref),
				BorderLayout.CENTER);
		dialog.getContentPane()
			  .add(btn = new JButton("OK"), BorderLayout.SOUTH);
		btn.addActionListener(new ActionListener()
		{
			public void actionPerformed(ActionEvent evt)
			{
				dialog.dispose();
			}
		});
		dialog.setModal(false);
		dialog.pack();
		dialog.show();
	}

	/**
	 *
	 */
	public void exportData(ActionEvent evt)
	{
		// get filename from dialog...
		String saveFilename = VistaUtils.getFilenameFromDialog(this,
				FileDialog.SAVE, "txt", "Text Format");
		if(saveFilename == null)
		{
			return;
		}
		try
		{
			DSSUtil.writeText(new DataReference[]{_ref}, saveFilename
					+ ".dss", saveFilename);
		}
		catch(IOException ioe)
		{
			VistaUtils.displayException(this, ioe);
		}
	}

	/**
	 * export data as seen in the data table
	 */
	public void exportDataGeneric(ActionEvent evt)
	{
		// get filename from dialog...
		String saveFilename = VistaUtils.getFilenameFromDialog(this,
				FileDialog.SAVE, "txt", "DSS Text Format");
		if(saveFilename == null)
		{
			return;
		}
		SetUtils
				.write(_ref.getData(), saveFilename, _ref.getData().isFlagged());
	}

	/**
	 * export data as seen in the data table
	 */
	public void exportDataTable(ActionEvent evt)
	{
		// get filename from dialog...
		String saveFilename = VistaUtils.getFilenameFromDialog(this,
				FileDialog.SAVE, "txt", "Text Format");
		if(saveFilename == null)
		{
			return;
		}
		AppUtils.dumpToText(getTable().getModel(), saveFilename);
	}

	/**
	 * export data as seen in the data table to dss format
	 */
	public void exportDataToDSS(ActionEvent evt, boolean withFlags)
	{
		// get filename from dialog...
		try
		{
			String saveFilename = VistaUtils.getFilenameFromDialog(this,
					FileDialog.SAVE, "dss", "DSS Format");
			if(saveFilename == null)
			{
				return;
			}
			saveFilename = VistaUtils.setExtension(saveFilename, "dss");
			DSSUtil.writeData(saveFilename, _ref.getPathname().toString(),
					SetUtils.convertFlagsToValues((TimeSeries) _ref.getData()), withFlags);
		}
		catch(Exception ioe)
		{
			VistaUtils.displayException(this, ioe);
		}
	}

	/**
	 *
	 */
	public void dispose()
	{
		super.dispose();
		_table = null;
		_graphFrame = null;
		_ref = null;
		_lineNumberField = null;
	}

	public void updatePathnameLabel()
	{
		pathnameLabel.setText(_ref.getPathname().toString());
	}

	/**
	 *
	 */
	private class GotoListener implements KeyListener, AdjustmentListener
	{
		/**
		 * if enter key is pressed in goto field, goto row number
		 */
		public void keyPressed(KeyEvent evt)
		{
			if(evt.getKeyCode() != KeyEvent.VK_ENTER)
			{
				return;
			}
			JTextField field = (JTextField) evt.getSource();
			JScrollBar scrollBar = _tableScrollPane.getVerticalScrollBar();
			double value = 0.0;
			try
			{
				value = new Double(field.getText()).doubleValue();
			}
			catch(NumberFormatException nfe)
			{
				String text = field.getText();

				Pattern pattern = Pattern.compile(text,
						Pattern.CASE_INSENSITIVE);
				int cpos = scrollBar.getValue();
				int nearestValue = Math.round(cpos * _table.getRowCount()
						/ scrollBar.getMaximum());
				value = nearestValue;
				boolean gotMatch = false;
				boolean forwardSearch = true;
				int column = 0;
				while(!gotMatch)
				{
					if(nearestValue >= _table.getRowCount() && forwardSearch)
					{
						break;
					}
					if(nearestValue < 0 && !forwardSearch)
					{
						break;
					}
					Object obj = _table.getValueAt(nearestValue, column);
					if(!(obj instanceof String))
					{
						if(forwardSearch)
						{
							nearestValue++;
						}
						else
						{
							nearestValue--;
						}
						continue;
					}
					String ctxt = (String) obj;
					gotMatch = pattern.matcher(ctxt).find();
					if(forwardSearch)
					{
						nearestValue++;
					}
					else
					{
						nearestValue--;
					}
				}
				if(gotMatch)
				{
					value = nearestValue;
				}
			}
			value = (value * scrollBar.getMaximum()) / _table.getRowCount();
			scrollBar.setValue((int) value);
		}

		/**
		 * if vertical scrollbar is adjusted reflect the change in the field.
		 */
		public void adjustmentValueChanged(AdjustmentEvent evt)
		{
			// int value = (int) Math.round((1.0*evt.getValue()*
			// _dataModel.getRowCount())/
			// _tableScrollPane.getVerticalScrollBar().getMaximum());
			// _lineNumberField.setText( new Integer( value ).toString());
		}

		public void keyTyped(KeyEvent evt)
		{
		}

		public void keyReleased(KeyEvent evt)
		{
		}
	} // end of GotoListener
}
