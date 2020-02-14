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

package gov.ca.water.calgui.presentation.display;


import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Toolkit;
import java.awt.datatransfer.Clipboard;
import java.awt.datatransfer.StringSelection;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ComponentEvent;
import java.awt.event.ComponentListener;
import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.List;
import javax.swing.*;
import javax.swing.table.AbstractTableModel;
import javax.swing.table.DefaultTableCellRenderer;

import gov.ca.water.calgui.busservice.IDSSGrabber1Svc;
import gov.ca.water.calgui.busservice.impl.DSSGrabber2SvcImpl;

import hec.heclib.util.HecTime;
import hec.io.TimeSeriesContainer;

/**
 * Creates a panel tabulating results by month (column) and year (row)
 */
public class MonthlyTablePanel extends JPanel implements ActionListener, ComponentListener
{

	private static final long serialVersionUID = 1L;
	private static final String LINE_BREAK = "\n";
	private static final String CELL_BREAK = "\t";
	private final Clipboard _clipboard = Toolkit.getDefaultToolkit().getSystemClipboard();
	private final JPanel _panel;
	private final JScrollPane _scrollPane;

	public MonthlyTablePanel(String title, TimeSeriesContainer[] primaryResults, TimeSeriesContainer[] secondaryResults,
							 IDSSGrabber1Svc dssGrabber, String sName)
	{
		this(title, primaryResults, secondaryResults, dssGrabber, null, sName, false);
	}

	public MonthlyTablePanel(String title, TimeSeriesContainer[] primaryResults, TimeSeriesContainer[] secondaryResults,
							 DSSGrabber2SvcImpl dssGrabber, String sName)
	{
		this(title, primaryResults, secondaryResults, null, dssGrabber, sName, false);
	}

	public MonthlyTablePanel(String title, TimeSeriesContainer[] primaryResults, TimeSeriesContainer[] secondaryResults,
							 IDSSGrabber1Svc dssGrabber, String sName, boolean isBase)
	{
		this(title, primaryResults, secondaryResults, dssGrabber, null, sName, isBase);
	}

	public MonthlyTablePanel(String title, TimeSeriesContainer[] primaryResults, TimeSeriesContainer[] secondaryResults,
							 DSSGrabber2SvcImpl dssGrabber, String sName, boolean isBase)
	{
		this(title, primaryResults, secondaryResults, null, dssGrabber, sName, isBase);
	}

	private MonthlyTablePanel(String title, TimeSeriesContainer[] primaryResults,
							  TimeSeriesContainer[] secondaryResults,
							  IDSSGrabber1Svc dssGrabber, DSSGrabber2SvcImpl dssGrabber2, String sName, boolean isBase)
	{

		super();

		_panel = new JPanel();
		_panel.setLayout((new BoxLayout(_panel, BoxLayout.PAGE_AXIS)));

		_scrollPane = new JScrollPane(_panel);
		_scrollPane.setPreferredSize(new Dimension(750, 600));

		DecimalFormat df1 = new DecimalFormat("#.#");
		HecTime hecTime = new HecTime();

		boolean isCFS = dssGrabber == null ? "CFS".equals(dssGrabber2.getOriginalUnits())
				: "CFS".equals(dssGrabber.getOriginalUnits());

		List<String> columns = getColumns(isCFS);

		int secondaryResultsLength = 0;
		if(secondaryResults != null)
		{
			secondaryResultsLength = secondaryResults.length;
		}

		int incrementAmount = 1;
		if(isBase)
		{
			incrementAmount = primaryResults.length;
		}

		//this is the master loop
		for(int s = 0; s < primaryResults.length + secondaryResultsLength; s += incrementAmount)
		{

			TimeSeriesContainer tsc = assignTimeSeriesContainer(s, primaryResults, secondaryResults);
			String sLabel = getLabelName(s, sName, tsc, title, primaryResults.length);

			JLabel label = new JLabel();
			if(tsc != null)
			{
				label.setText(sLabel + " (" + tsc.units + ") - " + tsc.fileName);
				_panel.add(label);

				// Get starting water year for first point

				List<String> data = new ArrayList<>();
				double sum = 0;
				double[] mins = {1e10, 1e10, 1e10, 1e10, 1e10, 1e10, 1e10, 1e10, 1e10, 1e10, 1e10, 1e10, 1e10,};
				double[] maxs = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,};
				double minTAFY = 1e10;
				double maxTAFY = 0;
				double sumTAFY = 0;
				double[] avgs = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
				int[] years = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};

				// Put in empty months (if any) first
				hecTime.set(tsc.times[0]);
				int waterYear = hecTime.year() + (hecTime.month() < 10 ? 0 : 1);
				data.add(Integer.toString(waterYear));

				for(int i = 0; i < (hecTime.month() + 2) % 12; i++)
				{
					data.add("");
				}

				for(int i = 0; i < tsc.numberValues; i++)
				{
					hecTime.set(tsc.times[i]);
					int year = hecTime.year();
					waterYear = (hecTime.month() < 10) ? year : year + 1;
					int month = (hecTime.month() + 2) % 12;

					// Put in column sum and new water year at the end of each
					// row.

					if(month == 0)
					{
						if(i != 0)
						{
							if(isCFS)
							{
								double aTAFY = dssGrabber == null ? dssGrabber2.getAnnualTAF(s, waterYear - 1)
										: dssGrabber.getAnnualTAF(s, waterYear - 1);
								if(aTAFY != -1)
								{
									data.add(df1.format(aTAFY));
									minTAFY = Math.min(minTAFY, aTAFY);
									maxTAFY = Math.max(maxTAFY, aTAFY);
									sumTAFY += aTAFY;
									years[12]++;
								}
								else
								{
									data.add("");
								}
							}
							data.add(Integer.toString(waterYear));
						}
					}

					// Aggregate

					years[month]++;
					sum = sum + tsc.values[i];
					avgs[month] = avgs[month] + tsc.values[i];
					mins[month] = Math.min(mins[month], tsc.values[i]);
					maxs[month] = Math.max(maxs[month], tsc.values[i]);
					data.add(df1.format(tsc.values[i]));
				}

				// Fill in end

				hecTime.set(tsc.times[tsc.numberValues - 1]);
				for(int i = 1 + (hecTime.month() + 2) % 12; i < 12; i++)
				{
					data.add("");
				}
				if(isCFS)
				{
					double aTAFY = dssGrabber == null ? dssGrabber2.getAnnualTAF(s, waterYear)
							: dssGrabber.getAnnualTAF(s, waterYear);
					if(aTAFY != -1)
					{
						data.add(df1.format(aTAFY));

						minTAFY = Math.min(minTAFY, aTAFY);
						maxTAFY = Math.max(maxTAFY, aTAFY);
						sumTAFY += aTAFY;
						years[12]++;
					}
					else
					{
						data.add("");
					}
				}

				// Column statistics

				data.add("Min");
				for(int i = 0; i < 12; i++)
				{
					data.add(years[i] > 0 ? df1.format(mins[i]) : "");
				}
				if(isCFS)
				{
					data.add(df1.format(minTAFY));
				}

				data.add("Max");
				for(int i = 0; i < 12; i++)
				{
					data.add(years[i] > 0 ? df1.format(maxs[i]) : "");
				}
				if(isCFS)
				{
					data.add(df1.format(maxTAFY));
				}

				data.add("Avg");
				for(int i = 0; i < 12; i++)
				{
					data.add(years[i] > 0 ? df1.format(avgs[i] / years[i]) : "");
				}
				if(isCFS)
				{
					data.add(df1.format(sumTAFY / years[12]));
				}

				SimpleTableModel2 model = new SimpleTableModel2(data, columns);
				JTable table = new JTable(model);
				table.setAutoResizeMode(JTable.AUTO_RESIZE_ALL_COLUMNS);
				for(int c = 0; c < table.getColumnCount(); c++)
				{
					table.getColumnModel().getColumn(c).setPreferredWidth((c == 0) ? 50 : 30);
				}

				table.setCellSelectionEnabled(true);
				DefaultTableCellRenderer renderer = (DefaultTableCellRenderer) table.getDefaultRenderer(String.class);
				renderer.setHorizontalAlignment(JLabel.RIGHT);

				addComponentListener(this);
				_panel.add(table.getTableHeader(), BorderLayout.NORTH);
				_panel.add(table);
			}
		}

		Box box = Box.createVerticalBox();

		box.add(_scrollPane);
		JButton copy = new JButton("Copy to Clipboard");
		copy.setAlignmentX(LEFT_ALIGNMENT);
		copy.addActionListener(this);
		box.add(copy);
		add(box);
	}

	private TimeSeriesContainer assignTimeSeriesContainer(int index, TimeSeriesContainer[] primaryResults,
														  TimeSeriesContainer[] secondaryResults)
	{
		TimeSeriesContainer tsc;
		if(index < primaryResults.length)
		{
			tsc = primaryResults[index];
		}
		else
		{
			tsc = secondaryResults[index - primaryResults.length];
		}
		return tsc;
	}

	private String getLabelName(int index, String name, TimeSeriesContainer tsc, String title, int primaryResultsLength)
	{
		String sLabel;
		if(index < primaryResultsLength)
		{
			sLabel = title;
		}
		else
		{
			if("".equals(name) && tsc != null)
			{
				sLabel = tsc.fullName;
			}
			else
			{
				sLabel = name;
			}
		}

		return sLabel;
	}

	private List<String> getColumns(boolean isCFS)
	{
		List<String> columns = new ArrayList<>();
		columns.add("WY");
		columns.add("Oct");
		columns.add("Nov");
		columns.add("Dec");
		columns.add("Jan");
		columns.add("Feb");
		columns.add("Mar");
		columns.add("Apr");
		columns.add("May");
		columns.add("Jun");
		columns.add("Jul");
		columns.add("Aug");
		columns.add("Sep");

		if(isCFS)
		{
			columns.add("Ann (TAF)");
		}

		return columns;
	}


	@Override
	public void actionPerformed(ActionEvent e)
	{
		// TODO Auto-generated method stub
		JComponent component = (JComponent) e.getSource();
		if(component instanceof JButton)
		{
			JButton btn = (JButton) component;
			String cName = btn.getText();
			if(cName != null)
			{
				if(cName.startsWith("Copy"))
				{
					StringBuffer excelStr = new StringBuffer();

					Component[] components = _panel.getComponents();

					for(int i = 0; i < components.length; i++)
					{
						if(components[i] instanceof JTable)
						{
							JTable table = (JTable) components[i];
							int numCols = table.getColumnCount();
							int numRows = table.getRowCount();

							// get column headers
							for(int k = 0; k < numCols; k++)
							{
								excelStr.append(table.getColumnModel().getColumn(k).getHeaderValue());
								if(k < numCols - 1)
								{
									excelStr.append(CELL_BREAK);
								}
							}
							excelStr.append(LINE_BREAK);

							// get cell values
							for(int j = 0; j < numRows; j++)
							{
								for(int k = 0; k < numCols; k++)
								{
									excelStr.append(escape(table.getValueAt(j, k)));
									if(k < numCols - 1)
									{
										excelStr.append(CELL_BREAK);
									}
								}
								excelStr.append(LINE_BREAK);
							}

							StringSelection sel = new StringSelection(excelStr.toString());
							_clipboard.setContents(sel, sel);
						}
						else if(components[i] instanceof JLabel)
						{
							JLabel label = (JLabel) components[i];
							excelStr.append(label.getText());
							excelStr.append(LINE_BREAK);
						}
					}
				}
			}
		}
	}

	private String escape(Object cell)
	{
		return cell.toString().replace(LINE_BREAK, " ").replace(CELL_BREAK, " ");
	}

	@Override
	public void componentHidden(ComponentEvent e)
	{
		// TODO Auto-generated method stub

	}

	@Override
	public void componentMoved(ComponentEvent e)
	{
		// TODO Auto-generated method stub

	}

	@Override
	public void componentResized(ComponentEvent e)
	{
		// TODO Auto-generated method stub

		Dimension dim = super.getSize();
		int width = (int) (dim.width * 0.99);
		int height = (int) (dim.height * 0.90);
		_scrollPane.setPreferredSize(new Dimension(width, height));
		_scrollPane.revalidate();
	}

	@Override
	public void componentShown(ComponentEvent e)
	{
		// TODO Auto-generated method stub

	}

	class SimpleTableModel2 extends AbstractTableModel
	{
		/**
		 *
		 */
		private static final long serialVersionUID = 1L;
		protected List<String> data;
		protected List<String> columnNames;

		public SimpleTableModel2(List<String> datain, List<String> columnin)
		{
			data = datain;
			columnNames = columnin;
		}

		@Override
		public int getRowCount()
		{
			return data.size() / getColumnCount();
		}

		@Override
		public int getColumnCount()
		{
			return columnNames.size();
		}

		@Override
		public String getColumnName(int columnIndex)
		{
			String colName = "";
			if(columnIndex <= getColumnCount())
			{
				colName = columnNames.get(columnIndex);
			}
			return colName;
		}

		@Override
		public Class getColumnClass(int columnIndex)
		{
			return String.class;
		}

		@Override
		public boolean isCellEditable(int rowIndex, int columnIndex)
		{
			return false;
		}

		@Override
		public Object getValueAt(int rowIndex, int columnIndex)
		{
			return data.get((rowIndex * getColumnCount()) + columnIndex);
		}

		@Override
		public void setValueAt(Object aValue, int rowIndex, int columnIndex)
		{
			data.set(((rowIndex * getColumnCount()) + columnIndex), (String) aValue);
		}
	}
}