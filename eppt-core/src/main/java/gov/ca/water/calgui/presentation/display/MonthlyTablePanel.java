/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */

package gov.ca.water.calgui.presentation.display;

import java.awt.*;
import java.awt.datatransfer.Clipboard;
import java.awt.datatransfer.StringSelection;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ComponentEvent;
import java.awt.event.ComponentListener;
import java.text.DecimalFormat;
import java.util.Vector;
import javax.swing.*;
import javax.swing.table.AbstractTableModel;
import javax.swing.table.DefaultTableCellRenderer;

import gov.ca.water.calgui.bus_service.IDSSGrabber1Svc;
import gov.ca.water.calgui.bus_service.impl.DSSGrabber2SvcImpl;

import hec.heclib.util.HecTime;
import hec.io.TimeSeriesContainer;

/**
 * Creates a panel tabulating results by month (column) and year (row)
 */
public class MonthlyTablePanel extends JPanel implements ActionListener, ComponentListener
{

	private static final long serialVersionUID = 1L;
	final String LINE_BREAK = "\n";
	final String CELL_BREAK = "\t";
	final Clipboard CLIPBOARD = Toolkit.getDefaultToolkit().getSystemClipboard();
	JPanel panel;
	JScrollPane scrollPane;

	public MonthlyTablePanel(String title, TimeSeriesContainer[] tscs, TimeSeriesContainer[] stscs,
							 IDSSGrabber1Svc dss_Grabber, String sName)
	{
		this(title, tscs, stscs, dss_Grabber, null, sName, false);

	}

	public MonthlyTablePanel(String title, TimeSeriesContainer[] tscs, TimeSeriesContainer[] stscs,
							 DSSGrabber2SvcImpl dss_Grabber, String sName)
	{
		this(title, tscs, stscs, null, dss_Grabber, sName, false);

	}

	public MonthlyTablePanel(String title, TimeSeriesContainer[] tscs, TimeSeriesContainer[] stscs,
							 IDSSGrabber1Svc dss_Grabber, String sName, boolean isBase)
	{
		this(title, tscs, stscs, dss_Grabber, null, sName, isBase);
	}

	public MonthlyTablePanel(String title, TimeSeriesContainer[] tscs, TimeSeriesContainer[] stscs,
							 DSSGrabber2SvcImpl dss_Grabber, String sName, boolean isBase)
	{
		this(title, tscs, stscs, null, dss_Grabber, sName, isBase);
	}

	public MonthlyTablePanel(String title, TimeSeriesContainer[] tscs, TimeSeriesContainer[] stscs,
							 IDSSGrabber1Svc dss_Grabber, DSSGrabber2SvcImpl dss_Grabber2, String sName, boolean isBase)
	{

		super();

		panel = new JPanel();
		panel.setLayout((new BoxLayout(panel, BoxLayout.PAGE_AXIS)));

		scrollPane = new JScrollPane(panel);
		scrollPane.setPreferredSize(new Dimension(750, 600));

		DecimalFormat df1 = new DecimalFormat("#.#");
		HecTime ht = new HecTime();

		Vector<String> columns = new Vector<String>();
		columns.addElement("WY");
		columns.addElement("Oct");
		columns.addElement("Nov");
		columns.addElement("Dec");
		columns.addElement("Jan");
		columns.addElement("Feb");
		columns.addElement("Mar");
		columns.addElement("Apr");
		columns.addElement("May");
		columns.addElement("Jun");
		columns.addElement("Jul");
		columns.addElement("Aug");
		columns.addElement("Sep");
		boolean isCFS = dss_Grabber == null ? dss_Grabber2.getOriginalUnits().equals("CFS")
				: dss_Grabber.getOriginalUnits().equals("CFS");
		if(isCFS)
		{
			columns.addElement("Ann (TAF)");
		}

		for(int s = 0; s < tscs.length + (stscs == null ? 0 : stscs.length); s += (isBase ? tscs.length : 1))
		{

			String sLabel = sName;
			TimeSeriesContainer tsc;
			if(s < tscs.length)
			{
				tsc = tscs[s];
				sLabel = title;
			}
			else
			{
				tsc = stscs[s - tscs.length];
				if(sName.equals(""))
				{
					String[] parts = tsc.fullName.split("/");
					sLabel = parts[2] + "/" + parts[3];
				}
				else
				{
					sLabel = sName;
				}
			}
			JLabel label = new JLabel();
			if(tsc != null)
			{
				label.setText(sLabel + " (" + tsc.units + ") - " + tsc.fileName);
				panel.add(label);

				// int first = 0;
				// while (ht.month() != 10) {
				// first++;
				// ht.set(tsc.times[first]);
				// }

				// Get starting water year for first point

				Vector<String> data = new Vector<String>();
				double sum = 0;
				double[] mins = {1e10, 1e10, 1e10, 1e10, 1e10, 1e10, 1e10, 1e10, 1e10, 1e10, 1e10, 1e10, 1e10,};
				double[] maxs = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,};
				double minTAFY = 1e10;
				double maxTAFY = 0;
				double sumTAFY = 0;
				double[] avgs = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
				int[] years = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};

				// Put in empty months (if any) first
				ht.set(tsc.times[0]);
				int wy = ht.year() + (ht.month() < 10 ? 0 : 1);
				data.addElement(Integer.toString(wy));
				for(int i = 0; i < (ht.month() + 2) % 12; i++)
				{
					data.addElement("");
				}

				for(int i = 0; i < tsc.numberValues; i++)
				{
					ht.set(tsc.times[i]);
					int y = ht.year();
					wy = (ht.month() < 10) ? y : y + 1;
					int m = (ht.month() + 2) % 12;

					// Put in column sum and new water year at the end of each
					// row.

					if(m == 0)
					{
						if(i != 0)
						{
							if(isCFS)
							{
								double aTAFY = dss_Grabber == null ? dss_Grabber2.getAnnualTAF(s, wy - 1)
										: dss_Grabber.getAnnualTAF(s, wy - 1);
								if(aTAFY != -1)
								{
									data.addElement(df1.format(aTAFY));
									minTAFY = Math.min(minTAFY, aTAFY);
									maxTAFY = Math.max(maxTAFY, aTAFY);
									sumTAFY += aTAFY;
									years[12]++;
								}
								else
								{
									data.addElement("");
								}
							}
							data.addElement(Integer.toString(wy));
						}
					}

					// Aggregate

					years[m]++;
					sum = sum + tsc.values[i];
					avgs[m] = avgs[m] + tsc.values[i];
					mins[m] = Math.min(mins[m], tsc.values[i]);
					maxs[m] = Math.max(maxs[m], tsc.values[i]);
					data.addElement(df1.format(tsc.values[i]));
				}

				// Fill in end

				ht.set(tsc.times[tsc.numberValues - 1]);
				for(int i = 1 + (ht.month() + 2) % 12; i < 12; i++)
				{
					data.addElement("");
				}
				if(isCFS)
				{
					double aTAFY = dss_Grabber == null ? dss_Grabber2.getAnnualTAF(s, wy)
							: dss_Grabber.getAnnualTAF(s, wy);
					if(aTAFY != -1)
					{
						data.addElement(df1.format(aTAFY));

						minTAFY = Math.min(minTAFY, aTAFY);
						maxTAFY = Math.max(maxTAFY, aTAFY);
						sumTAFY += aTAFY;
						years[12]++;
					}
					else
					{
						data.addElement("");
					}
				}

				// Column statistics

				data.addElement("Min");
				for(int i = 0; i < 12; i++)
				{
					data.addElement(years[i] > 0 ? df1.format(mins[i]) : "");
				}
				if(isCFS)
				{
					data.addElement(df1.format(minTAFY));
				}

				data.addElement("Max");
				for(int i = 0; i < 12; i++)
				{
					data.addElement(years[i] > 0 ? df1.format(maxs[i]) : "");
				}
				if(isCFS)
				{
					data.addElement(df1.format(maxTAFY));
				}

				data.addElement("Avg");
				for(int i = 0; i < 12; i++)
				{
					data.addElement(years[i] > 0 ? df1.format(avgs[i] / years[i]) : "");
				}
				if(isCFS)
				{
					data.addElement(df1.format(sumTAFY / years[12]));
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
				panel.add(table.getTableHeader(), BorderLayout.NORTH);
				panel.add(table);
			}
		}

		Box box = Box.createVerticalBox();

		box.add(scrollPane);
		JButton copy = new JButton("Copy to Clipboard");
		copy.setAlignmentX(LEFT_ALIGNMENT);
		copy.addActionListener(this);
		box.add(copy);
		add(box);
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

					Component[] components = panel.getComponents();

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
							CLIPBOARD.setContents(sel, sel);
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
		scrollPane.setPreferredSize(new Dimension(width, height));
		scrollPane.revalidate();
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
		protected Vector<String> data;
		protected Vector<String> columnNames;

		public SimpleTableModel2(Vector<String> datain, Vector<String> columnin)
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
				colName = columnNames.elementAt(columnIndex);
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
			return data.elementAt((rowIndex * getColumnCount()) + columnIndex);
		}

		@Override
		public void setValueAt(Object aValue, int rowIndex, int columnIndex)
		{
			data.setElementAt((String) aValue, ((rowIndex * getColumnCount()) + columnIndex));
			// return;
		}
	}
}
