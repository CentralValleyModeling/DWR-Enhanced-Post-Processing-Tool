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

import calsim.app.MultipleTimeSeries;
import gov.ca.water.calgui.busservice.impl.DSSGrabber2SvcImpl;

import hec.heclib.util.HecTime;
import hec.io.TimeSeriesContainer;

/**
 * Monthly table panel for mts
 *
 * @author tslawecki
 */
public class MonthlyTablePanel2 extends JPanel implements ActionListener, ComponentListener
{

	private static final long serialVersionUID = 1L;
	final String LINE_BREAK = "\n";
	final String CELL_BREAK = "\t";
	final Clipboard CLIPBOARD = Toolkit.getDefaultToolkit().getSystemClipboard();
	JPanel panel;
	JScrollPane scrollPane;

	public MonthlyTablePanel2(String title, TimeSeriesContainer[][] mtscs, DSSGrabber2SvcImpl dss_Grabber2,
							  String sName, boolean isBase, MultipleTimeSeries mts)
	{

		super();

		panel = new JPanel();
		panel.setLayout((new BoxLayout(panel, BoxLayout.PAGE_AXIS)));

		scrollPane = new JScrollPane(panel);
		scrollPane.setPreferredSize(new Dimension(750, 600));

		DecimalFormat df1 = new DecimalFormat("#.#");
		HecTime ht = new HecTime();
		// Count forward to right month - hardcoded to 10 for now
		// TODO - match to input
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
		boolean isCFS = dss_Grabber2.getOriginalUnits().equals("CFS");
		if(isCFS)
		{
			columns.addElement("Ann (TAF)");
		}

		for(int mtsI = 0; mtsI < mtscs.length; mtsI++)
		{

			TimeSeriesContainer[] tscs = mtscs[mtsI];
			dss_Grabber2.calcTAFforCFS(tscs, null);

			for(int s = 0; s < tscs.length; s++)
			{

				TimeSeriesContainer tsc;
				tsc = tscs[s];
				if(tsc != null && tsc.numberValues > 0)
				{
					String sLabel = (mts.getDTSNameAt(mtsI).equals("")
							? mts.getBPartAt(mtsI) + "/" + mts.getCPartAt(mtsI) : mts.getDTSNameAt(mtsI));

					JLabel label = new JLabel();
					label.setText(sLabel + " (" + tsc.units + ") - " + tsc.fileName);
					panel.add(label);

					int first = 0;
					ht.set(tsc.times[first]);
					while(ht.month() != 10)
					{
						first++;
						ht.set(tsc.times[first]);
					}

					Vector<String> data = new Vector<String>();
					double sum = 0;
					double[] mins, maxs, avgs;
					mins = new double[13];
					maxs = new double[13];
					avgs = new double[13];
					double minTAFY = 1e10;
					double maxTAFY = 0;
					double sumTAFY = 0;
					int years = 0;
					int wy = 0;
					for(int i = first; i < tsc.numberValues; i++)
					{
						ht.set(tsc.times[i]);
						int y = ht.year();
						int m = ht.month();
						wy = (m < 10) ? y : y + 1;
						if((i - first) % 12 == 0)
						{
							if(i != first && isCFS)
							{
								double aTAFY = dss_Grabber2.getAnnualTAF(s, wy - 1);
								data.addElement(df1.format(aTAFY));
								minTAFY = Math.min(minTAFY, aTAFY);
								maxTAFY = Math.max(maxTAFY, aTAFY);
								sumTAFY += aTAFY;

							}
							data.addElement(Integer.toString(wy));
							years++;
						}
						sum = sum + tsc.values[i];
						m = (m + 2) % 12;
						avgs[m] = avgs[m] + tsc.values[i];
						mins[m] = (years == 1) ? tsc.values[i] : Math.min(mins[m], tsc.values[i]);
						maxs[m] = (years == 1) ? tsc.values[i] : Math.max(maxs[m], tsc.values[i]);
						data.addElement(df1.format(tsc.values[i]));
					}
					if(isCFS)
					{
						double aTAFY = dss_Grabber2.getAnnualTAF(s, wy - 1);
						data.addElement(df1.format(aTAFY));
						minTAFY = Math.min(minTAFY, aTAFY);
						maxTAFY = Math.max(maxTAFY, aTAFY);
						sumTAFY += aTAFY;
					}

					data.addElement("Min");
					for(int i = 0; i < 12; i++)
					{
						data.addElement(df1.format(mins[i]));
					}
					if(isCFS)
					{
						data.addElement(df1.format(minTAFY));
					}

					data.addElement("Max");
					for(int i = 0; i < 12; i++)
					{
						data.addElement(df1.format(maxs[i]));
					}
					if(isCFS)
					{
						data.addElement(df1.format(maxTAFY));
					}
					data.addElement("Avg");
					for(int i = 0; i < 12; i++)
					{
						data.addElement(df1.format(avgs[i] / years));
					}
					if(isCFS)
					{
						data.addElement(df1.format(sumTAFY / years));
					}

					SimpleTableModel2 model = new SimpleTableModel2(data, columns);
					JTable table = new JTable(model);
					table.setAutoResizeMode(JTable.AUTO_RESIZE_ALL_COLUMNS);
					for(int c = 0; c < table.getColumnCount(); c++)
					{
						table.getColumnModel().getColumn(c).setPreferredWidth((c == 0) ? 50 : 30);
					}

					table.setCellSelectionEnabled(true);
					DefaultTableCellRenderer renderer = (DefaultTableCellRenderer) table
							.getDefaultRenderer(String.class);
					renderer.setHorizontalAlignment(JLabel.RIGHT);

					addComponentListener(this);
					panel.add(table.getTableHeader(), BorderLayout.NORTH);
					panel.add(table);
				}
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
