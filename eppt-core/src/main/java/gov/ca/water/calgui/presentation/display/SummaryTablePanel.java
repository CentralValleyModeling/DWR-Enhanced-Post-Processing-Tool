/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
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
import java.util.Arrays;
import java.util.Vector;
import javax.swing.*;
import javax.swing.table.AbstractTableModel;
import javax.swing.table.DefaultTableCellRenderer;
import javax.swing.table.TableColumn;

import gov.ca.water.calgui.bus_service.IDSSGrabber1Svc;
import gov.ca.water.calgui.bus_service.impl.DSSGrabber2SvcImpl;

import hec.heclib.util.HecTime;
import hec.io.TimeSeriesContainer;

//import com.limno.calgui.table.ColumnGroup;
//import com.limno.calgui.table.GroupableTableHeader;

public class SummaryTablePanel extends JPanel implements ActionListener, ComponentListener
{

	/**
	 *
	 */
	private static final long serialVersionUID = 1707702265756648942L;
	private static int[][] ylt = {{1920, 2, 2, 1, 1, 0, 3, 2, 0,}, {1921, 2, 2, 1, 1, 0, 3, 2, 0,},
			{1922, 2, 1, 1, 1, 0, 4, 2, 0,}, {1923, 3, 2, 3, 1, 0, 4, 3, 0,}, {1924, 5, 5, 4, 2, 1, 5, 6, 0,},
			{1925, 4, 3, 1, 1, 0, 2, 5, 0,}, {1926, 4, 4, 3, 1, 0, 4, 5, 0,}, {1927, 1, 2, 1, 1, 0, 2, 1, 0,},
			{1928, 2, 3, 1, 1, 0, 3, 2, 0,}, {1929, 5, 5, 3, 1, 0, 5, 6, 1,}, {1930, 4, 5, 2, 1, 0, 4, 5, 1,},
			{1931, 5, 5, 4, 2, 1, 5, 6, 1,}, {1932, 4, 2, 4, 1, 0, 4, 5, 1,}, {1933, 5, 4, 4, 1, 0, 4, 6, 1,},
			{1934, 5, 5, 4, 2, 1, 5, 6, 1,}, {1935, 3, 2, 1, 1, 0, 4, 3, 0,}, {1936, 3, 2, 1, 1, 0, 3, 3, 0,},
			{1937, 3, 1, 2, 1, 0, 4, 3, 0,}, {1938, 1, 1, 1, 1, 0, 1, 1, 0,}, {1939, 4, 4, 3, 2, 0, 5, 5, 0,},
			{1940, 2, 2, 1, 1, 0, 2, 2, 0,}, {1941, 1, 1, 1, 1, 0, 1, 1, 0,}, {1942, 1, 1, 1, 1, 0, 2, 1, 0,},
			{1943, 1, 1, 1, 1, 0, 3, 1, 0,}, {1944, 4, 3, 3, 1, 0, 5, 5, 0,}, {1945, 3, 2, 1, 1, 0, 3, 3, 0,},
			{1946, 3, 2, 1, 1, 0, 2, 3, 0,}, {1947, 4, 4, 3, 1, 0, 4, 5, 0,}, {1948, 3, 3, 1, 1, 0, 3, 3, 0,},
			{1949, 4, 3, 2, 1, 0, 3, 5, 0,}, {1950, 3, 3, 2, 1, 0, 4, 3, 0,}, {1951, 2, 2, 1, 1, 0, 2, 2, 0,},
			{1952, 1, 1, 1, 1, 0, 2, 1, 0,}, {1953, 1, 3, 1, 1, 0, 2, 1, 0,}, {1954, 2, 3, 1, 1, 0, 2, 2, 0,},
			{1955, 4, 4, 2, 1, 0, 4, 5, 0,}, {1956, 1, 1, 1, 1, 0, 1, 1, 0,}, {1957, 2, 3, 1, 1, 0, 3, 2, 0,},
			{1958, 1, 1, 1, 1, 0, 1, 1, 0,}, {1959, 3, 4, 1, 1, 0, 3, 3, 0,}, {1960, 4, 5, 1, 1, 0, 3, 5, 0,},
			{1961, 4, 5, 1, 1, 0, 3, 5, 0,}, {1962, 3, 3, 1, 1, 0, 3, 3, 0,}, {1963, 1, 2, 1, 1, 0, 2, 1, 0,},
			{1964, 4, 4, 3, 1, 0, 4, 5, 0,}, {1965, 1, 1, 1, 1, 0, 2, 1, 0,}, {1966, 3, 3, 1, 1, 0, 3, 3, 0,},
			{1967, 1, 1, 1, 1, 0, 2, 1, 0,}, {1968, 3, 4, 1, 1, 0, 3, 3, 0,}, {1969, 1, 1, 1, 1, 0, 1, 1, 0,},
			{1970, 1, 2, 1, 1, 0, 2, 1, 0,}, {1971, 1, 3, 1, 1, 0, 2, 1, 0,}, {1972, 3, 4, 1, 1, 0, 3, 3, 0,},
			{1973, 2, 2, 1, 1, 0, 2, 2, 0,}, {1974, 1, 1, 1, 1, 0, 1, 1, 0,}, {1975, 1, 1, 1, 1, 0, 2, 1, 0,},
			{1976, 5, 5, 3, 2, 0, 4, 6, 2,}, {1977, 5, 5, 4, 2, 1, 5, 7, 2,}, {1978, 2, 1, 1, 1, 0, 1, 2, 0,},
			{1979, 3, 2, 2, 1, 0, 4, 3, 0,}, {1980, 2, 1, 1, 1, 0, 2, 2, 0,}, {1981, 4, 4, 2, 2, 0, 4, 5, 0,},
			{1982, 1, 1, 1, 1, 0, 1, 1, 0,}, {1983, 1, 1, 1, 1, 0, 1, 1, 0,}, {1984, 1, 2, 1, 1, 0, 2, 1, 0,},
			{1985, 4, 4, 3, 1, 0, 4, 5, 0,}, {1986, 1, 1, 1, 1, 0, 2, 1, 0,}, {1987, 4, 5, 3, 2, 0, 4, 5, 3,},
			{1988, 5, 5, 3, 2, 1, 4, 6, 3,}, {1989, 4, 5, 1, 1, 0, 3, 5, 3,}, {1990, 5, 5, 3, 2, 0, 4, 6, 3,},
			{1991, 5, 5, 4, 1, 1, 5, 6, 3,}, {1992, 5, 5, 4, 2, 0, 4, 6, 3,}, {1993, 2, 1, 1, 1, 0, 2, 2, 0,},
			{1994, 5, 5, 4, 2, 0, 5, 6, 0,}, {1995, 1, 1, 1, 1, 0, 1, 0, 0,}, {1996, 1, 1, 1, 1, 0, 2, 0, 0,},
			{1997, 1, 1, 1, 1, 0, 2, 0, 0,}, {1998, 1, 1, 1, 1, 0, 1, 0, 0,}, {1999, 1, 2, 1, 1, 0, 2, 0, 0,},
			{2000, 2, 2, 1, 1, 0, 2, 0, 0,}, {2001, 4, 4, 1, 2, 0, 4, 0, 0,}, {2002, 4, 4, 1, 1, 0, 3, 0, 0,},
			{2003, 2, 3, 1, 1, 0, 2, 0, 0,}};
	final String LINE_BREAK = "\n";
	final String CELL_BREAK = "\t";
	final Clipboard CLIPBOARD = Toolkit.getDefaultToolkit().getSystemClipboard();
	int[][][] n;
	double[][][] _x;
	double[][][] _xx;
	double[][][] _avg;
	double[][][] _sdev;
	double[][][] _min;
	double[][][] _max;
	double[][][] _med;
	double[][][][] _medx;
	JPanel panel;

	// Year, Sac403030, SJR, SHASTA, ?, Feather, ?, ?, dry
	JScrollPane scrollPane;
	Vector<String> columns;

	public SummaryTablePanel(String title, TimeSeriesContainer[] tscs, TimeSeriesContainer[] stscs, String tagString,
							 String sName, IDSSGrabber1Svc dss_Grabber)
	{

		this(title, tscs, stscs, tagString, sName, dss_Grabber, null, false);

	}

	public SummaryTablePanel(String title, TimeSeriesContainer[] tscs, TimeSeriesContainer[] stscs, String tagString,
							 String sName, DSSGrabber2SvcImpl dss_Grabber)
	{

		this(title, tscs, stscs, tagString, sName, null, dss_Grabber, false);

	}

	public SummaryTablePanel(String title, TimeSeriesContainer[] tscs, TimeSeriesContainer[] stscs, String tagString,
							 String sName, IDSSGrabber1Svc dss_Grabber, boolean isBase)
	{

		this(title, tscs, stscs, tagString, sName, dss_Grabber, null, isBase);

	}

	public SummaryTablePanel(String title, TimeSeriesContainer[] tscs, TimeSeriesContainer[] stscs, String tagString,
							 String sName, DSSGrabber2SvcImpl dss_Grabber, boolean isBase)
	{

		this(title, tscs, stscs, tagString, sName, null, dss_Grabber, isBase);

	}

	public SummaryTablePanel(String title, TimeSeriesContainer[] tscs, TimeSeriesContainer[] stscs, String tagString,
							 String sName, IDSSGrabber1Svc dss_Grabber, DSSGrabber2SvcImpl dss_Grabber2, boolean isBase)
	{

		super();

		panel = new JPanel();
		panel.setLayout((new BoxLayout(panel, BoxLayout.PAGE_AXIS)));

		int nDatasets = (isBase ? 1 : tscs.length);
		if(stscs != null)
		{
			nDatasets = nDatasets + (isBase ? 1 : stscs.length);
		}

		Vector<String>[] data = new Vector[nDatasets];

		columns = new Vector<String>(15);
		columns.addElement("Year Group");
		columns.addElement("Statistic");
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
		columns.addElement("All (TAF)");

		boolean isCFS = dss_Grabber == null ? dss_Grabber2.getOriginalUnits().equals("CFS")
				: dss_Grabber.getOriginalUnits().equals("CFS");

		// loop over all Primary datasets

		for(int t = 0; t < nDatasets; t++)
		{

			TimeSeriesContainer tsc;
			if(isBase)
			{
				if(t == 0)
				{
					tsc = tscs[0];
				}
				else
				{
					tsc = stscs[0];
				}
			}
			else
			{
				if(t < tscs.length)
				{
					tsc = tscs[t];
				}
				else
				{
					tsc = stscs[t - tscs.length];
				}
			}

			if(tsc != null)
			{

				// Initialize accumulators

				n = new int[6][6][14];
				_x = new double[6][6][14];
				_xx = new double[6][6][14];
				_min = new double[6][6][14];
				_max = new double[6][6][14];
				for(int i1 = 0; i1 < 6; i1++)
				{
					for(int i2 = 0; i2 < 6; i2++)
					{
						for(int i3 = 0; i3 < 14; i3++)
						{

							n[i1][i2][i3] = 0;
							_x[i1][i2][i3] = 0;
							_xx[i1][i2][i3] = 0;
							_min[i1][i2][i3] = 1e20;
							_max[i1][i2][i3] = -1e20;
						}
					}
				}

				_med = new double[6][6][14];
				_medx = new double[6][6][14][tsc.numberValues];

				// Loop through timeseries

				HecTime ht = new HecTime();
				int lastWY = -9999;

				for(int i = 0; i < tsc.numberValues; i++)
				{

					ht.set(tsc.times[i]);
					int y = ht.year();
					int m = ht.month();
					int wy = (m < 10) ? y : y + 1;
					boolean isNewWY = (wy != lastWY);
					lastWY = wy;

					// int ySac403030 = (m < 2) ? y - 1 : y;
					int ySac403030 = wy;

					int ySHASTAindex = (m < 3) ? y - 1 : y;
					int yFEATHERindex = (m < 2) ? y - 1 : y;
					int ySJRindex = (m < 2) ? y - 1 : y;

					update(0, 0, tsc.values[i], m);
					update(1, ylt[ySac403030 - 1920][1], tsc.values[i], m);
					update(2, ylt[ySHASTAindex - 1920][3], tsc.values[i], m);
					update(3, ylt[yFEATHERindex - 1920][5], tsc.values[i], m);
					update(4, ylt[ySJRindex - 1920][2], tsc.values[i], m);

					if(ylt[wy - 1920][8] != 0)
					{
						update(5, ylt[wy - 1920][8], tsc.values[i], m);
						update(5, 0, tsc.values[i], m);
					}
					if(isNewWY && isCFS)
					{
						// Calculate values based on annual totals
						double value;
						if(title.contains("Difference"))
						{
							value = dss_Grabber == null ? dss_Grabber2.getAnnualTAFDiff(t, wy)
									: dss_Grabber.getAnnualTAFDiff(t, wy);
						}
						else
						{
							value = dss_Grabber == null ? dss_Grabber2.getAnnualTAF(t, wy)
									: dss_Grabber.getAnnualTAF(t, wy);
						}

						update2(0, 0, value, m);
						update2(1, ylt[ySac403030 - 1920][1], value, m);
						update2(2, ylt[ySHASTAindex - 1920][3], value, m);
						update2(3, ylt[yFEATHERindex - 1920][5], value, m);
						update2(4, ylt[ySJRindex - 1920][2], value, m);

						if(ylt[wy - 1920][8] != 0)
						{
							update2(5, ylt[wy - 1920][8], value, m);
							update2(5, 0, value, m);
						}

					}

				}

				_avg = new double[6][6][14];
				_sdev = new double[6][6][14];
				data[t] = new Vector<String>();
				String[] leftPart = {"All", "Sac 40-30-30", "Shasta", "Feather", "SJR", "Dry"};
				String[] rightPartsclimate = {"", "Wet", "Above", "Normal", "Dry", "Extreme"};
				String[] rightPartsclimate2 = {"", "Normal", "Below Normal", "Dry", "Critical", ""};

				String[] rightPartsDry = {"All dry periods", "1928-1934", "1976-1977", "1986-1992", "UNKNOWN 4",
						"UNKNOWN 5"};
				DecimalFormat df1 = new DecimalFormat("#.#");

				// Calculate results
				for(int i1 = 0; i1 < 6; i1++)
				{
					for(int i2 = 0; i2 < 6; i2++)
					{
						for(int i3 = 0; i3 < 14; i3++)

						{
							if((((i1 == 0) && tagString.contains("All years") && (i2 == 0))
									|| ((i1 == 1) && tagString.contains("40-30-30"))
									|| ((i1 == 2) && tagString.contains("Shasta"))
									|| ((i1 == 3) && tagString.contains("Feather"))
									|| ((i1 == 4) && tagString.contains("SJR Index"))
									|| ((i1 == 5) && tagString.contains("All dry"))
									|| ((i1 == 5) && (i2 == 1) && tagString.contains("1934"))
									|| ((i1 == 5) && (i2 == 2) && tagString.contains("1977"))
									|| ((i1 == 5) && (i2 == 3) && tagString.contains("1992"))) && (n[i1][i2][i3] > 0))
							{

								_avg[i1][i2][i3] = _x[i1][i2][i3] / n[i1][i2][i3];
								_sdev[i1][i2][i3] = Math.sqrt(
										Math.abs(_xx[i1][i2][i3] / n[i1][i2][i3] - _avg[i1][2][i3] * _avg[i1][2][i3]));

								int nmed = n[i1][i2][i3];
								double[] medx2 = new double[nmed];
								for(int i4 = 0; i4 < nmed; i4++)
								{
									medx2[i4] = _medx[i1][i2][i3][i4];
								}
								Arrays.sort(medx2);
								// TODO fix logic for even sizes
								nmed = nmed / 2;
								_med[i1][i2][i3] = medx2[nmed];
							}
						}
					}
				}

				// Put into table
				String[] tagStringList = {"Avg", "StdDev", "Median", "Min", "Max"};

				for(int tag = 0; tag < tagStringList.length; tag++)
				{
					if(tagString.contains(tagStringList[tag]))
					{
						for(int i1 = 0; i1 < 6; i1++)
						{
							boolean groupHasData = false;

							for(int i2 = 0; i2 < 6; i2++)

							{
								if((((i1 == 0) && tagString.contains("All years") && (i2 == 0))
										|| ((i1 == 1) && tagString.contains("40-30-30"))
										|| ((i1 == 2) && tagString.contains("Shasta"))
										|| ((i1 == 3) && tagString.contains("Feather"))
										|| ((i1 == 4) && tagString.contains("SJR Index"))
										|| ((i1 == 5) && tagString.contains("All dry"))
										|| ((i1 == 5) && (i2 == 1) && tagString.contains("1928"))
										|| ((i1 == 5) && (i2 == 2) && tagString.contains("1976"))
										|| ((i1 == 5) && (i2 == 3) && tagString.contains("1986")))
										&& (n[i1][i2][0] > 0))
								{

									groupHasData = true;

									String rightPart;
									if(i1 == 0)
									{
										rightPart = ""; // All years
									}
									else if(i1 == 1 || i1 == 4)
									{
										rightPart = " (" + rightPartsclimate[i2] + ")"; // 40-30-30/SJR
									}
									else if(i1 == 2)
									{
										rightPart = " (" + rightPartsclimate2[i2] + ")"; // Shasta
									}
									else if(i1 <= 4)
									{
										rightPart = " " + i2; // Feather
									}
									else
									{
										rightPart = " (" + rightPartsDry[i2] + ")"; // Dry
									}
									// periods

									data[t].addElement(leftPart[i1] + rightPart);
									data[t].addElement(tagStringList[tag]);
									for(int i3 = 0; i3 < 13; i3++)
									{

										int i3m;
										if(i3 < 3)
										{
											i3m = i3 + 10;
										}
										else if(i3 < 12)
										{
											i3m = i3 - 2;
										}
										else if(isCFS)
										{
											i3m = 13;
										}
										else
										{
											i3m = 0;
										}

										switch(tag)
										{
											case 0:
												data[t].addElement(df1.format(_avg[i1][i2][i3m])); // *
												// (i3m
												// ==
												// 0
												// ?
												// 12
												// :
												// 1)));
												break;
											case 1:
												data[t].addElement(df1.format(_sdev[i1][i2][i3m]));
												break;
											case 2:
												data[t].addElement(df1.format(_med[i1][i2][i3m]));
												break;
											case 3:
												data[t].addElement(df1.format(_min[i1][i2][i3m]));
												break;
											case 4:
												data[t].addElement(df1.format(_max[i1][i2][i3m]));
												break;
											default:
										}
									}
								}
							}
							if(groupHasData)
							{
								for(int i3 = 0; i3 < 15; i3++)
								{
									data[t].addElement("");
								}
							}
						}
					}
				}

				// Delete extra blank row at end of table

				for(int i = 0; i < 15; i++)
				{
					data[t].removeElementAt(data[t].size() - 1);
				}

				SimpleTableModel model = new SimpleTableModel(data[t], columns);
				JTable table = new JTable(model);
				table.setAutoResizeMode(JTable.AUTO_RESIZE_ALL_COLUMNS);
				for(int c = 0; c < 15; c++)
				{
					TableColumn col = table.getColumnModel().getColumn(c);
					col.setPreferredWidth((c == 0) ? 150 : 45);
				}
				table.setCellSelectionEnabled(true);
				DefaultTableCellRenderer renderer = (DefaultTableCellRenderer) table.getDefaultRenderer(String.class);
				renderer.setHorizontalAlignment(JLabel.RIGHT);

				String labelText;
				if(isBase)
				{
					if(t == 0)
					{
						labelText = title;
					}
					else
					{
						if(!sName.equals(""))
						{
							labelText = sName;
						}
						else
						{
							String[] parts = tsc.fullName.split("/");
							labelText = parts[2] + "/" + parts[3];
						}
					}
				}
				else
				{
					if(t < tscs.length)
					{
						labelText = title;
					}
					else
					{
						if(!sName.equals(""))
						{
							labelText = sName;
						}
						else
						{
							String[] parts = tsc.fullName.split("/");
							labelText = parts[2] + "/" + parts[3];
						}
					}
				}
				JLabel label = new JLabel();
				label.setText(labelText + " (" + tsc.units + ") - " + tsc.fileName);
				label.setPreferredSize(new Dimension(500, 30));

				panel.add(label);
				panel.add(table.getTableHeader(), BorderLayout.NORTH);
				panel.add(table);
			}
		}

		addComponentListener(this);
		scrollPane = new JScrollPane();
		scrollPane.setViewportView(panel);
		scrollPane.setPreferredSize(new Dimension(790, 550));
		scrollPane.revalidate();
		scrollPane.setAlignmentX(LEFT_ALIGNMENT);

		JButton copy = new JButton("Copy to Clipboard");
		copy.setAlignmentX(LEFT_ALIGNMENT);
		copy.addActionListener(this);
		copy.setAlignmentX(LEFT_ALIGNMENT);
		copy.addActionListener(this);

		Box box = Box.createVerticalBox();

		box.add(scrollPane);
		box.add(copy);

		add(box);

	}

	private void update(int i1, int i2, double value, int m)
	{
		/*
		 * Accumulates monthly and all-value totals
		 */
		_x[i1][i2][m] += value;
		_xx[i1][i2][m] += (value * value);
		if(_min[i1][i2][m] > value)
		{
			_min[i1][i2][m] = value;
		}
		if(_max[i1][i2][m] < value)
		{
			_max[i1][i2][m] = value;
		}
		_medx[i1][i2][m][n[i1][i2][m]] = value;
		n[i1][i2][m]++;

		_x[i1][i2][0] += value;
		_xx[i1][i2][0] += (value * value);
		if(_min[i1][i2][0] > value)
		{
			_min[i1][i2][0] = value;
		}
		if(_max[i1][i2][0] < value)
		{
			_max[i1][i2][0] = value;
		}
		_medx[i1][i2][0][n[i1][i2][0]] = value;
		n[i1][i2][0]++;
	}

	private void update2(int i1, int i2, double value, int m)
	{
		/*
		 * Copy of update to handle ONLY annual totals
		 */
		_x[i1][i2][13] += value;
		_xx[i1][i2][13] += (value * value);
		if(_min[i1][i2][13] > value)
		{
			_min[i1][i2][13] = value;
		}
		if(_max[i1][i2][13] < value)
		{
			_max[i1][i2][13] = value;
		}
		_medx[i1][i2][13][n[i1][i2][13]] = value;
		n[i1][i2][13]++;
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

	public class SimpleTableModel extends AbstractTableModel
	{

		/**
		 *
		 */
		private static final long serialVersionUID = -7777566463291833750L;
		protected Vector<String> data;
		protected Vector<String> columnNames;

		public SimpleTableModel(Vector<String> datain, Vector<String> columnin)
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
