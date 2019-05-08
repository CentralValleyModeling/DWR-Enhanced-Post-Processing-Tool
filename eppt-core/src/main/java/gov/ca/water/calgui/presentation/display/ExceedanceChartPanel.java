/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */

package gov.ca.water.calgui.presentation.display;

import java.awt.*;
import java.awt.datatransfer.StringSelection;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.print.PageFormat;
import java.awt.print.Printable;
import java.awt.print.PrinterException;
import java.awt.print.PrinterJob;
import java.util.Date;
import javax.print.attribute.HashPrintRequestAttributeSet;
import javax.print.attribute.PrintRequestAttributeSet;
import javax.print.attribute.standard.Copies;
import javax.print.attribute.standard.OrientationRequested;
import javax.swing.*;

import org.apache.log4j.Logger;
import org.jfree.chart.ChartColor;
import org.jfree.chart.ChartFactory;
import org.jfree.chart.ChartPanel;
import org.jfree.chart.JFreeChart;
import org.jfree.chart.axis.ValueAxis;
import org.jfree.chart.plot.CombinedDomainXYPlot;
import org.jfree.chart.plot.XYPlot;
import org.jfree.chart.renderer.xy.XYItemRenderer;
import org.jfree.data.Range;
import org.jfree.data.xy.XYDataset;
import org.jfree.data.xy.XYSeries;
import org.jfree.data.xy.XYSeriesCollection;
import org.jfree.ui.RectangleInsets;

import hec.io.TimeSeriesContainer;

public class ExceedanceChartPanel extends JPanel implements Printable
{
	/**
	 * ChartPanel1 - Creates JPanel with a single ChartPanel
	 */
	private static final long serialVersionUID = 7398804723681056388L;
	private static Logger LOG = Logger.getLogger(ChartPanel.class.getName());
	JButton btnScatter;
	private String buffer;

	public ExceedanceChartPanel(String title, String yLabel, TimeSeriesContainer[] tscs, TimeSeriesContainer[] stscs,
								Date lower,
								Date upper, String sLabel, boolean isBase)
	{

		super();

		// create datasets ...

		double ymax = -1e20;
		double ymin = 1e20;

		JFreeChart chart = null;

		int primaries = 0;
		String sName = "";
		if("".equals(sLabel))
		{
			if(stscs != null)
			{
				sName = stscs[0].fullName;
			}
		}
		else
		{
			sName = sLabel;
		}

		// Primary datasets

		XYSeriesCollection dataset = new XYSeriesCollection();
		XYSeries[] series = new XYSeries[(isBase ? 1 : tscs.length)];
		for(int i = 0; i < (isBase ? 1 : tscs.length); i++)
		{
			series[i] = new XYSeries(tscs[i].fileName);
			primaries++;
			for(int j = 0; j < tscs[i].numberValues; j++)
			{
				series[i].add(100.0 - 100.0 * j / (tscs[i].numberValues - 1), tscs[i].values[j]);
			}
			dataset.addSeries(series[i]);
			if(ymin > tscs[i].minimumValue())
			{
				ymin = tscs[i].minimumValue();
			}
			if(ymax < tscs[i].maxmimumValue())
			{
				ymax = tscs[i].maxmimumValue();
			}
		}

		// Secondary datasets

		if(stscs != null)
		{
			XYSeries[] sseries = new XYSeries[(isBase ? 1 : stscs.length)];
			for(int i = 0; i < (isBase ? 1 : stscs.length); i++)
			{
				if(stscs[i].numberValues > 0)
				{
					sseries[i] = new XYSeries(sName);
					double maxval = -1e37;
					for(int j = 0; j < stscs[i].numberValues; j++)
					{
						if(stscs[i].values[j] == 99000)
						{
							sseries[i].add(100.0 - 100.0 * j / (stscs[i].numberValues - 1), null);
						}
						else
						{
							sseries[i].add(100.0 - 100.0 * j / (stscs[i].numberValues - 1), stscs[i].values[j]);
							if(maxval < stscs[i].values[j])
							{
								maxval = stscs[i].values[j];
							}
						}
					}
					dataset.addSeries(sseries[i]);
					if(ymin > stscs[i].minimumValue())
					{
						ymin = stscs[i].minimumValue();
					}
					if(ymax < maxval)
					{
						ymax = maxval;
					}
				}
			}

		}

		chart = ChartFactory.createXYLineChart(title.replace(";", "+"), // title
				"Percent", // x-axis label
				yLabel + ((yLabel.endsWith("(TAF)") ? "" : "(" + tscs[0].units + ")")), // y-axis label
				dataset); // create and display a frame...

		setChartOptions(chart, stscs, true, isBase, ymax, ymin, primaries);
		XYPlot plot = (XYPlot) chart.getPlot();
		ValueAxis axis = plot.getDomainAxis();

		final ChartPanel p1 = new ChartPanel(chart);

		// Copy title, all data series to clipboard

		JPopupMenu popupmenu = p1.getPopupMenu();
		JMenuItem item0 = popupmenu.add("Reset Axes");
		item0.addActionListener(new ActionListener()
		{
			@Override
			public void actionPerformed(ActionEvent e)
			{
				// JMenuItem mi = (JMenuItem) e.getSource();
				// JPopupMenu pm = (JPopupMenu) mi.getParent();
				// ChartPanel cp = (ChartPanel) pm.getParent();
				// cp.restoreAutoBounds();
				p1.restoreAutoBounds();
			}
		});
		JMenuItem item = popupmenu.add("Copy Data");
		item.addActionListener(new ActionListener()
		{
			@Override
			public void actionPerformed(ActionEvent e)
			{
				if(buffer == null)
				{
					return;
				}
				StringSelection clipString = new StringSelection(buffer);
				getToolkit().getSystemClipboard().setContents(clipString, clipString);
			}
		});

		// Finish up window

		p1.setPreferredSize(new Dimension(800, 600));
		this.setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));
		this.add(p1);

		// Put data in buffer for clipboard

		buffer = title + "\n";

		// Dataset titles

		XYDataset dataset2 = plot.getDataset();
		for(int i = 0; i < dataset2.getSeriesCount(); i++)
		{
			buffer = buffer + dataset2.getSeriesKey(i).toString() + "\t\t\t";
		}

		buffer = buffer + "\n";

		for(int i = 0; i < dataset2.getSeriesCount(); i++)
		{
			buffer = buffer + "%\t" + axis.getLabel() + "\t\t";
		}

		buffer = buffer + "\n";

		// Data

		for(int j = 0; j < dataset2.getItemCount(0); j++)
		{

			for(int i = 0; i < dataset2.getSeriesCount(); i++)
			{
				if(j < dataset.getItemCount(i))
				{
					buffer = buffer + dataset2.getXValue(i, j) + "\t" + dataset2.getYValue(i, j) + "\t\t";
				}
				else
				{
					buffer = buffer + "\t\t\t";
				}
			}

			buffer = buffer + "\n";
		}
	}

	/**
	 * Sets some common chart options
	 *
	 * @param chart
	 * @param stscs
	 * @param isExceed
	 * @param isBase
	 * @param ymax
	 * @param ymin
	 * @param primaries
	 */
	private void setChartOptions(JFreeChart chart, TimeSeriesContainer[] stscs, boolean isExceed, boolean isBase,
								 Double ymax,
								 Double ymin, Integer primaries)
	{

		chart.setBackgroundPaint(Color.WHITE);

		XYPlot plot = (XYPlot) chart.getPlot();
		plot.setBackgroundPaint(Color.WHITE); // White background
		plot.setDomainGridlinesVisible(false); // No gridlines
		plot.setRangeGridlinesVisible(false);
		plot.setAxisOffset(new RectangleInsets(0, 0, 0, 0)); // No axis offset

		XYItemRenderer r = plot.getRenderer();

		if(plot instanceof CombinedDomainXYPlot)
		{
		}
		else if(plot.getDataset(0).getSeriesCount() >= 4) // Fourth series assumed yellow, switched to black
		{
			r.setSeriesPaint(3, ChartColor.BLACK);
		}

		if(stscs != null)
		{ // Secondary time series as dashed lines
			for(int t = 0; t < (isBase ? 1 : stscs.length); t++)
			{
				Stroke stroke = new BasicStroke(1.0f, // Width
						BasicStroke.CAP_SQUARE, // End cap
						BasicStroke.JOIN_MITER, // Join style
						10.0f, // Miter limit
						new float[]{2.0f, 2.0f}, // Dash pattern
						0.0f); // Dash phase
				r.setSeriesStroke(primaries + t, stroke);
			}
		}

		ValueAxis axis = plot.getDomainAxis();
		if(isExceed)
		{
			axis.setInverted(true);
		}

		axis.setTickMarkInsideLength(axis.getTickMarkOutsideLength());
		if(isExceed)
		{
			axis.setRange(0.0, 100.0);
		}

		if((ymax - ymin) < 0.01)
		{
			ymax += 0.05;
			ymin -= 0.05;
		}
		if(plot instanceof CombinedDomainXYPlot)
		{
		}
		else
		{
			axis = plot.getRangeAxis();
			axis.setTickMarkInsideLength(axis.getTickMarkOutsideLength());
			axis.setRange(new Range(ymin - 0.05 * (ymax - ymin), ymax + 0.05 * (ymax - ymin)));
		}
		plot.setDomainPannable(true);
		plot.setRangePannable(true);
	}

	/**
	 * Prints chart
	 */
	public void createChartPrintJob()
	{

		PrintRequestAttributeSet set = new HashPrintRequestAttributeSet();
		set.add(OrientationRequested.PORTRAIT);
		set.add(new Copies(1));

		PrinterJob job = PrinterJob.getPrinterJob();
		job.setPrintable(this);
		if(job.printDialog(set))
		{
			try
			{
				job.print(set);
			}
			catch(PrinterException e)
			{
				JOptionPane.showMessageDialog(this, e);
				LOG.debug(e.getMessage());
			}
		}

	}

	@Override
	public int print(Graphics graphics, PageFormat pageFormat, int pageIndex) throws PrinterException
	{
		// TODO Auto-generated method stub
		return 0;
	}
}
