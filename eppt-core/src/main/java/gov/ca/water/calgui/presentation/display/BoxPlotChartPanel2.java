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
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import javax.print.attribute.HashPrintRequestAttributeSet;
import javax.print.attribute.PrintRequestAttributeSet;
import javax.print.attribute.standard.Copies;
import javax.print.attribute.standard.OrientationRequested;
import javax.swing.*;

import calsim.app.MultipleTimeSeries;
import gov.ca.water.calgui.techservice.IErrorHandlingSvc;
import gov.ca.water.calgui.techservice.impl.ErrorHandlingSvcImpl;
import org.apache.log4j.Logger;
import org.jfree.chart.ChartPanel;
import org.jfree.chart.JFreeChart;
import org.jfree.chart.axis.CategoryAxis;
import org.jfree.chart.axis.NumberAxis;
import org.jfree.chart.labels.BoxAndWhiskerToolTipGenerator;
import org.jfree.chart.plot.CategoryPlot;
import org.jfree.chart.renderer.category.BoxAndWhiskerRenderer;
import org.jfree.data.statistics.DefaultBoxAndWhiskerCategoryDataset;
import org.jfree.data.time.TimeSeries;
import org.jfree.ui.RectangleInsets;

import hec.io.TimeSeriesContainer;

/**
 * Variant of BoxPlotChartPanel specifically for plotting WRIMS GUI
 * MultipleTimeSeriesobjects
 *
 * @author tslawecki
 */
public class BoxPlotChartPanel2 extends JPanel implements Printable
{
	/**
	 * ChartPanel1 - Creates JPanel with a single ChartPanel
	 */
	private static final long serialVersionUID = 7398804723681056388L;
	private static Logger LOG = Logger.getLogger(ChartPanel.class.getName());
	JButton btnScatter;
	private String buffer;
	private IErrorHandlingSvc errorHandlingSvc = new ErrorHandlingSvcImpl();

	public BoxPlotChartPanel2(String title, String yLabel, TimeSeriesContainer[][] mtscs,
							  TimeSeriesContainer[][] mstscs, Date lower, Date upper, String sLabel, boolean isBase,
							  MultipleTimeSeries mts)
	{

		super();

		double ymin = 1e9;
		double ymax = -1e9;
		final DefaultBoxAndWhiskerCategoryDataset dataset = new DefaultBoxAndWhiskerCategoryDataset();

		for(int mtsI = 0; mtsI < mtscs.length; mtsI++)
		{

			TimeSeriesContainer[] tscs = mtscs[mtsI];
			TimeSeries[] series = new TimeSeries[tscs.length];

			for(int i = 0; i < tscs.length; i++)
			{
				if(tscs[i] != null)
				{
					series[i] = new TimeSeries(
							tscs[i].fileName + ": "
									+ (mts.getDTSNameAt(mtsI).equals("")
									? mts.getBPartAt(mtsI) + "/" + mts.getCPartAt(mtsI)
									: mts.getDTSNameAt(mtsI)));
				}
			}

			int seriesCount = isBase ? 1 : tscs.length;
			int categoryCount = 14; // 12 months, all, annual
			for(int i = 0; i < seriesCount; i++)
			{
				if(tscs[i] != null)
				{
					for(int j = 0; j < categoryCount; j++)
					{
						final List<Double> list = new ArrayList<Double>();

						if(j == 0)
						{
							// All data
							for(int k = 0; k < tscs[i].numberValues; k++)
							{
								list.add(tscs[i].values[k]);
								ymin = Math.min(ymin, tscs[i].values[k]);
								ymax = Math.max(ymax, tscs[i].values[k]);
							}
						}
						else if(j <= 13)
						{
							// Monthly
							for(int k = j - 1; k < tscs[i].numberValues; k += 12)
							{
								list.add(tscs[i].values[k]);
							}
						}

						else
						{
							// Annual
							for(int k = 0; k < tscs[i].numberValues; k += 12)
							{
								double sum = 0;
								for(int l = 0; l < 12; l++)
								{
									sum = sum + tscs[i].values[k + l];
								}
								list.add(sum / 12);
							}

						}
						dataset.add(list, tscs[i].fileName,
								j == 0 ? "All"
										: j == 13 ? "Annual"
										: "OctNovDecJanFebMarAprMayJunJulAugSep".substring(3 * j - 3, 3 * j));
						list.clear();
					}

					final CategoryAxis xAxis = new CategoryAxis("Period");
					final NumberAxis yAxis = new NumberAxis((String) series[i].getKey());
					yAxis.setAutoRangeIncludesZero(false);
					yAxis.setRange(ymin * 0.95, ymax * 1.05);
					final BoxAndWhiskerRenderer renderer = new BoxAndWhiskerRenderer();
					renderer.setFillBox(false);

					renderer.setBaseToolTipGenerator(new BoxAndWhiskerToolTipGenerator());
					final CategoryPlot plot = new CategoryPlot(dataset, xAxis, yAxis, renderer);

					plot.setBackgroundPaint(Color.WHITE); // White background
					plot.setDomainGridlinesVisible(false); // No gridlines
					plot.setRangeGridlinesVisible(false);
					plot.setAxisOffset(new RectangleInsets(0, 0, 0, 0)); // No
					// axis
					// offset

					JFreeChart chart = new JFreeChart(plot);
					chart.setTitle(title);

					final ChartPanel p1 = new ChartPanel(chart);

					// Copy title, all data series to clipboard

					JPopupMenu popupmenu = p1.getPopupMenu();
					JMenuItem item0 = popupmenu.add("Reset Axes");
					item0.addActionListener(new ActionListener()
					{
						@Override
						public void actionPerformed(ActionEvent e)
						{
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
				}
			}
		}
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
