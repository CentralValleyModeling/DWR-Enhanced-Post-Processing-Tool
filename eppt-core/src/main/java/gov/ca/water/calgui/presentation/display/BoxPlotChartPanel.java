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

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Graphics;
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
import org.jfree.ui.RectangleInsets;

import hec.io.TimeSeriesContainer;

public class BoxPlotChartPanel extends JPanel implements Printable
{
	/**
	 * ChartPanel1 - Creates JPanel with a single ChartPanel
	 */
	private static final long serialVersionUID = 7398804723681056388L;
	private static Logger LOG = Logger.getLogger(ChartPanel.class.getName());
	JButton btnScatter;
	private String buffer;
	private IErrorHandlingSvc errorHandlingSvc = new ErrorHandlingSvcImpl();

	public BoxPlotChartPanel(String title, TimeSeriesContainer[] tscs, boolean isBase)
	{

		super();

		double ymin = 1e9;
		double ymax = -1e9;
		final DefaultBoxAndWhiskerCategoryDataset dataset = new DefaultBoxAndWhiskerCategoryDataset();

		int seriesCount = isBase ? 1 : tscs.length;
		int categoryCount = 14; // 12 months, all, annual
		for(int i = 0; i < seriesCount; i++)
		{
			for(int j = 0; j < categoryCount; j++)
			{

				TimeSeriesContainer tsc = tscs[i];
				if(tsc != null)
				{
					final List<Double> list = new ArrayList<>();
					if(j == 0)
					{
						// All data
						for(int k = 0; k < tsc.numberValues; k++)
						{
							list.add(tsc.values[k]);
							ymin = Math.min(ymin, tsc.values[k]);
							ymax = Math.max(ymax, tsc.values[k]);
						}
					}
					else if(j < 13)
					{
						// Monthly
						for(int k = j - 1; k < tsc.numberValues; k += 12)
						{
							list.add(tsc.values[k]);
						}
					}

					else
					{
						// Annual
						for(int k = 0; k < tsc.numberValues; k += 12)
						{
							double sum = 0;
							for(int l = 0; l < 12; l++)
							{
								sum = sum + tsc.values[k + l];
							}
							list.add(sum / 12);
						}

					}
					dataset.add(list, tsc.fileName, j == 0 ? "All"
							: j == 13 ? "Annual" : "OctNovDecJanFebMarAprMayJunJulAugSep".substring(3 * j - 3, 3 * j));
					list.clear();
				}
			}

			final CategoryAxis xAxis = new CategoryAxis("Period");
			final NumberAxis yAxis = new NumberAxis("Value");
			yAxis.setAutoRangeIncludesZero(false);
			yAxis.setRange(ymin * 0.95, ymax * 1.05);
			final BoxAndWhiskerRenderer renderer = new BoxAndWhiskerRenderer();
			renderer.setFillBox(false);

			renderer.setBaseToolTipGenerator(new BoxAndWhiskerToolTipGenerator());
			final CategoryPlot plot = new CategoryPlot(dataset, xAxis, yAxis, renderer);

			plot.setBackgroundPaint(Color.WHITE); // White background
			plot.setDomainGridlinesVisible(false); // No gridlines
			plot.setRangeGridlinesVisible(false);
			plot.setAxisOffset(new RectangleInsets(0, 0, 0, 0)); // No axis
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
