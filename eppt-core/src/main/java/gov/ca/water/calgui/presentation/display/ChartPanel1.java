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

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Stroke;
import java.awt.datatransfer.StringSelection;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.print.PageFormat;
import java.awt.print.Printable;
import java.awt.print.PrinterException;
import java.awt.print.PrinterJob;
import java.text.SimpleDateFormat;
import java.util.Date;
import javax.print.attribute.HashPrintRequestAttributeSet;
import javax.print.attribute.PrintRequestAttributeSet;
import javax.print.attribute.standard.Copies;
import javax.print.attribute.standard.OrientationRequested;
import javax.swing.*;

import gov.ca.water.calgui.constant.Constant;
import org.apache.log4j.Logger;
import org.jfree.chart.ChartColor;
import org.jfree.chart.ChartFactory;
import org.jfree.chart.ChartPanel;
import org.jfree.chart.JFreeChart;
import org.jfree.chart.axis.AxisLocation;
import org.jfree.chart.axis.DateAxis;
import org.jfree.chart.axis.NumberAxis;
import org.jfree.chart.axis.ValueAxis;
import org.jfree.chart.plot.CombinedDomainXYPlot;
import org.jfree.chart.plot.PlotOrientation;
import org.jfree.chart.plot.XYPlot;
import org.jfree.chart.renderer.xy.StandardXYItemRenderer;
import org.jfree.chart.renderer.xy.XYItemRenderer;
import org.jfree.chart.renderer.xy.XYLineAndShapeRenderer;
import org.jfree.chart.title.LegendTitle;
import org.jfree.data.Range;
import org.jfree.data.general.Series;
import org.jfree.data.time.Month;
import org.jfree.data.time.TimeSeries;
import org.jfree.data.time.TimeSeriesCollection;
import org.jfree.data.xy.XYDataset;
import org.jfree.data.xy.XYSeries;
import org.jfree.data.xy.XYSeriesCollection;
import org.jfree.ui.RectangleInsets;

import hec.heclib.util.HecTime;
import hec.io.TimeSeriesContainer;

public class ChartPanel1 extends JPanel implements Printable
{
	/**
	 * ChartPanel1 - Creates JPanel with a single ChartPanel
	 */
	private static final long serialVersionUID = 7398804723681056388L;
	private static Logger LOG = Logger.getLogger(ChartPanel.class.getName());
	private JButton _btnScatter;
	private String _buffer;

	public ChartPanel1(String title, String yLabel, TimeSeriesContainer[] tscs, TimeSeriesContainer[] stscs,
					   boolean isExceed, String sLabel)
	{
		this(title, yLabel, tscs, stscs, isExceed, sLabel, false);
	}


	public ChartPanel1(String title, String yLabel, TimeSeriesContainer[] tscs, TimeSeriesContainer[] stscs,
					   boolean isExceed, String sLabel, boolean isBase)
	{

		super();
		// create datasets ...

		double ymax = -1e20;
		double ymin = 1e20;

		JFreeChart chart;
		JFreeChart chartXY = null;

		int primaries = 0;
		String sName = "";
		if("".equals(sLabel))
		{
			if(stscs != null && stscs[0] != null)
			{
				sName = stscs[0].fullName;
			}
		}
		else
		{
			sName = sLabel;
		}

		boolean scatterAvailable = (stscs == null) && (tscs.length == 2) && !isExceed;

		boolean isSchVw = false;
		String[] svNames = null;

		if(title.startsWith(Constant.SCHEMATIC_PREFIX))
		{
			title = title.substring(Constant.SCHEMATIC_PREFIX.length());
			isSchVw = true;
			svNames = sLabel.split(",");
		}

		if(isExceed)
		{

			// Primary datasets
			XYSeriesCollection dataset = new XYSeriesCollection();
			XYSeries[] series = new XYSeries[(isBase ? 1 : tscs.length)];
			for(int i = 0; i < (isBase ? 1 : tscs.length); i++)
			{
				//requires unique name
				String uniqueName = getUniqueLegendNameForCurve(series, tscs[i]);
				series[i] = new XYSeries(uniqueName);
				primaries++;
				for(int j = 0; j < tscs[i].numberValues; j++)
				{
					series[i].add(100.0 - 100.0 * j / (tscs[i].numberValues - 1), tscs[i].values[j]);
					ymin = Math.min(ymin, tscs[i].values[j]);
					ymax = Math.max(ymax, tscs[i].values[j]);
				}
				dataset.addSeries(series[i]);
			}

			// Secondary datasets

			if(stscs != null)
			{
				XYSeries[] sseries = new XYSeries[(isBase ? 1 : stscs.length)];
				for(int i = 0; i < (isBase ? 1 : stscs.length); i++)
				{
					if(stscs[i].numberValues > 0)
					{
						stscs[i].fullName = sName + " - " + stscs[i].fullName;
						String uniqueName = getUniqueLegendNameForCurve(sseries, stscs[i]);
						sseries[i] = new XYSeries(uniqueName);
						for(int j = 0; j < stscs[i].numberValues; j++)
						{
							if(stscs[i].values[j] == 99000)
							{
								sseries[i].add(100.0 - 100.0 * j / (stscs[i].numberValues - 1), null);
							}
							else
							{
								sseries[i].add(100.0 - 100.0 * j / (stscs[i].numberValues - 1), stscs[i].values[j]);
								ymin = Math.min(ymin, stscs[i].values[j]);
								ymax = Math.max(ymax, stscs[i].values[j]);
							}
						}

						dataset.addSeries(sseries[i]);
					}
				}
			}

			chart = ChartFactory.createXYLineChart(title.replace(";", "+"), // title
					"Percent", // x-axis label
					yLabel + ((yLabel.endsWith("(TAF)") ? "" : "(" + tscs[0].units + ")")), // y-axis
					// label
					dataset); // create and display a frame...
			LegendTitle legend = chart.getLegend();

		}
		else
		{

			HecTime ht = new HecTime();
			TimeSeries[] series = new TimeSeries[(isBase ? 1 : tscs.length)];

			if(isSchVw && sLabel.contains("EVAPORATION"))
			{

				// MULTIPLE (COMBINED) CHARTS FOR SCHEMATIC VIEW

				// Build the data series
				for(int i = 0; i < (isBase ? 1 : tscs.length); i++)
				{
					if(isSchVw)
					{
						series[i] = new TimeSeries("");
					}
					else
					{
						series[i] = new TimeSeries(getUniqueLegendNameForCurve(series, tscs[i]));// tscs[i].fileName);
					}
					for(int j = 0; j < tscs[i].numberValues; j++)
					{
						ht.set(tscs[i].times[j]);
						series[i].addOrUpdate(new Month(ht.month(), ht.year()), tscs[i].values[j]);
						ymin = Math.min(ymin, tscs[i].values[j]);
						ymax = Math.max(ymax, tscs[i].values[j]);
					}
				}

				// Subplot 1 - STORAGE

				TimeSeriesCollection dataset1 = new TimeSeriesCollection();
				dataset1.addSeries(series[0]);
				XYItemRenderer renderer1 = new StandardXYItemRenderer();
				NumberAxis rangeAxis1 = new NumberAxis("Storage (" + tscs[0].units + ")");
				XYPlot subplot1 = new XYPlot(dataset1, null, rangeAxis1, renderer1);
				subplot1.setRangeAxisLocation(AxisLocation.BOTTOM_OR_LEFT);
				subplot1.setBackgroundPaint(Color.WHITE); // White background
				subplot1.setDomainGridlinesVisible(false); // No gridlines
				subplot1.setRangeGridlinesVisible(false);
				subplot1.setAxisOffset(new RectangleInsets(0, 0, 0, 0)); // No
				// axis
				// offset

				// SUBPLOT2 - EVAPORATION ...

				TimeSeriesCollection dataset2 = new TimeSeriesCollection();
				dataset2.addSeries(series[1]);
				NumberAxis rangeAxis2 = new NumberAxis("Evaporation (" + tscs[1].units + ")");

				XYPlot subplot2 = new XYPlot(dataset2, null, rangeAxis2, renderer1);
				subplot2.setRangeAxisLocation(AxisLocation.BOTTOM_OR_LEFT);
				subplot2.setBackgroundPaint(Color.WHITE); // White background
				subplot2.setDomainGridlinesVisible(false); // No gridlines
				subplot2.setRangeGridlinesVisible(false);
				subplot2.setAxisOffset(new RectangleInsets(0, 0, 0, 0)); // No
				// axis
				// offset

				// ... and SURFACE AREA

				TimeSeriesCollection dataset2a = new TimeSeriesCollection();
				dataset2a.addSeries(series[2]);
				subplot2.setDataset(1, dataset2a);
				NumberAxis axis2 = new NumberAxis("Surface Area (" + tscs[2].units + ")");
				subplot2.setRangeAxis(1, axis2);
				subplot2.setRangeAxisLocation(1, AxisLocation.BOTTOM_OR_RIGHT);
				subplot2.mapDatasetToRangeAxis(1, 1);
				final XYItemRenderer renderer2 = new StandardXYItemRenderer();
				subplot2.setRenderer(1, renderer2);
				axis2.setTickLabelPaint(ChartColor.BLUE);

				// SUBPLOT 3 - FLOWS

				TimeSeriesCollection dataset3 = new TimeSeriesCollection();
				dataset3.addSeries(series[3]);
				if(tscs.length > 4)
				{
					dataset3.addSeries(series[4]);
				}

				final NumberAxis rangeAxis3 = new NumberAxis("Flow (" + tscs[3].units + ")");
				XYPlot subplot3 = new XYPlot(dataset3, null, rangeAxis3, renderer1);
				subplot3.setRangeAxisLocation(AxisLocation.BOTTOM_OR_LEFT);
				subplot3.setBackgroundPaint(Color.WHITE); // White background
				subplot3.setDomainGridlinesVisible(false); // No gridlines
				subplot3.setRangeGridlinesVisible(false);
				subplot3.setAxisOffset(new RectangleInsets(0, 0, 0, 0)); // No
				// axis
				// offset

				// COMBINE THE SUBPLOTS

				CombinedDomainXYPlot plot = new CombinedDomainXYPlot(new DateAxis(""));
				plot.add(subplot1, 1);
				plot.add(subplot2, 1);
				plot.add(subplot3, 1);
				plot.setGap(15.0);
				plot.setOrientation(PlotOrientation.VERTICAL);
				chart = new JFreeChart(title, JFreeChart.DEFAULT_TITLE_FONT, plot, false);

				// TODO: HANDLE LEGEND AND MULTIPLE SCENARIOS

			}
			else
			{
				TimeSeriesCollection dataset = new TimeSeriesCollection();
				for(int i = 0; i < (isBase ? 1 : tscs.length); i++)
				{
					TimeSeriesContainer tsc = tscs[i];
					if(tsc != null)
					{
						if(isSchVw)
						{
							tsc.fullName = svNames[i] + " - " + tsc.fullName;
							series[i] = new TimeSeries(getUniqueLegendNameForCurve(series, tsc));
						}
						else
						{
							series[i] = new TimeSeries(getUniqueLegendNameForCurve(series, tsc));//tsc.getName());
						}
						primaries++;
						for(int j = 0; j < tsc.numberValues; j++)
						{
							ht.set(tsc.times[j]);
							series[i].addOrUpdate(new Month(ht.month(), ht.year()), tsc.values[j]);
							ymin = Math.min(ymin, tsc.values[j]);
							ymax = Math.max(ymax, tsc.values[j]);
						}
						dataset.addSeries(series[i]);
					}
				}

				if(stscs != null)
				{
					TimeSeries[] sseries = new TimeSeries[(isBase ? 1 : stscs.length)];
					for(int i = 0; i < (isBase ? 1 : stscs.length); i++)
					{
						if(stscs[i] != null && stscs[i].numberValues > 0)
						{
							stscs[i].fullName = sName + " - " + stscs[i].fullName;
							sseries[i] = new TimeSeries(getUniqueLegendNameForCurve(sseries, stscs[i]));//sName);
							double maxval = -1e37;
							for(int j = 0; j < stscs[i].numberValues; j++)
							{
								ht.set(stscs[i].times[j]);
								if(stscs[i].values[j] == 99000)
								{
									sseries[i].add(new Month(ht.month(), ht.year()), null);
								}
								else
								{
									sseries[i].addOrUpdate(new Month(ht.month(), ht.year()), stscs[i].values[j]);
									ymin = Math.min(ymin, stscs[i].values[j]);
									ymax = Math.max(ymax, stscs[i].values[j]);
								}
							}
							dataset.addSeries(sseries[i]);
						}
					}
				}

				chart = ChartFactory.createTimeSeriesChart(title.replace(";", "+"), // title
						"Time (1MON)", // x-axis label //TODO - Hard-coded to
						// monthly!
						yLabel + " (" + tscs[0].units + ")", // y-axis label
						dataset); // create and display a frame...

				if(scatterAvailable && tscs[0] != null && tscs[1] != null)
				{

					XYSeriesCollection datasetXY = new XYSeriesCollection();
					datasetXY = new XYSeriesCollection();
					XYSeries seriesXY = new XYSeries("");
					for(int j = 0; j < Math.min(tscs[0].numberValues, tscs[1].numberValues); j++)
					{
						seriesXY.addOrUpdate(tscs[0].values[j], tscs[1].values[j]);
					}
					datasetXY.addSeries(seriesXY);
					chartXY = ChartFactory.createXYLineChart(title.replace(";", "+") + " (" + tscs[0].units + ")",
							// title
							tscs[0].fileName, // x-axis label
							tscs[1].fileName, // y-axis label
							datasetXY, // data
							PlotOrientation.VERTICAL,
							false,
							false,
							false); // create and display a frame...
					XYPlot plot = (XYPlot) chartXY.getPlot();
					XYLineAndShapeRenderer renderer = (XYLineAndShapeRenderer) plot.getRenderer();
					renderer.setSeriesLinesVisible(0, false);
					renderer.setSeriesShapesVisible(0, true);
					renderer.setDrawOutlines(true);
					renderer.setUseFillPaint(true);
					renderer.setBaseFillPaint(Color.white);

				}
				else
				{
					scatterAvailable = false;
				}

			}
		}

		setChartOptions(chart, stscs, isExceed, isBase, ymax, ymin, primaries);
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
				if(_buffer == null)
				{
					return;
				}
				StringSelection clipString = new StringSelection(_buffer);
				getToolkit().getSystemClipboard().setContents(clipString, clipString);
			}
		});

		// Finish up window

		p1.setMaximumDrawHeight(1200);
		p1.setMaximumDrawWidth(1920);
		p1.setMinimumDrawHeight(480);
		p1.setMinimumDrawWidth(640);
		p1.setPreferredSize(new Dimension(800, 600));
		this.setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));
		this.add(p1);

		if(isSchVw)
		{
			ChartPanel p2 = new ChartPanel(chart);
			this.add(p2);
		}
		if(scatterAvailable && chartXY != null)
		{

			setChartOptions(chartXY, stscs, isExceed, isBase, ymax, ymin, primaries);
			final ChartPanel p2 = new ChartPanel(chartXY);
			p2.setMaximumDrawHeight(1200);
			p2.setMaximumDrawWidth(1920);
			p2.setMinimumDrawHeight(480);
			p2.setMinimumDrawWidth(640);
			p2.setVisible(false);
			p2.setPreferredSize(new Dimension(800, 600));
			p2.setMaximumSize(new Dimension(1920, 1200));
			this.add(p2);
			this.setBackground(Color.WHITE);

			// Button for XY scatter
			_btnScatter = new JButton("XY Scatter");
			_btnScatter.setSize(new Dimension(100, 20));
			_btnScatter.setMaximumSize(new Dimension(110, 20));
			_btnScatter.setPreferredSize(new Dimension(110, 20));
			_btnScatter.addActionListener(new ActionListener()
			{

				@Override
				public void actionPerformed(ActionEvent arg0)
				{
					if(_btnScatter.getText().equals("XY Scatter"))
					{
						_btnScatter.setText("Time Series");
						p1.setVisible(false);
						p2.setVisible(true);
					}
					else
					{
						_btnScatter.setText("XY Scatter");
						p1.setVisible(true);
						p2.setVisible(false);
					}
				}
			});

			this.add(_btnScatter);
		}

		if(plot instanceof CombinedDomainXYPlot)
		{
		}
		else
		{
			// Put data in buffer for clipboard

			_buffer = title + "\n";

			// Dataset titles

			XYDataset dataset = plot.getDataset();
			for(int i = 0; i < dataset.getSeriesCount(); i++)
			{
				_buffer = _buffer + dataset.getSeriesKey(i).toString() + "\t\t\t";
			}

			_buffer = _buffer + "\n";

			for(int i = 0; i < dataset.getSeriesCount(); i++)
			{
				_buffer = _buffer + (isExceed ? "%" : "Date") + "\t" + axis.getLabel() + "\t\t";
			}

			_buffer = _buffer + "\n";

			// Data
			SimpleDateFormat df = new SimpleDateFormat("MM/dd/yyyy");
			for(int j = 0; j < dataset.getItemCount(0); j++)
			{

				for(int i = 0; i < dataset.getSeriesCount(); i++)
				{
					if(j < dataset.getItemCount(i))
					{
						if(isExceed)

						{
							_buffer = _buffer + dataset.getXValue(i, j) + "\t" + dataset.getYValue(i, j) + "\t\t";
						}
						else
						{
							_buffer = _buffer + df.format(new Date((long) dataset.getXValue(i, j))) + "\t"
									+ dataset.getYValue(i, j) + "\t\t";
						}
					}
					else
					{
						_buffer = _buffer + "\t\t\t";
					}
				}

				_buffer = _buffer + "\n";
			}
		}
	}

	private String getUniqueLegendNameForCurve(Series[] dataSet, TimeSeriesContainer tsc)
	{
		String uniqueName = tsc.fullName;

		//check for uniqueness and add numbers at the end until it is
		boolean nameIsUnique;
		int i = 2;
		do
		{
			nameIsUnique = true;
			if(dataSet.length > 0)
			{
				for(Series line : dataSet)
				{
					if(line != null)
					{
						if(line.getKey().equals(uniqueName))
						{
							nameIsUnique = false;
							uniqueName = uniqueName.concat(" " + i);
							i++;
						}
					}
				}
			}
		}
		while(!nameIsUnique);


		return uniqueName;

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
								 Double ymax, Double ymin, Integer primaries)
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
		else if(plot.getDataset(0).getSeriesCount() >= 4) // Fourth series
		// assumed yellow,
		// switched to black
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
