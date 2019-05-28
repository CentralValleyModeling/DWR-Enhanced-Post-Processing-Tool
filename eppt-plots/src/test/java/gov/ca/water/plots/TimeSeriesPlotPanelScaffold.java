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

package gov.ca.water.plots;

import java.awt.BorderLayout;
import java.awt.Color;
import java.time.ZonedDateTime;
import java.util.Calendar;
import java.util.Collections;
import java.util.Date;
import javax.swing.*;

import hec.heclib.util.HecTime;
import hec.heclib.util.HecTimeArray;
import hec.io.TimeSeriesContainer;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 05-22-2019
 */
public class TimeSeriesPlotPanelScaffold
{

	public final void initScaffold()
	{

		try
		{
			UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
		}
		catch(ClassNotFoundException | InstantiationException | IllegalAccessException | UnsupportedLookAndFeelException e)
		{
			e.printStackTrace();
		}
		TimeSeriesContainer tsc = new TimeSeriesContainer();
		HecTimeArray hecTimeArray = new HecTimeArray(2);
		Calendar cal = Calendar.getInstance();
		cal.set(2010, 2, 5, 2, 2, 2);
		hecTimeArray.set(0, new HecTime(cal));
		cal.set(Calendar.YEAR, 2015);
		hecTimeArray.set(1, new HecTime(cal));
		tsc.set(new double[]{1.0, 50000000.01}, hecTimeArray);
		tsc.setFullName("Test Data Set");
		tsc.setType("Storage");
		TimeSeriesPlotPanel epptPanel = new TimeSeriesPlotPanel(Collections.singletonList(tsc));
		JFrame jFrame = new JFrame();
		jFrame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		SwingUtilities.invokeLater(() ->
		{
			jFrame.setLayout(new BorderLayout());
			jFrame.add(epptPanel, BorderLayout.CENTER);
			jFrame.setSize(1000, 700);
			jFrame.setBackground(Color.WHITE);
			jFrame.setVisible(true);
		});
	}

	public static void main(String[] args)
	{
		new TimeSeriesPlotPanelScaffold().initScaffold();
	}
}
