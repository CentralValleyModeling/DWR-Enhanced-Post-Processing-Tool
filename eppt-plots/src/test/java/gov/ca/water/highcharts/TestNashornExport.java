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

package gov.ca.water.highcharts;

import java.nio.file.Path;
import java.nio.file.Paths;
import javax.swing.*;

import gov.ca.water.highcharts.HighChartsSvgPrinter;
import javafx.application.Platform;
import javafx.embed.swing.JFXPanel;
import org.json.JSONObject;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 08-02-2019
 */
public class TestNashornExport
{
	public static void main(String[] args) throws Exception
	{
		JFrame jFrame = new JFrame();
		jFrame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		SwingUtilities.invokeLater(() ->
		{
			try
			{
				printSvgs();
			}
			catch(InterruptedException e)
			{
				e.printStackTrace();
			}
			jFrame.setVisible(true);
		});

	}

	private static void printSvgs() throws InterruptedException
	{
		new JFXPanel();

		Platform.runLater(() ->
		{
			HighChartsSvgPrinter highChartsSvgPrinter = new HighChartsSvgPrinter();
			Path htmlPath = Paths.get(
					"C:\\Git\\DWR\\EPPT\\DWR-Enhanced-Post-Processing-Tool\\eppt-plots\\src\\main\\resources\\_qa_qc\\qaqc_scatter.html");

			highChartsSvgPrinter.load(htmlPath, () ->
			{
				for(int i = 0; i < 30; i++)
				{
					Path file = Paths.get("C:\\Git\\DWR\\EPPT\\DWR-Enhanced-Post-Processing-Tool\\eppt-ui\\Test" + i);
					System.out.println("print to: " + file);
					highChartsSvgPrinter.exportToSvgScript(new JSONObject());
				}
			});
		});
	}
}

