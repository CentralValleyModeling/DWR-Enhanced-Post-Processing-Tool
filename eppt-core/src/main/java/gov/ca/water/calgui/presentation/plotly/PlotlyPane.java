/*
 * Enhanced Post Processing Tool (EPPT) Copyright (c) 2020.
 *
 * EPPT is copyrighted by the State of California, Department of Water Resources. It is licensed
 * under the GNU General Public License, version 2. This means it can be
 * copied, distributed, and modified freely, but you may not restrict others
 * in their ability to copy, distribute, and modify it. See the license below
 * for more details.
 *
 * GNU General Public License
 */

package gov.ca.water.calgui.presentation.plotly;

import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.logging.Level;
import java.util.logging.Logger;

import gov.ca.water.calgui.busservice.impl.EpptReportingComputedSet;
import gov.ca.water.calgui.constant.Constant;
import gov.ca.water.calgui.presentation.JavaFxChartsPane;
import javafx.application.Platform;
import javafx.embed.swing.JFXPanel;
import javafx.scene.Scene;
import javafx.scene.layout.BorderPane;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 01-14-2020
 */
public abstract class PlotlyPane extends JFXPanel
{
	private final EpptReportingComputedSet _epptReportingComputedSet;
	private JavaFxChartsPane _javaFxChartsPane;
	private boolean _initialized;

	PlotlyPane(EpptReportingComputedSet epptReportingComputedSet)
	{
		Platform.setImplicitExit(false);
		_epptReportingComputedSet = epptReportingComputedSet;
	}

	public void initPlot()
	{
		if(!_initialized)
		{
			_initialized = true;
			Platform.runLater(() ->
			{
				try
				{
					BorderPane borderPane = new BorderPane();
					_javaFxChartsPane = new JavaFxChartsPane(Paths.get(Constant.CONFIG_DIR).resolve("plots").resolve(getHtmlPath()),
							"plot(" + _epptReportingComputedSet.toJson().toString(0) + ");");
					borderPane.setCenter(_javaFxChartsPane);
					setScene(new Scene(borderPane));
				}
				catch(RuntimeException e)
				{
					Logger.getLogger(PlotlyPane.class.getName()).log(Level.SEVERE, "Error generating plot: " + getHtmlPath(), e);
				}
			});
		}
	}

	abstract Path getHtmlPath();
}
