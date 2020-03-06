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
abstract class PlotlyPane extends JFXPanel
{
	private JavaFxChartsPane _javaFxChartsPane;

	PlotlyPane(EpptReportingComputedSet epptReportingComputedSet)
	{

		Platform.setImplicitExit(false);
		Platform.runLater(() ->
		{
			BorderPane borderPane = new BorderPane();
			_javaFxChartsPane = new JavaFxChartsPane(Paths.get(Constant.CONFIG_DIR).resolve("plots").resolve(getHtmlPath()),
					"plot(" + epptReportingComputedSet.toJson() + ");");
			borderPane.setCenter(_javaFxChartsPane);
			setScene(new Scene(borderPane));
		});
	}

	abstract Path getHtmlPath();
}
