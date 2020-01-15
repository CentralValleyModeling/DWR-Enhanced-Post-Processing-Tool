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

package gov.ca.water.trendreporting;

import java.io.IOException;

import javafx.application.Application;
import javafx.scene.Scene;
import javafx.scene.layout.StackPane;
import javafx.scene.web.WebEngine;
import javafx.scene.web.WebView;
import javafx.stage.Stage;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 01-09-2020
 */
public class WebTest extends Application {
		@Override
		public void start(Stage stage) throws Exception {
			StackPane pane = new StackPane();
			WebView view = new WebView();

			WebEngine engine = view.getEngine();
			engine.load("http://css3test.com/");
			pane.getChildren().add(view);

			Scene scene = new Scene(pane, 960, 600);
			stage.setScene(scene);
			stage.show();
		}

		public static void main(String[] args) throws IOException
		{
			Application.launch(args);
		}
}
