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

package gov.ca.water.quickresults.ui;

import java.nio.file.Path;
import java.nio.file.Paths;

import gov.ca.water.calgui.constant.Constant;
import javafx.application.Platform;
import javafx.embed.swing.JFXPanel;
import javafx.scene.Scene;
import javafx.scene.layout.BorderPane;
import javafx.scene.web.WebView;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 09-12-2019
 */
public class HelpPanel extends JFXPanel
{
	private WebView _webView;

	public HelpPanel()
	{
		Platform.setImplicitExit(false);
		Platform.runLater(this::init);
	}

	private void init()
	{
		Path path = getCoverPagePath();
		_webView = new WebView();
		BorderPane borderPane = new BorderPane();
		borderPane.setCenter(_webView);
		setScene(new Scene(borderPane));
		_webView.getEngine().load(path.toUri().toString());
	}

	private Path getCoverPagePath()
	{
		return Paths.get(Constant.DOCS_DIR).toAbsolutePath().getParent().resolve("docs").resolve("Help").resolve("Responsive HTML5").resolve(
				"coverpage.htm");
	}

	public void loadHelp(String helpId)
	{
		Platform.runLater(() ->
		{
			_webView.getEngine().load(getCoverPagePath().toUri().toString() + "#t=" + helpId);
		});
	}
}
