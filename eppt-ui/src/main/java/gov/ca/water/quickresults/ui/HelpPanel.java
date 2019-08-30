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

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.stream.Stream;

import gov.ca.water.calgui.constant.Constant;
import javafx.application.Platform;
import javafx.beans.value.ObservableValue;
import javafx.embed.swing.JFXPanel;
import javafx.scene.Scene;
import javafx.scene.control.ListView;
import javafx.scene.layout.BorderPane;
import javafx.scene.web.WebEngine;
import javafx.scene.web.WebView;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 08-20-2019
 */
public class HelpPanel extends JFXPanel
{
	private static final Logger LOGGER = Logger.getLogger(HelpPanel.class.getName());
	private WebEngine _engine;
	private ListView<Path> _listView;

	public HelpPanel()
	{
		Platform.setImplicitExit(false);
		Platform.runLater(this::init);
	}

	private void init()
	{
		_listView = new ListView<>();
		initHelpPages();
		WebView webView = new WebView();
		_engine = webView.getEngine();
		BorderPane borderPane = new BorderPane();
		borderPane.setCenter(webView);
		borderPane.setLeft(_listView);
		setScene(new Scene(borderPane));
		_listView.getSelectionModel().selectedItemProperty().addListener(this::changed);
		_listView.getSelectionModel().select(0);
	}

	private void changed(ObservableValue<? extends Path> observableValue, Path path, Path path1)
	{
		Path selectedItem = _listView.getSelectionModel().getSelectedItem();
		if(selectedItem != null)
		{
			load(selectedItem.toString());
		}
	}

	private void initHelpPages()
	{
		try
		{
			try(Stream<Path> help = Files.walk(Paths.get(Constant.DOCS_DIR).resolve("Help"), 1))
			{
				help.filter(p -> p.toString().endsWith("htm"))
					.map(Path::getFileName)
					.forEach(_listView.getItems()::add);
			}
		}
		catch(IOException e)
		{
			LOGGER.log(Level.SEVERE, "Unable to read help pages", e);
		}
	}

	public void loadPage(String page)
	{
		Platform.runLater(() -> _listView.getItems().stream()
										 .filter(p -> p.toString().toLowerCase().contains(page.toLowerCase()))
										 .forEach(_listView.getSelectionModel()::select));
	}

	private void load(String page)
	{
		Path path = Paths.get(Constant.DOCS_DIR).resolve("Help").resolve(page);
		_engine.load(path.toUri().toString());
	}
}
