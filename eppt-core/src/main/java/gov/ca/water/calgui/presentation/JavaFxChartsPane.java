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

package gov.ca.water.calgui.presentation;

import java.nio.file.Path;
import java.util.logging.Level;
import java.util.logging.Logger;

import javafx.application.Platform;
import javafx.beans.value.ObservableValue;
import javafx.concurrent.Worker;
import javafx.scene.layout.BorderPane;
import javafx.scene.web.WebView;
import netscape.javascript.JSException;
import netscape.javascript.JSObject;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 05-22-2019
 */
public class JavaFxChartsPane extends BorderPane
{
	private static final Logger LOGGER = Logger.getLogger(JavaFxChartsPane.class.getName());
	private final WebView _webView;
	private final String _callbackScript;

	public JavaFxChartsPane(Path path, String callbackScript)
	{
		_callbackScript = callbackScript;
		_webView = new WebView();
		_webView.setContextMenuEnabled(true);
		_webView.getEngine().getLoadWorker().exceptionProperty().addListener(this::handleException);
		_webView.getEngine().getLoadWorker().stateProperty().addListener(this::callbackScript);
		setCenter(_webView);
		load(path);
	}

	private void callbackScript(ObservableValue<? extends Worker.State> observableValue, Worker.State state, Worker.State newValue)
	{
		if(newValue == Worker.State.SUCCEEDED)
		{
			Object window = _webView.getEngine().executeScript("window");
			if(window != null)
			{
				Platform.runLater(()->
				{
					((JSObject)window).setMember("javaObj", new JavascriptImageExporter());
					executeScript("console.log = function(message) { javaObj.log(message); }");
				});
			}
			Platform.runLater(()->executeScript(_callbackScript));
		}
	}

	private void handleException(ObservableValue<? extends Throwable> j, Throwable o, Throwable e)
	{
		if(e != null)
		{
			LOGGER.log(Level.SEVERE, "Error rendering plot", e);
		}
		if(o != null)
		{
			LOGGER.log(Level.SEVERE, "Error rendering plot", o);
		}
	}

	private void load(Path url)
	{
		if(url != null)
		{
			_webView.getEngine().load(url.toUri().toString());
		}
	}

	private void executeScript(String script)
	{
		if(script != null)
		{
			LOGGER.log(Level.FINE, "Executing script: {0}", script);
			try
			{
				_webView.getEngine().executeScript(script);
			}
			catch(JSException ex)
			{
				LOGGER.log(Level.FINE, "Error in script", ex);
			}
		}
	}
}
