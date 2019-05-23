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

import java.net.URL;
import java.util.logging.Level;
import java.util.logging.Logger;

import javafx.beans.value.ObservableValue;
import javafx.scene.layout.BorderPane;
import javafx.scene.web.WebView;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 05-22-2019
 */
public class JavascriptPanel extends BorderPane
{
	private static final Logger LOGGER = Logger.getLogger(JavascriptPanel.class.getName());
	private final WebView _webView;
	private final String _callbackScript;

	public JavascriptPanel(String url, String callbackScript)
	{
		_callbackScript = callbackScript;
		_webView = new WebView();
		_webView.getEngine().getLoadWorker().exceptionProperty().addListener(this::handleException);
		setCenter(_webView);
		load(url);
		_webView.getEngine().getLoadWorker().workDoneProperty().addListener(this::callbackScript);
	}

	private void callbackScript(ObservableValue<? extends Number> j, Number o, Number n)
	{
		boolean running = _webView.getEngine().getLoadWorker().isRunning();
		if(!running)
		{
			executeScript(_callbackScript);
		}
	}

	private void handleException(ObservableValue<? extends Throwable> j, Throwable o, Throwable e)
	{
		if(e != null)
		{
			LOGGER.log(Level.SEVERE, "Error rendering plot", e);
		}
	}

	private void load(String url)
	{
		URL resource = Thread.currentThread().getContextClassLoader().getResource(url);
		if(resource != null)
		{
			_webView.getEngine().load(resource.toString());
		}
	}

	private void executeScript(String script)
	{
		_webView.getEngine().executeScript(script);
	}
}
