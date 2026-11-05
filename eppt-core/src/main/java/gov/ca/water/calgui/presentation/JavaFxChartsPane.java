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
	private final Object[] _renderArgs;
	//the WebView only holds a weak reference to objects handed to javascript, so keep the bridge alive for the life of the pane
	private final JavascriptImageExporter _javaBridge = new JavascriptImageExporter();

	/**
	 * @param path       html page to load
	 * @param plotJson   plot data serialized as JSON; handed to the page's {@code renderFromJava} function, which parses it
	 *                   and forwards it to the page's {@code plot} function
	 * @param extraArgs  additional (primitive) arguments forwarded to the page's {@code plot} function
	 */
	public JavaFxChartsPane(Path path, String plotJson, Object... extraArgs)
	{
		_renderArgs = new Object[extraArgs.length + 1];
		_renderArgs[0] = plotJson;
		System.arraycopy(extraArgs, 0, _renderArgs, 1, extraArgs.length);
		_webView = new WebView();
		_webView.setContextMenuEnabled(true);
		_webView.getEngine().getLoadWorker().exceptionProperty().addListener(this::handleException);
		_webView.getEngine().getLoadWorker().stateProperty().addListener(this::pageLoaded);
		setCenter(_webView);
		load(path);
	}

	private void pageLoaded(ObservableValue<? extends Worker.State> observableValue, Worker.State state, Worker.State newValue)
	{
		if(newValue == Worker.State.SUCCEEDED)
		{
			callJavascript("connectJava", _javaBridge);
			callJavascript("renderFromJava", _renderArgs);
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

	private void callJavascript(String function, Object... args)
	{
		try
		{
			((JSObject) _webView.getEngine().executeScript("window")).call(function, args);
		}
		catch(JSException ex)
		{
			LOGGER.log(Level.WARNING, "Error calling javascript function: " + function, ex);
		}
	}
}
