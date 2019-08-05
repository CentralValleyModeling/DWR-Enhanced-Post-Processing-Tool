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
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.logging.Level;
import java.util.logging.Logger;

import javafx.beans.value.ObservableValue;
import javafx.concurrent.Worker;
import javafx.scene.web.WebEvent;
import javafx.scene.web.WebView;
import org.json.JSONObject;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 05-22-2019
 */
class HighChartsSvgPrinter
{
	private static final Logger LOGGER = Logger.getLogger(HighChartsSvgPrinter.class.getName());
	private final WebView _webView;
	private Runnable _callback;
	private final AtomicBoolean _hasCallbackRun = new AtomicBoolean(false);

	HighChartsSvgPrinter()
	{
		_webView = new WebView();
		_webView.getEngine().getLoadWorker().exceptionProperty().addListener(this::handleException);
		_webView.getEngine().setOnAlert(this::handleAlert);
		_webView.getEngine().getLoadWorker().stateProperty().addListener(this::callbackScript);
	}

	private void handleAlert(WebEvent<String> event)
	{
		LOGGER.log(Level.INFO, "Alert {0} {1}", new Object[]{event, event.getData()});
	}

	private void callbackScript(ObservableValue<? extends Worker.State> observableValue, Worker.State state, Worker.State newValue)
	{
		if(newValue == Worker.State.SUCCEEDED)
		{
			handleCallback();
		}
	}

	private void handleCallback()
	{
		if(!_hasCallbackRun.getAndSet(true))
		{
			_callback.run();
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

	void load(Path url, Runnable callback)
	{
		if(url != null)
		{
			_hasCallbackRun.set(false);
			_callback = callback;
			try
			{
				_webView.getEngine().load(url.toUri().toString());
			}
			catch(RuntimeException ex)
			{
				LOGGER.log(Level.SEVERE, "Error loading script: " + url, ex);
			}
		}
	}

	String exportToSvgScript(JSONObject jsonObject)
	{
		Object o = _webView.getEngine().executeScript("plot(" + jsonObject.toString() + ")");
		return o.toString();
	}
}
