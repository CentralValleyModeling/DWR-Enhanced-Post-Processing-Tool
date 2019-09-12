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

import java.io.File;
import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import java.util.logging.Level;
import java.util.logging.Logger;

import javafx.beans.value.ObservableValue;
import javafx.concurrent.Worker;
import javafx.scene.layout.BorderPane;
import javafx.scene.web.WebView;
import netscape.javascript.JSException;
import org.apache.commons.io.FileUtils;
import sun.misc.BASE64Decoder;

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
	private final Path _outputPath;

	public JavaFxChartsPane(Path path, String callbackScript)
	{
		_callbackScript = callbackScript;
		_outputPath = null;
		_webView = new WebView();
		_webView.getEngine().getLoadWorker().exceptionProperty().addListener(this::handleException);
		_webView.getEngine().getLoadWorker().stateProperty().addListener(this::callbackScript);
		setPrefWidth(900);
		setCenter(_webView);
		load(path);
	}

	public JavaFxChartsPane(Path path, String callbackScript, Path outputPath)
	{
		_callbackScript = callbackScript;
		_outputPath = outputPath;
		_webView = new WebView();
		_webView.getEngine().getLoadWorker().exceptionProperty().addListener(this::handleException);
		_webView.getEngine().getLoadWorker().stateProperty().addListener(this::callbackScript);
		setPrefWidth(900);
		setCenter(_webView);
		load(path);
	}

	private void callbackScript(ObservableValue<? extends Worker.State> observableValue, Worker.State state, Worker.State newValue)
	{
		LOGGER.log(Level.FINE, _webView.getEngine().getLocation());
		LOGGER.log(Level.FINE, newValue.toString());
		if(newValue == Worker.State.SUCCEEDED)
		{
			executeScript(_callbackScript);
		}
		else if(newValue == Worker.State.CANCELLED)
		{
			String location = _webView.getEngine().getLocation();
			if(location.contains("base64,"))
			{
				if(location.contains("pdf"))
				{
					handlePdf(location);
				}

			}
			else if(location.contains("svg+xml"))
			{
				String s = location.split("charset=UTF-8,")[1];
				String result = null;
				try
				{
					result = java.net.URLDecoder.decode(s, StandardCharsets.UTF_8.name());
					FileUtils.writeStringToFile(new File(_outputPath.toString() + ".svg"), result, StandardCharsets.UTF_8.name());
				}
				catch(UnsupportedEncodingException e)
				{
					e.printStackTrace();
				}
				catch(IOException e)
				{
					e.printStackTrace();
				}
			}
			else
			{
				LOGGER.log(Level.INFO, location);
			}
		}
	}

	private void handlePdf(String location)
	{
		try
		{
			LOGGER.log(Level.INFO, location);
			String filename = location.split("filename=")[1].split(";")[0];
			String byteData = location.split("base64,")[1];
			BASE64Decoder decoder = new BASE64Decoder();
			byte[] bytes = decoder.decodeBuffer(byteData);
			LOGGER.log(Level.INFO, byteData);
			File file = new File(filename);
			FileUtils.writeByteArrayToFile(file, bytes, false);
		}
		catch(IOException e)
		{
			LOGGER.log(Level.SEVERE, "Error writing PDF", e);
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
