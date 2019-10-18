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

import java.awt.Desktop;
import java.awt.Frame;
import java.io.BufferedWriter;
import java.io.File;
import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Objects;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.swing.*;

import gov.ca.water.calgui.bo.SimpleFileFilter;
import gov.ca.water.calgui.constant.Constant;
import gov.ca.water.calgui.constant.EpptPreferences;
import gov.ca.water.calgui.techservice.impl.FileSystemSvcImpl;
import gov.ca.water.plotly.PlotlyPrintException;
import javafx.application.Platform;
import javafx.beans.value.ObservableValue;
import javafx.concurrent.Worker;
import javafx.scene.control.ScrollPane;
import javafx.scene.layout.BorderPane;
import javafx.scene.web.WebView;
import netscape.javascript.JSException;
import netscape.javascript.JSObject;
import org.apache.commons.io.FileUtils;
import org.json.JSONArray;
import org.json.JSONObject;
import sun.misc.BASE64Decoder;

import static gov.ca.water.calgui.constant.Constant.ORCA_EXE;

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
		_webView.setContextMenuEnabled(true);
		_webView.getEngine().getLoadWorker().exceptionProperty().addListener(this::handleException);
		_webView.getEngine().getLoadWorker().stateProperty().addListener(this::callbackScript);
		_webView.setMinHeight(600);
		setPrefWidth(1200);
		_webView.setMinWidth(1000);
		setCenter(_webView);
		load(path);
	}

	private void callbackScript(ObservableValue<? extends Worker.State> observableValue, Worker.State state, Worker.State newValue)
	{
		JSObject document = (JSObject) _webView.getEngine().executeScript("window");
		if(document != null)
		{
			document.setMember("javaObj", new JavaApp());
		}
		if(newValue == Worker.State.SUCCEEDED)
		{
			executeScript(_callbackScript);
		}
		else if(newValue == Worker.State.CANCELLED)
		{
			handleCancel();
		}
	}

	private void handleCancel()
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
				LOGGER.log(Level.SEVERE, "Error decoding: " + s, e);
			}
			catch(IOException e)
			{
				LOGGER.log(Level.SEVERE, "Error saving file: " + _outputPath, e);
			}
		}
		else
		{
			LOGGER.log(Level.INFO, location);
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

	public static class JavaApp
	{

		public void interruptFunction(String format, Object dataJson, Object layoutJson)
		{
			if(format != null && dataJson != null && layoutJson != null)
			{
				try
				{
					JFileChooser fileChooser = new JFileChooser(EpptPreferences.getLastProjectConfiguration().toFile());
					fileChooser.setFileFilter(new SimpleFileFilter(format, "Export " + format + " file"));
					fileChooser.setMultiSelectionEnabled(false);
					fileChooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
					fileChooser.setDialogType(JFileChooser.OPEN_DIALOG);
					fileChooser.setSelectedFile(EpptPreferences.getLastProjectConfiguration().getParent().resolve("Export." + format).toFile());
					if(JFileChooser.APPROVE_OPTION == fileChooser.showSaveDialog(Frame.getFrames()[0]))
					{
						File selectedFile = fileChooser.getSelectedFile();
						Path outputPath = selectedFile.toPath();
						String jsonFilename = outputPath.getFileName().toString().replace(format, "json");
						Path jsonPath = outputPath.getParent().resolve(jsonFilename);
						if(!jsonPath.getFileName().toString().endsWith(".json"))
						{
							jsonPath = Paths.get(jsonPath.toString() + ".json");
						}
						if(!outputPath.getFileName().toString().endsWith("." + format))
						{
							outputPath = Paths.get(outputPath.toString() + "." + format);
						}
						writeToJson(jsonPath, dataJson, layoutJson);
						exportToFormat(jsonPath, outputPath, format);
					}
				}
				catch(InterruptedException e)
				{
					Thread.currentThread().interrupt();
					LOGGER.log(Level.SEVERE, "Error exporting to: " + format);
				}
				catch(IOException | RuntimeException e)
				{
					LOGGER.log(Level.SEVERE, "Error exporting to: " + format, e);
				}
			}
		}

		private void exportToFormat(Path jsonPath, Path outputPath, String format) throws IOException, InterruptedException
		{
			String orcaCommandline = "\"" + ORCA_EXE + "\" graph " + jsonPath + " --format " + format + " \"" + outputPath + "\"";
			LOGGER.log(Level.INFO, "Plotly SVG generation command line: {0}", orcaCommandline);
			Process exec = new ProcessBuilder()
					.directory(jsonPath.getParent().toFile())
					.inheritIO()
					.command(orcaCommandline)
					.start();
			exec.waitFor();
			Desktop.getDesktop().open(outputPath.toFile());
		}

		private void writeToJson(Path json, Object dataJson, Object layoutJson) throws IOException
		{
			if("undefined".equals(layoutJson) || "undefined".equals(dataJson))
			{
				throw new IOException("Unable to write JSON object with undefined Plotly data or layout");
			}
			JSONObject jsonObject = new JSONObject();
			jsonObject.put("layout", new JSONObject(layoutJson.toString()));
			JSONArray jsonArray = new JSONArray(dataJson.toString());
			jsonObject.put("data", jsonArray);
			try(BufferedWriter writer = Files.newBufferedWriter(json))
			{
				jsonObject.write(writer);
			}
		}
	}
}
