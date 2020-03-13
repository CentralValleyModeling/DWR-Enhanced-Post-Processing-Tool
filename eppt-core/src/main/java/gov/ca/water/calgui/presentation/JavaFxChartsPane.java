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

import java.awt.Container;
import java.awt.Cursor;
import java.awt.Desktop;
import java.awt.Frame;
import java.awt.Window;
import java.io.BufferedWriter;
import java.io.File;
import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.concurrent.CompletableFuture;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import javax.swing.*;

import gov.ca.water.calgui.bo.SimpleFileFilter;
import gov.ca.water.calgui.constant.Constant;
import gov.ca.water.calgui.constant.EpptPreferences;
import gov.ca.water.calgui.techservice.impl.DialogSvcImpl;
import javafx.application.Platform;
import javafx.beans.value.ObservableValue;
import javafx.concurrent.Worker;
import javafx.scene.layout.BorderPane;
import javafx.scene.web.WebView;
import netscape.javascript.JSException;
import netscape.javascript.JSObject;
import oracle.jrockit.jfr.JFR;
import org.apache.commons.io.FileUtils;
import org.json.JSONArray;
import org.json.JSONObject;
import sun.misc.BASE64Decoder;

import hec.util.TextUtil;
import rma.util.RMAIO;

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

		public void interruptFunction(String format, Object dataJson, Object layoutJson, Object width, Object height)
		{
			if(format != null && dataJson != null && layoutJson != null)
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
					export(format, dataJson, layoutJson, width, height, outputPath, jsonPath);
				}
			}
		}

		private void export(String format, Object dataJson, Object layoutJson, Object width, Object height, Path outputPath, Path jsonPath)
		{
			CompletableFuture.runAsync(() ->
			{
				try
				{
					JFrame frame = (JFrame) Frame.getFrames()[0];
					Container contentPane = frame.getContentPane();
					contentPane.setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
					writeToJson(jsonPath, dataJson, layoutJson);
					exportToFormat(jsonPath, outputPath, format, width, height);
				}
				catch(InterruptedException e)
				{
					Thread.currentThread().interrupt();
					LOGGER.log(Level.WARNING, "Error exporting to: " + format, e);
					DialogSvcImpl.getDialogSvcInstance().getOK("Error exporting to: " + format + "\n" + e.getMessage(),
							JOptionPane.WARNING_MESSAGE);
				}
				catch(IOException | RuntimeException e)
				{
					LOGGER.log(Level.SEVERE, "Error exporting to: " + format, e);
					LOGGER.log(Level.WARNING, "Error exporting to: " + format, e);
					DialogSvcImpl.getDialogSvcInstance().getOK("Error exporting to: " + format + "\n" + e.getMessage(),
							JOptionPane.WARNING_MESSAGE);
				}
				finally
				{
					JFrame frame = (JFrame) Frame.getFrames()[0];
					Container contentPane = frame.getContentPane();
					contentPane.setCursor(Cursor.getDefaultCursor());
				}
			});
		}

		private void exportToFormat(Path jsonPath, Path outputPath, String format, Object width, Object height)
				throws IOException, InterruptedException
		{
			String orcaCommandline = "\"" + ORCA_EXE + "\" graph \"" + jsonPath + "\" --width " + width + " --height " + height +
					" --format " + format + " \"" + outputPath + "\"" + " --verbose --plotly \"" +
					Paths.get(Constant.CONFIG_DIR).getParent().resolve("lib").resolve("plotly").resolve("dist").resolve("plotly.min.js") + "\"";
			LOGGER.log(Level.FINE, "Plotly SVG generation command line: {0}", orcaCommandline);
			Process exec = null;
			try
			{
				exec = new ProcessBuilder()
						.directory(jsonPath.getParent().toFile())
						.command(orcaCommandline)
						.start();
				exec.waitFor();
				if(outputPath.toFile().exists())
				{
					Desktop.getDesktop().open(outputPath.toFile());
				}
				else
				{
					throw new IOException("File was not created by Plotly: " + outputPath);
				}
			}
			finally
			{
				if(exec != null)
				{
					exec.destroyForcibly();
				}
			}
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
