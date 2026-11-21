/*
 * Enhanced Post Processing Tool (EPPT) Copyright (c) 2026.
 *
 * EPPT is copyrighted by the State of California, Department of Water Resources. It is licensed
 * under the GNU General Public License, version 2. This means it can be
 * copied, distributed, and modified freely, but you may not restrict others
 * in their ability to copy, distribute, and modify it. See the license below
 * for more details.
 *
 * GNU General Public License
 */

package gov.ca.water.plotly;

import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import java.util.logging.Level;
import java.util.logging.Logger;

import javafx.application.Platform;
import javafx.concurrent.Worker;
import javafx.scene.Scene;
import javafx.scene.web.WebView;
import netscape.javascript.JSObject;

/**
 * Renders plotly figure JSON files to SVG with plotly.js running in a JavaFX WebView that is never shown.
 */
final class WebViewSvgRenderer
{
	private static final Logger LOGGER = Logger.getLogger(WebViewSvgRenderer.class.getName());
	private static final String TIMEOUT_PROPERTY = "eppt.plotly.renderTimeoutSeconds";
	private static final int DEFAULT_TIMEOUT_SECONDS = 60;
	private static final String PAGE = "<!DOCTYPE html><html><head><meta charset=\"utf-8\"><script src=\"%s\"></script></head><body></body></html>";
	private static final String RENDER_SCRIPT = "plotly_svg_render.js";

	private final Path _plotlyJs;
	private final long _timeoutSeconds;
	private WebView _webView;
	//the WebView only holds a weak reference to objects handed to javascript
	private Callback _pending;

	WebViewSvgRenderer(Path plotlyJs)
	{
		_plotlyJs = plotlyJs;
		_timeoutSeconds = Math.max(1, Integer.getInteger(TIMEOUT_PROPERTY, DEFAULT_TIMEOUT_SECONDS));
	}

	/**
	 * Writes a same-named .svg next to each JSON file. Figures that fail are logged and skipped, then reported together.
	 */
	void render(List<Path> jsonFiles) throws PlotlyPrintException, InterruptedException
	{
		if(Platform.isFxApplicationThread())
		{
			throw new IllegalStateException("Plotly SVG rendering waits on the JavaFX thread and cannot be called from it");
		}
		if(!Files.isRegularFile(_plotlyJs))
		{
			throw new PlotlyPrintException("Plotly library not found: " + _plotlyJs);
		}
		String renderScript = readRenderScript();
		startToolkit();
		Path page = null;
		try
		{
			page = Files.createTempFile("eppt-plotly-render", ".html");
			Files.write(page, String.format(PAGE, _plotlyJs.toUri()).getBytes(StandardCharsets.UTF_8));
			loadPage(page, renderScript);
			List<String> failures = new ArrayList<>();
			long start = System.nanoTime();
			for(int i = 0; i < jsonFiles.size(); i++)
			{
				Path jsonFile = jsonFiles.get(i);
				try
				{
					renderFigure(jsonFile);
				}
				catch(IOException | PlotlyPrintException e)
				{
					if(e.getCause() instanceof TimeoutException)
					{
						//the page is stuck, so the remaining figures would each wait out the timeout too
						throw (PlotlyPrintException) e;
					}
					LOGGER.log(Level.SEVERE, "Plotly SVG generation failed for " + jsonFile, e);
					failures.add(jsonFile.getFileName() + ": " + e.getMessage());
				}
				LOGGER.log(Level.FINE, "Plotly SVG generation: {0} of {1} done", new Object[]{i + 1, jsonFiles.size()});
			}
			LOGGER.log(Level.INFO, "Plotly SVG generation: {0} of {1} images rendered in {2} ms",
					new Object[]{jsonFiles.size() - failures.size(), jsonFiles.size(),
							TimeUnit.NANOSECONDS.toMillis(System.nanoTime() - start)});
			if(!failures.isEmpty())
			{
				throw new PlotlyPrintException("Unable to create " + failures.size() + " of " + jsonFiles.size() + " plots:\n"
						+ String.join("\n", failures));
			}
		}
		catch(IOException e)
		{
			throw new PlotlyPrintException("Unable to create plotly render page", e);
		}
		finally
		{
			dispose();
			deleteQuietly(page);
		}
	}

	private static String readRenderScript() throws PlotlyPrintException
	{
		try(InputStream in = WebViewSvgRenderer.class.getResourceAsStream(RENDER_SCRIPT))
		{
			if(in == null)
			{
				throw new PlotlyPrintException("Missing resource: " + RENDER_SCRIPT);
			}
			return new String(in.readAllBytes(), StandardCharsets.UTF_8);
		}
		catch(IOException e)
		{
			throw new PlotlyPrintException("Unable to read resource: " + RENDER_SCRIPT, e);
		}
	}

	private static void deleteQuietly(Path page)
	{
		if(page != null)
		{
			try
			{
				Files.deleteIfExists(page);
			}
			catch(IOException e)
			{
				LOGGER.log(Level.WARNING, "Unable to delete plotly render page: " + page, e);
			}
		}
	}

	private static void startToolkit()
	{
		//the app starts the toolkit during module install; standalone callers such as tests start it here
		Platform.setImplicitExit(false);
		try
		{
			Platform.startup(() ->
			{
			});
		}
		catch(IllegalStateException alreadyStarted)
		{
			LOGGER.log(Level.FINEST, "JavaFX toolkit already started", alreadyStarted);
		}
	}

	private void loadPage(Path page, String renderScript) throws PlotlyPrintException, InterruptedException
	{
		CompletableFuture<Void> loaded = new CompletableFuture<>();
		Platform.runLater(() ->
		{
			try
			{
				_webView = new WebView();
				//never shown, the scene only gives the WebView a size
				new Scene(_webView, 800, 600);
				_webView.getEngine().getLoadWorker().stateProperty().addListener((obs, oldState, newState) ->
				{
					if(newState == Worker.State.SUCCEEDED)
					{
						try
						{
							_webView.getEngine().executeScript(renderScript);
							loaded.complete(null);
						}
						catch(RuntimeException e)
						{
							loaded.completeExceptionally(e);
						}
					}
					else if(newState == Worker.State.FAILED)
					{
						loaded.completeExceptionally(new IOException("Unable to load plotly render page",
								_webView.getEngine().getLoadWorker().getException()));
					}
				});
				_webView.getEngine().load(page.toUri().toString());
			}
			catch(RuntimeException e)
			{
				loaded.completeExceptionally(e);
			}
		});
		await(loaded, "loading plotly");
	}

	private void renderFigure(Path jsonFile) throws IOException, PlotlyPrintException, InterruptedException
	{
		String json = new String(Files.readAllBytes(jsonFile), StandardCharsets.UTF_8);
		Callback callback = new Callback();
		_pending = callback;
		Platform.runLater(() ->
		{
			try
			{
				JSObject window = (JSObject) _webView.getEngine().executeScript("window");
				window.call("renderFigure", json, callback);
			}
			catch(RuntimeException e)
			{
				callback._result.completeExceptionally(e);
			}
		});
		String svg = await(callback._result, "rendering " + jsonFile.getFileName());
		String svgName = jsonFile.getFileName().toString().replaceAll("\\.json$", "") + ".svg";
		Files.write(jsonFile.resolveSibling(svgName), svg.getBytes(StandardCharsets.UTF_8));
	}

	private <T> T await(CompletableFuture<T> future, String task) throws PlotlyPrintException, InterruptedException
	{
		try
		{
			return future.get(_timeoutSeconds, TimeUnit.SECONDS);
		}
		catch(TimeoutException e)
		{
			throw new PlotlyPrintException("Timed out after " + _timeoutSeconds + " s " + task
					+ " (raise with -D" + TIMEOUT_PROPERTY + ")", e);
		}
		catch(ExecutionException e)
		{
			throw new PlotlyPrintException("Error " + task + ": " + e.getCause().getMessage(), e.getCause());
		}
	}

	private void dispose()
	{
		WebView webView = _webView;
		_webView = null;
		_pending = null;
		if(webView != null)
		{
			Platform.runLater(() -> webView.getEngine().load(null));
		}
	}

	/**
	 * Called from javascript, so it must be public.
	 */
	public static final class Callback
	{
		private final CompletableFuture<String> _result = new CompletableFuture<>();

		public void done(String svg)
		{
			_result.complete(svg);
		}

		public void failed(String error)
		{
			_result.completeExceptionally(new PlotlyPrintException(error));
		}
	}
}
