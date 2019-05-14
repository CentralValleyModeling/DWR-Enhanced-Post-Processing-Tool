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

import java.awt.Color;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.swing.*;
import javax.swing.text.BadLocationException;
import javax.swing.text.Style;
import javax.swing.text.StyleConstants;
import javax.swing.text.StyledDocument;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 05-10-2019
 */
public class TextAreaPrintStream
{
	private static final Logger LOGGER = Logger.getLogger(TextAreaPrintStream.class.getName());
	private final JTextPane _textArea;
	private final ExecutorService _executorService;
	private boolean _stopThreads = false;
	//Used to print error messages in red
	private StyledDocument _doc;
	private Style _style;

	public TextAreaPrintStream(JTextPane textArea, InputStream inputStream, InputStream errorStream)
	{
		_textArea = textArea;
		_doc = (StyledDocument) textArea.getDocument();
		_style = _doc.addStyle("ConsoleStyle", null);
		StyleConstants.setFontFamily(_style, "MonoSpaced");
		StyleConstants.setFontSize(_style, 12);

		BufferedReader stdInput = new BufferedReader(new InputStreamReader(inputStream));

		final BufferedReader stdError = new BufferedReader(new InputStreamReader(errorStream));

		_executorService = Executors.newFixedThreadPool(2);
		StreamGobbler errorGobbler = new StreamGobbler(errorStream, "ERROR");

		// any output?
		StreamGobbler outputGobbler = new StreamGobbler(inputStream, "OUTPUT");

		// start gobblers
		_executorService.submit(outputGobbler);
		_executorService.submit(errorGobbler);
	}

	/**
	 * Stops the "_stdOutReader" threads
	 */
	public synchronized void close()
	{

		// Notify the threads that they must stop
		_stopThreads = true;
		this.notifyAll();
		_executorService.shutdownNow();
	}

	private void appendErrorText(String str)
	{
		SwingUtilities.invokeLater(() ->
		{
			StyleConstants.setForeground(_style, Color.red);
			appendText(str);
		});
	}

	private void appendNormalText(String str)
	{
		SwingUtilities.invokeLater(() ->
		{
			StyleConstants.setForeground(_style, Color.black);
			appendText(str);
		});
	}

	private void appendText(String str)
	{
		try
		{
			int length = _doc.getLength();
			_doc.insertString(length, "\n" + str, _style);
			_textArea.setCaretPosition(length + 1);
		}
		catch(BadLocationException e)
		{
			LOGGER.log(Level.SEVERE, "Error appending text", e);
		}
	}

	private final class StreamGobbler implements Runnable
	{

		private InputStream _is;
		private String _type;

		private StreamGobbler(InputStream is, String type)
		{
			this._is = is;
			this._type = type;
		}

		@Override
		public void run()
		{
			try
			{
				InputStreamReader isr = new InputStreamReader(_is);
				BufferedReader br = new BufferedReader(isr);
				String line = null;
				while((line = br.readLine()) != null && !_stopThreads)
				{
					String str = line;
					if("ERROR".equalsIgnoreCase(_type))
					{
						appendErrorText(str);
					}
					else
					{
						appendNormalText(str);
					}
				}
			}
			catch(IOException ioe)
			{
				LOGGER.log(Level.SEVERE, "Error processing WRESL Text", ioe);
			}
		}
	}

}
