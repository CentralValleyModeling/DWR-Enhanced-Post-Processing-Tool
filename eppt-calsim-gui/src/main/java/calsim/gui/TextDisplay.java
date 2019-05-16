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

package calsim.gui;

import java.awt.BorderLayout;
import java.awt.FileDialog;
import java.awt.event.KeyEvent;
import java.io.FileWriter;
import java.io.PrintWriter;
import javax.swing.*;
import javax.swing.text.BadLocationException;
import javax.swing.text.DefaultStyledDocument;
import javax.swing.text.Style;
import javax.swing.text.StyleConstants;
import javax.swing.text.StyleContext;
import javax.swing.text.StyledDocument;
import javax.swing.text.html.MinimalHTMLWriter;

import vista.gui.VistaUtils;

/**
 * Panel for monthly table display
 *
 * @author Nicky Sandhu
 * @version $Id: TextDisplay.java,v 1.1.2.15 2001/07/12 02:00:02 amunevar Exp $
 */
public class TextDisplay extends MPanel
{
	private static final boolean DEBUG = false;
	private static final String[] ITEM_TEXT =
			{
					"Save",
					"Save As Html",
					"Print",
					"Quit",
			};
	private static final String[] TOOL_TIP_TEXT =
			{
					"Saves to a text file",
					"Saves to a html file",
					"Prints as displayed",
					"Closes this display frame",
			};
	private static final int[] ITEM_KEYS =
			{
					KeyEvent.VK_S,
					KeyEvent.VK_H,
					KeyEvent.VK_P,
					KeyEvent.VK_Q,
			};
	private StyledDocument _doc;
	private String _frameTitle = "REPORT";
	private JMenuBar _mbar;

	/**
	 *
	 */
	protected TextDisplay()
	{
	}

	/**
	 *
	 */
	public TextDisplay(StyledDocument doc)
	{
		addDocument(doc);
	}

	/**
	 *
	 */
	public TextDisplay(String[] lines)
	{
		String lineSeparator = System.getProperty("line.separator");
		StyledDocument doc = new DefaultStyledDocument();
		StyleContext sc = new StyleContext();
		Style def = sc.getStyle(StyleContext.DEFAULT_STYLE);
		Style normal = sc.addStyle("Normal", def);
		StyleConstants.setFontSize(normal, 8);
		for(final String line : lines)
		{
			try
			{
				doc.insertString(doc.getEndPosition().getOffset() - 1, line + lineSeparator, normal);
			}
			catch(BadLocationException ble)
			{
				System.err.println("Could not insert string for line: " + line);
			}
		}
		addDocument(doc);
	}

	/**
	 * adds the given document to this frame
	 */
	void addDocument(StyledDocument doc)
	{
		_doc = doc;
		JTextPane jtp = new ReportPane();
		jtp.setEditable(false);
		jtp.setDocument(doc);
		setLayout(new BorderLayout());
		add(new JScrollPane(jtp), BorderLayout.CENTER);
	}

	/**
	 *
	 */
	public String getFrameTitle()
	{
		return _frameTitle;
	}

	/**
	 *
	 */
	public void setFrameTitle(String str)
	{
		_frameTitle = str;
	}

	/**
	 *
	 */
	public JMenuBar getJMenuBar()
	{
		if(_mbar == null)
		{
			_mbar = createJMenuBar();
		}
		return _mbar;
	}

	/**
	 *
	 */
	public JMenuBar createJMenuBar()
	{

		int mindex = 0;
		JMenuItem saveItem = new JMenuItem(ITEM_TEXT[mindex]);
		saveItem.setToolTipText(TOOL_TIP_TEXT[mindex]);
		saveItem.setAccelerator(KeyStroke.getKeyStroke(ITEM_KEYS[mindex], KeyEvent.CTRL_MASK));
		saveItem.addActionListener(evt -> save());
		mindex++;
		JMenuItem saveAsHtmlItem = new JMenuItem(ITEM_TEXT[mindex]);
		saveAsHtmlItem.setToolTipText(TOOL_TIP_TEXT[mindex]);
		saveAsHtmlItem.setAccelerator(KeyStroke.getKeyStroke(ITEM_KEYS[mindex], KeyEvent.CTRL_MASK));
		saveAsHtmlItem.addActionListener(evt -> saveAsHtml());
		mindex++;
		JMenuItem printItem = new JMenuItem(ITEM_TEXT[mindex]);
		printItem.setToolTipText(TOOL_TIP_TEXT[mindex]);
		printItem.setAccelerator(KeyStroke.getKeyStroke(ITEM_KEYS[mindex], KeyEvent.CTRL_MASK));
		printItem.addActionListener(evt -> print());
		mindex++;
		JMenuItem quitItem = new JMenuItem(ITEM_TEXT[mindex]);
		quitItem.setToolTipText(TOOL_TIP_TEXT[mindex]);
		quitItem.setAccelerator(KeyStroke.getKeyStroke(ITEM_KEYS[mindex], KeyEvent.CTRL_MASK));
		quitItem.addActionListener(evt -> quit());
		JMenu fileMenu = new JMenu("File");
		fileMenu.add(saveItem);
		fileMenu.add(saveAsHtmlItem);
		fileMenu.add(printItem);
		fileMenu.addSeparator();
		fileMenu.addSeparator();
		fileMenu.add(quitItem);
		//
		_mbar = new JMenuBar();
		_mbar.add(fileMenu);
		return _mbar;
	}

	/**
	 *
	 */
	void print()
	{
		if(DEBUG)
		{
			System.out.println("Print");
		}
		Style s = null;
		try
		{
			s = _doc.getStyle("main");
			StyleConstants.setFontSize(s,
					6);  //CB changed to 6, from 7, to fit add'l lines from extending hydrology on same page
			Style dateStyle = _doc.getStyle("date style");
			if(dateStyle != null)
			{
				StyleConstants.setFontSize(dateStyle, 5);
			}
			GuiUtils.print(GuiUtils.getComponent(JTextPane.class, this));
		}
		catch(Exception e)
		{
			VistaUtils.displayException(this, e);
		}
		finally
		{
			if(s != null)
			{
				s = _doc.getStyle("main");
				StyleConstants.setFontSize(s, 12);
				Style dateStyle = _doc.getStyle("date style");
				if(dateStyle != null)
				{
					StyleConstants.setFontSize(dateStyle, 5);
				}
			}
		}
	}

	/**
	 *
	 */
	void save()
	{
		if(_doc != null)
		{
			String saveFile = VistaUtils.getFilenameFromDialog(this, FileDialog.SAVE,
					"txt", "Text File");
			if(saveFile == null)
			{
				return;
			}
			try(PrintWriter writer = new PrintWriter(new FileWriter(saveFile)))
			{
				String txt = _doc.getText(0, _doc.getLength());
				java.util.StringTokenizer st =
						new java.util.StringTokenizer(txt, System.getProperty("line.separator"));
				while(st.hasMoreTokens())
				{
					String line = st.nextToken().trim();
					writer.println(line);
				}
			}
			catch(Exception e)
			{
				VistaUtils.displayException(this, e);
			}
		}
		if(DEBUG)
		{
			System.out.println("Save");
		}

	}

	/**
	 *
	 */
	void saveAsHtml()
	{
		if(_doc != null)
		{
			try
			{
				String saveFile = VistaUtils.getFilenameFromDialog(this, FileDialog.SAVE,
						"html", "HTML files");
				if(saveFile == null)
				{
					return;
				}
				FileWriter writer = new FileWriter(saveFile);
				new MinimalHTMLWriter(writer, _doc).write();
				writer.close();
			}
			catch(Exception e)
			{
				VistaUtils.displayException(this, e);
			}
		}
		if(DEBUG)
		{
			System.out.println("Save As Html");
		}
	}

	/**
	 *
	 */
	void quit()
	{
		if(DEBUG)
		{
			System.out.println("Quit");
		}
		JOptionPane.getFrameForComponent(this).dispose();
	}
}
