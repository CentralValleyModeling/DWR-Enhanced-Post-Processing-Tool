/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */

package calsim.gui;
//import calsim.app.*;

import java.awt.*;
import javax.swing.*;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;

//import java.awt.event.*;
//import java.io.*;
//import java.util.*;
//import javax.swing.text.*;
//import javax.swing.border.*;

/**
 * The panel for the control tab.
 *
 * @author Yan-Ping Zuo
 * @version $Id: ResultPanel.java,v 1.1.2.11 2001/07/12 01:59:57 amunevar Exp $
 */

public class ResultPanel extends JPanel
{
	public static boolean DEBUG = true;
	/**
	 *
	 */
	private JTextArea _textArea;

	/**
	 * Constructor
	 */
	public ResultPanel()
	{
		setLayout(new BorderLayout());
		add(createScrollPane(), BorderLayout.CENTER);
	}

	/**
	 * Create the center panel
	 */
	JScrollPane createScrollPane()
	{
		_textArea = new JTextArea(30, 80);
		_textArea.setEditable(true);
		_textArea.setLineWrap(true);
		_textArea.setWrapStyleWord(true);
		final JScrollPane areaScrollPane = new JScrollPane(_textArea);
		areaScrollPane.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_ALWAYS);
		// CB added to better show progress
		_textArea.getDocument().addDocumentListener(new DocumentListener()
		{
			public void insertUpdate(DocumentEvent e)
			{
				_textArea.setCaretPosition(_textArea.getDocument().getLength());
			}

			public void removeUpdate(DocumentEvent e)
			{
				_textArea.setCaretPosition(_textArea.getDocument().getLength());
			}

			public void changedUpdate(DocumentEvent e)
			{
			}
		});

		return areaScrollPane;
	}

	/**
	 * Get the text area
	 */
	public JTextArea getTextArea()
	{
		return _textArea;
	}
}


