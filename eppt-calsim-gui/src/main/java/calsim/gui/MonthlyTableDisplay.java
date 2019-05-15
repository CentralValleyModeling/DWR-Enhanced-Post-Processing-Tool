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

import java.awt.event.KeyEvent;
import javax.swing.*;
import javax.swing.text.BadLocationException;
import javax.swing.text.DefaultStyledDocument;
import javax.swing.text.Style;
import javax.swing.text.StyleConstants;
import javax.swing.text.StyleContext;
import javax.swing.text.StyledDocument;

import calsim.app.AppUtils;
import calsim.app.MonthlyReport;
import vista.set.DataReference;
import vista.set.RegularTimeSeries;

/**
 * Frame for monthly tables
 *
 * @author Nicky Sandhu
 * @version $Id: MonthlyTableDisplay.java,v 1.1.2.15 2001/07/12 01:59:52 amunevar Exp $
 */
public class MonthlyTableDisplay extends TextDisplay
{
	public static final boolean DEBUG = false;
	private static final String[] ITEM_TEXT =
			{
			"Table",
			"Graph"
	};
	private static final String[] TOOL_TIP_TEXT =
			{
			"Shows data in a table",
			"Shows data in a graph"
	};
	private static final int[] ITEM_KEYS = {
			KeyEvent.VK_T,
			KeyEvent.VK_G
	};
	private DataReference _ref;
	private DataReference[] _refs;
	private MonthlyReport _mr;
	private JMenuBar _mbar;

	/**
	 *
	 */
	public MonthlyTableDisplay(DataReference ref)
	{
		super();
		setFrameTitle("MONTHLY REPORT");
		_ref = ref;
		_mr = new MonthlyReport((RegularTimeSeries) _ref.getData(),
				_ref.getPathname(),
				_ref.getFilename()
		);
		addDocument(_mr.getStyledDocument());
	}

	/**
	 *
	 */
	public MonthlyTableDisplay(DataReference ref, boolean isWaterYear, boolean isStartMonth, String startMonth,
							   int[] years)
	{
		super();
		setFrameTitle("MONTHLY REPORT");
		_ref = ref;
		_mr = new MonthlyReport((RegularTimeSeries) _ref.getData(),
				_ref.getPathname(),
				_ref.getFilename(),
				isWaterYear,
				isStartMonth,
				startMonth,
				years);
		addDocument(_mr.getStyledDocument());
	}

	/**
	 *
	 */
	public MonthlyTableDisplay(DataReference[] refs, boolean isWaterYear, boolean isStartMonth, String startMonth,
							   int[] years)
	{
		super();
		setFrameTitle("MONTHLY REPORT");
		_refs = refs;
		StyledDocument doc = null;
		for(int i = 0; i < _refs.length; i++)
		{
			DataReference ref = _refs[i];
			if(ref == null)
			{
				continue;
			}
			MonthlyReport mr = new MonthlyReport((RegularTimeSeries) ref.getData(),
					ref.getPathname(),
					ref.getFilename(),
					isWaterYear,
					isStartMonth,
					startMonth,
					years);
			if(doc == null)
			{
				doc = mr.getStyledDocument();
			}
			else
			{
				doc = mr.appendTo(doc, 0);
			}
		}
		if(doc != null)
		{
			addDocument(doc);
		}
	}

	/**
	 *
	 */
	public MonthlyTableDisplay(String[] lines)
	{
		super();
		String lineSeparator = System.getProperty("line.separator");
		StyledDocument doc = new DefaultStyledDocument();
		StyleContext sc = new StyleContext();
		Style def = sc.getStyle(StyleContext.DEFAULT_STYLE);
		Style normal = sc.addStyle("Normal", def);
		StyleConstants.setFontSize(normal, 8);
		for(int i = 0; i < lines.length; i++)
		{
			try
			{
				doc.insertString(doc.getEndPosition().getOffset() - 1, lines[i] + lineSeparator, normal);
			}
			catch(BadLocationException ble)
			{
				System.err.println("Could not insert string for line: " + lines[i]);
			}
		}
		addDocument(doc);
	}

	/**
	 *
	 */
	@Override
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
	@Override
	public JMenuBar createJMenuBar()
	{
		int mindex = 0;
		JMenuItem tableItem = new JMenuItem(ITEM_TEXT[mindex]);
		tableItem.setToolTipText(TOOL_TIP_TEXT[mindex]);
		tableItem.setAccelerator(KeyStroke.getKeyStroke(ITEM_KEYS[mindex], KeyEvent.CTRL_MASK));
		tableItem.addActionListener(evt -> table());
		mindex++;
		JMenuItem graphItem = new JMenuItem(ITEM_TEXT[mindex]);
		graphItem.setToolTipText(TOOL_TIP_TEXT[mindex]);
		graphItem.setAccelerator(KeyStroke.getKeyStroke(ITEM_KEYS[mindex], KeyEvent.CTRL_MASK));
		graphItem.addActionListener(evt -> graph());
		_mbar = super.createJMenuBar();
		JMenu fileMenu = _mbar.getMenu(0);
		fileMenu.insert(tableItem, fileMenu.getItemCount() - 2);
		fileMenu.insert(graphItem, fileMenu.getItemCount() - 2);
		return _mbar;
	}

	/**
	 *
	 */
	void table()
	{
		if(DEBUG)
		{
			System.out.println("Table");
		}
		JFrame fr = null;
		if(_ref != null)
		{
			fr = AppUtils.tabulate(_ref);
		}
		else
		{
			fr = AppUtils.tabulate(_refs);
		}
		if(fr != null)
		{
			fr.setVisible(true);
		}
	}

	/**
	 *
	 */
	void graph()
	{
		if(DEBUG)
		{
			System.out.println("Graph");
		}
		JFrame fr = null;
		if(_ref != null)
		{
			fr = AppUtils.plot(_ref);
		}
		else
		{
			fr = AppUtils.plot(_refs);
		}
		if(fr != null)
		{
			fr.setVisible(true);
		}
	}
}
