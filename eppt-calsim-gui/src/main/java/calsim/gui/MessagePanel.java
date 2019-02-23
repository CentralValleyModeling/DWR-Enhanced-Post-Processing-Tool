/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */

package calsim.gui;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.GridLayout;
import java.awt.Insets;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.util.Vector;
import javax.swing.*;

import calsim.app.AppUtils;
import vista.time.TimeWindow;
//import vista.gui.*;
//import java.io.*;

/**
 * The message panel in the main panel of Calsim GUI
 *
 * @author Yan-Ping Zuo
 * @version $Id: MessagePanel.java,v 1.1.2.18 2001/10/23 16:28:43 jfenolio Exp $
 */

public class MessagePanel
{
	private static final boolean DEBUG = false;

	private static String[] labelText =
			{
					" Project Name: ", " Mode:  ",
					" TW:  ", " Base Files ", " DV:    ", "               SV: ",
					" Comp Files 1", " DV:    ", "               SV: ",
					"           Units:  ", " Comp Files 2", " DV:    ",
					"               SV: ", " Comp Files 3", " DV:    ",
					"               SV: ", "  View:  "
			};
	private static JTextField[] _messages;
	private Font _font;
	private JButton _basedv = new JButton("DV: ");
	private JButton _basesv = new JButton("  SV: ");
	private JButton _comp1dv = new JButton("DV: ");
	private JButton _comp1sv = new JButton("  SV: ");
	private JButton _comp2dv = new JButton("DV: ");
	private JButton _comp2sv = new JButton("  SV: ");
	private JButton _comp3dv = new JButton("DV: ");
	private JButton _comp3sv = new JButton("  SV: ");
	private JCheckBox _basebox = new JCheckBox();
	private JCheckBox _comp1box = new JCheckBox();
	private JCheckBox _comp2box = new JCheckBox();
	private JCheckBox _comp3box = new JCheckBox();
	private final ItemListener _listener = e -> {
		if(e.getSource() == _basebox)
		{
			AppUtils.baseOn = _basebox.isSelected();
			if(DEBUG)
			{
				System.out.println(AppUtils.baseOn);
			}
		}
		if(e.getSource() == _comp1box)
		{
			AppUtils.comp1On = _comp1box.isSelected();
			if(DEBUG)
			{
				System.out.println(AppUtils.comp1On);
			}
		}
		if(e.getSource() == _comp2box)
		{
			AppUtils.comp2On = _comp2box.isSelected();
			if(DEBUG)
			{
				System.out.println(AppUtils.comp2On);
			}
		}
		if(e.getSource() == _comp3box)
		{
			AppUtils.comp3On = _comp3box.isSelected();
			if(DEBUG)
			{
				System.out.println(AppUtils.comp3On);
			}
		}
	};
	private JRadioButton _taf = new JRadioButton("TAF");
	private JRadioButton _cfs = new JRadioButton("CFS");
	private JRadioButton _base = new JRadioButton("Base");
	private JRadioButton _comp = new JRadioButton("Comp");
	private JRadioButton _diff = new JRadioButton("Diff");
	private JCheckBox _plot = new JCheckBox("Plot");
	private JCheckBox _table = new JCheckBox("Table");
	private JCheckBox _monthly = new JCheckBox("Monthly");
	/*
	 * private variables
	 */
	private JLabel[] _labelNames;
	private Vector _twitems = new Vector(1, 1);
	private JPanel _messagePanelComp;
	private MainPanel _mp;
	private JFrame _frame;

	/**
	 * constructor
	 */
	public MessagePanel(JFrame frame, MainPanel mp)
	{
		_frame = frame;
		_mp = mp;
		_messagePanelComp = createMessagePanel();
	}

	/**
	 * Alternate constructor added to handle CalLite GUI needs
	 *
	 * @param fr
	 */
	public MessagePanel(JFrame fr)
	{
		_frame = fr;
		_mp = null;
		_messagePanelComp = createMessagePanel();

	}

	static void setDtsMasterMessage(String str)
	{
		_messages[9].setText(str);
	}

	static String getDtsMessage()
	{
		return _messages[9].getText();
	}

	/**
	 * create active messages panel
	 */
	JPanel createMessagePanel()
	{
		int fontSize = 10;
		int numOfMessages = 17;
		int numOfPanels = 5;
		JPanel panel = new JPanel();
		JPanel panel1 = new JPanel();
		JPanel[] panels = new JPanel[numOfPanels];
		_labelNames = new JLabel[labelText.length];
		_messages = new JTextField[numOfMessages];
		for(int i = 0; i < numOfPanels; i++)
		{
			panels[i] = new JPanel();
		}
		for(int i = 0; i < labelText.length; i++)
		{
			_labelNames[i] = new JLabel(labelText[i]);
			_font = _labelNames[i].getFont();
			_font = new Font(_font.getName(), _font.getStyle(), fontSize);
			_labelNames[i].setFont(_font);
			_labelNames[i].setForeground(Color.black);
			if(i != 4 || i != 7)
			{
				_labelNames[i].setSize(10, 10);
			}
			else
			{
				_labelNames[i].setSize(10, 10);
			}
		}
		for(int i = 0; i < numOfMessages; i++)
		{
			_messages[i] = new JTextField(30);
			_messages[i].setBackground(Color.white);
			_messages[i].setEditable(false);
			Font font = _messages[i].getFont();
			font = new Font(font.getName(), font.getStyle(), fontSize);
			_messages[i].setFont(font);
			_messages[i].setMinimumSize(_messages[i].getPreferredSize());
			_messages[i].setHorizontalAlignment(JTextField.CENTER);
		}
		Dimension d = new Dimension(0, 0);
		_basedv.addActionListener(new GuiTaskListener("Retrieving DV File...")
		{
			public void doWork()
			{
				MainMenuBar.openBaseDVFile();
			}
		});
		_basedv.setFont(_font);
		_basedv.setMaximumSize(d);
		_basesv.setFont(_font);
		_basesv.setSize(8, 7);
		_basesv.addActionListener(new GuiTaskListener("Retrieving SV File...")
		{
			public void doWork()
			{
				MainMenuBar.openBaseSVFile();
			}
		});
		_comp1dv.addActionListener(new GuiTaskListener("Retrieving DV File...")
		{
			public void doWork()
			{
				MainMenuBar.openCompDVFile();
			}
		});
		_comp1dv.setFont(_font);
		_comp1dv.setSize(8, 7);
		_comp1sv.setFont(_font);
		_comp1sv.setSize(8, 7);
		_comp1sv.addActionListener(new GuiTaskListener("Retrieving SV File...")
		{
			public void doWork()
			{
				MainMenuBar.openCompSVFile();
			}
		});
		_comp2dv.addActionListener(new GuiTaskListener("Retrieving DV File...")
		{
			public void doWork()
			{
				MainMenuBar.openComp2DVFile();
			}
		});
		_comp2dv.setFont(_font);
		_comp2dv.setSize(8, 7);
		_comp2sv.setFont(_font);
		_comp2sv.setSize(8, 7);
		_comp2sv.addActionListener(new GuiTaskListener("Retrieving SV File...")
		{
			public void doWork()
			{
				MainMenuBar.openComp2SVFile();
			}
		});
		_comp3dv.addActionListener(new GuiTaskListener("Retrieving DV File...")
		{
			public void doWork()
			{
				MainMenuBar.openComp3DVFile();
			}
		});
		_comp3dv.setFont(_font);
		_comp3dv.setSize(8, 7);
		_comp3sv.setFont(_font);
		_comp3sv.setSize(8, 7);
		_comp3sv.addActionListener(new GuiTaskListener("Retrieving SV File...")
		{
			public void doWork()
			{
				MainMenuBar.openComp3SVFile();
			}
		});
		_basebox.addItemListener(_listener);
		_comp1box.addItemListener(_listener);
		_comp2box.addItemListener(_listener);
		_comp3box.addItemListener(_listener);
		JLabel name = new JLabel(" Dts Tree: ");
		Font font = _messages[0].getFont();
		name.setFont(new Font(font.getName(), font.getStyle(), 10));
		//
		setModeMessage(AppUtils.BASE);
		_plot.setSelected(AppUtils.VIEW_GRAPH);
		_table.setSelected(AppUtils.VIEW_TABLE);
		_monthly.setSelected(AppUtils.viewMonthlyTable);
		_basebox.setSelected(AppUtils.baseOn);
		_comp1box.setSelected(AppUtils.comp1On);
		_comp2box.setSelected(AppUtils.comp2On);
		_comp3box.setSelected(AppUtils.comp3On);
		setTimeWindowMessage("");
		GridBagLayout gb = new GridBagLayout();
		GridBagConstraints gc = new GridBagConstraints();
		panel.setLayout(gb);
		gc.anchor = GridBagConstraints.NORTHWEST;
		gc.gridx = 0;
		gc.gridy = 0;
		panel.add(_labelNames[0], gc);

		gc.gridx = 1;
		gc.insets = new Insets(0, 2, 0, 3);
		panel.add(new JLabel("   "), gc);

		gc.gridx = 2;
		panel.add(new JLabel("     "), gc);

		gc.gridx = 3;
		gc.weightx = 1.0;
		panel.add(_messages[0], gc);

		gc.gridx = 4;
		panel.add(name, gc);

		gc.gridx = 5;
		gc.weightx = 1.0;
		panel.add(_messages[9], gc);

		gc.anchor = GridBagConstraints.WEST;
		gc.gridx = 0;
		gc.gridy = 1;
		panel.add(_labelNames[3], gc);

		gc.gridx = 1;
		gc.insets = new Insets(0, 2, 0, 3);
		panel.add(_basebox, gc);

		gc.gridx = 2;
		panel.add(_basedv, gc);

		gc.gridx = 3;
		gc.weightx = 1.0;
		panel.add(_messages[3], gc);

		gc.gridx = 4;
		gc.insets = new Insets(0, 2, 0, 3);
		panel.add(_basesv, gc);

		gc.gridx = 5;
		gc.weightx = 1.0;
		panel.add(_messages[4], gc);

		gc.anchor = GridBagConstraints.WEST;
		gc.gridx = 0;
		gc.gridy = 2;
		panel.add(_labelNames[6], gc);

		gc.gridx = 1;
		gc.insets = new Insets(0, 2, 0, 3);
		panel.add(_comp1box, gc);

		gc.gridx = 2;
		panel.add(_comp1dv, gc);

		gc.gridx = 3;
		gc.weightx = 1.0;
		panel.add(_messages[5], gc);

		gc.gridx = 4;
		gc.insets = new Insets(0, 2, 0, 3);
		panel.add(_comp1sv, gc);

		gc.gridx = 5;
		gc.gridwidth = GridBagConstraints.REMAINDER;
		gc.weightx = 1.0;
		panel.add(_messages[6], gc);

		gc.anchor = GridBagConstraints.WEST;
		gc.gridx = 0;
		gc.gridy = 3;
		panel.add(_labelNames[10], gc);

		gc.gridx = 1;
		gc.insets = new Insets(0, 2, 0, 2);
		panel.add(_comp2box, gc);

		gc.gridx = 2;
		panel.add(_comp2dv, gc);

		gc.gridx = 3;
		gc.weightx = 1.0;
		panel.add(_messages[10], gc);

		gc.gridx = 4;
		gc.insets = new Insets(0, 2, 0, 2);
		panel.add(_comp2sv, gc);

		gc.gridx = 5;
		gc.gridwidth = GridBagConstraints.REMAINDER;
		gc.weightx = 1.0;
		panel.add(_messages[11], gc);

		gc.anchor = GridBagConstraints.WEST;
		gc.gridx = 0;
		gc.gridy = 4;
		panel.add(_labelNames[13], gc);

		gc.gridx = 1;
		gc.insets = new Insets(0, 2, 0, 2);
		panel.add(_comp3box, gc);

		gc.gridx = 2;
		panel.add(_comp3dv, gc);

		gc.gridx = 3;
		gc.weightx = 1.0;
		panel.add(_messages[12], gc);

		gc.gridx = 4;
		gc.insets = new Insets(0, 2, 0, 2);
		panel.add(_comp3sv, gc);

		gc.gridx = 5;
		gc.gridwidth = GridBagConstraints.REMAINDER;
		gc.weightx = 1.0;
		panel.add(_messages[13], gc);

		panel.setBackground(new Color(229, 240, 203));

		panel1.setBorder(BorderFactory.createTitledBorder(
				BorderFactory.createMatteBorder(1, 1, 1, 1, Color.blue),
				"Message Panel"));
		panel1.setLayout(new GridBagLayout());
		panel1.setBackground(new Color(229, 240, 203));
		gc.anchor = GridBagConstraints.NORTHWEST;
		gc.gridx = 0;
		gc.gridy = 0;
		panel1.add(panel, gc);
		gc.anchor = GridBagConstraints.WEST;
		gc.insets = new Insets(3, 0, 0, 0);
		gc.gridx = 0;
		gc.gridy = 1;
		panel1.add(createUtilsPanel(), gc);
		NodeArcMenuBar nodeArcMenuBar = new NodeArcMenuBar(_mp);
		_frame.setJMenuBar(nodeArcMenuBar.getMenuBar());
		return panel1;
	}

	private JPanel createUtilsPanel()
	{
		JPanel panel = new JPanel();
		GridBagLayout gb = new GridBagLayout();
		GridBagConstraints gc = new GridBagConstraints();
		panel.setLayout(gb);
		gc.anchor = GridBagConstraints.WEST;
		gc.gridx = 0;
		gc.gridy = 0;
		panel.add(_labelNames[1], gc);
		gc.gridx = 1;
		panel.add(createModePanel(), gc);
		gc.gridx = 2;
		panel.add(_labelNames[16], gc);
		gc.gridx = 3;
		panel.add(createViewPanel(), gc);
		gc.gridx = 4;
		panel.add(new JLabel("              "), gc);
		gc.gridx = 5;
		panel.add(_labelNames[2], gc);
		gc.gridx = 6;
		panel.add(createTWBox(), gc);
		gc.gridx = 7;
		panel.add(_labelNames[9], gc);
		gc.gridx = 8;
		panel.add(createUnitsPanel(), gc);
		panel.setBackground(new Color(229, 240, 203));
		return panel;
	}

	public JPanel createUnitsPanel()
	{
		JPanel panel = new JPanel();
		ButtonGroup g = new ButtonGroup();
		g.add(_taf);
		g.add(_cfs);
		_taf.setBackground(new Color(229, 240, 203));
		_cfs.setBackground(new Color(229, 240, 203));
		_taf.addActionListener(e ->
		{
			if(DEBUG)
			{
				System.out.println("TAF");
			}
			if(_taf.isSelected())
			{
				AppUtils.useUnits(AppUtils.TAF);
			}
		});
		_cfs.addActionListener(e ->
		{
			if(DEBUG)
			{
				System.out.println("CFS");
			}
			if(_cfs.isSelected())
			{
				AppUtils.useUnits(AppUtils.CFS);
			}
		});
		_taf.setFont(new Font(_font.getName(), _font.getStyle(), 10));
		_cfs.setFont(new Font(_font.getName(), _font.getStyle(), 10));
		panel.setPreferredSize(new Dimension(100, 12));
		panel.setLayout(new GridLayout(1, 0));
		panel.add(_taf);
		panel.add(_cfs);
		panel.setBackground(new Color(229, 240, 203));
		return panel;
	}

	public JPanel createModePanel()
	{
		JPanel panel = new JPanel();
		ButtonGroup g = new ButtonGroup();
		g.add(_base);
		g.add(_comp);
		g.add(_diff);
		_base.setBackground(new Color(229, 240, 203));
		_comp.setBackground(new Color(229, 240, 203));
		_diff.setBackground(new Color(229, 240, 203));
		_base.addItemListener(e ->
		{
			if(e.getStateChange() == ItemEvent.SELECTED)
			{
				AppUtils.plotComparitive = false;
				AppUtils.plotDifference = false;
			}
			if(DEBUG)
			{
				System.out.println("base");
				System.out.println(AppUtils.plotComparitive);
				System.out.println(AppUtils.plotDifference);
			}
		});
		_comp.addItemListener(e ->
		{
			if(e.getStateChange() == ItemEvent.SELECTED)
			{
				AppUtils.plotComparitive = true;
				AppUtils.plotDifference = false;
			}
			if(DEBUG)
			{
				System.out.println("comp");
				System.out.println(AppUtils.plotComparitive);
				System.out.println(AppUtils.plotDifference);
			}
		});
		_diff.addItemListener(e ->
		{
			if(e.getStateChange() == ItemEvent.SELECTED)
			{
				AppUtils.plotComparitive = false;
				AppUtils.plotDifference = true;
			}
			if(DEBUG)
			{
				System.out.println("diff");
				System.out.println(AppUtils.plotComparitive);
				System.out.println(AppUtils.plotDifference);
			}
		});
		_base.setFont(new Font(_font.getName(), _font.getStyle(), 10));
		_comp.setFont(new Font(_font.getName(), _font.getStyle(), 10));
		_diff.setFont(new Font(_font.getName(), _font.getStyle(), 10));
		panel.setPreferredSize(new Dimension(150, 12));
		panel.setLayout(new GridLayout(1, 0));
		panel.add(_base);
		panel.add(_comp);
		panel.add(_diff);
		panel.setBackground(new Color(229, 240, 203));
		return panel;
	}

	public JComboBox createTWBox()
	{
		String[] twSelections = {"OCT1921 - SEP2003", "OCT1921 - SEP1994",
				"MAY1928 - OCT1934", "JUN1986 - SEP1992", "OCT1975 - SEP1977",
				"OCT1983 - SEP1993",};
		for(int i = 0; i < twSelections.length; i++)
		{
			_twitems.addElement(twSelections[i]);
		}
		JComboBox twbox = new JComboBox(_twitems);
		twbox.setEditable(true);
		twbox.addActionListener(e ->
		{
			JComboBox tb = (JComboBox) e.getSource();
			String tw = (String) tb.getSelectedItem();
			if(DEBUG)
			{
				System.out.println(tw);
			}
			AppUtils.getCurrentProject().setTimeWindow(tw);
		});
		Dimension d = new Dimension(350, 17);
		twbox.setMinimumSize(d);
		twbox.setFont(new Font(_font.getName(), _font.getStyle(), 10));
		return twbox;
	}

	public JPanel createViewPanel()
	{
		JPanel panel = new JPanel();
		_plot.addItemListener(e ->
		{
			if(e.getStateChange() == ItemEvent.SELECTED)
			{
				if(DEBUG)
				{
					System.out.println("Plot Selected");
				}
				AppUtils.VIEW_GRAPH = true;
			}
			else
			{
				if(DEBUG)
				{
					System.out.println("Plot Deselected");
				}
				AppUtils.VIEW_GRAPH = false;
			}
		});
		_table.addItemListener(e ->
		{
			if(e.getStateChange() == ItemEvent.SELECTED)
			{
				if(DEBUG)
				{
					System.out.println("Table Selected");
				}
				AppUtils.VIEW_TABLE = true;
			}
			else
			{
				if(DEBUG)
				{
					System.out.println("Table Deselected");
				}
				AppUtils.VIEW_TABLE = false;
			}
		});
		_monthly.addItemListener(e ->
		{
			if(e.getStateChange() == ItemEvent.SELECTED)
			{
				if(DEBUG)
				{
					System.out.println("Monthly Table Selected");
				}
				AppUtils.viewMonthlyTable = true;
			}
			else
			{
				if(DEBUG)
				{
					System.out.println("Monthly Table Deselected");
				}
				AppUtils.viewMonthlyTable = false;
			}
		});
		_plot.setFont(new Font(_font.getName(), _font.getStyle(), 10));
		_table.setFont(new Font(_font.getName(), _font.getStyle(), 10));
		_monthly.setFont(new Font(_font.getName(), _font.getStyle(), 10));
		_plot.setBackground(new Color(229, 240, 203));
		_table.setBackground(new Color(229, 240, 203));
		_monthly.setBackground(new Color(229, 240, 203));
		panel.setPreferredSize(new Dimension(165, 12));
		panel.setLayout(new GridLayout(1, 0));
		panel.add(_plot);
		panel.add(_table);
		panel.add(_monthly);
		return panel;
	}

	/**
	 * Update message panel in main gui
	 */
	void updateMessagePanel()
	{
		setTimeWindowMessage(getTimeWindowString());
		setProjectNameMessage(getProjectName());
		setDVFileMessage(getDVFilename());
		setSVFileMessage(getSVFilename());
		setDV2FileMessage(getDV2Filename());
		setSV2FileMessage(getSV2Filename());
		setDV3FileMessage(getDV3Filename());
		setSV3FileMessage(getSV3Filename());
		setDV4FileMessage(getDV4Filename());
		setSV4FileMessage(getSV4Filename());
		setUnitsMessage(getUnits());
		setDtsMasterMessage(getDtsFromProject());
	}

	/**
	 * sets time window messages
	 */
	void setTimeWindowMessage(String str)
	{
		_messages[2].setText(str);
	}

	/**
	 *
	 */
	void setModeMessage(String str)
	{
		if(str.equals(AppUtils.BASE))
		{
			_base.setSelected(true);
		}
		else if(str.equals(AppUtils.COMP))
		{
			_base.setSelected(true);
		}
		else if(str.equals(AppUtils.DIFF))
		{
			_diff.setSelected(true);
		}
	}

	/**
	 *
	 */
	void setProjectNameMessage(String str)
	{
		_messages[0].setText(str);
	}

	/**
	 *
	 */
	void setDVFileMessage(String str)
	{
		_messages[3].setText(str);
	}

	/**
	 *
	 */
	void setSVFileMessage(String str)
	{
		_messages[4].setText(str);
	}

	/**
	 *
	 */
	void setDV2FileMessage(String str)
	{
		_messages[5].setText(str);
	}

	/**
	 *
	 */
	void setSV2FileMessage(String str)
	{
		_messages[6].setText(str);
	}

	/**
	 *
	 */
	void setDV3FileMessage(String str)
	{
		_messages[10].setText(str);
	}

	/**
	 *
	 */
	void setSV3FileMessage(String str)
	{
		_messages[11].setText(str);
	}

	/**
	 *
	 */
	void setDV4FileMessage(String str)
	{
		_messages[12].setText(str);
	}

	/**
	 *
	 */
	void setSV4FileMessage(String str)
	{
		_messages[13].setText(str);
	}

	/**
	 *
	 */
	void setUnitsMessage(String str)
	{
		if(str.equals(AppUtils.CFS))
		{
			_cfs.setSelected(true);
		}
		else
		{
			_taf.setSelected(true);
		}
	}

	/**
	 *
	 */
	String getProjectName()
	{
		return AppUtils.getCurrentProject().getFilename();
	}

	/**
	 *
	 */
	String getModeString()
	{
		if(AppUtils.plotComparitive)
		{
			return AppUtils.COMP;
		}
		else if(AppUtils.plotDifference)
		{
			return AppUtils.DIFF;
		}
		else
		{
			return AppUtils.BASE;
		}
	}

	/**
	 * get the base DV file name that user chose from the main GUI.
	 */
	String getDVFilename()
	{
		return AppUtils.getCurrentProject().getDVFile();
	}

	/**
	 * get the base SV file name that user chose from the main GUI.
	 */
	String getSVFilename()
	{
		return AppUtils.getCurrentProject().getSVFile();
	}

	/**
	 * get the compare DV file name that user chose from the main GUI.
	 */
	String getDV2Filename()
	{
		return AppUtils.getCurrentProject().getDV2File();
	}

	/**
	 * get the compare SV file name that user chose from the main GUI.
	 */
	String getSV2Filename()
	{
		return AppUtils.getCurrentProject().getSV2File();
	}

	/**
	 * get the compare DV file name that user chose from the main GUI.
	 */
	String getDV3Filename()
	{
		return AppUtils.getCurrentProject().getDV3File();
	}

	/**
	 * get the compare SV file name that user chose from the main GUI.
	 */
	String getSV3Filename()
	{
		return AppUtils.getCurrentProject().getSV3File();
	}

	/**
	 * get the compare DV file name that user chose from the main GUI.
	 */
	String getDV4Filename()
	{
		return AppUtils.getCurrentProject().getDV4File();
	}

	/**
	 * get the compare SV file name that user chose from the main GUI.
	 */
	String getSV4Filename()
	{
		return AppUtils.getCurrentProject().getSV4File();
	}

	/**
	 * get the Units of the current project.
	 */
	String getUnits()
	{
		return AppUtils.getCurrentUnits();
	}

	/**
	 * get the time window that user modified from a panel.
	 */
	String getTimeWindowString()
	{
		TimeWindow tw = AppUtils.getCurrentProject().getTimeWindow();
		return tw == null ? " " : tw.getStartTime().toString().substring(2, 9)
				+ " - " + tw.getEndTime().toString().substring(2, 9);
	}

	String getDtsFromProject()
	{
		return AppUtils.getCurrentProject().getDtsPath();
	}

	/**
	 * get the message panel component
	 */
	public JPanel getMessagePanelComp()
	{
		return _messagePanelComp;
	}
}
