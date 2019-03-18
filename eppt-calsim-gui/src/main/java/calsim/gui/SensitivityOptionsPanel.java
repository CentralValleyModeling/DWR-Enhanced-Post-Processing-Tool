/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */

package calsim.gui;

import java.awt.FlowLayout;
import java.awt.GridLayout;
import java.awt.Rectangle;
import javax.swing.*;

import calsim.app.Study;
import vista.gui.XYGridLayout;

//import java.io.*;
//import javax.swing.event.*;
//import javax.swing.border.*;

/**
 * The panel for the control tab.
 *
 * @author Yan-Ping Zuo
 * @version $Id: OptionPanel.java,v 1.1.2.11.2.2 2002/06/20 19:12:27 adraper Exp $
 */

public class SensitivityOptionsPanel extends JPanel
{

	public static boolean DEBUG = true;
	//  public static String[] listString = { "None",	"VAR", "CON", "BOTH" };
	private JCheckBox[] _check;

	/**
	 * Constructor
	 */
	public SensitivityOptionsPanel()
	{

		_check = new JCheckBox[2];
		setLayout(new XYGridLayout(22, 28));
		// state variable output options
		JPanel panel = new JPanel();
		//panel.setLayout(new GridLayout(4,4));
		panel.setLayout(new GridLayout(2, 2));
		panel.setBorder(BorderFactory.createTitledBorder("Sensitivity Analysis Options"));
		panel.add(createLabel("Pricing and RHS:"));
		panel.add(createCheckBoxPanel(0));
		panel.add(createLabel("Column or row activity:"));
		panel.add(createCheckBoxPanel(1));
		add(panel, new Rectangle(1, 1, 8, 6));
/*    // slack/ surplus output
    panel = new JPanel();
    panel.setLayout(new GridLayout(2,2));
    panel.setBorder(BorderFactory.createTitledBorder("Slack/Surplus Output"));
    panel.add(createLabel("Save Report:"));
    panel.add(createCheckBoxPanel(3));
    panel.add(createLabel("Save All   :"));
    panel.add(createCheckBoxPanel(4));
    add(panel, new Rectangle(12,8,8,6));
    // dss options
    panel = new JPanel();
    panel.setLayout(new GridLayout(2,2));
    panel.setBorder(BorderFactory.createTitledBorder("DSS Options"));
    panel.add(createLabel("   Debug :"));
    panel.add(createCheckBoxPanel(5));
    panel.add(createLabel("Save Old :"));
    panel.add(createCheckBoxPanel(6));
    add(panel, new Rectangle(1,8,8,6));
    // add xa
    panel = new JPanel();
    //panel.setLayout( new FlowLayout() );
    panel.setLayout(new GridLayout(4,2));
    panel.setBorder(BorderFactory.createTitledBorder("State Variable Output"));
    panel.add(createLabel("Save Report:"));
    panel.add(createCheckBoxPanel(1));
    panel.add(createLabel("Save All   :"));
    panel.add(createCheckBoxPanel(2));
    panel.add(createLabel("Use Restart   :"));
    panel.add(createCheckBoxPanel(9));
    panel.add(createLabel("Gen Restart   :"));
    panel.add(createCheckBoxPanel(10));
    _check[9].setEnabled(false);
    _check[10].setEnabled(false);
    add(panel, new Rectangle(12,15,8,10));

    //add(panel, new Rectangle(3,18,13,3));
    // lf90 debug option
//    panel = new JPanel();
//    panel.setLayout(new GridLayout(1,2));
//    add(panel, new Rectangle(1,21,8,3));

    // wsi-di table creation option
//    panel = new JPanel();
//    panel.setLayout(new GridLayout(1,2));
//    add(panel, new Rectangle(12,21,8,3));
    // position analysis option
    panel = new JPanel();
    panel.setBorder(BorderFactory.createTitledBorder("Position Analysis"));
    panel.setLayout(new GridLayout(5,2));
    panel.add(createLabel("Run:"));
    panel.add(createCheckBoxPanel(11));
    panel.add(createLabel("Start Month:"));
    JPanel monpanel = new JPanel();
    monpanel.setLayout(new FlowLayout(FlowLayout.LEFT));
    monpanel.add(_months);
    panel.add(monpanel);
    panel.add(createLabel("Periods:"));
    monpanel = new JPanel();
    monpanel.setLayout(new FlowLayout(FlowLayout.LEFT));
    monpanel.add(_nper);
    _nper.setText("0");
    panel.add(monpanel);
    panel.add(createLabel("First Start Year:"));
    monpanel = new JPanel();
    monpanel.setLayout(new FlowLayout(FlowLayout.LEFT));
    monpanel.add(_start);
    _start.setSelectedItem("1921");
    panel.add(monpanel);
    panel.add(createLabel("Last Start Year:"));
    monpanel = new JPanel();
    monpanel.setLayout(new FlowLayout(FlowLayout.LEFT));
    monpanel.add(_stop);
    _stop.setSelectedItem("1921");
    panel.add(monpanel);
    add(panel, new Rectangle(1,15,8,10));

		_months.setEnabled(false);
		_nper.setEnabled(false);
		_start.setEnabled(false);
		_stop.setEnabled(false);

    panel = new JPanel();
    //panel.setLayout( new FlowLayout() );
    panel.setLayout( new GridLayout(1,2) );
    panel.add(createLabel("XA Options:"));
    panel.add(createTextPanel());
    add(panel, new Rectangle(1,25,13,3)); */
	}

	/**
	 *
	 */
	private JPanel createLabel(String str)
	{
		JLabel label = new JLabel("              " + str + "      ");
		label.setHorizontalAlignment(SwingConstants.RIGHT);
		JPanel panel = new JPanel();
		panel.setLayout(new FlowLayout(FlowLayout.RIGHT));
		panel.add(label);
		return panel;
	}

	/**
	 *
	 */
	private JPanel createCheckBoxPanel(int type)
	{
		JPanel panel = new JPanel();
		panel.setLayout(new FlowLayout(FlowLayout.LEFT));
		_check[type] = new JCheckBox("", false);
		panel.add(_check[type]);
		return panel;
	}
	/**
	 *
	 */
/*  private JPanel createComboBoxPanel() {
    JPanel panel = new JPanel();
    panel.setLayout(new FlowLayout(FlowLayout.LEFT));
    _solverRepList = new JComboBox(listString);
    panel.add(_solverRepList);
    return panel;
  }*/
	/**
	 *
	 */
/*  private JPanel createTextPanel() {
    JPanel panel = new JPanel();
    panel.setLayout(new FlowLayout(FlowLayout.LEFT));
    _addXaOptions = new JTextField(20);
    panel.add(_addXaOptions);
    return panel;
  }*/

	/**
	 * Set the current study
	 */
	public void setStudy(Study study)
	{
		_check[0].setSelected(study.getSolverReportOption().booleanValue());
		_check[1].setSelected(study.getSvReportOption().booleanValue());
	}

	/**
	 * Update the study
	 */
	public void updateStudy(Study study)
	{
		String tmp = "";
		study.setSolverReportOption(new Boolean(_check[0].isSelected()));
		study.setSvReportOption(new Boolean(_check[1].isSelected()));
	}
}
