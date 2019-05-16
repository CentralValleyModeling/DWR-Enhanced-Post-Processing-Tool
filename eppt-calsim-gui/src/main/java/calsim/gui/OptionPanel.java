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

import java.awt.Color;
import java.awt.FlowLayout;
import java.awt.GridLayout;
import java.awt.Rectangle;
import java.util.Hashtable;
import javax.swing.*;

import calsim.app.AppUtils;
import calsim.app.Study;
import vista.gui.XYGridLayout;

/**
 * The panel for the control tab.
 *
 * @author Yan-Ping Zuo
 * @version $Id: OptionPanel.java,v 1.1.2.11.2.2 2002/06/20 19:12:27 adraper Exp $
 */

public class OptionPanel extends JPanel
{

	static final JTextField _nper = new JTextField(5);
	static final JComboBox _start = new JComboBox(AppUtils.getYearArray());
	static final JComboBox _stop = new JComboBox(AppUtils.getYearArray());
	private static final String[] listString = {"None", "VAR", "CON", "BOTH"};
	private static final JComboBox _months = new JComboBox(GeneralPanel.getMonths());
	private JCheckBox[] _check;
	private JComboBox _solverRepList;
	private JTextField _addXaOptions;


	/**
	 * Constructor
	 */
	public OptionPanel()
	{

		_check = new JCheckBox[12];
		setLayout(new XYGridLayout(22, 28));
		// solver options
		JPanel panel = new JPanel();
		panel.setLayout(new GridLayout(2, 2));
		panel.setBorder(BorderFactory.createTitledBorder("Solver Output"));
		panel.add(createLabel("Save Report :"));
		panel.add(createCheckBoxPanel(0));
		panel.add(createLabel("     Listing:"));
		panel.add(createComboBoxPanel());
		add(panel, new Rectangle(1, 1, 8, 6));
		// state variable output options
		panel = new JPanel();
		panel.setLayout(new GridLayout(2, 2));
		panel.setBorder(BorderFactory.createTitledBorder("Other Options"));
		panel.add(createLabel("LF90 Output:"));
		panel.add(createCheckBoxPanel(7));
		panel.add(createLabel("Gen WSI-DI Tables:"));
		panel.add(createCheckBoxPanel(8));
		add(panel, new Rectangle(12, 1, 8, 6));
		// slack/ surplus output
		panel = new JPanel();
		panel.setLayout(new GridLayout(2, 2));
		panel.setBorder(BorderFactory.createTitledBorder("Slack/Surplus Output"));
		panel.add(createLabel("Save Report:"));
		panel.add(createCheckBoxPanel(3));
		panel.add(createLabel("Save All   :"));
		panel.add(createCheckBoxPanel(4));
		add(panel, new Rectangle(12, 8, 8, 6));
		// dss options
		panel = new JPanel();
		panel.setLayout(new GridLayout(2, 2));
		panel.setBorder(BorderFactory.createTitledBorder("DSS Options"));
		panel.add(createLabel("   Debug :"));
		panel.add(createCheckBoxPanel(5));
		panel.add(createLabel("Save Old :"));
		panel.add(createCheckBoxPanel(6));
		add(panel, new Rectangle(1, 8, 8, 6));
		// add xa
		panel = new JPanel();
		panel.setLayout(new GridLayout(4, 2));
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
		add(panel, new Rectangle(12, 15, 8, 10));


		panel = new JPanel();
		panel.setBorder(BorderFactory.createTitledBorder("Position Analysis"));
		panel.setLayout(new GridLayout(5, 2));
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
		add(panel, new Rectangle(1, 15, 8, 10));

		_months.setEnabled(false);
		_nper.setEnabled(false);
		_start.setEnabled(false);
		_stop.setEnabled(false);

		panel = new JPanel();
		panel.setLayout(new GridLayout(1, 2));
		panel.add(createLabel("XA Options:"));
		panel.add(createTextPanel());
		add(panel, new Rectangle(1, 25, 13, 3));

		final Hashtable monthIndex = new Hashtable(12);
		monthIndex.put("OCT", 0);
		monthIndex.put("NOV", 1);
		monthIndex.put("DEC", 2);
		monthIndex.put("JAN", 3);
		monthIndex.put("FEB", 4);
		monthIndex.put("MAR", 5);
		monthIndex.put("APR", 6);
		monthIndex.put("MAY", 7);
		monthIndex.put("JUN", 8);
		monthIndex.put("JUL", 9);
		monthIndex.put("AUG", 10);
		monthIndex.put("SEP", 11);

		_check[11].addActionListener(e ->
		{
			Study sty = AppUtils.getCurrentStudy();
			updateStudy(sty);
			if(_check[11].isSelected())
			{
				enablePositionComponents(true);
				GeneralPanel.getTimeStep().setSelectedItem("1MON");
				actionSteps();
				AppUtils.POSITION = true;
			}
			else
			{
				AppUtils.POSITION = false;
				enablePositionComponents(false);
				GeneralPanel.getTimeStep().setSelectedItem("1MON");
			}
		});
		_months.addActionListener(e ->
		{
			if(_check[11].isSelected())
			{
				actionSteps();
			}
		});
		_nper.addActionListener(e ->
		{
			if(_check[11].isSelected())
			{
				actionSteps();
			}
		});
		_start.addActionListener(e ->
		{
			if(_check[11].isSelected())
			{
				actionSteps();
			}
		});
		_stop.addActionListener(e ->
		{
			if(_check[11].isSelected())
			{
				actionSteps();
			}
		});


	}

	private void actionSteps()
	{
		GeneralPanel.getMonth()[0].setSelectedItem(_months.getSelectedItem());
		GeneralPanel.getYear()[0].setSelectedItem(_start.getSelectedItem());
		String s = _nper.getText();
		int i = new Integer(s);
		System.out.println("nperiods = " + i);
		AppUtils.nperiods = i;
		int nyrs = i / 12;
		System.out.println("nyrs = " + nyrs);
		int remainder = i - 12 * nyrs;
		int startmonthindex = _months.getSelectedIndex();
		int stopmonthindex = startmonthindex + remainder - 1;
		if(stopmonthindex > 11)
		{
			stopmonthindex = stopmonthindex - 12;
		}
		else if(stopmonthindex < 0)
		{
			stopmonthindex = stopmonthindex + 12;
		}
		GeneralPanel.getMonth()[1].setSelectedIndex(stopmonthindex);
		if((startmonthindex < 3 && stopmonthindex > 2 && remainder > 0) ||
				(startmonthindex - 12 + remainder - 1 > 2))
		{
			nyrs++;
		}
		System.out.println("nyrs = " + nyrs);

		GeneralPanel.getYear()[1].setSelectedIndex(_stop.getSelectedIndex() + nyrs);
		System.out.println("_stop = " + _stop.getSelectedIndex());
		System.out.println("_year = " + GeneralPanel.getYear()[1].getSelectedIndex());
		GeneralPanel.getNumberStepsLabel().setText("POSITION ANALYSIS ON");
		GeneralPanel.getNumberStepsLabel().setForeground(Color.red);
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
	private JPanel createComboBoxPanel()
	{
		JPanel panel = new JPanel();
		panel.setLayout(new FlowLayout(FlowLayout.LEFT));
		_solverRepList = new JComboBox(listString);
		panel.add(_solverRepList);
		return panel;
	}

	/**
	 *
	 */
	private JPanel createTextPanel()
	{
		JPanel panel = new JPanel();
		panel.setLayout(new FlowLayout(FlowLayout.LEFT));
		_addXaOptions = new JTextField(20);
		panel.add(_addXaOptions);
		return panel;
	}

	public void enablePositionComponents(boolean on)
	{
		if(on)
		{
			_months.setEnabled(true);
			_nper.setEnabled(true);
			_start.setEnabled(true);
			_stop.setEnabled(true);
			GeneralPanel.enableDateBoxes(false);
		}
		else
		{
			_months.setEnabled(false);
			_nper.setEnabled(false);
			_start.setEnabled(false);
			_stop.setEnabled(false);
			GeneralPanel.enableDateBoxes(true);
		}
	}

	/**
	 * Set the current study
	 */
	public void setStudy(Study study)
	{
		_check[0].setSelected(study.getSolverReportOption().booleanValue());
		_solverRepList.setSelectedItem(study.getSolverReportList());
		_check[1].setSelected(study.getSvReportOption().booleanValue());
		_check[2].setSelected(study.getSvReportSaveOption().booleanValue());
		_check[3].setSelected(study.getSlackReportOption().booleanValue());
		_check[4].setSelected(study.getSlackReportSaveOption().booleanValue());
		_check[5].setSelected(study.getDssDebugOption().booleanValue());
		_check[6].setSelected(study.getDssSaveOption().booleanValue());
		_addXaOptions.setText(study.getAddXaOptions());
		_check[8].setSelected(study.getWsiDiOption().booleanValue());
		_check[9].setSelected(study.getUseRestartOption().booleanValue());
		_check[10].setSelected(study.getGenerateRestartOption().booleanValue());
		_check[11].setSelected(study.getPosAnalysisOption().booleanValue());
		if(_check[11].isSelected())
		{
			enablePositionComponents(true);
		}
		else
		{
			enablePositionComponents(false);
		}
		_months.setSelectedItem(study.getStartMonth());
		_start.setSelectedItem(GeneralPanel.getYear()[0].getSelectedItem());
		_stop.setSelectedItem(GeneralPanel.getYear()[1].getSelectedItem());
	}

	/**
	 * Update the study
	 */
	public void updateStudy(Study study)
	{
		String tmp = "";
		study.setSolverReportOption(_check[0].isSelected());
		study.setSolverReportList((String) _solverRepList.getSelectedItem());
		study.setSvReportOption(_check[1].isSelected());
		study.setSvReportSaveOption(_check[2].isSelected());
		study.setSlackReportOption(_check[3].isSelected());
		study.setSlackReportSaveOption(_check[4].isSelected());
		study.setDssDebugOption(_check[5].isSelected());
		study.setDssSaveOption(_check[6].isSelected());

		tmp = GuiUtils.removeEOLChars(_addXaOptions.getText());
		if(_check[0].isSelected())
		{
			tmp = "MUTE NO LISTINPUT NO " + tmp;
		}
		study.setAddXaOptions(tmp);

		study.setWsiDiOption(_check[8].isSelected());
		study.setUseRestartOption(Boolean.FALSE);
		study.setGenerateRestartOption(Boolean.FALSE);
		study.setPosAnalysisOption(_check[11].isSelected());
	}

	/**
	 * Get LF90 Error Output Option
	 */
	public boolean getLF90OutputOption()
	{
		return _check[7].isSelected();
	}

}
