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

package gov.ca.water.quickresults.ui.global;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.Frame;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.KeyEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.time.Month;
import java.time.YearMonth;
import java.time.format.DateTimeFormatter;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Objects;
import javax.swing.*;
import javax.swing.border.TitledBorder;

import gov.ca.water.calgui.bo.WaterYearDefinition;
import gov.ca.water.calgui.bo.WaterYearPeriod;
import gov.ca.water.calgui.bo.WaterYearPeriodRange;
import gov.ca.water.calgui.bo.WaterYearPeriodRangeFilter;
import gov.ca.water.calgui.bo.WaterYearPeriodRangesFilter;
import gov.ca.water.calgui.bo.WaterYearType;
import gov.ca.water.calgui.techservice.impl.DialogSvcImpl;
import javafx.application.Platform;
import javafx.embed.swing.JFXPanel;
import javafx.scene.Scene;

class AddAnnualFilterDialog extends JDialog
{
	private final WaterYearDefinition _waterYearDefinition;
	private JPanel _contentPane;
	private JButton _buttonOK;
	private JButton _buttonCancel;
	private JSpinner _startYearSpinner;
	private JSpinner _endYearSpinner;
	private JCheckBox _overrideWaterYearDefinitionCheckBox;
	private JComboBox _startMonthCombobox;
	private JComboBox _endMonthCombobox;
	private JLabel _periodOfRecordLabel;
	private JFXPanel _jfxPanel;
	private boolean _canceled = true;
	private EpptPeriodRangePane _epptPeriodRangePane;

	AddAnnualFilterDialog(Frame frame, WaterYearDefinition waterYearDefinition)
	{
		super(frame, "New Annual Filter", true);
		_waterYearDefinition = waterYearDefinition;
		$$$setupUI$$$();
		setContentPane(_contentPane);
		setModal(true);
		getRootPane().setDefaultButton(_buttonOK);
		_buttonOK.addActionListener(e -> onOK());
		_buttonCancel.addActionListener(e -> onCancel());
		// call onCancel() when cross is clicked
		setDefaultCloseOperation(DO_NOTHING_ON_CLOSE);
		addWindowListener(new WindowAdapter()
		{
			@Override
			public void windowClosing(WindowEvent e)
			{
				onCancel();
			}
		});

		// call onCancel() on ESCAPE
		_contentPane.registerKeyboardAction(e -> onCancel(), KeyStroke.getKeyStroke(KeyEvent.VK_ESCAPE, 0), JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT);
		pack();
		setMinimumSize(new Dimension(300, 260));
		setLocationRelativeTo(frame);
		Arrays.asList(Month.values()).forEach(e ->
		{
			_startMonthCombobox.addItem(e);
			_endMonthCombobox.addItem(e);
		});
		_startMonthCombobox.setSelectedItem(_waterYearDefinition.getStartMonth());
		_endMonthCombobox.setSelectedItem(_waterYearDefinition.getEndMonth());
		_startYearSpinner.setValue(1921);
		_endYearSpinner.setValue(2003);
		_overrideWaterYearDefinitionCheckBox.addActionListener(e ->
		{
			_startMonthCombobox.setEnabled(_overrideWaterYearDefinitionCheckBox.isSelected());
			_endMonthCombobox.setEnabled(_overrideWaterYearDefinitionCheckBox.isSelected());
			if(!_overrideWaterYearDefinitionCheckBox.isSelected())
			{
				_startMonthCombobox.setSelectedItem(_waterYearDefinition.getStartMonth());
				_endMonthCombobox.setSelectedItem(_waterYearDefinition.getEndMonth());
			}
			updatePeriodOfRecord();
		});
		JSpinner.NumberEditor editor = new JSpinner.NumberEditor(_startYearSpinner, "#");
		_startYearSpinner.setEditor(editor);
		editor = new JSpinner.NumberEditor(_endYearSpinner, "#");
		_endYearSpinner.setEditor(editor);
		_startYearSpinner.addChangeListener(e -> updatePeriodOfRecord());
		_endYearSpinner.addChangeListener(e -> updatePeriodOfRecord());
		_startMonthCombobox.addItemListener(e -> updatePeriodOfRecord());
		_endMonthCombobox.addItemListener(e -> updatePeriodOfRecord());
		updatePeriodOfRecord();
		setSize(new Dimension(1100, 450));
	}

	AddAnnualFilterDialog(Frame frame, WaterYearPeriodRange selectedItem, Month startMonth, Month endMonth, boolean overrideWaterYearDefinition,
						  WaterYearDefinition waterYearDefinition)
	{
		this(frame, waterYearDefinition);
		_startYearSpinner.setValue(selectedItem.getStartYear().getYear());
		_endYearSpinner.setValue(selectedItem.getEndYear().getYear());
		_startMonthCombobox.setSelectedItem(startMonth);
		_endMonthCombobox.setSelectedItem(endMonth);
		_overrideWaterYearDefinitionCheckBox.setSelected(overrideWaterYearDefinition);
		_startMonthCombobox.setEnabled(overrideWaterYearDefinition);
		_endMonthCombobox.setEnabled(overrideWaterYearDefinition);
		updatePeriodOfRecord();
	}

	private void updatePeriodOfRecord()
	{
		String text;
		WaterYearPeriodRange waterYearPeriodRange;
		WaterYearDefinition waterYearDefinition;
		if(_overrideWaterYearDefinitionCheckBox.isSelected())
		{
			WaterYearPeriod waterYearPeriod = new WaterYearPeriod("");
			waterYearPeriodRange = new WaterYearPeriodRange(waterYearPeriod, new WaterYearType(getStartYear(), waterYearPeriod),
					new WaterYearType(getEndYear(), waterYearPeriod));
			waterYearDefinition = new WaterYearDefinition("", getStartMonth(), getEndMonth(), 1, 1);
			text = "[" + waterYearPeriodRange.toString(waterYearDefinition, DateTimeFormatter.ofPattern("MMM yyyy")) + "]";
		}
		else
		{
			WaterYearPeriod waterYearPeriod = new WaterYearPeriod("");
			waterYearPeriodRange = new WaterYearPeriodRange(waterYearPeriod, new WaterYearType(getStartYear(), waterYearPeriod),
					new WaterYearType(getEndYear(), waterYearPeriod));
			waterYearDefinition = _waterYearDefinition;
			text = "[" + waterYearPeriodRange.toString(waterYearDefinition, DateTimeFormatter.ofPattern("MMM yyyy")) + "]";
		}
		_periodOfRecordLabel.setText(text);

		Platform.runLater(() ->
		{
			if(_epptPeriodRangePane != null)
			{
				WaterYearPeriodRangesFilter waterYearPeriodRangesFilter = new WaterYearPeriodRangesFilter("", "", Collections.singletonList(waterYearPeriodRange),
						waterYearDefinition);
				_epptPeriodRangePane.fill(waterYearPeriodRangesFilter);
			}
		});

	}

	public boolean isCanceled()
	{
		return _canceled;
	}

	private void onOK()
	{
		if(overrideWaterYearDefinition())
		{
			if(Objects.equals(getStartMonth(), getEndMonth()))
			{
				DialogSvcImpl.getDialogSvcInstance().getOK("Start Month cannot equal End Month", JOptionPane.WARNING_MESSAGE);
			}
			else if(!YearMonth.of(getStartYear(), getStartMonth()).isBefore(YearMonth.of(getEndYear(), getEndMonth())))
			{
				DialogSvcImpl.getDialogSvcInstance().getOK("End Year-Month must be after Start Year-Month", JOptionPane.WARNING_MESSAGE);
			}
			else
			{
				_canceled = false;
				// add your code here
				dispose();
			}
		}
		else if(getStartYear() > getEndYear())
		{
			DialogSvcImpl.getDialogSvcInstance().getOK("End Year must be after Start Year", JOptionPane.WARNING_MESSAGE);
		}
		else
		{
			_canceled = false;
			// add your code here
			dispose();
		}
	}

	private void onCancel()
	{
		// add your code here if necessary
		dispose();
	}

	int getStartYear()
	{
		return (int) _startYearSpinner.getValue();
	}

	int getEndYear()
	{
		return (int) _endYearSpinner.getValue();
	}

	boolean overrideWaterYearDefinition()
	{
		return _overrideWaterYearDefinitionCheckBox.isSelected();
	}

	Month getStartMonth()
	{
		return (Month) _startMonthCombobox.getSelectedItem();
	}

	Month getEndMonth()
	{
		return (Month) _endMonthCombobox.getSelectedItem();
	}

	/**
	 * Method generated by IntelliJ IDEA GUI Designer
	 * >>> IMPORTANT!! <<<
	 * DO NOT edit this method OR call it in your code!
	 *
	 * @noinspection ALL
	 */
	private void $$$setupUI$$$()
	{
		createUIComponents();
		_contentPane = new JPanel();
		_contentPane.setLayout(new BorderLayout(5, 5));
		_contentPane.setPreferredSize(new Dimension(480, 500));
		_contentPane.setBorder(
				BorderFactory.createTitledBorder(BorderFactory.createEmptyBorder(5, 5, 0, 5), null, TitledBorder.DEFAULT_JUSTIFICATION, TitledBorder.DEFAULT_POSITION, null,
						null));
		final JPanel panel1 = new JPanel();
		panel1.setLayout(new BorderLayout(0, 0));
		_contentPane.add(panel1, BorderLayout.SOUTH);
		final JPanel panel2 = new JPanel();
		panel2.setLayout(new BorderLayout(0, 0));
		panel1.add(panel2, BorderLayout.CENTER);
		final JLabel label1 = new JLabel();
		Font label1Font = this.$$$getFont$$$(null, Font.ITALIC, -1, label1.getFont());
		if(label1Font != null)
		{
			label1.setFont(label1Font);
		}
		label1.setText("*User Defined parameters do not persist on application close");
		panel2.add(label1, BorderLayout.CENTER);
		final JPanel panel3 = new JPanel();
		panel3.setLayout(new FlowLayout(FlowLayout.CENTER, 5, 5));
		panel1.add(panel3, BorderLayout.EAST);
		_buttonOK = new JButton();
		_buttonOK.setMaximumSize(new Dimension(70, 25));
		_buttonOK.setMinimumSize(new Dimension(70, 25));
		_buttonOK.setPreferredSize(new Dimension(70, 25));
		_buttonOK.setText("OK");
		panel3.add(_buttonOK);
		_buttonCancel = new JButton();
		_buttonCancel.setMaximumSize(new Dimension(70, 25));
		_buttonCancel.setMinimumSize(new Dimension(70, 25));
		_buttonCancel.setPreferredSize(new Dimension(70, 25));
		_buttonCancel.setText("Cancel");
		panel3.add(_buttonCancel);
		_contentPane.add(_jfxPanel, BorderLayout.CENTER);
		final JPanel panel4 = new JPanel();
		panel4.setLayout(new FlowLayout(FlowLayout.LEFT, 5, 5));
		_contentPane.add(panel4, BorderLayout.NORTH);
		final JPanel panel5 = new JPanel();
		panel5.setLayout(new BorderLayout(10, 10));
		panel5.setPreferredSize(new Dimension(250, 200));
		panel4.add(panel5);
		final JPanel panel6 = new JPanel();
		panel6.setLayout(new GridBagLayout());
		panel6.setPreferredSize(new Dimension(250, 108));
		panel5.add(panel6, BorderLayout.CENTER);
		final JLabel label2 = new JLabel();
		label2.setText("Start (Water) Year:");
		GridBagConstraints gbc;
		gbc = new GridBagConstraints();
		gbc.gridx = 0;
		gbc.gridy = 0;
		gbc.anchor = GridBagConstraints.WEST;
		gbc.insets = new Insets(5, 5, 5, 5);
		panel6.add(label2, gbc);
		_startYearSpinner = new JSpinner();
		gbc = new GridBagConstraints();
		gbc.gridx = 1;
		gbc.gridy = 0;
		gbc.anchor = GridBagConstraints.WEST;
		gbc.fill = GridBagConstraints.HORIZONTAL;
		panel6.add(_startYearSpinner, gbc);
		final JLabel label3 = new JLabel();
		label3.setText("Start Month:");
		gbc = new GridBagConstraints();
		gbc.gridx = 0;
		gbc.gridy = 3;
		gbc.anchor = GridBagConstraints.WEST;
		gbc.insets = new Insets(5, 5, 5, 5);
		panel6.add(label3, gbc);
		_startMonthCombobox = new JComboBox();
		_startMonthCombobox.setEnabled(false);
		gbc = new GridBagConstraints();
		gbc.gridx = 1;
		gbc.gridy = 3;
		gbc.weightx = 1.0;
		gbc.weighty = 1.0;
		gbc.anchor = GridBagConstraints.WEST;
		gbc.fill = GridBagConstraints.HORIZONTAL;
		gbc.insets = new Insets(5, 5, 5, 5);
		panel6.add(_startMonthCombobox, gbc);
		final JLabel label4 = new JLabel();
		label4.setText("Calendar Period:");
		gbc = new GridBagConstraints();
		gbc.gridx = 0;
		gbc.gridy = 5;
		gbc.weightx = 1.0;
		gbc.weighty = 1.0;
		gbc.anchor = GridBagConstraints.WEST;
		gbc.insets = new Insets(5, 5, 5, 5);
		panel6.add(label4, gbc);
		_periodOfRecordLabel = new JLabel();
		_periodOfRecordLabel.setPreferredSize(new Dimension(200, 38));
		_periodOfRecordLabel.setText("Label");
		gbc = new GridBagConstraints();
		gbc.gridx = 1;
		gbc.gridy = 5;
		gbc.weightx = 1.0;
		gbc.weighty = 1.0;
		gbc.anchor = GridBagConstraints.WEST;
		gbc.insets = new Insets(5, 5, 5, 5);
		panel6.add(_periodOfRecordLabel, gbc);
		final JLabel label5 = new JLabel();
		label5.setText("End (Water) Year:");
		gbc = new GridBagConstraints();
		gbc.gridx = 0;
		gbc.gridy = 1;
		gbc.anchor = GridBagConstraints.WEST;
		gbc.insets = new Insets(5, 5, 5, 5);
		panel6.add(label5, gbc);
		_endYearSpinner = new JSpinner();
		gbc = new GridBagConstraints();
		gbc.gridx = 1;
		gbc.gridy = 1;
		gbc.anchor = GridBagConstraints.WEST;
		gbc.fill = GridBagConstraints.HORIZONTAL;
		panel6.add(_endYearSpinner, gbc);
		_overrideWaterYearDefinitionCheckBox = new JCheckBox();
		_overrideWaterYearDefinitionCheckBox.setText("Override Water Year Definition Months");
		gbc = new GridBagConstraints();
		gbc.gridx = 0;
		gbc.gridy = 2;
		gbc.gridwidth = 2;
		gbc.fill = GridBagConstraints.HORIZONTAL;
		gbc.insets = new Insets(5, 5, 5, 5);
		panel6.add(_overrideWaterYearDefinitionCheckBox, gbc);
		final JLabel label6 = new JLabel();
		label6.setText("End Month:");
		gbc = new GridBagConstraints();
		gbc.gridx = 0;
		gbc.gridy = 4;
		gbc.anchor = GridBagConstraints.WEST;
		gbc.insets = new Insets(5, 5, 5, 5);
		panel6.add(label6, gbc);
		_endMonthCombobox = new JComboBox();
		_endMonthCombobox.setEnabled(false);
		gbc = new GridBagConstraints();
		gbc.gridx = 1;
		gbc.gridy = 4;
		gbc.weightx = 1.0;
		gbc.weighty = 1.0;
		gbc.anchor = GridBagConstraints.WEST;
		gbc.fill = GridBagConstraints.HORIZONTAL;
		gbc.insets = new Insets(5, 5, 5, 5);
		panel6.add(_endMonthCombobox, gbc);
	}

	/**
	 * @noinspection ALL
	 */
	private Font $$$getFont$$$(String fontName, int style, int size, Font currentFont)
	{
		if(currentFont == null)
		{
			return null;
		}
		String resultName;
		if(fontName == null)
		{
			resultName = currentFont.getName();
		}
		else
		{
			Font testFont = new Font(fontName, Font.PLAIN, 10);
			if(testFont.canDisplay('a') && testFont.canDisplay('1'))
			{
				resultName = fontName;
			}
			else
			{
				resultName = currentFont.getName();
			}
		}
		return new Font(resultName, style >= 0 ? style : currentFont.getStyle(), size >= 0 ? size : currentFont.getSize());
	}

	/**
	 * @noinspection ALL
	 */
	public JComponent $$$getRootComponent$$$()
	{
		return _contentPane;
	}

	private void createUIComponents()
	{
		_jfxPanel = new JFXPanel();
		Platform.setImplicitExit(false);
		Platform.runLater(() ->
		{
			_epptPeriodRangePane = new EpptPeriodRangePane(_waterYearDefinition);
			_jfxPanel.setScene(new Scene(_epptPeriodRangePane));
		});
	}
}


