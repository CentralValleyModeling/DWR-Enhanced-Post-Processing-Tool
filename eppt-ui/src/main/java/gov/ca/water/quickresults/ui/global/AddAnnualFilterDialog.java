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
import java.util.Arrays;
import java.util.Objects;
import javax.swing.*;
import javax.swing.border.TitledBorder;

import gov.ca.water.calgui.bo.WaterYearPeriodRange;
import gov.ca.water.calgui.techservice.impl.DialogSvcImpl;
import jdk.nashorn.internal.scripts.JO;

class AddAnnualFilterDialog extends JDialog
{
	private JPanel _contentPane;
	private JButton _buttonOK;
	private JButton _buttonCancel;
	private JSpinner _startYearSpinner;
	private JSpinner _endYearSpinner;
	private JCheckBox _overrideWaterYearDefinitionCheckBox;
	private JComboBox _startMonthCombobox;
	private JComboBox _endMonthCombobox;
	private boolean _canceled = true;

	AddAnnualFilterDialog(Frame frame)
	{
		super(frame, "New Annual Filter", true);
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
		setPreferredSize(new Dimension(550, 200));
		setMinimumSize(new Dimension(550, 200));
		setLocationRelativeTo(frame);
		_startYearSpinner.setValue(1921);
		_endYearSpinner.setValue(2003);
		Arrays.asList(Month.values()).forEach(e -> {
			_startMonthCombobox.addItem(e);
			_endMonthCombobox.addItem(e);
		});
		_startMonthCombobox.setSelectedItem(Month.MAY);
		_endMonthCombobox.setSelectedItem(Month.SEPTEMBER);
		_overrideWaterYearDefinitionCheckBox.addActionListener(e -> {
			_startMonthCombobox.setEnabled(_overrideWaterYearDefinitionCheckBox.isSelected());
			_endMonthCombobox.setEnabled(_overrideWaterYearDefinitionCheckBox.isSelected());
		});
		JSpinner.NumberEditor editor = new JSpinner.NumberEditor(_startYearSpinner, "#");
		_startYearSpinner.setEditor(editor);
		editor = new JSpinner.NumberEditor(_endYearSpinner, "#");
		_endYearSpinner.setEditor(editor);
	}

	AddAnnualFilterDialog(Frame frame, WaterYearPeriodRange selectedItem, Month startMonth, Month endMonth)
	{
		this(frame, selectedItem);
		_startMonthCombobox.setSelectedItem(startMonth);
		_endMonthCombobox.setSelectedItem(endMonth);
		_overrideWaterYearDefinitionCheckBox.setSelected(true);
		_startMonthCombobox.setEnabled(true);
		_endMonthCombobox.setEnabled(true);
	}

	AddAnnualFilterDialog(Frame frame, WaterYearPeriodRange selectedItem)
	{
		this(frame);
		_startYearSpinner.setValue(selectedItem.getStartYear().getYear());
		_endYearSpinner.setValue(selectedItem.getEndYear().getYear());
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
		_contentPane = new JPanel();
		_contentPane.setLayout(new BorderLayout(5, 5));
		_contentPane.setBorder(BorderFactory.createTitledBorder(BorderFactory.createEmptyBorder(5, 5, 0, 5), null, TitledBorder.DEFAULT_JUSTIFICATION, TitledBorder.DEFAULT_POSITION, null, null));
		final JPanel panel1 = new JPanel();
		panel1.setLayout(new BorderLayout(0, 0));
		_contentPane.add(panel1, BorderLayout.SOUTH);
		final JPanel panel2 = new JPanel();
		panel2.setLayout(new BorderLayout(0, 0));
		panel1.add(panel2, BorderLayout.WEST);
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
		final JPanel panel4 = new JPanel();
		panel4.setLayout(new BorderLayout(10, 10));
		_contentPane.add(panel4, BorderLayout.CENTER);
		final JPanel panel5 = new JPanel();
		panel5.setLayout(new GridBagLayout());
		panel5.setPreferredSize(new Dimension(250, 108));
		panel4.add(panel5, BorderLayout.CENTER);
		final JPanel panel6 = new JPanel();
		panel6.setLayout(new GridBagLayout());
		GridBagConstraints gbc;
		gbc = new GridBagConstraints();
		gbc.gridx = 1;
		gbc.gridy = 0;
		gbc.weightx = 1.0;
		gbc.anchor = GridBagConstraints.WEST;
		gbc.fill = GridBagConstraints.VERTICAL;
		gbc.insets = new Insets(5, 5, 5, 5);
		panel5.add(panel6, gbc);
		final JPanel spacer1 = new JPanel();
		gbc = new GridBagConstraints();
		gbc.gridx = 1;
		gbc.gridy = 0;
		gbc.fill = GridBagConstraints.HORIZONTAL;
		panel6.add(spacer1, gbc);
		final JPanel spacer2 = new JPanel();
		gbc = new GridBagConstraints();
		gbc.gridx = 0;
		gbc.gridy = 1;
		gbc.fill = GridBagConstraints.VERTICAL;
		panel6.add(spacer2, gbc);
		_startYearSpinner = new JSpinner();
		gbc = new GridBagConstraints();
		gbc.gridx = 0;
		gbc.gridy = 0;
		gbc.anchor = GridBagConstraints.WEST;
		gbc.fill = GridBagConstraints.HORIZONTAL;
		panel6.add(_startYearSpinner, gbc);
		_endYearSpinner = new JSpinner();
		gbc = new GridBagConstraints();
		gbc.gridx = 2;
		gbc.gridy = 0;
		gbc.anchor = GridBagConstraints.WEST;
		gbc.fill = GridBagConstraints.HORIZONTAL;
		panel6.add(_endYearSpinner, gbc);
		final JLabel label2 = new JLabel();
		label2.setText("Seasonal Period:");
		gbc = new GridBagConstraints();
		gbc.gridx = 0;
		gbc.gridy = 0;
		gbc.anchor = GridBagConstraints.WEST;
		gbc.insets = new Insets(5, 5, 5, 5);
		panel5.add(label2, gbc);
		_overrideWaterYearDefinitionCheckBox = new JCheckBox();
		_overrideWaterYearDefinitionCheckBox.setText("Override Water Year Definition Months");
		gbc = new GridBagConstraints();
		gbc.gridx = 0;
		gbc.gridy = 1;
		gbc.anchor = GridBagConstraints.WEST;
		panel5.add(_overrideWaterYearDefinitionCheckBox, gbc);
		final JPanel panel7 = new JPanel();
		panel7.setLayout(new GridBagLayout());
		gbc = new GridBagConstraints();
		gbc.gridx = 1;
		gbc.gridy = 1;
		gbc.anchor = GridBagConstraints.WEST;
		gbc.fill = GridBagConstraints.VERTICAL;
		gbc.insets = new Insets(5, 5, 5, 5);
		panel5.add(panel7, gbc);
		final JPanel spacer3 = new JPanel();
		gbc = new GridBagConstraints();
		gbc.gridx = 1;
		gbc.gridy = 0;
		gbc.fill = GridBagConstraints.HORIZONTAL;
		panel7.add(spacer3, gbc);
		final JPanel spacer4 = new JPanel();
		gbc = new GridBagConstraints();
		gbc.gridx = 0;
		gbc.gridy = 1;
		gbc.fill = GridBagConstraints.VERTICAL;
		panel7.add(spacer4, gbc);
		_startMonthCombobox = new JComboBox();
		_startMonthCombobox.setEnabled(false);
		gbc = new GridBagConstraints();
		gbc.gridx = 0;
		gbc.gridy = 0;
		gbc.anchor = GridBagConstraints.WEST;
		gbc.fill = GridBagConstraints.HORIZONTAL;
		panel7.add(_startMonthCombobox, gbc);
		_endMonthCombobox = new JComboBox();
		_endMonthCombobox.setEnabled(false);
		gbc = new GridBagConstraints();
		gbc.gridx = 2;
		gbc.gridy = 0;
		gbc.anchor = GridBagConstraints.WEST;
		gbc.fill = GridBagConstraints.HORIZONTAL;
		panel7.add(_endMonthCombobox, gbc);
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

}


