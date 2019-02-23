/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */

package vista.app;

import java.awt.FlowLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.*;

import vista.db.dss.DSSUtil;
import vista.set.DataReference;
import vista.set.DataSet;
import vista.set.IrregularTimeSeries;
import vista.set.Pathname;
import vista.set.RegularTimeSeries;
import vista.set.TimeSeries;
import vista.set.TimeSeriesMath;
import vista.time.TimeFactory;
import vista.time.TimeInterval;

@SuppressWarnings("serial")
public class ShiftingTimeOperationsPanel extends JPanel {
	// FIXME: duplicate of ConversionOperationsPanel (merge them)
	private static final String[] INTERVALS = new String[] { "15MIN", "1HOUR",
			"1DAY", "1MON" };
	GroupTable groupTable;
	private JComboBox intervalBox;
	private JButton shiftForwardButton;
	private JButton shiftBackwardButton;

	public ShiftingTimeOperationsPanel(GroupTable table) {
		this.groupTable = table;
		this.setBorder(BorderFactory.createTitledBorder(BorderFactory
				.createEtchedBorder(),
				"Shift selected timeseries using parameters below"));
		JPanel mainPanel = new JPanel();
		mainPanel.setLayout(new BoxLayout(mainPanel, BoxLayout.Y_AXIS));

		JPanel buttonPanel = new JPanel();
		buttonPanel.setLayout(new FlowLayout());
		buttonPanel.add(shiftForwardButton = new JButton("Shift Forwards"));
		buttonPanel.add(shiftBackwardButton = new JButton("Shift Backwards"));
		mainPanel.add(buttonPanel);

		shiftForwardButton.addActionListener(new ActionListener() {

			@Override
			public void actionPerformed(ActionEvent e) {
				doShift(true);
			}
		});

		shiftBackwardButton.addActionListener(new ActionListener() {

			@Override
			public void actionPerformed(ActionEvent e) {
				doShift(false);
			}
		});

		intervalBox = new JComboBox();
		intervalBox.setEditable(true);
		for (int i = 0; i < INTERVALS.length; i++) {
			intervalBox.addItem(INTERVALS[i]);
		}
		mainPanel.add(intervalBox);
		this.add(mainPanel);
	}

	protected void doShift(boolean forwards) {
		String intervalStr = intervalBox.getSelectedItem().toString();
		try {
			TimeInterval timeInterval = TimeFactory.getInstance()
					.createTimeInterval(intervalStr);
			if (!forwards){
				timeInterval = timeInterval.__mul__(-1);
			}
			DataReference[] references = groupTable.getSelectedReferences();
			for (int i = 0; i < references.length; i++) {
				DataReference ref = references[i];
				DataSet data = ref.getData();
				if (!(data instanceof TimeSeries)) {
					JOptionPane
							.showConfirmDialog(
									this,
									data.getName()
											+ " is not a time series and will be ignored for shift operation",
									"Warning", JOptionPane.WARNING_MESSAGE);
					continue;
				}
				TimeSeries shifted = null;
				if (data instanceof IrregularTimeSeries) {
					IrregularTimeSeries its = (IrregularTimeSeries) data;
					shifted = TimeSeriesMath.createShifted(its, timeInterval);

				} else if (data instanceof RegularTimeSeries) {
					RegularTimeSeries rts = (RegularTimeSeries) data;
					shifted = TimeSeriesMath.createShifted(rts, timeInterval);
				}
				if (shifted != null) {
					Pathname pathname = Pathname.createPathname(ref
							.getPathname());
					pathname.setPart(Pathname.F_PART, pathname
							.getPart(Pathname.F_PART)
							+ "-SHIFTED " + timeInterval.getIntervalAsString());
					DataReference newRef = DSSUtil.createDataReference("local",
							ref.getFilename(), pathname.toString(), shifted);
					addReferenceToGroup(newRef);
				} else {
					JOptionPane.showConfirmDialog(this,
							"Could not created shifted time series for "
									+ data.getName(), "Warning",
							JOptionPane.WARNING_MESSAGE);
				}
			}
		} catch (Exception ex) {
			JOptionPane.showConfirmDialog(this, ex.getMessage(), "Error",
					JOptionPane.ERROR_MESSAGE);
			return;
		}
	}

	// FIXME: copied from ConversionOperationsPanel
	private void addReferenceToGroup(DataReference ref) {
		// add to group and reset reference to null for next operation
		groupTable.addReferenceAtCurrentSelection(ref);
		groupTable.updateInfoPanel();
		groupTable.paintAll(groupTable.getGraphics());
	}
}
