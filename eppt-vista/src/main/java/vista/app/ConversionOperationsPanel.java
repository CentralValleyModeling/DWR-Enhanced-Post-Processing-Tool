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

package vista.app;

import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.*;

import vista.set.DataReference;
import vista.set.DataSet;
import vista.set.DefaultReference;
import vista.set.IrregularTimeSeries;
import vista.set.Pathname;
import vista.set.RegularTimeSeries;
import vista.set.TimeSeries;
import vista.set.TimeSeriesMath;
import vista.time.TimeFactory;
import vista.time.TimeInterval;

@SuppressWarnings("serial")
public class ConversionOperationsPanel extends JPanel {
	private static final String[] INTERVALS = new String[] { "15MIN", "1HOUR",
			"1DAY", "1MON" };
	private GroupTable groupTable;
	private JComboBox modeOptionsBox;
	private JButton convertButton;
	private JComboBox intervalBox;
	private JComboBox modeBox;

	public ConversionOperationsPanel(GroupTable groupTable) {
		this.groupTable = groupTable;
		setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));
		setBorder(BorderFactory.createTitledBorder(BorderFactory
				.createEtchedBorder(),
				"Convert timeseries to regular by either snap or interpolate"));
		setToolTipText("Select time series from table view below and then select appropriate options and click convert");
		JPanel panel = new JPanel();
		panel.setMaximumSize(new Dimension(32767, 20));
		panel.setLayout(new BoxLayout(panel, BoxLayout.X_AXIS));

		JLabel lblInterval = new JLabel("INTERVAL");
		panel.add(lblInterval);

		intervalBox = new JComboBox();
		intervalBox.setEditable(true);
		for (int i = 0; i < INTERVALS.length; i++) {
			intervalBox.addItem(INTERVALS[i]);
		}
		panel.add(intervalBox);

		JLabel lblMode = new JLabel("MODE");
		panel.add(lblMode);

		modeBox = new JComboBox();
		modeBox.addItem("SNAP");
		modeBox.addItem("SAMPLE");
		panel.add(modeBox);

		panel.add(new JLabel("MODE OPTIONS"));
		modeOptionsBox = new JComboBox();
		panel.add(modeOptionsBox);
		setOptions("SNAP");
		modeBox.addActionListener(new ActionListener() {
			
			public void actionPerformed(ActionEvent evt) {
				if (modeBox.getSelectedItem().equals("SNAP")) {
					setOptions("SNAP");
				} else {
					setOptions("SAMPLE");
				}
			}
		});

		this.add(panel);

		Box verticalBox = Box.createVerticalBox();
		verticalBox.setPreferredSize(new Dimension(0, 10));
		verticalBox.setMinimumSize(new Dimension(10, 10));
		add(verticalBox);
		this.add(convertButton = new JButton("Convert Selected"));
		convertButton.addActionListener(new ActionListener() {

			@Override
			public void actionPerformed(ActionEvent e) {
				DataReference[] selectedReferences = ConversionOperationsPanel.this.groupTable
						.getSelectedReferences();
				if (selectedReferences == null)
					return;
				for (int i = 0; i < selectedReferences.length; i++) {
					DataReference ref = selectedReferences[i];
					DataSet ds = ref.getData();
					if (modeBox.getSelectedItem().equals("SNAP")) {
						if (!(ds instanceof IrregularTimeSeries)) {
							JOptionPane.showMessageDialog(
									ConversionOperationsPanel.this,
									"Ignoring SNAP for non irregular time series: "
											+ ds.getName(), "Warning",
									JOptionPane.WARNING_MESSAGE);
							continue;
						}
						IrregularTimeSeries its = (IrregularTimeSeries) ds;
						TimeInterval ti = TimeFactory.getInstance()
								.createTimeInterval(
										intervalBox.getSelectedItem()
												.toString());
						int snapType = TimeSeriesMath.SNAP_NEAREST;
						if (modeOptionsBox.getSelectedItem()
								.equals("BACKWARDS")) {
							snapType = TimeSeriesMath.SNAP_BACKWARD;
						}
						if (modeOptionsBox.getSelectedItem().equals("FORWARDS")) {
							snapType = TimeSeriesMath.SNAP_FORWARD;
						}
						RegularTimeSeries snap = TimeSeriesMath.snap(its, ti,
								snapType);
						Pathname pathname = Pathname.createPathname(ref
								.getPathname());
						pathname.setPart(Pathname.F_PART, pathname
								.getPart(Pathname.F_PART)
								+ "-SNAP");
						pathname.setPart(Pathname.E_PART, snap
								.getTimeInterval().toString());
						snap.setName(pathname.toString());
						DataReference newRef = new DefaultReference("", "",
								pathname.toString(), snap);
						addReferenceToGroup(newRef);
					} else if (modeBox.getSelectedItem().equals("SAMPLE")) {
						if (!(ds instanceof IrregularTimeSeries)) {
							JOptionPane.showMessageDialog(
									ConversionOperationsPanel.this,
									"Ignoring SAMPLE for non time series: "
											+ ds.getName(), "Warning",
									JOptionPane.WARNING_MESSAGE);
							continue;
						}
						TimeSeries ts = (TimeSeries) ds;
						TimeInterval ti = TimeFactory.getInstance()
								.createTimeInterval(
										intervalBox.getSelectedItem()
												.toString());
						RegularTimeSeries sampled = null;
						if (modeOptionsBox.getSelectedItem().equals(
								"LAST VALUE")) {
							sampled = TimeSeriesMath.sample(ts, ti,
									TimeSeriesMath.LAST_VAL);
						} else if (modeOptionsBox.getSelectedItem().equals(
								"INTERPOLATE")) {
							sampled = TimeSeriesMath.sample(ts, ti,
									TimeSeriesMath.LINEAR);
						}
						Pathname pathname = Pathname.createPathname(ref
								.getPathname());
						pathname.setPart(Pathname.F_PART, pathname
								.getPart(Pathname.F_PART)
								+ "-SAMPLE");
						pathname.setPart(Pathname.E_PART, sampled
								.getTimeInterval().toString());
						sampled.setName(pathname.toString());
						DataReference newRef = new DefaultReference("", "",
								pathname.toString(), sampled);
						addReferenceToGroup(newRef);
					}
				}
			}

		});

	}

	private void addReferenceToGroup(DataReference ref) {
		// add to group and reset reference to null for next operation
		groupTable.addReferenceAtCurrentSelection(ref);
		groupTable.updateInfoPanel();
		groupTable.paintAll(groupTable.getGraphics());

	}

	protected void setOptions(String value) {
		modeOptionsBox.removeAllItems();
		if (value.equals("SNAP")) {
			modeOptionsBox.addItem("NEAREST");
			modeOptionsBox.addItem("BACKWARDS");
			modeOptionsBox.addItem("FORWARDS");
		} else if (value.equals("SAMPLE")) {
			modeOptionsBox.addItem("LAST VALUE");
			modeOptionsBox.addItem("INTERPOLATE");
		}
	}

}
