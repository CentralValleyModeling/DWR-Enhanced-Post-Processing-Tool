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

import java.awt.BorderLayout;
import java.awt.FlowLayout;
import java.text.NumberFormat;
import java.util.StringTokenizer;
import javax.swing.*;

import vista.gui.Changeable;
import vista.gui.DialogButtonPanel;
import vista.set.DataReference;
import vista.set.DataSet;
import vista.set.RegularTimeSeries;

/**
 * 
 * 
 * @author Nicky Sandhu
 * @version $Id: ProfileAnimationDialog.java,v 1.1 2003/10/02 20:48:38 redwood
 *          Exp $
 */
public class ProfileAnimationDialog extends JDialog implements Changeable {
	public JTextField _tf;
	private DataReference[] _refs;
	private NumberFormat nf = NumberFormat.getInstance();
	private RegularTimeSeries[] _rts;

	/**
   *
   */
	public ProfileAnimationDialog(DataReference[] refs) {
		if (refs == null || refs.length == 0)
			throw new IllegalArgumentException("No references "
					+ "selected for profile plot");
		//
		_refs = refs;
		//
		_rts = new RegularTimeSeries[_refs.length];
		for (int i = 0; i < _refs.length; i++) {
			DataSet ds = _refs[i].getData();
			if (ds == null)
				throw new IllegalArgumentException("Could not get data for: "
						+ refs[i]);
			if (ds instanceof RegularTimeSeries)
				_rts[i] = (RegularTimeSeries) ds;
			else
				throw new IllegalArgumentException("Only regular time series "
						+ "references allowed for profile" + " animation");
		}
		//
		nf.setGroupingUsed(false);
		// show references
		String txt = "Profiling references: ";
		for (int i = 0; i < refs.length; i++) {
			txt += refs[i].getData().getName();
		}
		JLabel lb1 = new JLabel(txt);
		//
		JLabel lb = new JLabel("Enter comma seperated distances");
		_tf = new JTextField("", 90);
		JPanel p1 = new JPanel();
		p1.setLayout(new FlowLayout());
		p1.add(lb);
		p1.add(_tf);
		//
		JPanel mp = new JPanel();
		mp.setLayout(new BorderLayout());
		mp.add(lb1, BorderLayout.NORTH);
		mp.add(p1, BorderLayout.CENTER);
		mp.add(new DialogButtonPanel(this), BorderLayout.SOUTH);
		//
		getContentPane().add(mp);
		pack();
		setVisible(true);
	}

	/**
   *
   */
	public void applyChanges() {
		double[] distances = getDistances();
		if (distances == null || distances.length != _refs.length) {
			throw new IllegalArgumentException(
					"Incorrect number of distances entered:"
							+ "Please enter exactly:" + _refs.length
							+ "distances");
		}
		new ProfilePlotAnimator(_rts, distances);
	}

	/**
   *
   */
	public double[] getDistances() {
		String txt = _tf.getText();
		StringTokenizer st = new StringTokenizer(txt, ",");
		int count = st.countTokens();
		if (count != _refs.length) {
			JOptionPane.showMessageDialog(this, "Please enter exactly "
					+ _refs.length + " distances seperated by commas",
					"Error message", JOptionPane.ERROR_MESSAGE);
			return null;
		}
		double[] distances = new double[count];
		int i = 0;
		try {
			while (st.hasMoreTokens()) {
				distances[i] = nf.parse(st.nextToken()).doubleValue();
				i++;
			}
		} catch (Exception e) {
			JOptionPane.showMessageDialog(this,
					"Illegal number entered in sequence", "Error message",
					JOptionPane.ERROR_MESSAGE);
			return null;
		}
		return distances;
	}

	/**
   *
   */
	public void doneChanges() {
		this.dispose();
	}
}
