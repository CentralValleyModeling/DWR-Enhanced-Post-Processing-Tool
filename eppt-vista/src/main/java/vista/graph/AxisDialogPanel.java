/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package vista.graph;

import java.awt.BorderLayout;
import java.awt.FlowLayout;
import java.awt.Rectangle;
import java.text.Format;
import java.text.NumberFormat;
import java.text.ParseException;
import javax.swing.*;

import vista.app.AppUtils;
import vista.time.TimeFactory;
import vista.time.TimeFormat;

/**
 * An editor for the attributes and state of the TextLine object
 * 
 * @see TextLine
 * @author Nicky Sandhu
 * @version $Id: AxisDialogPanel.java,v 1.0.1.0 1998/05/19 22:41:47 nsandhu Exp
 *          nsandhu $
 */
public class AxisDialogPanel extends GEDialogPanel {
	/**
	 * constructor
	 */
	public AxisDialogPanel(Axis axis) {
		super(axis);
	}

	/**
	 * creates panels
	 */
	protected JPanel createPanel() {
		JPanel basicPanel = super.createPanel();
		// text editing
		Axis axis = (Axis) getGraphicElement();
		mmPanel = new MMPanel(axis);
		mmPanel.setBorder(BorderFactory.createTitledBorder(BorderFactory
				.createEtchedBorder(), "Axis Attributes"));
		//
		JTabbedPane interiorPane = new JTabbedPane();
		interiorPane.addTab(BASIC, null, basicPanel);
		interiorPane.addTab(AXIS, null, mmPanel);
		interiorPane.setSelectedIndex(0);

		JPanel mainPanel = new JPanel();
		mainPanel.setLayout(new BorderLayout());
		mainPanel.add("Center", interiorPane);

		return mainPanel;
	}

	/**
	 * apply changes for both the basic graphic element and its specialization
	 */
	public void applyChanges() {
		super.applyChanges();
		try {
			double minX = mmPanel.getMinimum();
			double maxX = mmPanel.getMaximum();
			Axis axis = (Axis) getGraphicElement();
			axis.getTickGenerator().useDataMinMax(true);
			axis.setDCRange(minX, maxX);
		} catch (Exception nfe) {
			vista.gui.VistaUtils.displayException(this, nfe);
		}
	}

	/**
   *
   */
	private MMPanel mmPanel;
	protected final String BASIC = "Basic";
	protected final String AXIS = "Axis";

	/**
	 * A max min display panel
	 * 
	 * @author Nicky Sandhu
	 * @version $Id: AxisDialogPanel.java,v 1.1 2003/10/02 20:48:49 redwood Exp
	 *          $
	 */
	private class MMPanel extends JPanel {
		private Axis _axis;
		private JTextField _minField, _maxField;

		/**
		 * create a min max panel for the plot
		 */
		public MMPanel(Axis axis) {
			_axis = axis;
			Scale scale = axis.getScale();
			_minField = new JTextField(15);
			_maxField = new JTextField(15);
			int pos = axis.getPosition();
			String label = "";
			if (pos == AxisAttr.TOP)
				label = "Top   ";
			else if (pos == AxisAttr.BOTTOM)
				label = "Bottom";
			else if (pos == AxisAttr.LEFT)
				label = "Left  ";
			else
				label = "Right ";
			setLayout(new FlowLayout());
			add(new JLabel(label + " From: "));
			add(_minField);
			add(new JLabel("To: "));
			add(_maxField);
			Rectangle r = axis.getBounds();
			updateInfo(r);
		}

		/**
   *
   */
		public double getMinimum() throws ParseException {
			Format f = AppUtils.getFormatter(_axis);
			if (f instanceof TimeFormat) {
				return TimeFactory.getInstance()
						.createTime(_minField.getText()).getTimeInMinutes();
			} else {
				Number n = NumberFormat.getInstance()
						.parse(_minField.getText());
				return n.doubleValue();
			}
		}

		/**
   *
   */
		public double getMaximum() throws ParseException {
			Format f = AppUtils.getFormatter(_axis);
			if (f instanceof TimeFormat) {
				return TimeFactory.getInstance()
						.createTime(_maxField.getText()).getTimeInMinutes();
			} else {
				Number n = NumberFormat.getInstance()
						.parse(_maxField.getText());
				return n.doubleValue();
			}
		}

		/**
		 * update panel with new rectangle dimension
		 */
		public void updateInfo(Rectangle r) {
			int pos = _axis.getPosition();
			Scale scale = _axis.getScale();
			double minV = 0, maxV = 0;
			if (pos == AxisAttr.TOP || pos == AxisAttr.BOTTOM) {
				minV = scale.scaleToDC(r.x);
				maxV = scale.scaleToDC(r.x + r.width);
			} else {
				minV = scale.scaleToDC(r.y + r.height);
				maxV = scale.scaleToDC(r.y);
			}
			Format f = AppUtils.getFormatter(_axis);
			String minT = "", maxT = "";
			if (f instanceof TimeFormat) {
				minT = TimeFactory.getInstance().createTime((long) minV)
						.toString();
				maxT = TimeFactory.getInstance().createTime((long) maxV)
						.toString();
			} else {
				minT = f.format(new Double(minV));
				maxT = f.format(new Double(maxV));
			}
			_minField.setText(minT);
			_maxField.setText(maxT);
			paintAll(getGraphics());
		}
	} // end of class MMPanel

}
