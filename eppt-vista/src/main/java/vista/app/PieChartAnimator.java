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
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.*;

import vista.graph.AnimationObservable;
import vista.graph.AnimationObserver;
import vista.graph.Animator;
import vista.graph.AnimatorCanvas;
import vista.graph.GEAttr;
import vista.graph.GEBorderLayout;
import vista.graph.Graph;
import vista.graph.GraphAttr;
import vista.graph.PieChart;
import vista.graph.PieChartModel;
import vista.graph.TextLine;
import vista.graph.TextLineAttr;
import vista.set.Constants;
import vista.set.DataSetAttr;
import vista.set.DataSetElement;
import vista.set.DataSetIterator;
import vista.set.ElementFilter;
import vista.set.MultiIterator;
import vista.set.NaNFilter;
import vista.set.RegularTimeSeries;
import vista.set.Tuple;

/**
 * 
 * 
 * @author Nicky Sandhu
 * @version $Id: PieChartAnimator.java,v 1.1 2003/10/02 20:48:38 redwood Exp $
 */
public class PieChartAnimator implements AnimationObserver {
	private DataSetIterator _dsi;
	private ElementFilter _filter;
	private AnimatorCanvas _canvas;
	private Animator timer;
	private PieChartModel _pdm;
	private TextLine _timeText;
	private boolean goForward = true;
	private DataSetElement _dse;

	public PieChartAnimator(RegularTimeSeries[] rts) {
		int delay = 500;
		timer = new Animator();
		timer.setInterval(delay);
		timer.addAnimateDisplay(this);
		_canvas = setUpPieChart(rts);
		_filter = new NaNFilter();
		// set up buttons
		JButton fasterBtn = new JButton("faster");
		JButton slowerBtn = new JButton("slower");
		JButton stopBtn = new JButton("#");
		JButton forwardBtn = new JButton(">");
		JButton reverseBtn = new JButton("<");
		fasterBtn.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent evt) {
				increaseAnimationSpeed(50);
			}
		});
		slowerBtn.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent evt) {
				increaseAnimationSpeed(-50);
			}
		});
		forwardBtn.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent evt) {
				goForward = true;
				timer.startAnimation();
			}
		});
		stopBtn.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent evt) {
				timer.stopAnimation();
			}
		});
		reverseBtn.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent evt) {
				goForward = false;
				timer.startAnimation();
			}
		});
		// setup button panel
		JPanel buttonPanel = new JPanel();
		buttonPanel.setLayout(new FlowLayout());
		buttonPanel.add(forwardBtn);
		buttonPanel.add(stopBtn);
		buttonPanel.add(reverseBtn);
		buttonPanel.add(fasterBtn);
		buttonPanel.add(slowerBtn);
		//
		JFrame fr = new JFrame();
		fr.getContentPane().setLayout(new BorderLayout());
		fr.getContentPane().add(buttonPanel, BorderLayout.SOUTH);
		fr.getContentPane().add(_canvas, BorderLayout.CENTER);
		fr.pack();
		fr.setVisible(true);
		timer.startAnimation();
	}

	/**
   *
   */
	public void update(AnimationObservable observable, Object args) {
		if (goForward)
			drawNext();
		else
			drawPrevious();
	}

	/**
   *
   */
	public AnimatorCanvas setUpPieChart(RegularTimeSeries[] rts) {
		_dsi = new MultiIterator(rts, Constants.DEFAULT_FLAG_FILTER);
		DataSetElement dse2 = _dsi.getMaximum();
		double ymax = Float.MIN_VALUE;
		for (int i = 1; i < dse2.getDimension(); i++) {
			double d = dse2.getX(i);
			if (Double.doubleToLongBits(d) != 0x7ff8000000000000L) {
				ymax = Math.max(ymax, d);
			}
		}
		dse2 = _dsi.getMinimum();
		double ymin = Float.MAX_VALUE;
		for (int i = 1; i < dse2.getDimension(); i++) {
			double d = dse2.getX(i);
			if (Double.doubleToLongBits(d) != 0x7ff8000000000000L) {
				ymin = Math.min(ymin, d);
			}
		}
		_dsi.resetIterator();
		String[] labels = new String[rts.length];
		for (int i = 0; i < labels.length; i++)
			labels[i] = rts[i].getName();
		double[] vals = new double[rts.length];
		_dse = new Tuple(vals);
		DataSetElement dse = _dsi.getElement();
		for (int i = 0; i < _dse.getDimension(); i++)
			_dse.setX(i, dse.getX(i + 1));
		// System.out.println(_dse);
		_pdm = new DefaultPieChartModel(_dse, labels);
		Graph graph = new Graph(new GraphAttr());
		String title = "PieChart : ";
		for (int i = 0; i < rts.length; i++) {
			DataSetAttr attr = rts[i].getAttributes();
			if (attr != null && attr.getLocationName() != null) {
				title += " | " + attr.getLocationName();
			}
		}
		graph.setTitle(title);
		PieChart pc = new PieChart(new GEAttr(), _pdm);
		graph.add(GEBorderLayout.CENTER, pc);
		_timeText = new TextLine(new TextLineAttr(), dse.getXString());
		graph.add(GEBorderLayout.SOUTH, _timeText);
		_canvas = new AnimatorCanvas(graph, timer);
		return _canvas;
	}

	/**
   *
   */
	public void drawNext() {
		if (_dsi.atEnd()) {
			timer.stopAnimation();
		}
		DataSetElement dse = null;
		while (!_dsi.atEnd()) {
			dse = _dsi.getElement();
			if (_filter.isAcceptable(dse))
				break;
			_dsi.advance();
		}
		if (dse == null)
			return; // no more good elements, probably at end
		_timeText.setText(dse.getXString());
		for (int i = 0; i < _dse.getDimension(); i++)
			_dse.setX(i, dse.getX(i + 1));
		// System.out.println(_dse);
		_pdm.setReferenceObject(_dse);
		_canvas.redoNextPaint();
		_canvas.repaint();
		if (!_dsi.atEnd())
			_dsi.advance();
	}

	/**
   *
   */
	public void drawPrevious() {
		if (!_dsi.atStart())
			_dsi.retreat();
		DataSetElement dse = null;
		while (!_dsi.atStart()) {
			dse = _dsi.getElement();
			if (_filter.isAcceptable(dse))
				break;
			_dsi.retreat();
		}
		_timeText.setText(dse.getXString());
		for (int i = 0; i < _dse.getDimension(); i++)
			_dse.setX(i, dse.getX(i + 1));
		_pdm.setReferenceObject(_dse);
		_canvas.redoNextPaint();
		_canvas.repaint();
		if (_dsi.atStart()) {
			timer.stopAnimation();
			return;
		}
	}

	/**
   *
   */
	public void increaseAnimationSpeed(int increment) {
		int delay = (int) timer.getInterval();
		if (delay - increment > 0) {
			timer.setInterval(delay - increment);
		}
	}
}
