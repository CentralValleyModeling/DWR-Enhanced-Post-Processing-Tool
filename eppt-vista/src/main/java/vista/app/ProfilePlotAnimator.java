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
import java.awt.Color;
import java.awt.FlowLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.*;

import vista.graph.AnimationObservable;
import vista.graph.AnimationObserver;
import vista.graph.Animator;
import vista.graph.AnimatorCanvas;
import vista.graph.Curve;
import vista.graph.CurveAttr;
import vista.graph.DefaultCurve;
import vista.graph.Graph;
import vista.graph.GraphAttr;
import vista.graph.Legend;
import vista.graph.LegendAttr;
import vista.graph.LegendItem;
import vista.graph.LegendItemAttr;
import vista.graph.Plot;
import vista.graph.PlotAttr;
import vista.graph.SymbolFactory;
import vista.set.Constants;
import vista.set.DataSetAttr;
import vista.set.DataSetElement;
import vista.set.DataSetIterator;
import vista.set.ElementFilter;
import vista.set.MultiIterator;
import vista.set.NaNFilter;
import vista.set.RegularTimeSeries;

/**
 * 
 * 
 * @author Nicky Sandhu
 * @version $Id: ProfilePlotAnimator.java,v 1.1 2003/10/02 20:48:39 redwood Exp
 *          $
 */
public class ProfilePlotAnimator implements AnimationObserver {
	private DataSetIterator _dsi;
	private ElementFilter _filter;
	private AnimatorCanvas _canvas;
	private Animator timer;
	private ProfileDataModel _pdm;
	private LegendItem _li;
	private boolean goForward = true;

	public ProfilePlotAnimator(RegularTimeSeries[] rts, double[] distances) {
		if (rts.length != distances.length)
			throw new IllegalArgumentException("# of time series not "
					+ "compatible with number of distances");

		int delay = 500;
		timer = new Animator();
		timer.setInterval(delay);
		timer.addAnimateDisplay(this);
		_canvas = setUpProfilePlot(rts, distances);
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
	public AnimatorCanvas setUpProfilePlot(RegularTimeSeries[] rts,
			double[] distances) {
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
		_pdm = new ProfileDataModel(_dsi.getElement(), distances, ymax, ymin,
				_dsi.getElement().getXString(0));
		Graph graph = new Graph(new GraphAttr());
		String title = "Profile Plot: ";
		for (int i = 0; i < rts.length; i++) {
			DataSetAttr attr = rts[i].getAttributes();
			if (attr != null && attr.getLocationName() != null) {
				title += " | " + attr.getLocationName();
			}
		}
		graph.setTitle(title);
		Plot plot = new Plot(new PlotAttr());
		CurveAttr ca = new CurveAttr();
		ca._foregroundColor = Color.blue;
		ca._drawLines = true;
		ca._drawSymbol = true;
		ca._symbol = SymbolFactory.createTriangle(true, ca._foregroundColor, 4);
		ca._dataPerSymbol = 1;
		Curve crv = new DefaultCurve(ca, _pdm);
		plot.addCurve(crv);
		graph.addPlot(plot);
		Legend l = new Legend(new LegendAttr());
		_li = new LegendItem(new LegendItemAttr(), crv);
		l.add(_li);
		graph.setLegend(l);
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
		_pdm.setReferenceObject(dse);
		_li.setLegendText(dse.getXString());
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
		_pdm.setReferenceObject(dse);
		_li.setLegendText(dse.getXString());
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
