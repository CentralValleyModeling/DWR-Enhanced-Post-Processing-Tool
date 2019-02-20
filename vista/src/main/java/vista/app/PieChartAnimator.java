/*
    Copyright (C) 1996, 1997, 1998 State of California, Department of 
    Water Resources.

    VISTA : A VISualization Tool and Analyzer. 
	Version 1.0beta
	by Nicky Sandhu
    California Dept. of Water Resources
    Division of Planning, Delta Modeling Section
    1416 Ninth Street
    Sacramento, CA 95814
    (916)-653-7552
    nsandhu@water.ca.gov

    Send bug reports to nsandhu@water.ca.gov

    This program is licensed to you under the terms of the GNU General
    Public License, version 2, as published by the Free Software
    Foundation.

    You should have received a copy of the GNU General Public License
    along with this program; if not, contact Dr. Francis Chung, below,
    or the Free Software Foundation, 675 Mass Ave, Cambridge, MA
    02139, USA.

    THIS SOFTWARE AND DOCUMENTATION ARE PROVIDED BY THE CALIFORNIA
    DEPARTMENT OF WATER RESOURCES AND CONTRIBUTORS "AS IS" AND ANY
    EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
    IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
    PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE CALIFORNIA
    DEPARTMENT OF WATER RESOURCES OR ITS CONTRIBUTORS BE LIABLE FOR
    ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
    CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT
    OR SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA OR PROFITS; OR
    BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
    LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
    (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE
    USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
    DAMAGE.

    For more information about VISTA, contact:

    Dr. Francis Chung
    California Dept. of Water Resources
    Division of Planning, Delta Modeling Section
    1416 Ninth Street
    Sacramento, CA  95814
    916-653-5601
    chung@water.ca.gov

    or see our home page: http://wwwdelmod.water.ca.gov/

    Send bug reports to nsandhu@water.ca.gov or call (916)-653-7552

 */
package vista.app;

import java.awt.BorderLayout;
import java.awt.FlowLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JPanel;

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
