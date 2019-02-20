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
package vista.graph;

import java.awt.BorderLayout;
import java.awt.FlowLayout;
import java.awt.Rectangle;
import java.text.Format;
import java.text.NumberFormat;
import java.text.ParseException;

import javax.swing.BorderFactory;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTabbedPane;
import javax.swing.JTextField;

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
