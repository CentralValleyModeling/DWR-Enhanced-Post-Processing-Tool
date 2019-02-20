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
import java.text.NumberFormat;
import java.util.StringTokenizer;

import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTextField;

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
