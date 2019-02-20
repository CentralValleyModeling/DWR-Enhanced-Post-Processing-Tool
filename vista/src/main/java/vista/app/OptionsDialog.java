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
import java.awt.Container;
import java.awt.Frame;
import java.awt.GridLayout;
import java.util.Properties;

import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;

import vista.gui.Changeable;
import vista.gui.DialogButtonPanel;

/**
 * a panel for user option settings
 * 
 * @author Nicky Sandhu
 * @version $Id: OptionsDialog.java,v 1.1 2003/10/02 20:48:37 redwood Exp $
 */
public class OptionsDialog extends JDialog implements Changeable {
	private Properties _props;
	private JComboBox laf, sip1, sip2;
	private JTextField swf, shf, ds, dd, dp, tt, lt;
	private JCheckBox connectChoice, flagChoice, localChoice;

	/**
   *
   */
	public OptionsDialog(Frame f, Properties p) {
		super(f);
		// look and feel options
		laf = new JComboBox();
		laf.addItem("xplaf");
		laf.addItem("system");
		laf.addItem("motif");
		laf.addItem("windows");
		laf.addItem("mac");
		// laf.addItem("basic");
		// width /height of original session view frame
		swf = new JTextField(5);
		shf = new JTextField(5);
		// initial position of session view frame
		sip1 = new JComboBox();
		sip2 = new JComboBox();
		sip1.addItem("top");
		sip1.addItem("bottom");
		sip2.addItem("left");
		sip2.addItem("right");
		// initial connection state
		connectChoice = new JCheckBox();
		// default server and directory
		ds = new JTextField(15);
		dd = new JTextField(25);
		dp = new JTextField(4);
		// flag retrieval
		flagChoice = new JCheckBox();
		localChoice = new JCheckBox();
		// arrangement...
		JPanel lafPanel = new JPanel();
		lafPanel.setLayout(new GridLayout(1, 1));
		lafPanel.add(new JLabel("Look and Feel"));
		lafPanel.add(laf);

		JPanel sPanel1 = new JPanel();
		sPanel1.setLayout(new GridLayout(1, 1));
		sPanel1.add(new JLabel("Session Frame Width "));
		sPanel1.add(swf);
		JPanel sPanel2 = new JPanel();
		sPanel2.setLayout(new GridLayout(1, 1));
		sPanel2.add(new JLabel("Session Frame Height "));
		sPanel2.add(shf);

		JPanel sip = new JPanel();
		sip.setLayout(new GridLayout(1, 1));
		sip.add(new JLabel("Position of Session Frame"));
		sip.add(sip1);
		sip.add(sip2);

		JPanel dPanel1 = new JPanel();
		dPanel1.setLayout(new GridLayout(1, 1));
		dPanel1.add(new JLabel("Default server    "));
		dPanel1.add(ds);
		JPanel dPanel2 = new JPanel();
		dPanel2.setLayout(new GridLayout(1, 1));
		dPanel2.add(new JLabel("Default directory "));
		dPanel2.add(dd);
		JPanel dPanel3 = new JPanel();
		dPanel3.setLayout(new GridLayout(1, 1));
		dPanel3.add(new JLabel("Default port      "));
		dPanel3.add(dp);

		JPanel connectPanel = new JPanel();
		connectPanel.setLayout(new GridLayout(1, 1));
		connectPanel.add(new JLabel("Connect at start ?"));
		connectPanel.add(connectChoice);

		JPanel flagPanel = new JPanel();
		flagPanel.setLayout(new GridLayout(1, 1));
		flagPanel.add(new JLabel("Flags with data?"));
		flagPanel.add(flagChoice);

		JPanel localPanel = new JPanel();
		localPanel.setLayout(new GridLayout(1, 1));
		localPanel.add(new JLabel("Local Access?"));
		localPanel.add(localChoice);

		JPanel tPanel1 = new JPanel();
		tPanel1.setLayout(new GridLayout(1, 1));
		tPanel1.add(new JLabel("Title Template"));
		tPanel1.add(tt = new JTextField(15));
		JPanel tPanel2 = new JPanel();
		tPanel2.setLayout(new GridLayout(1, 1));
		tPanel2.add(new JLabel("Legend Template "));
		tPanel2.add(lt = new JTextField(15));

		JPanel mPanel = new JPanel();
		mPanel.setLayout(new GridLayout(25, 1));
		mPanel.add(lafPanel);
		mPanel.add(sPanel1);
		mPanel.add(sPanel2);
		mPanel.add(sip);
		mPanel.add(dPanel1);
		mPanel.add(dPanel2);
		mPanel.add(dPanel3);
		mPanel.add(tPanel1);
		mPanel.add(tPanel2);
		mPanel.add(connectPanel);
		mPanel.add(flagPanel);
		mPanel.add(localPanel);
		mPanel.setLayout(new GridLayout(mPanel.getComponentCount(), 1));
		getContentPane().setLayout(new BorderLayout());
		getContentPane().add(mPanel, BorderLayout.CENTER);
		getContentPane().add(new DialogButtonPanel(this), BorderLayout.SOUTH);
		//
		_props = p;
		initializeFrom(_props);
		//
		pack();
	}

	/**
   *
   */
	public void initializeFrom(Properties p) {
		laf.setSelectedItem(_props.getProperty("gui.lookAndFeel"));
		swf.setText(_props.getProperty("gui.width"));
		shf.setText(_props.getProperty("gui.height"));
		String pos = _props.getProperty("gui.initialPosition");
		if (pos.indexOf("top") >= 0)
			sip1.setSelectedIndex(0);
		else
			sip1.setSelectedIndex(1);
		if (pos.indexOf("left") >= 0)
			sip2.setSelectedIndex(0);
		else
			sip2.setSelectedIndex(1);
		if (_props.getProperty("gui.connect").indexOf("true") >= 0)
			connectChoice.setSelected(true);
		else
			connectChoice.setSelected(false);
		// _props.getProperty("","");
		ds.setText(_props.getProperty("default.server"));
		dd.setText(_props.getProperty("default.dir"));
		dp.setText(_props.getProperty("default.port"));
		if (_props.getProperty("dss.flags").indexOf("true") >= 0)
			flagChoice.setSelected(true);
		else
			flagChoice.setSelected(false);
		if (_props.getProperty("dss.localAccess").indexOf("true") >= 0)
			localChoice.setSelected(true);
		else
			localChoice.setSelected(false);
		//
		tt.setText(_props.getProperty("graph.titleTemplate"));
		lt.setText(_props.getProperty("graph.legendTemplate"));
	}

	/**
   *
   */
	public Properties getProperties() {
		// get latest values...
		// default properites...
		_props.put("gui.width", swf.getText());
		_props.put("gui.height", shf.getText());
		_props.put("gui.lookAndFeel", laf.getSelectedItem());
		_props.put("gui.initialPosition", sip1.getSelectedItem() + "&"
				+ sip2.getSelectedItem());
		_props.put("gui.connect", new Boolean(connectChoice.isSelected())
				.toString());
		// _props.put("","");
		_props.put("default.server", ds.getText());
		_props.put("default.dir", dd.getText());
		_props.put("default.port", dp.getText());
		_props
				.put("dss.flags", new Boolean(flagChoice.isSelected())
						.toString());
		_props.put("dss.localAccess", new Boolean(localChoice.isSelected())
				.toString());
		_props.put("graph.titleTemplate", tt.getText());
		_props.put("graph.legendTemplate", lt.getText());

		return _props;
	}

	/**
   *
   */
	public void applyChanges() {
		Properties p = getProperties();
		Container f = getParent();
		if (f instanceof SessionFrame) {
			SessionFrame sf = (SessionFrame) f;
			sf.setProperties(p);
		}
	}

	/**
   *
   */
	public void doneChanges() {
		dispose();
	}
}
