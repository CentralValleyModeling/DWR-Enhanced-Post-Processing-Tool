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
import java.awt.GridLayout;

import javax.swing.JComboBox;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;

import vista.gui.Changeable;
import vista.gui.DialogButtonPanel;
import vista.set.DataSet;
import vista.set.DataSetAttr;
import vista.set.DataType;

/**
 * 
 * 
 * @author Nicky Sandhu
 * @version $Id: DataSetAttrEditor.java,v 1.1 2003/10/02 20:48:26 redwood Exp $
 */
public class DataSetAttrEditor extends JDialog implements Changeable {
	private JTextField _groupField, _locationField, _typeField, _sourceField;
	private JTextField _xtf, _xuf, _yuf;
	private JComboBox _typeBox, _ytf;
	private DataSet ds;

	public DataSetAttrEditor(DataSet ds) {
		if (ds == null || ds.getAttributes() == null)
			throw new IllegalArgumentException(
					"Data set or its attributes are not available");
		//
		this.ds = ds;
		DataSetAttr attr = ds.getAttributes();
		JPanel gp = null;
		if (attr.getGroupName() != null) {
			gp = new JPanel();
			gp.setLayout(new FlowLayout());
			gp.add(new JLabel("Group Name:"));
			_groupField = new JTextField(attr.getGroupName(), 25);
			gp.add(_groupField);
		}
		JPanel lp = null;
		if (attr.getLocationName() != null) {
			lp = new JPanel();
			lp.setLayout(new FlowLayout());
			lp.add(new JLabel("Location Name:"));
			_locationField = new JTextField(attr.getLocationName(), 25);
			lp.add(_locationField);
		}
		JPanel tp = null;
		if (attr.getTypeName() != null) {
			tp = new JPanel();
			tp.setLayout(new FlowLayout());
			tp.add(new JLabel("Type Name:"));
			_typeField = new JTextField(attr.getTypeName(), 25);
			tp.add(_typeField);
		}
		JPanel sp = null;
		if (attr.getSourceName() != null) {
			sp = new JPanel();
			sp.setLayout(new FlowLayout());
			sp.add(new JLabel("Source Name:"));
			_sourceField = new JTextField(attr.getSourceName(), 25);
			sp.add(_sourceField);
		}
		//
		JPanel ttp = new JPanel();
		ttp.setLayout(new FlowLayout());
		ttp.add(new JLabel("Type of data:"));
		String[] types = new String[3];
		types[0] = DataType.getTypeRepresentation(DataType.REGULAR_TIME_SERIES);
		types[1] = DataType
				.getTypeRepresentation(DataType.IRREGULAR_TIME_SERIES);
		types[2] = DataType.getTypeRepresentation(DataType.PAIRED);
		types[2] = DataType.getTypeRepresentation(DataType.UNDEFINED);
		ttp.add(_typeBox = new JComboBox(types));
		String typeStr = DataType.getTypeRepresentation(attr.getType());
		_typeBox.setSelectedItem(typeStr);
		//
		JPanel xp = new JPanel();
		xp.setLayout(new FlowLayout());
		xp.add(new JLabel("X Type:"));
		xp.add(_xtf = new JTextField(attr.getXType(), 10));
		xp.add(new JLabel("X Units:"));
		xp.add(_xuf = new JTextField(attr.getXUnits(), 10));
		//
		JPanel yp = new JPanel();
		yp.setLayout(new FlowLayout());
		yp.add(new JLabel("Y Type:"));
		_ytf = new JComboBox(new String[]{"INST-VAL","PER-AVER"});
		_ytf.setEditable(true);
		_ytf.setSelectedItem(attr.getYType());
		yp.add(_ytf);
		yp.add(new JLabel("Y Units:"));
		yp.add(_yuf = new JTextField(attr.getYUnits(), 10));
		//
		JPanel mp = new JPanel();
		mp.setLayout(new GridLayout(8, 1));
		if (gp != null)
			mp.add(gp);
		if (lp != null)
			mp.add(lp);
		if (tp != null)
			mp.add(tp);
		if (sp != null)
			mp.add(sp);
		mp.add(ttp);
		mp.add(xp);
		mp.add(yp);
		//
		mp.setLayout(new GridLayout(mp.getComponentCount(), 1));
		//
		getContentPane().setLayout(new BorderLayout());
		getContentPane().add(mp, BorderLayout.CENTER);
		getContentPane().add(new DialogButtonPanel(this), BorderLayout.SOUTH);
		pack();
		setVisible(true);
		//
	}

	/**
	 * Apply the changes (OK/Apply button pressed)
	 */
	public void applyChanges() {
		DataSetAttr attr = ds.getAttributes();
		String gn = null, ln = null, tn = null, sn = null;
		int type = 0;
		String xu, xt, yu, yt;
		if (_groupField != null)
			gn = _groupField.getText();
		else
			gn = "";
		if (_locationField != null)
			ln = _locationField.getText();
		else
			ln = "";
		if (_typeField != null)
			tn = _typeField.getText();
		else
			tn = "";
		if (_sourceField != null)
			sn = _sourceField.getText();
		else
			sn = "";
		type = DataType.getType((String) _typeBox.getSelectedItem());
		xt = _xtf.getText();
		xu = _xuf.getText();
		yt = _ytf.getSelectedItem().toString();
		yu = _yuf.getText();
		DataSetAttr newAttr = new DataSetAttr(gn, ln, tn, sn, type, xu, yu, xt,
				yt);
		ds.setAttributes(newAttr);
	}

	/**
	 * Done with making changes (OK/Cancel button pressed)
	 */
	public void doneChanges() {
		this.dispose();
	}
}
