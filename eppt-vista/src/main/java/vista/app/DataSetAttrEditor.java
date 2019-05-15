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
import java.awt.GridLayout;
import javax.swing.*;

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
