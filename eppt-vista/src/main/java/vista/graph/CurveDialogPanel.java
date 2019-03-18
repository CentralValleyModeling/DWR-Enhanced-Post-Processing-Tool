/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package vista.graph;

import java.awt.BorderLayout;
import java.awt.GridLayout;
import java.util.StringTokenizer;
import javax.swing.*;

/**
 * An editor for the attributes and state of the Curve object
 * 
 * @see Curve
 * @author Nicky Sandhu
 * @version $Id: CurveDialogPanel.java,v 1.1 2003/10/02 20:48:52 redwood Exp $
 */
public class CurveDialogPanel extends GEDialogPanel {
	/**
	 * constructor
	 */
	public CurveDialogPanel(Curve curve) {
		super(curve);
	}

	/**
	 * creates panels
	 */
	protected JPanel createPanel() {
		JPanel basicPanel = super.createPanel();
		// text editing
		Curve curve = (Curve) getGraphicElement();
		CurveAttr attr = (CurveAttr) curve.getAttributes();

		textField = new JTextField(attr._curveName, 15);
		textField.setBorder(BorderFactory.createTitledBorder(BorderFactory
				.createEtchedBorder(), "Curve Name"));
		// 
		thicknessField = new JTextField(new Float(attr._thickness).toString(),
				5);
		thicknessField.setBorder(BorderFactory.createTitledBorder(BorderFactory
				.createEtchedBorder(), "thickness"));
		StringBuffer dashsb = new StringBuffer(10);
		for (int i = 0; i < attr._dashArray.length; i++) {
			dashsb.append(new Float(attr._dashArray[i]).toString());
			dashsb.append(" ");
		}
		dashArrayField = new JTextField(dashsb.toString(), 15);
		dashArrayField.setBorder(BorderFactory.createTitledBorder(BorderFactory
				.createEtchedBorder(), "Dash Array"));
		symbolCheck = new JCheckBox("Draw Symbol?", attr._drawSymbol);
		lineCheck = new JCheckBox("Draw Line?", attr._drawLines);
		dataPerSymbolField = new JTextField(new Integer(attr._dataPerSymbol)
				.toString());
		dataPerSymbolField.setBorder(BorderFactory.createTitledBorder(
				BorderFactory.createEtchedBorder(), "Data/Symbol"));
		//
		JPanel taPanel = new JPanel();
		taPanel.setLayout(new GridLayout(3, 1));
		taPanel.add(thicknessField);
		taPanel.add(dashArrayField);
		taPanel.add(symbolCheck);
		taPanel.add(lineCheck);
		taPanel.add(dataPerSymbolField);
		taPanel.setBorder(BorderFactory.createTitledBorder(BorderFactory
				.createEtchedBorder(), "Curve Attributes"));
		//
		JPanel curvePanel = new JPanel();
		curvePanel.setLayout(new GridLayout(5, 1));
		curvePanel.add(textField);
		curvePanel.add(taPanel);
		curvePanel.setLayout(new GridLayout(curvePanel.getComponentCount(), 1));
		//
		symbolPanel = new SymbolDialogPanel(curve.getSymbol());
		//  
		JTabbedPane interiorPane = new JTabbedPane();
		interiorPane.addTab(BASIC, null, basicPanel);
		interiorPane.addTab(CURVE, null, curvePanel);
		interiorPane.addTab(SYMBOL, null, symbolPanel);
		interiorPane.setSelectedIndex(0);
		//
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
		symbolPanel.applyChanges();
		Curve curve = (Curve) getGraphicElement();
		CurveAttr attr = (CurveAttr) curve.getAttributes();
		attr._curveName = textField.getText();
		try {
			attr._thickness = new Float(thicknessField.getText()).floatValue();
		} catch (NumberFormatException nfe) {
			System.err.println("Incorrect thickness spec");
		}

		try {
			StringTokenizer st = new StringTokenizer(dashArrayField.getText()
					.trim());
			int count = st.countTokens();
			if (count > 0) {
				float[] array = new float[count];
				int i = 0;
				while (st.hasMoreTokens()) {
					array[i] = new Float(st.nextToken()).floatValue();
					i++;
				}
				attr._dashArray = array;
			}
		} catch (NumberFormatException nfe) {
			System.err.println("Error in dash array");
		}

		int dps = attr._dataPerSymbol;
		try {
			dps = new Integer(dataPerSymbolField.getText()).intValue();
		} catch (NumberFormatException nfe) {
			System.err.println("Incorrect value for Data Per Symbol Field");
		}
		attr._dataPerSymbol = dps;
		attr._drawSymbol = symbolCheck.isSelected();
		attr._drawLines = lineCheck.isSelected();
		if (attr._drawSymbol)
			curve.setSymbol((Symbol) symbolPanel.getGraphicElement());
	}

	/**
   *
   */
	private JTextField textField, dataPerSymbolField, thicknessField,
			dashArrayField;
	private JCheckBox symbolCheck, lineCheck;
	private SymbolDialogPanel symbolPanel;
	/**
 *
 */
	protected final String BASIC = "Basic";
	/**
 *
 */
	protected final String CURVE = "Curve";
	/**
 *
 */
	protected final String SYMBOL = "Symbol";

}
