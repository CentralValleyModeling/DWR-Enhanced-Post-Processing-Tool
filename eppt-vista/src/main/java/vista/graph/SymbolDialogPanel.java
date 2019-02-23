/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package vista.graph;

import java.awt.GridLayout;
import javax.swing.*;

/**
 * An editor for the attributes and state of the Curve object
 * 
 * @see Curve
 * @author Nicky Sandhu
 * @version $Id: SymbolDialogPanel.java,v 1.1 2003/10/02 20:49:09 redwood Exp $
 */
public class SymbolDialogPanel extends GEDialogPanel {
	/**
	 * constructor
	 */
	public SymbolDialogPanel(Symbol symbol) {
		super(symbol);
	}

	/**
	 * creates panels
	 */
	protected JPanel createPanel() {
		JPanel basicPanel = super.createPanel();
		// text editing
		Symbol symbol = (Symbol) getGraphicElement();
		SymbolAttr attr = (SymbolAttr) symbol.getAttributes();
		//
		symbolType = new JComboBox();
		symbolType.addItem(TRIANGLE);
		symbolType.addItem(SQUARE);
		symbolType.addItem(CROSS);
		symbolType.addItem(SLASH);
		symbolType.addItem(X);
		symbolType.addItem(BUTTERFLY);
		symbolType.addItem(HOURGLASS);
		//
		symbolFill = new JCheckBox("Is Filled?", attr._isFilled);
		symbolSizeField = new JTextField("2");
		//
		JPanel taPanel = new JPanel();
		taPanel.setLayout(new GridLayout(3, 1));
		taPanel.add(symbolType);
		taPanel.add(symbolFill);
		taPanel.add(symbolSizeField);
		taPanel.setBorder(BorderFactory.createTitledBorder(BorderFactory
				.createEtchedBorder(), "Symbol Attributes"));
		//
		basicPanel.add(taPanel);
		basicPanel.setLayout(new GridLayout(basicPanel.getComponentCount(), 1));
		//
		return basicPanel;
	}

	/**
	 * apply changes for both the basic graphic element and its specialization
	 */
	public void applyChanges() {
		super.applyChanges();
		Symbol symbol = (Symbol) getGraphicElement();
		SymbolAttr attr = (SymbolAttr) symbol.getAttributes();
		attr._isFilled = symbolFill.isSelected();
		int size = 2;
		try {
			size = new Integer(symbolSizeField.getText()).intValue();
		} catch (NumberFormatException e) {
			JOptionPane.showMessageDialog(this, e);
		}

		if (symbolType.getSelectedItem().equals(TRIANGLE)) {
			attr.setSymbol(SymbolFactory.createTriangleShape(size));
		} else if (symbolType.getSelectedItem().equals(SQUARE)) {
			attr.setSymbol(SymbolFactory.createSquareShape(size));
		} else if (symbolType.getSelectedItem().equals(CROSS)) {
			attr.setSymbol(SymbolFactory.createCrossShape(size));
		} else if (symbolType.getSelectedItem().equals(SLASH)) {
			attr.setSymbol(SymbolFactory.createSlashShape(size));
		} else if (symbolType.getSelectedItem().equals(X)) {
			attr.setSymbol(SymbolFactory.createXShape(size));
		} else if (symbolType.getSelectedItem().equals(BUTTERFLY)) {
			attr.setSymbol(SymbolFactory.createButterflyShape(size));
		} else if (symbolType.getSelectedItem().equals(HOURGLASS)) {
			attr.setSymbol(SymbolFactory.createHourGlassShape(size));
		} else {
			attr.setSymbol(SymbolFactory.createSquareShape(size));
		}
	}

	/**
    *
    */
	private JTextField textField, symbolSizeField;
	private JCheckBox symbolFill;
	private JComboBox symbolType;
	/**
 *
 */
	private final String BASIC = "Basic";
	/**
 *
 */
	private final String SYMBOL = "Symbol";
	private final String TRIANGLE = "Triangle", SQUARE = "Square",
			CROSS = "Cross", SLASH = "Slash", X = "X", BUTTERFLY = "ButterFly",
			HOURGLASS = "Hour Glass";
}
