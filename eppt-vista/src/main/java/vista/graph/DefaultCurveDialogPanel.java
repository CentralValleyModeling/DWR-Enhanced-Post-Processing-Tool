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
package vista.graph;

import java.awt.BorderLayout;
import java.awt.GridLayout;
import java.util.StringTokenizer;
import javax.swing.*;

/**
 * An editor for the attributes and state of the Curve object
 *
 * @author Nicky Sandhu
 * @version $Id: DefaultCurveDialogPanel.java,v 1.4 1999/01/07 21:05:36 nsandhu
 * Exp $
 * @see Curve
 */
public class DefaultCurveDialogPanel extends GEDialogPanel
{
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
	/**
	 *
	 */
	private JTextField textField, dataPerSymbolField, thicknessField,
			dashArrayField;
	private JCheckBox symbolCheck, lineCheck;
	private SymbolDialogPanel symbolPanel;
	/**
	 * constructor
	 */
	public DefaultCurveDialogPanel(DefaultCurve curve)
	{
		super(curve);
	}

	/**
	 * creates panels
	 */
	protected JPanel createPanel()
	{
		JPanel basicPanel = super.createPanel();
		// text editing
		DefaultCurve curve = (DefaultCurve) getGraphicElement();
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
		for(int i = 0; i < attr._dashArray.length; i++)
		{
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
	public void applyChanges()
	{
		super.applyChanges();
		symbolPanel.applyChanges();
		DefaultCurve curve = (DefaultCurve) getGraphicElement();
		CurveAttr attr = (CurveAttr) curve.getAttributes();
		attr._curveName = textField.getText();
		try
		{
			attr._thickness = new Float(thicknessField.getText()).floatValue();
		}
		catch(NumberFormatException nfe)
		{
			System.err.println("Incorrect thickness spec");
		}

		try
		{
			StringTokenizer st = new StringTokenizer(dashArrayField.getText()
																   .trim());
			int count = st.countTokens();
			if(count > 0)
			{
				float[] array = new float[count];
				int i = 0;
				while(st.hasMoreTokens())
				{
					array[i] = new Float(st.nextToken()).floatValue();
					i++;
				}
				attr._dashArray = array;
			}
		}
		catch(NumberFormatException nfe)
		{
			System.err.println("Error in dash array");
		}

		int dps = attr._dataPerSymbol;
		try
		{
			dps = new Integer(dataPerSymbolField.getText()).intValue();
		}
		catch(NumberFormatException nfe)
		{
			System.err.println("Incorrect value for Data Per Symbol Field");
		}
		attr._dataPerSymbol = dps;
		attr._drawSymbol = symbolCheck.isSelected();
		attr._drawLines = lineCheck.isSelected();
		if(attr._drawSymbol)
		{
			curve.setSymbol((Symbol) symbolPanel.getGraphicElement());
		}
	}

}
