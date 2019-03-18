/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package vista.graph;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.FlowLayout;
import java.awt.GridLayout;
import java.awt.Label;
import javax.swing.*;
import javax.swing.border.Border;

import vista.gui.Changeable;

/**
 * A dialog class for handling attribute change requests.
 */
public class GEDialogPanel extends JPanel implements Changeable {
	/**
	 * The constructor for initializing class variables and creating panel.
	 */
	public GEDialogPanel(GraphicElement ge) {
		_ge = ge;
		_attr = _ge.getAttributes();
		setLayout(new BorderLayout());
		add(createPanel(), BorderLayout.CENTER);
	}

	/**
	 * get the graphic element
	 */
	public GraphicElement getGraphicElement() {
		return _ge;
	}

	/**
	 * creates Panel with controls to control each attribute
	 */
	protected JPanel createPanel() {
		JPanel mainPanel;
		mainPanel = new JPanel();
		mainPanel.setLayout(new GridLayout(5, 1));
		Border eBorder = BorderFactory.createRaisedBevelBorder();
		Border border = BorderFactory.createTitledBorder(eBorder, _ge
				.toString());
		mainPanel.setBorder(border);
		bgChooser = new ColorChoice(Labels.BACKGROUND_COLOR,
				_attr._backgroundColor);
		fgChooser = new ColorChoice(Labels.FOREGROUND_COLOR,
				_attr._foregroundColor);
		// orientation
		JPanel oPanel = new JPanel();
		oPanel.setLayout(new FlowLayout(FlowLayout.LEFT));
		oPanel.add(new Label(Labels.ORIENTATION));
		orientationChoice = new JComboBox();
		orientationChoice.addItem(Labels.HORIZONTAL);
		orientationChoice.addItem(Labels.VERTICAL);
		if (_attr._orientation == GEAttr.HORIZONTAL)
			orientationChoice.setSelectedItem(Labels.HORIZONTAL);
		else
			orientationChoice.setSelectedItem(Labels.VERTICAL);
		oPanel.add(orientationChoice);

		visibilityCheckBox = new JCheckBox(Labels.IS_VISIBLE, _attr._isVisible);
		visibilityCheckBox.setHorizontalAlignment(JCheckBox.LEFT);
		visibilityCheckBox.setHorizontalTextPosition(JCheckBox.LEFT);

		clipCheckBox = new JCheckBox(Labels.CLIP_WITHIN_BOUNDS,
				_attr._clipWithinBounds);
		clipCheckBox.setHorizontalAlignment(JCheckBox.LEFT);
		clipCheckBox.setHorizontalTextPosition(JCheckBox.LEFT);

		insetsPanel = new InsetsEditPanel(_ge.getInsets());
		//
		JPanel attrPanel = new JPanel();
		attrPanel.setLayout(new GridLayout(3, 1));
		attrPanel.add(oPanel);
		attrPanel.add(visibilityCheckBox);
		attrPanel.add(clipCheckBox);
		attrPanel.setBorder(BorderFactory.createLineBorder(Color.black));
		//
		JPanel colorPanel = new JPanel();
		colorPanel.setLayout(new GridLayout(2, 1));
		colorPanel.add(bgChooser);
		colorPanel.add(fgChooser);
		colorPanel.setBorder(BorderFactory.createLineBorder(Color.black));
		//
		mainPanel.add(colorPanel);
		mainPanel.add(attrPanel);
		mainPanel.add(insetsPanel);
		mainPanel.setLayout(new GridLayout(mainPanel.getComponentCount(), 1));

		return mainPanel;
	}

	private ColorChoice bgChooser, fgChooser;
	private JCheckBox visibilityCheckBox, clipCheckBox;
	private JComboBox orientationChoice;
	private InsetsEditPanel insetsPanel;

	/**
	 * applies the changes to the attribute object and/or the graphic element
	 */
	public void applyChanges() {
		_attr._backgroundColor = bgChooser.getColor();
		_attr._foregroundColor = fgChooser.getColor();
		//
		if (orientationChoice.getSelectedItem().equals(Labels.HORIZONTAL))
			_attr._orientation = GEAttr.HORIZONTAL;
		else
			_attr._orientation = GEAttr.VERTICAL;
		//
		_attr._isVisible = visibilityCheckBox.isSelected();
		_attr._clipWithinBounds = clipCheckBox.isSelected();
		_ge.setInsets(insetsPanel.getInsets());
	}

	/**
   *
   */
	public void doneChanges() {
	}

	/**
	 * The graphic element whose attributes are to be displayed
	 */
	private GraphicElement _ge;
	/**
	 * The attributes of the graphic element.
	 */
	private GEAttr _attr;
}
