/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package vista.gui;

import javax.swing.*;
import javax.swing.border.Border;
import javax.swing.border.LineBorder;
import javax.swing.plaf.BorderUIResource;
import javax.swing.plaf.ColorUIResource;
import javax.swing.plaf.metal.DefaultMetalTheme;

/**
 * This class describes the default Metal Theme.
 * 
 * @version 1.6 02/04/98
 * @author Steve Wilson
 */

public class VistaMetalTheme extends DefaultMetalTheme {

	public String getName() {
		return "Vista";
	}

	private final ColorUIResource primary1 = new ColorUIResource(0, 0, 0);
	private final ColorUIResource primary2 = new ColorUIResource(188, 188, 204);
	private final ColorUIResource primary3 = new ColorUIResource(220, 220, 255);
	private final ColorUIResource primaryHighlight = new ColorUIResource(102,
			102, 150);

	private final ColorUIResource secondary2 = new ColorUIResource(200, 200,
			230);
	private final ColorUIResource secondary3 = new ColorUIResource(200, 210,
			255);
	private final ColorUIResource controlHighlight = new ColorUIResource(102,
			102, 150);

	protected ColorUIResource getPrimary1() {
		return primary1;
	}

	protected ColorUIResource getPrimary2() {
		return primary2;
	}

	protected ColorUIResource getPrimary3() {
		return primary3;
	}

	public ColorUIResource getPrimaryControlHighlight() {
		return primaryHighlight;
	}

	protected ColorUIResource getSecondary2() {
		return secondary2;
	}

	protected ColorUIResource getSecondary3() {
		return secondary3;
	}

	public ColorUIResource getControlHighlight() {
		return super.getSecondary3();
	}

	public ColorUIResource getFocusColor() {
		return getBlack();
	}

	private ColorUIResource lightBlue = new ColorUIResource(230, 230, 250);

	public ColorUIResource getTextHighlightColor() {
		return getBlack();
	}

	public ColorUIResource getHighlightedTextColor() {
		return lightBlue;
	}

	public ColorUIResource getMenuSelectedBackground() {
		return getBlack();
	}

	public ColorUIResource getMenuSelectedForeground() {
		return lightBlue;
	}

	public ColorUIResource getAcceleratorForeground() {
		return getBlack();
	}

	public ColorUIResource getAcceleratorSelectedForeground() {
		return getWhite();
	}

	public ColorUIResource getWindowBackground() {
		return lightBlue;
	}

	public ColorUIResource getWindowTitleBackground() {
		return getPrimary3();
	}

	public ColorUIResource getWindowTitleForeground() {
		return getBlack();
	}

	public ColorUIResource getWindowTitleInactiveBackground() {
		return getSecondary3();
	}

	public ColorUIResource getWindowTitleInactiveForeground() {
		return getBlack();
	}

	public void addCustomEntriesToTable(UIDefaults table) {

		Border blackLineBorder = new BorderUIResource(
				new LineBorder(getBlack()));
		Border whiteLineBorder = new BorderUIResource(
				new LineBorder(getWhite()));

		table.put("ToolTip.border", blackLineBorder);
		table.put("TitledBorder.border", blackLineBorder);
		table.put("Table.focusCellHighlightBorder", whiteLineBorder);
		table.put("Table.focusCellForeground", getWhite());

	}
}
