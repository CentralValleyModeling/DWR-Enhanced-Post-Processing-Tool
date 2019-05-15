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
/* (swing1.1) */
package vista.gui;

import java.awt.Component;
import java.awt.Graphics;
import javax.swing.*;
import javax.swing.plaf.basic.BasicArrowButton;

/**
 * @version 1.0 02/26/99
 */
public class ArrowIcon implements Icon, SwingConstants {
	private static final int DEFAULT_SIZE = 11;
	// private static final int DEFAULT_SIZE = 5;

	private int size;
	private int iconSize;
	private int direction;
	private boolean isEnabled;
	private BasicArrowButton iconRenderer;

	public ArrowIcon(int direction, boolean isPressedView) {
		this(DEFAULT_SIZE, direction, isPressedView);
	}

	public ArrowIcon(int iconSize, int direction, boolean isEnabled) {
		this.size = iconSize / 2;
		this.iconSize = iconSize;
		this.direction = direction;
		this.isEnabled = isEnabled;
		iconRenderer = new BasicArrowButton(direction);
	}

	public void paintIcon(Component c, Graphics g, int x, int y) {
		iconRenderer.paintTriangle(g, x, y, size, direction, isEnabled);
	}

	public int getIconWidth() {
		// int retCode;
		switch (direction) {
		case NORTH:
		case SOUTH:
			return iconSize;
		case EAST:
		case WEST:
			return size;
		}
		return iconSize;
	}

	public int getIconHeight() {
		switch (direction) {
		case NORTH:
		case SOUTH:
			return size;
		case EAST:
		case WEST:
			return iconSize;
		}
		return size;
	}
}
