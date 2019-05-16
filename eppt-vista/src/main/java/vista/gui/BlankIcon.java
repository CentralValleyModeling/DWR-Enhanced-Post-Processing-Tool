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

import java.awt.Color;
import java.awt.Component;
import java.awt.Graphics;
import javax.swing.*;

/**
 * @version 1.0 02/26/99
 */
public class BlankIcon implements Icon
{
	private Color fillColor;
	private int size;

	public BlankIcon()
	{
		this(null, 11);
	}

	public BlankIcon(Color color, int size)
	{
		// UIManager.getColor("control")
		// UIManager.getColor("controlShadow")
		fillColor = color;

		this.size = size;
	}

	public void paintIcon(Component c, Graphics g, int x, int y)
	{
		if(fillColor != null)
		{
			g.setColor(fillColor);
			g.drawRect(x, y, size - 1, size - 1);
		}
	}

	public int getIconWidth()
	{
		return size;
	}

	public int getIconHeight()
	{
		return size;
	}
}
