/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package vista.graph;

import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Rectangle;
import java.awt.print.PageFormat;
import java.awt.print.Printable;
import java.awt.print.PrinterException;

/**
  *
  *
  */
public class Printable2D implements Printable {
	private GECanvas gec;

	/**
    *
    */
	public Printable2D(GECanvas gec) {
		this.gec = gec;
	}

	/**
    *
    */
	public int print(Graphics graphics, PageFormat pageFormat, int pageIndex)
			throws PrinterException {
		try {
			if (pageIndex > 0)
				return NO_SUCH_PAGE;
			Graphics2D g2d = (Graphics2D) graphics;
			g2d.translate(pageFormat.getImageableX(), pageFormat
					.getImageableY());
			Rectangle rb = gec.getBounds();
			double sx = pageFormat.getImageableWidth() / rb.width;
			double sy = pageFormat.getImageableHeight() / rb.height;
			double smin = Math.min(sx, sy);
			g2d.scale(smin, smin);
			gec.setDoubleBuffered(false);
			gec.paintAll(g2d);
			return PAGE_EXISTS;
		} catch (Exception e) {
			throw new RuntimeException(e.getMessage());
		} finally {
			gec.setDoubleBuffered(true);
		}
	}
}
