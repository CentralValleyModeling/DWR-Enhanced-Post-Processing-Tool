/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package vista.graph;

import java.awt.print.PageFormat;
import java.awt.print.Paper;
import java.awt.print.PrinterException;
import java.awt.print.PrinterJob;

/**
 *
 */
public class Print2D
{
	/**
	 *
	 */
	public static void print2d(String printer, boolean landscape, GECanvas gec)
	{
		PrinterJob pjob = PrinterJob.getPrinterJob();
		PageFormat pgf = new PageFormat();
		if(landscape)
		{
			pgf.setOrientation(PageFormat.LANDSCAPE);
		}
		else
		{
			pgf.setOrientation(PageFormat.PORTRAIT);
		}
		Paper paper = new Paper();
		double pwidth = (8.5 - 1.0) * 72;
		double pheight = (11 - 1.0) * 72;
		paper.setSize(pwidth + 72, pheight + 72);
		paper.setImageableArea(0.5 * 72, 0.5 * 72, pwidth, pheight);
		pgf.setPaper(paper);
		// show this dialog
		pgf = pjob.pageDialog(pgf);
		if(pgf == null)
		{
			return;
		}
		//
		boolean doPrint = pjob.printDialog();
		if(!doPrint)
		{
			return;
		}
		//
		pjob.setPrintable(new Printable2D(gec), pgf);
		try
		{
			pjob.print();
		}
		catch(PrinterException exc)
		{
			throw new RuntimeException("Printing exception " + exc.getMessage());
		}
	}
}
