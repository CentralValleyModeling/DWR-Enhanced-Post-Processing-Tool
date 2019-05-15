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

import java.awt.Component;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Image;
import java.awt.PrintJob;
import java.awt.Toolkit;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.util.Properties;

/**
 * This class saves the image of a given component to a file in various formats
 */
public class ImageSerializer implements Runnable {
	/**
	 * Output format is gif
	 */
	public static final int GIF = 10;
	/**
	 * Output format is jpeg
	 */
	public static final int JPEG = GIF + 1;
	/**
   *
   */
	public static final int PS = JPEG + 1;

	/**
	 * constructor
	 */
	public ImageSerializer(String plotFile, Component comp, int format) {
		_plotFile = plotFile;
		_comp = comp;
		if (format != JPEG)
			_format = format;
		else
			_format = JPEG;
	}

	/**
	 * run
	 */
	public void run() {
		if (_format == PS)
			printPS(_comp, _plotFile);
		else {
			java.awt.Dimension d = _comp.getSize();
			Image osi = _comp.createImage(d.width, d.height);
			java.awt.Graphics osg = osi.getGraphics();
			_comp.print(osg);

			if (_format == GIF)
				printGif(osi, _plotFile);
			else if (_format == JPEG)
				printJpeg(osi, _plotFile);
		}
	}

	/**
   *
   */
	private void printPS(Component comp, String filename) {
		try {
			OutputStream os = new FileOutputStream(filename);
			Dimension d = comp.getSize();
			boolean landscape = false;
			PrintJob pjob = null;
			if (landscape) {
				// pjob = new sun.awt.motif.PSPrintJob( printStream, "Graph", d,
				// 72);
			} else {
				// pjob =
				// new sun.awt.motif.PSPrintJob( printStream, "Graph",
				// new Dimension(d.height, d.width), 72);
			}
			if (pjob == null) {
				// set up printing properties
				Properties printProps = new Properties();
				printProps.put("awt.print.destination", "file");
				printProps.put("awt.print.fileName", filename);
				if (landscape)
					printProps.put("awt.print.orientation", "landscape");
				else
					printProps.put("awt.print.orientation", "portrait");
				pjob = Toolkit.getDefaultToolkit().getPrintJob(null, "",
						printProps);
			}
			Graphics pg = pjob.getGraphics();
			comp.paintAll(pg);
			pg.dispose();
			pjob.end();
		} catch (IOException ioe) {
			System.out.println("Error writing out post script to " + filename);
		}
	}

	/**
	 * prints image in gif format.
	 */

	private void printGif(Image img, String filename) {
		try {
			new vista.graph.GifEncoder(img, new java.io.FileOutputStream(
					filename)).encode();
		} catch (java.io.IOException ioe) {
			System.out.println("Unsuccesful in printing out graphics to "
					+ _plotFile);
		}
	}

	/**
	 * prints image in jpeg format. Acme package has not implemented this yet.
	 */
	private void printJpeg(Image img, String filename) {
		try {
			FileOutputStream fos = new java.io.FileOutputStream(filename);
			new vista.graph.JpegEncoder(img, 85, fos).Compress();
			fos.close();
		} catch (java.io.IOException ioe) {
			System.out.println("Unsuccesful in printing out graphics to "
					+ _plotFile);
		}
	}

	/**
	 * Name of file containing image
	 */
	private String _plotFile;
	/**
	 * Component whose image is to be saved
	 */
	private Component _comp;
	/**
	 * Format for saving image
	 */
	private int _format = JPEG;
}
