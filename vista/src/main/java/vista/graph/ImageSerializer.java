/*
    Copyright (C) 1996, 1997, 1998 State of California, Department of
    Water Resources.

    VISTA : A VISualization Tool and Analyzer.
	Version 1.0beta
	by Nicky Sandhu
    California Dept. of Water Resources
    Division of Planning, Delta Modeling Section
    1416 Ninth Street
    Sacramento, CA 95814
    (916)-653-7552
    nsandhu@water.ca.gov

    Send bug reports to nsandhu@water.ca.gov

    This program is licensed to you under the terms of the GNU General
    Public License, version 2, as published by the Free Software
    Foundation.

    You should have received a copy of the GNU General Public License
    along with this program; if not, contact Dr. Francis Chung, below,
    or the Free Software Foundation, 675 Mass Ave, Cambridge, MA
    02139, USA.

    THIS SOFTWARE AND DOCUMENTATION ARE PROVIDED BY THE CALIFORNIA
    DEPARTMENT OF WATER RESOURCES AND CONTRIBUTORS "AS IS" AND ANY
    EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
    IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
    PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE CALIFORNIA
    DEPARTMENT OF WATER RESOURCES OR ITS CONTRIBUTORS BE LIABLE FOR
    ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
    CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT
    OR SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA OR PROFITS; OR
    BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
    LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
    (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE
    USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
    DAMAGE.

    For more information about VISTA, contact:

    Dr. Francis Chung
    California Dept. of Water Resources
    Division of Planning, Delta Modeling Section
    1416 Ninth Street
    Sacramento, CA  95814
    916-653-5601
    chung@water.ca.gov

    or see our home page: http://wwwdelmod.water.ca.gov/

    Send bug reports to nsandhu@water.ca.gov or call (916)-653-7552

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
