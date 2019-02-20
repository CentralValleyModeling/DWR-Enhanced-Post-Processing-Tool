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

import java.awt.FileDialog;
import java.awt.Frame;
import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FilenameFilter;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.PrintWriter;
import java.util.Date;
import java.util.NoSuchElementException;
import java.util.Properties;
import java.util.SortedSet;
import java.util.TreeSet;

/**
 * This class serializes the properties of the graph to a ascii file in the
 * Properties format.
 * 
 * @see java.util.Property
 */
public class AttributeSerializer {
	/**
	 * Initiailize with the graph whose properties are to serialized
	 */
	public AttributeSerializer(GraphicElement g) {
		_graph = g;
	}

	/**
	 * returns true if successful in loading attributes
	 */
	public boolean loadAttributes() {
		boolean success = false;
		FileDialog dialog = new FileDialog(new Frame(),
				"Select properties file...", FileDialog.LOAD);
		FilenameFilter filter = new PropertyFilenameFilter();
		dialog.setFilenameFilter(filter);
		dialog.setFile(_previousFileSaved);
		dialog.pack();
		dialog.setVisible(true);
		if (dialog.getFile() != null) {
			_previousFileSaved = dialog.getFile();
			success = load(dialog.getFile());
		}
		return success;
	}

	/**
	 * save attributes of the graph.
	 */
	public boolean saveAttributes() {
		boolean success = false;
		FileDialog dialog = new FileDialog(new Frame(),
				"Select properties file...", FileDialog.SAVE);
		FilenameFilter filter = new PropertyFilenameFilter();
		dialog.setFilenameFilter(filter);
		dialog.setFile(_previousFileSaved);
		dialog.pack();
		dialog.setVisible(true);
		if (dialog.getFile() != null) {
			_previousFileSaved = dialog.getFile();
			success = save(dialog.getFile());
		}
		return success;
	}

	/**
	 * load attributes and sets properties in graph object.
	 */
	public boolean load(String propertiesFile) {
		boolean success = false;
		Properties p = new Properties();
		String tag = "";
		try {
			InputStream is = new BufferedInputStream(new FileInputStream(
					propertiesFile));
			p.load(is);
			_graph.fromProperties(p, tag);
			success = true;
		} catch (IOException ioe) {
			System.out.println("File " + propertiesFile + " not found");
			System.out.println("Error reading file " + propertiesFile);
		} catch (NoSuchElementException nsee) {
			System.out.println(nsee);
			System.out.println("File " + propertiesFile
					+ " has incorrect format");
		}
		return success;
	}

	/**
	 * load attributes and sets properties in graph object.
	 */
	public boolean load(InputStream is) {
		boolean success = false;
		Properties p = new Properties();
		String tag = "";
		try {
			p.load(is);
			_graph.fromProperties(p, tag);
			success = true;
		} catch (IOException ioe) {
			System.out.println(ioe.getMessage());
		} catch (NoSuchElementException nsee) {
			System.out.println(nsee.getMessage());
		}
		return success;
	}

	/**
   *
   */
	/**
	 * Save attributes to
	 */
	public boolean save(String propertiesFile) {
		boolean success = false;
		Properties p = new Properties();
		String tag = "";
		_graph.toProperties(p, tag);

		if (DEBUG)
			p.list(System.out);
		try {
			OutputStream os = new BufferedOutputStream(new FileOutputStream(
					propertiesFile));
			save(p, os, " Graph Properties ");
			success = true;
			// os.close();
		} catch (FileNotFoundException fnfe) {
			System.out.println("File " + propertiesFile + " not found");
			System.out.println(fnfe);
		} catch (IOException ioe) {
			System.out.println("Could not save properties");
			System.out.println(ioe);
		}
		return success;
	}

	/**
	 * saves properties after sorting using ordered set.
	 */
	public synchronized void save(Properties p, OutputStream out, String header) {
		SortedSet<Object> set = new TreeSet<Object>(p.keySet());

		if (DEBUG)
			System.out.println(set.toString());

		PrintWriter prnt = new PrintWriter(out);
		boolean localize = false;

		if (header != null) {
			prnt.write('#');
			prnt.println(header);
		}
		prnt.write('#');
		prnt.println(new Date());

		for (Object obj : set) {
			String key = (String) obj;
			prnt.print(key);
			prnt.write('=');

			String val = (String) p.get(key);
			int len = val.length();
			boolean empty = false;

			for (int i = 0; i < len; i++) {
				int ch = val.charAt(i);

				switch (ch) {
				case '\\':
					prnt.write('\\');
					prnt.write('\\');
					break;
				case '\t':
					prnt.write('\\');
					prnt.write('t');
					break;
				case '\n':
					prnt.write('\\');
					prnt.write('n');
					break;
				case '\r':
					prnt.write('\\');
					prnt.write('r');
					break;

				default:
					if ((ch < ' ') || (ch >= 127) || (empty && (ch == ' '))) {
						if ((ch > 255) && localize) {
							prnt.write(ch);
						} else {
							prnt.write('\\');
							prnt.write('u');
							prnt.write(toHex((ch >> 12) & 0xF));
							prnt.write(toHex((ch >> 8) & 0xF));
							prnt.write(toHex((ch >> 4) & 0xF));
							prnt.write(toHex((ch >> 0) & 0xF));
						}
					} else {
						prnt.write(ch);
					}
				}
				empty = false;
			}
			prnt.write('\n');
		}
		prnt.flush();
	}

	/**
	 * Convert a nibble to a hex character
	 * 
	 * @param nibble
	 *            the nibble to convert.
	 */
	private static char toHex(int nibble) {
		return hexDigit[(nibble & 0xF)];
	}

	/** A table of hex digits */
	private static char[] hexDigit = { '0', '1', '2', '3', '4', '5', '6', '7',
			'8', '9', 'A', 'B', 'C', 'D', 'E', 'F' };
	private GraphicElement _graph = null;
	/**
   *
   */
	private String _previousFileSaved = "graph.properties";
	/**
   *
   */
	private static final boolean DEBUG = false;

	/**
   * 
   */
	private class PropertyFilenameFilter implements FilenameFilter {
		public boolean accept(File dir, String name) {
			return (name.endsWith(".props") || name.endsWith(".properties"));
		}
	}
}
