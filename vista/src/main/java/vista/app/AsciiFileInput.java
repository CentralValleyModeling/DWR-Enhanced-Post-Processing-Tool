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
package vista.app;

import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.LineNumberReader;
import java.lang.reflect.Array;
import java.lang.reflect.Field;
import java.util.NoSuchElementException;
import java.util.StringTokenizer;

/**
   *
   */
public class AsciiFileInput extends GenericFileInput {
	/**
	 * Initialize streams and/or open connection to file input
	 */
	public void initializeInput(String filename) throws IOException {
		// InputStream is = getInputStream(filename);
		is = getInputStream(filename);
		if (is == null)
			throw new IOException("Could not initialize from " + filename);
		reader = new LineNumberReader(new InputStreamReader(is), 8192 * 2);
		tokenizer = new StringTokenizer(" ");
	}

	/**
	 * reads line and returns as string
	 */
	public String readString() throws IOException {
		return reader.readLine();
	}

	/**
	 * read a short or interpret next data input as short
	 */
	public short readShort() throws IOException {
		String val = getNextToken();
		if (val == null)
			throw new IOException(" No more data ");
		return new Short(val).shortValue();
	}

	/**
	 * read an integer
	 */
	public int readInt() throws IOException {
		String val = getNextToken();
		if (val == null)
			throw new IOException(" No more data ");
		return new Integer(val).intValue();
	}

	/**
	 * read a float
	 */
	public float readFloat() throws IOException {
		String val = getNextToken();
		if (val == null)
			throw new IOException(" No more data ");
		return new Float(val).floatValue();
	}

	/**
	 * read a double
	 */
	public double readDouble() throws IOException {
		String val = getNextToken();
		if (val == null)
			throw new IOException(" No more data ");
		return new Double(val).doubleValue();
	}

	/**
	 * move to next record
	 */
	public void nextRecord() throws IOException {
		readString();
	}

	/**
	 * close the input stream and release resources
	 */
	public void closeStream() throws IOException {
		is.close();
	}

	/**
   *
   */
	private final String getNextToken() throws IOException {
		String val = null;
		try {
			val = tokenizer.nextToken();
		} catch (NoSuchElementException e) {
			tokenizer = new StringTokenizer(reader.readLine());
			val = tokenizer.nextToken();
		}
		return val;
	}

	/**
   * 
   */
	public Object readObject(Object obj) throws IOException {

		Class objectClass = obj.getClass();
		if (objectClass.isArray()) {
			int len = Array.getLength(obj);
			Field[] fields = objectClass.getComponentType().getFields();
			for (int i = 0; i < len; i++) {
				Array.set(obj, i, readThisObject(Array.get(obj, i), fields));
			}
		} else {
			obj = readThisObject(obj, obj.getClass().getFields());
		}

		return obj;
	}

	/**
   * 
   */
	private Object readThisObject(Object obj, Field[] fields)
			throws IOException {
		try {
			for (int i = 0; i < fields.length; i++) {
				// System.out.println(fields[i].getType());
				if (fields[i].getType() == Short.TYPE)
					fields[i].setShort(obj, readShort());
				else if (fields[i].getType() == Integer.TYPE)
					fields[i].setInt(obj, readInt());
				else if (fields[i].getType() == Float.TYPE)
					fields[i].setFloat(obj, readFloat());
				else if (fields[i].getType() == Double.TYPE)
					fields[i].setDouble(obj, readDouble());
			}
		} catch (IllegalAccessException iae) {
			throw new IOException(" Illegal Access Exception " + iae);
		}

		return obj;
	}

	/**
   *
   */
	private LineNumberReader reader;
	/**
   *
   */
	private StringTokenizer tokenizer;
	/**
   *
   */
	private InputStream is;
}
