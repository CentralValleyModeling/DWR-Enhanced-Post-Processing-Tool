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
