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

import java.io.BufferedInputStream;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.MalformedURLException;
import java.net.URL;

/**
 * An input that opens up connections to a local file or a remote file (via
 * http). This is an abstract file that provides the basic functionality in
 * terms of abstract functions These functions are then implemented by concrete
 * classes that can read the data from an ascii or binary stream or a F77 binary
 * stream.
 */
public abstract class GenericFileInput {
	/**
	 * Initialize streams and/or open connection to file input
	 */
	public abstract void initializeInput(String filename) throws IOException;

	/**
	 * read string
	 */
	public abstract String readString() throws IOException;

	/**
	 * read a short or interpret next data input as short
	 */
	public abstract short readShort() throws IOException;

	/**
	 * read an integer
	 */
	public abstract int readInt() throws IOException;

	/**
	 * read a float
	 */
	public abstract float readFloat() throws IOException;

	/**
	 * read a double
	 */
	public abstract double readDouble() throws IOException;

	/**
	 * move to next record
	 */
	public abstract void nextRecord() throws IOException;

	/**
	 * Updates the primitive fields in the object and returns the object
	 */
	public abstract Object readObject(Object obj) throws IOException;

	/**
	 * closes this input stream
	 */
	public abstract void closeStream() throws IOException;

	/**
	 * gets input stream whether to local file or remote file (via http)
	 */
	public final InputStream getInputStream(String filename) {
		URL fileURL = null;
		try {
			if (filename.startsWith("http://"))
				fileURL = new URL(filename);
			else if (filename.startsWith("file:/"))
				fileURL = new URL(filename);
			else {
				return new BufferedInputStream(new FileInputStream(filename));
			}
			return new BufferedInputStream(fileURL.openConnection()
					.getInputStream());
		} catch (MalformedURLException me) {
			System.out.println("Not a local file, trying http connection");
		} catch (IOException ioe) {
			System.out.println("Could not open file " + fileURL);
			System.out.println("Error: " + ioe.getMessage());
		}

		try {
			fileURL = new URL("http://" + filename);
			return new BufferedInputStream(fileURL.openConnection()
					.getInputStream());
		} catch (MalformedURLException me) {
			System.out.println("Failed to establish http connection");
		} catch (IOException ioe) {
			System.out.println("Could not open file " + fileURL);
		}

		return null;
	}

}
