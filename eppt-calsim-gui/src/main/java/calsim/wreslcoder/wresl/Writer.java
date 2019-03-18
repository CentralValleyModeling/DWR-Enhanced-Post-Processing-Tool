/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package calsim.wreslcoder.wresl;
//import java.util.Vector;

import java.io.File;
import java.io.PrintWriter;

/**
 * This class is for writing to text files when parsing in order to prevent the error when
 * Windows thinks two objects have simultaneous claims to a file.
 *
 * @author Clay Booher
 */

public class Writer
{

	//CB	private PrintWriter _writer;

	/**
	 * Creates a new instance.
	 */
	public Writer()
	{
	}

	/**
	 * Creates a new instance with specified <code>PrintWriter</code>.
	 *
	 * @param writer
	 */
/*CB	public Writer(PrintWriter writer) {
		_writer = writer;
	}

	void println(String text) {
		if (_writer != null)
			_writer.println(text);
	} */
	synchronized void println(PrintWriter writer, String text)
	{
		if(writer != null)
		{
			if(writer == WreslParser.localDefines)
			{
				File file = new File(
						WreslParser.rootDirectory + "\\deflocal.txt"); //CB TODO add IV to WRESLPARSER FOR deflocal.txt full pathname
				while(!file.canWrite())
				{
				}  //CB trying to wait for deflocal.txt to be freed before writing to it
				writer.println(text);
			}
			else
			{
				writer.println(text);
			}
		}
	}

	synchronized void println(PrintWriter writer, StringBuffer buffer)
	{
		this.println(writer, buffer.toString());
	}
}
