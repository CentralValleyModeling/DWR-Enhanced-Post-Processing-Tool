/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */

package calsim.app;

import java.io.FileReader;
import java.io.IOException;
import java.io.LineNumberReader;
import java.util.Vector;

/**
 * This class parses a comma delimited file
 *
 * @author Nicky Sandhu ,Armin Munevar
 * @version $Id: CSVParser.java,v 1.1.2.6 2001/07/12 01:58:22 amunevar Exp $
 */
public class CSVParser
{
	// save time on creating lots of vector objects only
	// to be discarded a short time later
	private static Vector _sv = new Vector();
	private String _file;
	private LineNumberReader _reader;
	private String _delimiter;

	/**
	 * opens a file delimited by "," for reading
	 */
	public CSVParser(String file)
	{
		try
		{
			_file = file;
			_reader = new LineNumberReader(new FileReader(file));
			_delimiter = ",";
		}
		catch(IOException ioe)
		{
			throw new RuntimeException("IOError: " + ioe.getMessage());
		}
	}

	/**
	 * opens a file delimited by a user specified delimiter for reading
	 */
	public CSVParser(String file, String delimiter)
	{
		try
		{
			_file = file;
			_reader = new LineNumberReader(new FileReader(file));
			_delimiter = delimiter;
		}
		catch(IOException ioe)
		{
			throw new RuntimeException("IOError: " + ioe.getMessage());
		}
	}

	/**
	 * @return an array of strings split on the delimiter or null
	 */
	public String[] nextLine()
	{
		try
		{
			String line = _reader.readLine();
			if(line == null)
			{
				_reader.close();
				return null;
			}
			line = line.trim();
			String[] tokens = split(line, _delimiter);
			return tokens;
		}
		catch(IOException ioe)
		{
			System.err.println("Error occured in reading file: " + _file);
			System.err.println("Error msg: " + ioe.getMessage());
			return null;
		}
	}

	/**
	 * @return an array of strings split on the delimiter or null
	 */
	public void close()
	{
		try
		{
			_reader.close();
		}
		catch(IOException ioe)
		{
			System.err.println("Error occured in closing file: " + _file);
			System.err.println("Error msg: " + ioe.getMessage());
		}
	}

	/**
	 *
	 */
	public String[] split(String line, String delimiter)
	{
		if(line == null)
		{
			return null;
		}
		int cindex = 0; // current position
		_sv.removeAllElements();
		while(true)
		{
			if(cindex >= line.length())
			{
				break;
			}
			// get index of delimiter from current position
			int sindex = line.indexOf(delimiter, cindex);
			// if not found get last part of string and break
			if(sindex == -1)
			{
				_sv.addElement(line.substring(cindex).trim());
				break;
			}
			_sv.addElement(line.substring(cindex, sindex).trim());
			cindex = sindex + delimiter.length();
		}
		if(_sv.size() == 0)
		{
			return null;
		}
		String[] sarray = new String[_sv.size()];
		_sv.copyInto(sarray);
		return sarray;
	}
}
