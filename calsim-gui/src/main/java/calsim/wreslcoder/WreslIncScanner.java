/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */


package calsim.wreslcoder;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.Enumeration;
import java.util.Hashtable;
import java.util.Vector;

import calsim.wreslcoder.wresl.ASCII_CharStream;
import calsim.wreslcoder.wresl.Token;
import calsim.wreslcoder.wresl.TokenMgrError;
import calsim.wreslcoder.wresl.WreslParser;
import calsim.wreslcoder.wresl.WreslParserConstants;
import calsim.wreslcoder.wresl.WreslParserTokenManager;

/**
 * Scans a Wresl input heirarchy to obtain a list of all the files.
 * Operates quickly using the <CODE>WreslParser</CODE> token manager.
 *
 * @author Armin Munevar
 * @version $Id: WreslIncScanner.java,v 1.1.2.5 2001/07/12 02:00:04 amunevar Exp $
 */

public class WreslIncScanner implements WreslParserConstants
{

	private WreslParserTokenManager tm;
	private File ourFile;
	private FileInputStream fis;

	/**
	 * Constructor method.
	 *
	 * @param wreslFile the file to be scanned
	 * @execption FileNotFoundException
	 */
	public WreslIncScanner(File wreslFile) throws FileNotFoundException
	{
		fis = new FileInputStream(wreslFile);
		ASCII_CharStream in = new ASCII_CharStream(fis, 1, 1);
		tm = new WreslParserTokenManager(in);
		ourFile = wreslFile;
	}

	/**
	 * Constructor method.
	 *
	 * @param wreslFilename the file to be scanned
	 * @execption FileNotFoundException
	 */
	public WreslIncScanner(String wreslFilename) throws FileNotFoundException
	{
		this(new File(WreslParser.nameWithExtension(wreslFilename)));
	}

	/**
	 * For debugging.  Scans a file and prints the list of files newer than a given file.
	 *
	 * @param arg0 The input Wresl file
	 * @param arg1 The object file to compare against
	 */
	public static void main(String[] arg)
	{
		if(arg.length == 2)
		{
			try
			{
				WreslIncScanner inc = new WreslIncScanner(arg[0]);
				Vector names = inc.getNewerList(arg[1]);
				for(Enumeration e = names.elements(); e.hasMoreElements(); )
				{
					String name = (String) e.nextElement();
					System.out.println(name);
				}
			}
			catch(FileNotFoundException ee)
			{
				System.out.println(ee.toString());
			}
		}
		else
		{
			System.out.println("Specify top wresl file (with .wresl) and the compare file.");
		}
	}

	/**
	 * Obtains a list of strings that are the absolute pathnames of
	 * included files that are newer than the specified file.
	 *
	 * @param compareFileName The file name to be used as basis for comparison.
	 *                        File need not exist.
	 * @return A Vector of String objects representing the absolute pathnames of each file
	 * that is newer than the specified file.
	 */
	public Vector getNewerList(String compareFileName) throws FileNotFoundException
	{
		Vector newerFileList = new Vector();
		long compare = new File(compareFileName).lastModified();
		Vector fileList = getIncFileList();
		for(Enumeration e = fileList.elements(); e.hasMoreElements(); )
		{
			File f = (File) e.nextElement();
			if(f.lastModified() > compare)
			{
				newerFileList.addElement(f.getAbsolutePath());
			}
		}
		return newerFileList;
	}

	/**
	 * Scans the file and returns a list of the Wresl input files used.
	 * The list includes the top level file and all of its include family.
	 *
	 * @return a String Vector containing this list
	 * @throws TokenMgrError         A grave parse error occurred.  Rare but possible.
	 * @throws FileNotFoundException Some included file was not found.
	 */

	public Vector getIncFileList() throws FileNotFoundException, TokenMgrError
	{
		Token t;
		Vector fileList = new Vector();

		// First and foremost, note the current file in the list of files!
		fileList.addElement(ourFile);

		try
		{
			while(EOF != (t = tm.getNextToken()).kind)
			{
				if(t.kind == INCLUDE)
				{
					Token fileToken = tm.getNextToken();
					if(fileToken.image == "[")
					{
						fileToken = tm.getNextToken(); // scope token
						fileToken = tm.getNextToken(); // "]" token
						fileToken = tm.getNextToken(); // finally the filename
					}
					File subFile = new File(ourFile.getParent(),
							WreslParser.nameWithExtension(fileToken.image.substring(1,
									fileToken.image.length() - 1)));

					// Open another WreslIncScanner object on this included file
					WreslIncScanner subScanner = new WreslIncScanner(subFile);
					Vector subVector = subScanner.getIncFileList();

					// Append the files found within to our own list
					for(Enumeration e = subVector.elements(); e.hasMoreElements(); )
					{
						fileList.addElement(e.nextElement());
					}
				}
			}
		}
		catch(TokenMgrError e)
		{
			try
			{
				fis.close();
			}
			catch(IOException ioe)
			{
				System.out.println(ioe.getMessage());
			}
			throw new TokenMgrError("In " + ourFile + ": " + e.getMessage(), -1);
		}
		return fileList;
	}

	/**
	 * Retrieves a list of modification dates of the include files.
	 *
	 * @return a hashtable consisting of the filenames and a corresponding <code>Long</code> value.
	 */
	public Hashtable getFileDates() throws FileNotFoundException
	{
		Hashtable hasher = new Hashtable();
		for(Enumeration e = getIncFileList().elements(); e.hasMoreElements(); )
		{
			File theFile = (File) e.nextElement();
			hasher.put(theFile, new Long(theFile.lastModified()));
		}
		return hasher;
	}

}
