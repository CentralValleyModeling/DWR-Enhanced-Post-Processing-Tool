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
package vista.db.dss;

import java.util.ArrayList;
import java.util.Enumeration;

import vista.set.Pathname;

/**
 * Reads a catalog (condensed if name ends in .dsd) file and creates a default
 * group based on that. Returns the group of data references via methods.
 * Enumeration support is provided in case of humongous catalog files so that
 * filtering /sorting can be supported without having to read in the catalog all
 * at once.
 */
class DSSCatalogReader implements Enumeration
{
	/**
	 *
	 */
	static final int MAX_LINE_LENGTH = 132;
	/**
	 *
	 */
	private static final boolean DEBUG = false;
	/**
	 *
	 */
	protected Pathname _masterPath = null;
	private String[] _catalogListing;
	private int _currentIndex = 0;
	private String _currentLine;
	private int _npaths;
	/**
	 *
	 */
	private String[] _parts = new String[Pathname.MAX_PARTS];
	/**
	 *
	 */
	private int[] _beginIndex = new int[Pathname.MAX_PARTS];
	/**
	 *
	 */
	private int[] _endIndex = new int[Pathname.MAX_PARTS];

	/**
	 * Initializes the reader with this file
	 */
	public DSSCatalogReader(String[] catalogListing)
	{
		if(catalogListing == null)
		{
			throw new IllegalArgumentException("Null catalog");
		}
		_catalogListing = catalogListing;
		readCatalog();
	}

	/**
	 * returns a string with .dss replaced by .dsd
	 */
	public static String getCatalogFilename(String dssfile)
	{
		return dssfile.substring(0, dssfile.lastIndexOf(".")).trim() + ".dsd";
	}

	/**
	 * returns a string with .dsd replaced by .dss
	 */
	public static String getDSSFilename(String dsdfile)
	{
		return dsdfile.substring(0, dsdfile.lastIndexOf(".")).trim() + ".dss";
	}

	/**
	 * true if name ends with ".dss"
	 */
	public static boolean isValidDSSFile(String dssfile)
	{
		return (dssfile.endsWith(".dss"));
	}

	// /**
	// *
	// */
	// private static Pattern _pattern;
	// static {
	// try {
	// _pattern = new AwkCompiler().compile("(- )+(-)");
	// } catch ( MalformedPatternException mpe ){
	// mpe.printStackTrace();
	// throw new RuntimeException("Incorrect Regular Expression ");
	// }
	// }
	/**
	 *
	 */
	// private static PatternMatcher _matcher = new AwkMatcher();

	/**
	 * true if name ends with ".dsd"
	 */
	public static boolean isValidDSDFile(String dsdfile)
	{
		return (dsdfile.endsWith(".dsd"));
	}

	/**
	 * Reads in the catalog
	 */
	private void readCatalog()
	{
		while(hasMoreLines())
		{
			if(nextLine().startsWith("Tag"))
			{
				break;
			}
		}
		String line = _currentLine;
		if(line == null)
		{
			throw new IllegalArgumentException("Catalog is empty ?");
		}
		_beginIndex[Pathname.A_PART] = line.indexOf("A Part");
		_endIndex[Pathname.A_PART] = line.indexOf("B Part");
		_beginIndex[Pathname.B_PART] = line.indexOf("B Part");
		_endIndex[Pathname.B_PART] = line.indexOf("C Part");
		_beginIndex[Pathname.C_PART] = line.indexOf("C Part");
		_endIndex[Pathname.C_PART] = line.indexOf("F Part");
		_beginIndex[Pathname.D_PART] = line.indexOf("D Part");
		_endIndex[Pathname.D_PART] = MAX_LINE_LENGTH; // line.length();
		_beginIndex[Pathname.E_PART] = line.indexOf("E Part");
		_endIndex[Pathname.E_PART] = line.indexOf("D Part");
		_beginIndex[Pathname.F_PART] = line.indexOf("F Part");
		_endIndex[Pathname.F_PART] = line.indexOf("E Part");
		// skip one line
		nextLine();
		_npaths = _catalogListing.length - _currentIndex;
	}

	public int getNumberOfPaths()
	{
		return _npaths;
	}

	public boolean hasMoreLines()
	{
		return (_currentIndex < _catalogListing.length);
	}

	public String nextLine()
	{
		// clean out memory
		if(_currentIndex > 0)
		{
			_catalogListing[_currentIndex - 1] = null;
		}
		_currentLine = _catalogListing[_currentIndex].trim();
		_currentIndex++;
		return _currentLine;
	}

	/**
	 * Checks to see if more data references are available.
	 */
	public boolean hasMoreElements()
	{
		return (hasMoreLines() && !_catalogListing[_currentIndex].trim()
																 .startsWith("*"));
	}

	/**
	 * Returns the next data reference
	 */
	public Object nextElement()
	{
		nextLine();
		Pathname path = makePathname(_currentLine);
		return path;
	}

	/**
	 * Returns an array of pathnames for this catalog
	 */
	public Pathname[] getPathnames()
	{
		ArrayList<Pathname> array = new ArrayList<Pathname>();
		while(hasMoreElements())
		{
			array.add((Pathname) nextElement());
		}
		Pathname[] pathnames = new Pathname[array.size()];
		return array.toArray(pathnames);
	}

	/**
	 *
	 */
	protected Pathname makePathname(String line)
	{
		if(DEBUG)
		{
			System.out.println(line);
		}
		if(_masterPath == null)
		{
			_endIndex[Pathname.D_PART] = _currentLine.length();
			for(int i = 0; i < _parts.length; i++)
			{
				_parts[i] = _currentLine
						.substring(_beginIndex[i], _endIndex[i]);
			}
			for(int i = 0; i < _parts.length; i++)
			{
				if(_parts[i].indexOf("(null)") >= 0)
				{
					_parts[i] = "";
				}
			}
			if(_parts[Pathname.D_PART].indexOf("*") >= 0)
			{
				int inx = _parts[Pathname.D_PART].indexOf("*");
				_parts[Pathname.D_PART] = _parts[Pathname.D_PART].substring(0,
						inx);
			}
			_masterPath = Pathname.createPathname(_parts);
		}
		else
		{
			_endIndex[Pathname.D_PART] = _currentLine.length();
			if(DEBUG)
			{
				System.out.println(_currentLine);
			}
			for(int i = 0; i < _parts.length; i++)
			{
				if(DEBUG)
				{
					System.out.println("index " + i);
				}
				if(DEBUG)
				{
					System.out.println("begin index " + _beginIndex[i]);
				}
				if(DEBUG)
				{
					System.out.println("end index " + _endIndex[i]);
				}
				int beginIndex = Math.min(_beginIndex[i], _currentLine.length());
				int endIndex = Math.min(_endIndex[i], _currentLine.length());
				_parts[i] = _currentLine.substring(beginIndex, endIndex);
			}
			updateFrom(_parts, _masterPath);
			for(int i = 0; i < _parts.length; i++)
			{
				if(_parts[i].indexOf("(null)") >= 0)
				{
					_parts[i] = "";
				}
			}
			if(_parts[Pathname.D_PART].indexOf("*") >= 0)
			{
				int inx = _parts[Pathname.D_PART].indexOf("*");
				_parts[Pathname.D_PART] = _parts[Pathname.D_PART].substring(0,
						inx);
			}
			Pathname path = Pathname.createPathname(_parts);
			_masterPath = path;
		}
		return _masterPath;

	}

	/**
	 *
	 */
	private void updateFrom(String[] parts, Pathname path)
	{
		for(int i = 0; i < parts.length; i++)
		{
			// if ( _matcher.matches( parts[i], _pattern ) ) parts[i] =
			// path.getPart(i);
			if(parts[i].indexOf("- -") >= 0)
			{
				parts[i] = path.getPart(i);
			}
		}
	}
}
