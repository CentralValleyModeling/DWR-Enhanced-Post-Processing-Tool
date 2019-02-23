/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package vista.set;

import java.io.Serializable;
import java.util.Hashtable;
import java.util.StringTokenizer;

/**
 * Pathname is a DSS reference to the data stored. Its format is /A/B/C/D/E/F/
 * where each string between the slashes is called a pathname part.
 */
public class Pathname implements Serializable {
	/**
	 * A part index
	 */
	public static final int A_PART = 0;
	/**
	 * B part index
	 */
	public static final int B_PART = A_PART + 1;
	/**
	 * C part index
	 */
	public static final int C_PART = A_PART + 2;
	/**
	 * D part index
	 */
	public static final int D_PART = A_PART + 3;
	/**
	 * E part index
	 */
	public static final int E_PART = A_PART + 4;
	/**
	 * F part index
	 */
	public static final int F_PART = A_PART + 5;
	/**
	 * Maximum number of parts of a pathname
	 */
	public static final int MAX_PARTS = 6;
	/**
	 * separator for seperating parts in a string representation of the pathname
	 */
	public static final String SEPARATOR = "/";

	/**
	 * private constructor
	 */
	private Pathname() {
		_parts = new String[MAX_PARTS];
	}

	/**
	 * creates a pathname from parts
	 */
	public static Pathname createPathname(String[] parts) {
		// check _parts and length
		if (parts == null)
			return null;
		if (parts.length != MAX_PARTS)
			return null;

		Pathname p = new Pathname();

		for (int i = 0; i < parts.length; i++) {
			String sPart = NULL_STRING_REP;
			if (parts[i] != null)
				sPart = parts[i].trim().toUpperCase();
			if (MEMSAVE)
				storeInTable(sPart);
			if (MEMSAVE)
				p._parts[i] = getFromTable(sPart);
			else
				p._parts[i] = sPart;
		}
		return p;
	}

	/**
	 * creates a pathname from the string
	 */
	public static Pathname createPathname(String pathname) {
		if (pathname == null)
			return null;
		if (!pathname.startsWith(SEPARATOR))
			return null;
		StringTokenizer st = new StringTokenizer(pathname, SEPARATOR, true);
		int ntokens = st.countTokens();
		Pathname p = new Pathname();
		int partId = -1;
		while (st.hasMoreTokens() && partId < MAX_PARTS) {
			String token = st.nextToken().trim().toUpperCase();
			if (token.equals(SEPARATOR)) {
				partId++;
				continue;
			}
			if (MEMSAVE)
				storeInTable(token);
			if (MEMSAVE)
				p._parts[partId] = getFromTable(token);
			else
				p._parts[partId] = token;
		}
		// replace null strings
		for (int i = 0; i < MAX_PARTS; i++) {
			if (p._parts[i] == null) {
				if (MEMSAVE)
					storeInTable(NULL_STRING_REP);
				if (MEMSAVE)
					p._parts[i] = getFromTable(NULL_STRING_REP);
				else
					p._parts[i] = NULL_STRING_REP;
			}
		}
		return p;
	}

	/**
	 * creates a pathname from the Pathname object
	 */
	public static Pathname createPathname(Pathname path) {
		if (path == null)
			return null;
		Pathname p = new Pathname();
		for (int i = 0; i < MAX_PARTS; i++)
			p._parts[i] = path._parts[i];
		return p;
	}

	/**
	 * returns the index to the part of the pathname containing the string else
	 * returns -1; For anything more complex such as regular expression use
	 * filtering on parts
	 * 
	 * @see DataReferenceFilter
	 */
	public int getPartId(String part) {
		for (int i = 0; i < _parts.length; i++) {
			if (_parts[i].indexOf(part) >= 0)
				return i;
		}
		return -1;
	}

	/**
	 * Sets the part specified to the value specified. If the part specified is
	 * not found nothing is done. If the value is null then nothing is done.
	 * 
	 * @partId Identity of the part as defined by the public variables ?_PART.
	 * @part The new value for the part.
	 */
	public void setPart(int partId, String part) {
		// check partId
		if (!isPartIdOK(partId))
			return;
		String sPart = part.trim().toUpperCase();
		if (MEMSAVE)
			storeInTable(sPart);
		if (MEMSAVE)
			_parts[partId] = getFromTable(sPart);
		else
			_parts[partId] = sPart;
	}

	/**
	 * Gets the part for the particular part identity as defined by the public
	 * variables ?_PART.
	 */
	public String getPart(int partId) {
		if (!isPartIdOK(partId))
			return null;
		return _parts[partId];
	}

	/**
	 * @return the full pathname of the form
	 *         /A_PART/B_PART/C_PART/D_PART/E_PART/F_PART/
	 */
	public String getFullPath() {
		return createNameFromParts();
	}

	/**
	 * a string representation of the pathname
	 */
	public String toString() {
		return getFullPath();
	}

	/**
	 * two pathnames are equal iff they have the same parts
	 */
	public boolean equals(Object obj) {
		return (obj != null) && (obj instanceof Pathname)
				&& (isSameAs((Pathname) obj));
	}

	/**
	 * checks if string representation of parts is equal using the equals()
	 * method of java.lang.String
	 */
	public boolean isSameAs(Pathname path) {
		return (path != null) && (path.toString().equals(this.toString()));
	}

	/**
	 * gets the string representation for the part id...
	 */
	public static String getPartName(int partId) {
		switch (partId) {
		case A_PART:
			return "A PART";
		case B_PART:
			return "B PART";
		case C_PART:
			return "C PART";
		case D_PART:
			return "D PART";
		case E_PART:
			return "E PART";
		case F_PART:
			return "F PART";
		default:
			return NULL_STRING_REP;
		}
	}

	/**
   *
   */
	public static int getPartNumber(String partName) {
		if (partName.equals("A PART")) {
			return A_PART;
		} else if (partName.equals("B PART")) {
			return B_PART;
		} else if (partName.equals("C PART")) {
			return C_PART;
		} else if (partName.equals("D PART")) {
			return D_PART;
		} else if (partName.equals("E PART")) {
			return E_PART;
		} else if (partName.equals("F PART")) {
			return F_PART;
		} else {
			throw new IllegalArgumentException(partName
					+ " is not a valid part name");
		}
	}

	/**
	 * creates pathnames from parts.
	 */
	private String createNameFromParts() {
		StringBuffer buf = new StringBuffer(16 + _parts[A_PART].length()
				+ _parts[B_PART].length() + _parts[C_PART].length()
				+ _parts[D_PART].length() + _parts[E_PART].length()
				+ _parts[F_PART].length());
		String backSlash = SEPARATOR;
		return buf.append(backSlash).append(_parts[A_PART]).append(backSlash)
				.append(_parts[B_PART]).append(backSlash)
				.append(_parts[C_PART]).append(backSlash)
				.append(_parts[D_PART]).append(backSlash)
				.append(_parts[E_PART]).append(backSlash)
				.append(_parts[F_PART]).append(backSlash).toString();
	}

	/**
	 * checks if part id is valid
	 */
	boolean isPartIdOK(int partId) {
		return (partId >= 0 && partId <= MAX_PARTS);
	}

	/**
	 * stores key/value in hashtable.
	 */
	private static void storeInTable(String value) {
		if (_partTable.contains(value)) {
		} else {
			_partTable.put(value, value);
		}
	}

	/**
	 * retrieves value from table.
	 */
	private static String getFromTable(String key) {
		return (String) _partTable.get(key);
	}

	/**
	 * The parts of the pathname stored for retreival speed.
	 */
	private String[] _parts;
	/**
	 * a global table to contain parts of pathnames
	 */
	private static Hashtable _partTable = new Hashtable();
	/**
	 * a variable to decide whether or not to use hashtable
	 */
	private static final boolean MEMSAVE = false;
	/**
   *
   */
	private static final String NULL_STRING_REP = "";
}
