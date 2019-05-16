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


package calsim.wreslcoder.wresl;

import java.util.Hashtable;


/**
 * Defines and maps predefined Wresl reserved words to their Fortran equivalent.  Contains a static
 * list of these.
 *
 * @author Armin Munevar
 * @version $Id: ReservedWords.java,v 1.1.2.6.2.1 2002/05/02 01:06:27 adraper Exp $
 */
public class ReservedWords
{
	private Hashtable words = new Hashtable(10);

	/**
	 * Constructor.  Creates a new Reserved Words checker instance and fills up a private hash table.
	 * The words are not case-sensitive.
	 */
	public ReservedWords()
	{
		//         Wresl         Fortran             Wresl items must be all lower case here
		//         -----         -------
		// date variables
		words.put("month", "date%month");
		words.put("day", "date%day");
		words.put("wateryear", "date%wateryear");
		//        words.put("week",       "date%week");
		words.put("daysin", "date%deltaT_days");
		words.put("daysinmonth", "daysInMonth(date)");  //DE added for daily timestep******************
		// month variables
		words.put("oct", "1");
		words.put("nov", "2");
		words.put("dec", "3");
		words.put("jan", "4");
		words.put("feb", "5");
		words.put("mar", "6");
		words.put("apr", "7");
		words.put("may", "8");
		words.put("jun", "9");
		words.put("jul", "10");
		words.put("aug", "11");
		words.put("sep", "12");
		// previous month variables
		words.put("prevoct", "prevoct()");
		words.put("prevnov", "prevnov()");
		words.put("prevdec", "prevdec()");
		words.put("prevjan", "prevjan()");
		words.put("prevfeb", "prevfeb()");
		words.put("prevmar", "prevmar()");
		words.put("prevapr", "prevapr()");
		words.put("prevmay", "prevmay()");
		words.put("prevjun", "prevjun()");
		words.put("prevjul", "prevjul()");
		words.put("prevaug", "prevaug()");
		words.put("prevsep", "prevsep()");
		// unit conversion terms
		words.put("cf_taf", "cf_taf()");
		words.put("taf_cf", "taf_cf()");
		words.put("af_taf", "af_taf()");
		words.put("taf_af", "taf_af()");
		//		words.put("ckm_taf",    "ckm_taf()");
		//		words.put("taf_ckm",    "taf_ckm()");
		//		words.put("cdm_taf",    "cdm_taf()");
		//		words.put("taf_cdm",    "taf_cdm()");
		//		words.put("cm_taf",    "cm_taf()");
		//		words.put("taf_cm",    "taf_cm()");
		words.put("taf_cfs", "taf_cfs()");
		words.put("cfs_taf", "cfs_taf()");
		words.put("af_cfs", "af_cfs()");
		words.put("cfs_af", "cfs_af()");
		//		words.put("cf_cfs",    "cf_cfs()");
		//		words.put("cfs_cf",    "cfs_cf()");
		//		words.put("ckm_cfs",    "ckm_cfs()");
		//		words.put("cfs_ckm",    "cfs_ckm()");
		//		words.put("cdm_cfs",    "cdm_cfs()");
		//		words.put("cfs_cdm",    "cfs_cdm()");
		//		words.put("cms_cfs",    "cms_cfs()");
		//		words.put("cfs_cms",    "cfs_cms()");
		// F90 Intrinsics
		//		words.put("abs",        "abs");
		//		words.put("int",        "int");
		//		words.put("max",        "max");
		//		words.put("min",        "min");
		//		words.put("mod",        "mod");
		//		words.put("real",       "real");
		//		words.put("pow",        "pow");
		//		words.put("exp",        "exp");
		//		words.put("log",        "log");
		//		words.put("log10",      "log10");
		//		words.put("sqrt",      "sqrt");
		// reserved indexing variables
		words.put("i", "i");
		words.put("j", "j");
		words.put("k", "k");
		words.put("position", "p");
		words.put("true", "1");
		words.put("false", "0");

	}

	/**
	 * Checks to see if a particular word is a reserved word.
	 *
	 * @param word The word to check
	 * @return Returns <code>True</code> if <code>word</code> is in the list.
	 */
	boolean isIn(String word)
	{
		return (words.containsKey(word.toLowerCase()));
	}

	/**
	 * Returns the Fortran equivalent of a given reserved word.  If the passed word is
	 * not in the reserved-words list, it returns that word.
	 *
	 * @param word The word to check
	 * @return A String that is the Fortran equivalent of <code>word</code>.
	 */
	String fortranExpr(String word)
	{
		String item;
		item = (String) words.get(word.toLowerCase());
		if(item == null)
		{
			return word;
		}
		else
		{
			return item;
		}
	}


}
