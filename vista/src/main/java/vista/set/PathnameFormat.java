/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package vista.set;

/**
 * A format class for templates in pathname ids
 *
 * @author Nicky Sandhu
 * @version $Id: PathnameFormat.java,v 1.1 2003/10/02 20:49:29 redwood Exp $
 */
public class PathnameFormat
{
	/**
	 * takes a template string and the associated reference and generates a new
	 * string with the template filled in. The template is of the form (string)*
	 * (([A|B|C|D|E|F])(string))*
	 */
	public static String format(String template, DataReference ref)
	{
		String str = template;
		Pathname path = ref.getPathname();
		str = replace(str, "[A]", path.getPart(Pathname.A_PART));
		str = replace(str, "[B]", path.getPart(Pathname.B_PART));
		str = replace(str, "[C]", path.getPart(Pathname.C_PART));
		str = replace(str, "[D]", path.getPart(Pathname.D_PART));
		str = replace(str, "[E]", path.getPart(Pathname.E_PART));
		str = replace(str, "[F]", path.getPart(Pathname.F_PART));
		return str;
	}

	/**
	 * formats a template with the given data references. Order of these
	 * references matter in the template The template is of the following form
	 * (string)* ([A|B|C|D|E|F($#)] string)* where $# is the $ sign with a
	 * number from 1 to refs.length
	 */
	public static String format(String template, DataReference[] refs)
	{
		String str = template;
		for(int i = 0; i < refs.length; i++)
		{
			Pathname path = refs[i].getPathname();
			str = replace(str, "[A$" + (i + 1) + "]", path
					.getPart(Pathname.A_PART));
			str = replace(str, "[B$" + (i + 1) + "]", path
					.getPart(Pathname.B_PART));
			str = replace(str, "[C$" + (i + 1) + "]", path
					.getPart(Pathname.C_PART));
			str = replace(str, "[D$" + (i + 1) + "]", path
					.getPart(Pathname.D_PART));
			str = replace(str, "[E$" + (i + 1) + "]", path
					.getPart(Pathname.E_PART));
			str = replace(str, "[F$" + (i + 1) + "]", path
					.getPart(Pathname.F_PART));
		}
		return str;
	}

	/**
	 * replaces string "toBe" with string "with" in string "orig"
	 */
	public static String replace(String orig, String toBe, String with)
	{
		String nstr = orig;
		int index = nstr.indexOf(toBe);
		while(index >= 0)
		{
			int nl = nstr.length();
			int tbl = toBe.length();
			nstr = new StringBuffer(nl - tbl + with.length()).append(
					nstr, 0, index).append(with).append(
					nstr, index + tbl, nl).toString();
			index = nstr.indexOf(toBe, index);
		}
		return nstr;
	}
}
