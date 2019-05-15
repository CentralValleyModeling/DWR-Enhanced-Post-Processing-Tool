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

/**
 * Defines one term of an LP constraint.  This consists of a name of a decision variable multiplied
 * by a coefficent.  The coefficient is stored herein as a <code>String</code>.  The coefficient
 * must not contain any decision variables.  The coefficient is stored as a <code>public</code>
 * variable so that you may manipulate it externally.
 *
 * @author Armin Munevar
 * @version $Id: LPTerm.java,v 1.1.2.2 2001/07/12 02:00:08 amunevar Exp $
 */
public class LPTerm
{
	/**
	 * The coefficient string
	 */
	public String coef = "1";         // string containing an expression with no dvars
	private String dvar = null;                    // string containing the dvar

	/**
	 * Creates a constant LP term having a coefficient of "1"
	 */
	public LPTerm()
	{
	}

	/**
	 * Creates an LP term consisting of the given components
	 *
	 * @param d the decision variable
	 * @param c the coefficient
	 */
	public LPTerm(String d, String c)
	{
		// optional constructor method.  Not required to use this one.
		if(d != null)
		{
			dvar = d.toUpperCase();
		}
		coef = c.toUpperCase();
	}

	/**
	 * Multiplies the coefficient of this LP term by -1.  This is done with some
	 * rudimentry intelligence to avoid things like -(-(-5))) but it is far from perfect.
	 */
	public void negate()
	{
		if(coef.length() == 0 || coef.equals("1"))
		{
			coef = "-1";
		}
		else if(coef.equals("-1"))
		{
			coef = "1";
		}
		else if(coef.equals("0"))
		{
			// do nothing
			//  } else if (coef.startsWith("-(") && coef.endsWith(")")) {
			//    coef = new String(coef.substring(2,coef.length()-3));
		}
		else
		{
			coef = "-(" + coef + ")";
		}
	}

	/**
	 * Multiplies this term by the specified term.
	 *
	 * @param t The multiplier
	 * @throws ParseException if both the multiplier and this term have non-NULL decision variable
	 *                        components
	 */
	public void multiply(LPTerm t) throws ParseException
	{
		if(t.coef.length() > 0)
		{
			if(coef.equals("1"))
			{
				coef = t.coef;
			}
			else if(!t.coef.equals("1"))
			{
				coef = coef.concat("*(" + t.coef + ")");
			}
		}
		if(t.dvar != null)
		{
			// they specified a decision variable
			if(dvar == null)
			{
				dvar = t.dvar;
			}
			else
			{
				throw new ParseException("Two dvars being nonlinearly combined: "
						+ dvar + ", " + t.dvar);
			}
		}
	}

	/**
	 * Divides this term by the specified term.
	 *
	 * @param t The divisor
	 * @throws ParseException if the divisor has a non-NULL decision variable
	 *                        component
	 */

	public void divide(LPTerm t) throws ParseException
	{
		if(t.coef.length() > 0)
		{
			if(!t.coef.equals("1"))
			{
				coef = coef.concat("/(" + t.coef + ")");
			}
		}
		if(t.dvar != null)
		{
			throw new ParseException("Trying to divide by a dvar :" + t.dvar);
		}
	}

	/**
	 * Retrieves the decision variable component of this term, possibly NULL.
	 *
	 * @return the decision variable component
	 */
	public String getDvar()
	{
		if(dvar == null)
		{
			return null;
		}
		return dvar;
	}

	/**
	 * Set the decision variable component of this term to the specified string.
	 *
	 * @param dv the new decision variable component.  Overwrites the old one.
	 */
	public void setDvar(String dv)
	{
		dvar = dv.toUpperCase();
	}

}
