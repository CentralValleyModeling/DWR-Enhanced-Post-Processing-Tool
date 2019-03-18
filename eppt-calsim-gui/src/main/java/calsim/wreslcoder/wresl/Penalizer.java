/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package calsim.wreslcoder.wresl;
//import java.io.*;

/**
 * Handles the Wresl lhs<rhs and lhs>rhs expressions.  Each goal must create
 * two instances of this class, and use them to store the desired penalty values or
 * the fact that it is a hard constraint in one or both directions.
 *
 * @author Armin Munevar
 * @version $Id: Penalizer.java,v 1.1.2.3 2001/07/12 02:00:08 amunevar Exp $
 */
public class Penalizer
{
	private String name;
	private String amount;

	/**
	 * Creates a new penalty indicator that has a hard constraint.
	 */
	public Penalizer()
	{
		name = "constrain";
		amount = "0.0";
	}

	/**
	 * Creates a new penalty indicator that has a penalty of the given amount.
	 *
	 * @param amt A constant equal to the desired penalty.
	 */
	public Penalizer(float amt)
	{
		name = "penalty  ";   // must be nine chars
		amount = new Float(amt).toString();
	}

	/**
	 * Creates a new penalty indicator that has a penalty of the given amount.
	 *
	 * @param amt A String expression that will later evaluate to the desired penalty.
	 */
	public Penalizer(String amt)
	{
		name = "penalty  ";   // must be nine chars
		amount = amt;
	}

	/**
	 * Revise this penalty indicator to have the specified penalty expression
	 *
	 * @param amt A String expression that will later evaluate to the desired penalty.
	 */
	public void newPenalty(String amt)
	{
		name = "penalty  ";   // must be nine chars
		amount = amt;
	}

	/**
	 * Returns a <code>String</code> representation of this penalty expresion.
	 *
	 * @return Such a string.
	 */
	public String asString()
	{
		return name + amount;
	}
}
