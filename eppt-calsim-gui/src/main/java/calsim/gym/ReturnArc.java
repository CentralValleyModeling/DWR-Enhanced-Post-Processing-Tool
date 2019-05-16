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

package calsim.gym;

/**
 * An return  arc
 *
 * @author Nicky Sandhu
 */
public class ReturnArc extends Arc
{
	/* the return flow factor*/
	private double _factor;

	/**
	 *
	 */
	public ReturnArc(String name, Node upNode, Node downNode)
	{
		super(name, upNode, downNode);
		_factor = 0.0;
	}

	/**
	 * type abbrev impl.
	 */
	public String getTypeAbbrev()
	{
		return "R";
	}

	/**
	 * type name impl
	 */
	public String getTypeName()
	{
		return "RETURN-ARC";
	}

	/**
	 * gets the return flow factor between 0.0 - 1.0
	 */
	public double getReturnFlowFactor()
	{
		return _factor;
	}

	/**
	 * gets the return flow factor between 0.0 - 1.0
	 */
	public void setReturnFlowFactor(double d)
	{
		_factor = d;
	}
}
