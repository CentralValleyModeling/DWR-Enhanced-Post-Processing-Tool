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
package vista.graph;

/**
 * @author Nicky Sandhu
 * @version $Id: PieModel.java,v 1.1 2003/10/02 20:49:06 redwood Exp $
 */
public interface PieModel
{
	/**
	 * get the label for this sector
	 */
	String getLabel();

	/**
	 * get the value for this sector
	 */
	double getValue();

	/**
	 * get the reference object if any for this sector
	 */
	Object getReferenceObject();
}
