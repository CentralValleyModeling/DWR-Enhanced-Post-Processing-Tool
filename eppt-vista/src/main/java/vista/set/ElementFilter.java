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
package vista.set;

import java.io.Serializable;

/**
 * The interface for filtering functions. The value returned by isAcceptable
 * method is used by the FilterIterator to either accept or reject the value
 *
 * @author Nicky Sandhu
 * @version $Id: ElementFilter.java,v 1.1 2003/10/02 20:49:22 redwood Exp $
 * @seeFilterIterator
 */
public interface ElementFilter extends Serializable
{
	/**
	 * true if value is acceptable
	 */
	boolean isAcceptable(DataSetElement dse);
}
