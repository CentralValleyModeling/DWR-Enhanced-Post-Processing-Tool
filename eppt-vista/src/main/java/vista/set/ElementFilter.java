/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package vista.set;

import java.io.Serializable;

/**
 * The interface for filtering functions. The value returned by isAcceptable
 * method is used by the FilterIterator to either accept or reject the value
 * 
 * @seeFilterIterator
 * @author Nicky Sandhu
 * @version $Id: ElementFilter.java,v 1.1 2003/10/02 20:49:22 redwood Exp $
 */
public interface ElementFilter extends Serializable {
	/**
	 * true if value is acceptable
	 */
	boolean isAcceptable(DataSetElement dse);
}
