/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package vista.graph;

/**
 * This is the interface implemented by a class that is interested in using the
 * range selector and which needs to be informed about the range selected and
 * when the selection operation is completed
 *
 * @author Nicky Sandhu
 * @version $Id: RangeActor.java,v 1.2 1998/10/08 00:03:52 nsandhu Exp $
 */
public interface RangeActor
{
	/**
	 *
	 */
	void selectedRange(int xmin, int xmax, int ymin, int ymax);
}
