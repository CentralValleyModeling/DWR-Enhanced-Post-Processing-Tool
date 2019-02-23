/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package vista.app;

import java.awt.Point;

/**
 * An interface for those Tables that want the ability to have their rows moved
 * around by RowMoveListener
 *
 * @author Nicky Sandhu
 * @version $Id: RowMovable.java,v 1.1 2003/10/02 20:48:39 redwood Exp $
 */
public interface RowMovable
{
	/**
	 * returns the row number at point p
	 */
	int rowAtPoint(Point p);

	/**
	 * moves row at oldPosition to newPosition
	 */
	void moveRow(int oldPosition, int newPosition);
}
