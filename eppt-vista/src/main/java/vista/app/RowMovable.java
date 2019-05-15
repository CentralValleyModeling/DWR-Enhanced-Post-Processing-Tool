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
