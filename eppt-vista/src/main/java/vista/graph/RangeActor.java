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
