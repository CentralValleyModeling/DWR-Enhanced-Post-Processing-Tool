/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */

package vista.set;

import vista.time.Time;

public interface TimeSeriesIterator extends DataSetIterator
{

	/**
	 * positions iterator at index
	 */
	void positionAtTime(Time tm);
}
