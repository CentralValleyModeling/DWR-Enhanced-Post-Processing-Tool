/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package vista.graph;

public class AnchoredElement {
	Bounded element;
	DoubleRect rect;

	public AnchoredElement(Bounded b, DoubleRect r) {
		element = b;
		rect = r;
	}

	public Bounded getElement() {
		return element;
	}

	public DoubleRect getBounds() {
		return rect;
	}
}
