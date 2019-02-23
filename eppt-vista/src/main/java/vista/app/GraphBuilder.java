/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package vista.app;

import vista.graph.Graph;
import vista.set.DataReference;

/**
 * This is the builder pattern. Its primary purpose is to build a graph from
 * specifications specified elsewhere. For example one instance of the
 * GraphBuilder could use specifications in a file to build the data.
 */
public interface GraphBuilder
{
	/**
	 *
	 */
	void addData(DataReference ref);

	/**
	 *
	 */
	void removeData(DataReference ref);

	/**
	 *
	 */
	void removeAll();

	/**
	 * Create a graph from specifications and return a reference to it.
	 */
	Graph[] createGraphs();
}
