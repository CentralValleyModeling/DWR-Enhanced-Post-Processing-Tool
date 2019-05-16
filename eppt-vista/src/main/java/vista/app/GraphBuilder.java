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
