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

package vista.db.hdf5;

import java.util.ArrayList;

class Reservoir {
	private String name;
	private double area;
	private double bottomElevation;
	private ArrayList<Integer> nodes;
	private ArrayList<Integer> gateNodes;
	
	public Reservoir(String name){
		this.name = name;
		nodes = new ArrayList<Integer>();
		gateNodes = new ArrayList<Integer>();
	}
	
	public void addNode(int nodeId){
		nodes.add(new Integer(nodeId));
	}
	
	public void addGateNode(int nodeId){
		gateNodes.add(new Integer(nodeId));
	}
	
	public int getNumberOfConnections(){
		return nodes.size() + gateNodes.size();
	}
	
	public Integer[] getNodes(){
		Integer[] nodeArray = new Integer[nodes.size()];
		return nodes.toArray(nodeArray);
	}
	
	public Integer[] getGateNodes(){
		Integer[] gateNodeArray = new Integer[gateNodes.size()];
		return gateNodes.toArray(gateNodeArray);
	}

	//FIXME: Assumption is that the index refers to nodes first then gate nodes
	public int getConnection(int index){
		if (index > nodes.size()){
			return gateNodes.get(index-nodes.size());
		} else {
			return nodes.get(index);
		}
	}

	public void setArea(double area) {
		this.area = area;
	}

	public double getArea() {
		return area;
	}

	public void setBottomElevation(double bottomElevation) {
		this.bottomElevation = bottomElevation;
	}

	public double getBottomElevation() {
		return bottomElevation;
	}
}
