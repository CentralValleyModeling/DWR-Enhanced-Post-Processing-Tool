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

package vista.dsm2.input.gui;

import gov.ca.dsm2.input.model.Channels;
import gov.ca.dsm2.input.model.DSM2Model;
import gov.ca.dsm2.input.model.Node;
import gov.ca.dsm2.input.model.Nodes;
import gov.ca.dsm2.input.model.calculator.ModelUtils;
import gov.ca.dsm2.input.parser.Parser;
import gov.ca.dsm2.input.parser.Tables;

/**
 * Identify DSM2 nodes that have more than two channels connected to them 
 * These would be true junctions as opposed to artifacts of just modeling a long single stream as a series of channels.
 * 
 * @author psandhu
 *
 */
public class DSM2IdentifyJunctionsInGrid {
	public static void main(String [] args) throws Exception{
		String channelsFile = args[0];
		Parser p = new Parser();
		Tables tables = p.parseModel(channelsFile);
		DSM2Model dsm2Model = tables.toDSM2Model();
		Channels channels = dsm2Model.getChannels();
		Nodes nodes = dsm2Model.getNodes();
		for(Node node: nodes.getNodes()){
			String channelsConnectedTo = ModelUtils.getChannelsConnectedTo(channels, node);
			String[] channelIds = channelsConnectedTo.split(",");
			if (channelIds.length > 2){
				System.out.println("Node: "+node.getId()+" | Channels: "+ channelsConnectedTo);
			}
		}
	}
}
