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
package vista.app.schematic;

import java.io.BufferedInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.LineNumberReader;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.HashMap;
import java.util.StringTokenizer;


/**
 * Encapsulates a network consisting of links and nodes where each link connects
 * two and only two nodes. This represents a graph structure.
 */
public class Network {
	/**
	 * Creates a network from the configuration given in a file.
	 */
	public static Network createNetwork(InputStream is) {
		try {
			Network network = new Network();
			LineNumberReader reader = new LineNumberReader(
					new InputStreamReader(is));
			if (reader.readLine().equals("NODE_LIST"))
				createNodes(network, reader);
			if (reader.readLine().equals("CHANNEL_LIST"))
				createChannels(network, reader);
			if (reader.readLine().equals("RESERVOIR_LIST"))
				createReservoirs(network, reader);
			return network;
		} catch (IOException ioe) {
			System.out.println(ioe);
			ioe.printStackTrace();
		}
		return null;
	}

	/**
   *
   */
	private static void createNodes(Network net, LineNumberReader reader)
			throws IOException {
		String line = reader.readLine();
		while (!(line = reader.readLine()).equals("END NODE_LIST")) {
			StringTokenizer chopper = new StringTokenizer(line);
			if (chopper.countTokens() == 3) {
				int id = new Integer(chopper.nextToken()).intValue();
				float x = new Float(chopper.nextToken()).floatValue();
				float y = new Float(chopper.nextToken()).floatValue();
				net.add(new Node(id, x, y));
			}
		}
	}

	/**
   *
   */
	private static void createChannels(Network net, LineNumberReader reader)
			throws IOException {
		String line = reader.readLine();
		while (!(line = reader.readLine()).equals("END CHANNEL_LIST")) {
			StringTokenizer chopper = new StringTokenizer(line);
			if (chopper.countTokens() == 4) {
				int id = new Integer(chopper.nextToken()).intValue();
				int upNodeId = new Integer(chopper.nextToken()).intValue();
				int downNodeId = new Integer(chopper.nextToken()).intValue();
				float length = new Float(chopper.nextToken()).floatValue();
				Node[] nodes = new Node[2];
				nodes[0] = net.getNode(upNodeId);
				nodes[1] = net.getNode(downNodeId);
				net.add(new Channel(id, nodes, length));
			}
		}
	}

	/**
   *
   */
	private static void createReservoirs(Network net, LineNumberReader reader)
			throws IOException {
		String line = reader.readLine();
		while (!(line = reader.readLine()).equals("END RESERVOIR_LIST")) {
			StringTokenizer chopper = new StringTokenizer(line);
			if (chopper.countTokens() == 2) {
				int id = new Integer(chopper.nextToken()).intValue();
				StringTokenizer nodeChopper = new StringTokenizer(chopper
						.nextToken(), ",");
				Node[] nodes = new Node[nodeChopper.countTokens()];
				for (int i = 0; i < nodes.length; i++) {
					int nodeId = new Integer(nodeChopper.nextToken())
							.intValue();
					nodes[i] = net.getNode(nodeId);
				}
				net.add(new Reservoir(id, nodes));
			}
		}
	}

	/**
   *
   */
	private static final InputStream getInputStream(String filename) {
		URL fileURL = null;
		try {
			fileURL = new URL("file://" + filename);
			return new BufferedInputStream(fileURL.openConnection()
					.getInputStream());
		} catch (MalformedURLException me) {
			System.out.println("Not a local file, trying http connection");
		} catch (IOException ioe) {
			System.out.println("Could not open file " + fileURL);
		}

		try {
			fileURL = new URL("http:/" + filename);
			return new BufferedInputStream(fileURL.openConnection()
					.getInputStream());
		} catch (MalformedURLException me) {
			System.out.println("Failed to establish http connection");
		} catch (IOException ioe) {
			System.out.println("Could not open file " + fileURL);
		}

		return null;
	}

	/**
	 * Adds node to network
	 */
	void add(Node n) {
		nodeArray.put(n.getId(), n);
	}

	/**
	 * Adds link to network
	 */
	void add(Link link) {
		linkArray.put(link.getId(), link);
	}

	/**
	 * Gets the number of links in the channel
	 */
	public int getNumberOfLinks() {
		return linkArray.size();
	}

	/**
	 * Gets the link object
	 */
	public Link getLink(int index) {
		return linkArray.get(index);
	}

	/**
	 * Gets the number of nodes
	 */
	public int getNumberOfNodes() {
		return nodeArray.size();
	}

	/**
	 * Gets the node object
	 */
	public Node getNode(int index) {
		return nodeArray.get(index);
	}

	/**
   * 
   */
	public String toString() {
		StringBuffer buf = new StringBuffer("Network: ");
		String eol = System.getProperty("line.separator");
		buf.append(eol);
		buf.append("Number Of Nodes: " + getNumberOfNodes()).append(eol);
		buf.append("Number Of Links: " + getNumberOfLinks()).append(eol);
		for (int i = 0; i < getNumberOfNodes(); i++) {
			Node n = getNode(i);
			if (n != null)
				buf.append(n.toString());
		}
		for (int i = 0; i < getNumberOfLinks(); i++) {
			Link l = getLink(i);
			if (l != null)
				buf.append(l.toString());
		}
		return buf.toString();
	}

	/**
   *
   */
	private HashMap<Integer, Node> nodeArray = new HashMap<Integer, Node>();
	/**
   *
   */
	private HashMap<Integer, Link> linkArray = new HashMap<Integer, Link>();
}
