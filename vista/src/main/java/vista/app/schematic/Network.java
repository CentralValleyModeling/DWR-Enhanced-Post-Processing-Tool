/*
    Copyright (C) 1996, 1997, 1998 State of California, Department of 
    Water Resources.

    VISTA : A VISualization Tool and Analyzer. 
	Version 1.0beta
	by Nicky Sandhu
    California Dept. of Water Resources
    Division of Planning, Delta Modeling Section
    1416 Ninth Street
    Sacramento, CA 95814
    (916)-653-7552
    nsandhu@water.ca.gov

    Send bug reports to nsandhu@water.ca.gov

    This program is licensed to you under the terms of the GNU General
    Public License, version 2, as published by the Free Software
    Foundation.

    You should have received a copy of the GNU General Public License
    along with this program; if not, contact Dr. Francis Chung, below,
    or the Free Software Foundation, 675 Mass Ave, Cambridge, MA
    02139, USA.

    THIS SOFTWARE AND DOCUMENTATION ARE PROVIDED BY THE CALIFORNIA
    DEPARTMENT OF WATER RESOURCES AND CONTRIBUTORS "AS IS" AND ANY
    EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
    IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
    PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE CALIFORNIA
    DEPARTMENT OF WATER RESOURCES OR ITS CONTRIBUTORS BE LIABLE FOR
    ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
    CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT
    OR SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA OR PROFITS; OR
    BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
    LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
    (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE
    USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
    DAMAGE.

    For more information about VISTA, contact:

    Dr. Francis Chung
    California Dept. of Water Resources
    Division of Planning, Delta Modeling Section
    1416 Ninth Street
    Sacramento, CA  95814
    916-653-5601
    chung@water.ca.gov

    or see our home page: http://wwwdelmod.water.ca.gov/

    Send bug reports to nsandhu@water.ca.gov or call (916)-653-7552

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
