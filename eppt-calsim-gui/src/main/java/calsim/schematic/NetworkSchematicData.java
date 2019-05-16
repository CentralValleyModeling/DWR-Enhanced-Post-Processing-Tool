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
package calsim.schematic;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.LineNumberReader;
import java.io.PrintWriter;
import java.text.NumberFormat;
import java.util.Arrays;
import java.util.Comparator;
import java.util.Enumeration;
import java.util.Hashtable;
import java.util.StringTokenizer;
import java.util.Vector;

import calsim.gym.Arc;
import calsim.gym.BoundaryNode;
import calsim.gym.ChannelArc;
import calsim.gym.DemandArc;
import calsim.gym.GymUtils;
import calsim.gym.InputArc;
import calsim.gym.Network;
import calsim.gym.Node;
import calsim.gym.ReturnArc;
import vista.graph.DoublePoint;
import vista.graph.GraphicElement;
import vista.graph.LineElement;
import vista.graph.SymbolFactory;

/**
 * An implementation of the CalsimSchematicDataModel for representing a view
 * on a network
 *
 * @author Nicky Sandhu
 * @version $Id: NetworkSchematicData.java,v 1.1.2.5 2000/04/14 16:36:04 amunevar Exp $
 * @see CalsimSymbolData, CalsimSchematicDataModel
 */
public class NetworkSchematicData implements CalsimSchematicDataModel
{
	public static final String STORAGE = "storage", JUNCTION = "junction", HIDDEN = "hidden";
	public double _xmax, _xmin, _ymax, _ymin;
	public float width, height;
	private boolean _minExplicitlySet = false;
	private boolean DEBUG = false;
	private boolean _maxExplicitlySet = false;
	private String _titleText = "";
	private Network _net;
	private Color flowColor = Color.blue.brighter().brighter(),
			inflowColor = Color.cyan.darker(),
			demandColor = Color.red.darker(),
			returnColor = Color.green.darker(),
			storageColor = Color.blue.darker(),
			nodeColor = Color.green.darker().darker().darker();
	private Color nodeLabelColor = Color.yellow.brighter();
	private Color flowLabelColor = Color.black;
	private Font lineFont = new Font("Times Roman", Font.BOLD, 10);
	private Font symFont = new Font("Times Roman", Font.BOLD, 10);
	private Hashtable _nodeSymbolMap, _arcSymbolMap;
	private Vector _nodeSymbolArray, _arcSymbolArray;
	private Enumeration _enumArc, _enumNode;

	/**
	 *
	 */
	public NetworkSchematicData(Network net, String xyfile)
	{
		_net = net;
		_nodeSymbolMap = new Hashtable();
		_arcSymbolMap = new Hashtable();
		_nodeSymbolArray = new Vector();
		_arcSymbolArray = new Vector();
		readXY(net, xyfile);
		cleanUpNetwork();
		initializeMinMax(net);
		createSymbolData();
	}

	/**
	 *
	 */
	private void cleanUpNetwork()
	{
		// create hidden nodes for all arcs with boundary nodes...
		// remove storage arcs and flood arcs...
		Arc[] arcs = _net.getAllArcs();
		if(arcs != null)
		{
			for(int i = 0; i < arcs.length; i++)
			{
				Arc arc = arcs[i];
				if(arc.getName().indexOf("_") >= 0)
				{
					if(DEBUG)
					{
						System.out.println("Removing " + arc.getName());
					}
					_net.remove(arc);
					continue;
				}
				if(arc.getName().indexOf("F") >= 0)
				{
					if(DEBUG)
					{
						System.out.println("Removing " + arc.getName());
					}
					_net.remove(arc);
					continue;
				}
				Node un1 = arc.getUpstreamNode();
				Node dn1 = arc.getDownstreamNode();
				if(un1 instanceof BoundaryNode)
				{
					Node pn = new Node("XU" + dn1.getName());
					pn.x = dn1.x + 1.0f;
					pn.y = dn1.y + 1.0f;
					_net.add(pn);
					arc.setUpstreamNode(pn);
					pn.addArc(arc);
				}
				if(dn1 instanceof BoundaryNode)
				{
					Node pn = new Node("XD" + un1.getName());
					pn.x = un1.x - 1.0f;
					pn.y = un1.y - 1.0f;
					_net.add(pn);
					arc.setDownstreamNode(pn);
					pn.addArc(arc);
				}
			}
		}
	}

	/**
	 *
	 */
	private void readXY(Network net, String xyfile)
	{
		StringTokenizer st;
		if(xyfile == null || net == null)
		{
			_xmin = 0.0;
			_xmax = 100.0;
			_ymin = 0.0;
			_ymax = 100.0;
			_minExplicitlySet = true;
			_maxExplicitlySet = true;

			return;
		}
		try(FileReader fileReader = new FileReader(xyfile);
			LineNumberReader reader = new LineNumberReader(fileReader))
		{
			while(true)
			{
				String line = reader.readLine();
				if(line == null)
				{
					break;
				}
				line = line.trim();
				if(line.indexOf("#") >= 0)
				{
					continue; // skip comment line
				}
				if(line.indexOf("Label") >= 0)
				{
					continue; // skip column header
				}
				if(line.indexOf("schematic.min") >= 0 ||
						line.indexOf("schematic.max") >= 0)
				{
					boolean min = false;
					if(line.indexOf("schematic.min") >= 0)
					{
						min = true;
					}
					int eqIndex = line.indexOf("=");
					if(eqIndex < 0)
					{
						System.err.println("Where's the equal to sign for schematic." + (min ? "min" : "max") + "?");
					}
					else
					{
						String minStr = line.substring(eqIndex + 1);
						int commaIndex = minStr.indexOf(",");
						if(commaIndex < 0)
						{
							System.err.println(
									"Need a comma to separate x from y in the schematic " + (min ? "min" : "max"));
						}
						else
						{
							if(min)
							{
								_xmin = new Double(minStr.substring(0, commaIndex));
								_ymin = new Double(minStr.substring(commaIndex + 1));
								_minExplicitlySet = true;
								if(DEBUG)
								{
									System.out.println(_xmin + ", " + _ymin);
								}
							}
							else
							{
								_xmax = new Double(minStr.substring(0, commaIndex));
								_ymax = new Double(minStr.substring(commaIndex + 1));
								_maxExplicitlySet = true;
								if(DEBUG)
								{
									System.out.println(_xmax + ", " + _ymax);
								}
							}
						}
					}
					continue; // skip reset
				}
				// schematic width and height
				if(line.indexOf("schematic.size") >= 0)
				{
					int eqi = line.indexOf("=");
					int commai = line.indexOf(",");
					if(eqi < 0 || commai < 0)
					{
						throw new RuntimeException("Incorrectly defined schematic size");
					}
					width = new Float(line.substring(eqi + 1, commai)).floatValue();
					height = new Float(line.substring(commai + 1)).floatValue();
					continue;
				}
				st = new StringTokenizer(line);
				if(st.countTokens() != 4)
				{
					continue;
				}
				String nodeNum = st.nextToken();
				//	String type = st.nextToken();
				String x1 = st.nextToken();
				String y1 = st.nextToken();
				setNodeXY(nodeNum, new Float(x1).floatValue(), new Float(y1).floatValue());
			}
		}
		catch(Exception e)
		{
			e.printStackTrace();
		}
	}

	/**
	 *
	 */
	public void writeXY(Network net, String xyfile)
			throws IOException
	{
		try(PrintWriter writer = new PrintWriter(new FileWriter(xyfile)))
		{
			NumberFormat nf = NumberFormat.getInstance();
			nf.setMaximumFractionDigits(1);
			nf.setMinimumFractionDigits(1);
			nf.setGroupingUsed(false);
			Node[] nodes = net.getAllNodes();
			writer.println("#Automatically generated from schematic");
			writer.println("# all junction/storage nodes");
			writer.println("schematic.max=\t" + nf.format(_xmax) + ",\t" + nf.format(_ymax));
			writer.println("schematic.min=\t" + nf.format(_xmin) + ",\t" + nf.format(_ymin));
			writer.println("schematic.size=\t" + nf.format(width) + ",\t" + nf.format(height));
			String gap = "\t\t";
			writer.println("Label\t\tSymbol\t\t\tX1\t\tY1");
			sort(nodes);
			for(int i = 0; i < nodes.length; i++)
			{
				Node n1 = nodes[i];
				if(n1 == null)
				{
					continue;
				}
				String line = n1.getName() + gap;
				if(n1.hasStorage())
				{
					line += "storage " + gap;
				}
				else if(n1.getName().indexOf("X") >= 0)
				{
					line += "hidden  " + gap;
				}
				else
				{
					line += "junction" + gap;
				}
				line += nf.format(n1.x) + gap;
				line += nf.format(n1.y) + gap;
				writer.println(line);
			}
		}
	}

	/**
	 *
	 */
	private void sort(Node[] nodes)
	{
		Comparator nc = new Comparator()
		{
			public int compare(Object obj1, Object obj2)
			{
				if(obj1 instanceof Node && obj2 instanceof Node)
				{
					Node n1 = (Node) obj1;
					Node n2 = (Node) obj2;
					return n1.getName().compareTo(n2.getName());
				}
				else
				{
					return -1;
				}
			}

			public boolean equals(Object obj)
			{
				return false;
			}
		};
		Arrays.sort(nodes, nc);
	}

	/**
	 * gets the maximum value for the x axis
	 */
	public double getXMax()
	{
		return _xmax;
	}

	/**
	 * gets the maximum value for the x axis
	 */
	public double getXMin()
	{
		return _xmin;
	}

	/**
	 * gets the maximum value for the x axis
	 */
	public double getYMax()
	{
		return _ymax;
	}

	/**
	 * gets the maximum value for the x axis
	 */
	public double getYMin()
	{
		return _ymin;
	}

	/**
	 * @return an array of the changed symbol data objects
	 */
	public CalsimSymbolData[] setNodeXY(String nodeNum, float x, float y)
	{
		Node n1 = _net.getNode(nodeNum);
		if(n1 == null)
		{
			System.out.println("No node found for name: " + nodeNum);
			return null;
		}
		n1.x = x;
		n1.y = y;
		// get the symbol data associated with this node and update it
		DefaultSymbolData dsd = (DefaultSymbolData) _nodeSymbolMap.get(n1);
		if(dsd == null)
		{
			return null;
		}
		Vector csdVector = new Vector();
		csdVector.addElement(dsd);
		dsd.anchorPoint.x = n1.x;
		dsd.anchorPoint.y = n1.y;
		// get all the arcs connected to this node
		Arc[] upArcs = GymUtils.getJustUpstreamArcs(n1);
		Arc[] downArcs = GymUtils.getJustDownstreamArcs(n1);
		// update the symbol data associated with arcs
		if(upArcs != null)
		{
			for(int i = 0; i < upArcs.length; i++)
			{
				Arc arc = upArcs[i];
				dsd = (DefaultSymbolData) _arcSymbolMap.get(arc);
				csdVector.addElement(dsd);
				dsd.otherPoint.x = n1.x;
				dsd.otherPoint.y = n1.y;
			}
		}
		if(downArcs != null)
		{
			for(int i = 0; i < downArcs.length; i++)
			{
				Arc arc = downArcs[i];
				dsd = (DefaultSymbolData) _arcSymbolMap.get(arc);
				csdVector.addElement(dsd);
				dsd.anchorPoint.x = n1.x;
				dsd.anchorPoint.y = n1.y;
			}
		}
		if(csdVector.size() == 0)
		{
			return null;
		}
		CalsimSymbolData[] csdArray = new CalsimSymbolData[csdVector.size()];
		csdVector.copyInto(csdArray);
		return csdArray;
	}

	/**
	 * @return an array of the changed symbol data objects
	 */
	public CalsimSymbolData[] updateNode(String nodeNum, float x, float y)
	{
		Node n1 = _net.getNode(nodeNum);
		if(n1 == null)
		{
			System.out.println("No node found for name: " + nodeNum);
			return null;
		}
		n1.x = x;
		n1.y = y;
		// get the symbol data associated with this node and update it
		DefaultSymbolData data = new DefaultSymbolData();
		String label = n1.getName();
		if(n1.hasStorage())
		{
			LabeledSymbol labelSym =
					new LabeledSymbol(label,
							SymbolFactory.createUprightTriangle(true, storageColor, 20));
			labelSym.getTextLine().setFont(symFont);
			labelSym.getTextLine().setForegroundColor(nodeLabelColor);
			data.ge = labelSym;
			double x1 = n1.x;
			double y1 = n1.y;
			double x2 = 10.0, y2 = 10.0;
			data.anchorPoint = new DoublePoint(x1, y1);
			data.otherPoint = new DoublePoint(x1 + x2, y1 - y2);
			data.ref = "storage" + " " + label;
		}
		else if(n1.getName().indexOf("X") >= 0)
		{
			LabeledSymbol labelSym =
					new LabeledSymbol(label,
							SymbolFactory.createSquare(false, Color.black, 5));
			labelSym.getTextLine().setFont(symFont);
			labelSym.getTextLine().setForegroundColor(Color.black);
			data.ge = labelSym;
			double x1 = n1.x;
			double y1 = n1.y;
			double x2 = 10.0, y2 = 10.0;
			data.anchorPoint = new DoublePoint(x1, y1);
			data.otherPoint = new DoublePoint(x1 + x2, y1 - y2);
			data.ref = "hidden" + " " + label;
		}
		else
		{
			LabeledSymbol labelSym =
					new LabeledSymbol(label,
							SymbolFactory.createCircle(true, nodeColor, 1));
			labelSym.getTextLine().setFont(symFont);
			labelSym.getTextLine().setForegroundColor(nodeLabelColor);
			data.ge = labelSym;
			double x1 = n1.x;
			double y1 = n1.y;
			double x2 = 10.0, y2 = 10.0;
			data.anchorPoint = new DoublePoint(x1, y1);
			data.otherPoint = new DoublePoint(x2 + x1, y1 - y2);
			data.ref = "junction" + " " + label;
		}
		_nodeSymbolArray.addElement(data);
		_nodeSymbolMap.put(n1, data);
		return new CalsimSymbolData[]{data};
	}

	/**
	 * @return an array of the changed symbol data objects
	 */
	public CalsimSymbolData[] updateArc(String arcName)
	{
		Arc arc = _net.getArc(arcName);
		if(arc == null)
		{
			return null;
		}
		Node un1 = arc.getUpstreamNode();
		Node dn1 = arc.getDownstreamNode();
		if(un1 == null || dn1 == null)
		{
			throw new RuntimeException("Upstream or downstream node not defined for arc: " + arcName);
		}
		// get the symbol data associated with this node and update it
		DefaultSymbolData data = new DefaultSymbolData();
		String symType = "flow";
		String label = arc.getName();
		LabeledLine lline = new LabeledLine(label);
		lline.getTextLine().setFont(lineFont);
		lline.getTextLine().setForegroundColor(flowLabelColor);
		LineElement le = lline.getLineElement();
		if(arc instanceof InputArc)
		{
			le.setForegroundColor(inflowColor);
			symType = "inflow";
		}
		else if(arc instanceof ChannelArc)
		{
			le.setForegroundColor(flowColor);
			symType = "flow";
		}
		else if(arc instanceof ReturnArc)
		{
			le.setForegroundColor(returnColor);
			symType = "return";
		}
		else if(arc instanceof DemandArc)
		{
			le.setForegroundColor(demandColor);
			symType = "demand";
		}
		else
		{
		}
		data.ge = lline;
		double x1 = un1.x;
		double y1 = un1.y;
		double x2 = dn1.x;
		double y2 = dn1.y;
		if(x1 == 0.0 && y1 == 0.0)
		{
			x1 = x2 - 1.0;
			y1 = y2 - 1.0;
		}
		else if(x2 == 0.0 && y2 == 0.0)
		{
			x2 = x1 + 1.0;
			y2 = y1 + 1.0;
		}
		if(DEBUG)
		{
			System.out.println("Arc " + arc.getName() + " from " + x1 + ", " + y1 +
					" to " + x2 + ", " + y2);
		}
		data.anchorPoint = new DoublePoint(x1, y1);
		data.otherPoint = new DoublePoint(x2, y2);
		data.ref = symType + " " + label;
		//
		_arcSymbolMap.put(arc, data);
		_arcSymbolArray.addElement(data);
		return new CalsimSymbolData[]{data};
	}

	/**
	 *
	 */
	public GraphicElement createArc(String label, String type)
	{
		LabeledLine lline = new LabeledLine(label);
		lline.getTextLine().setFont(lineFont);
		lline.getTextLine().setForegroundColor(flowLabelColor);
		LineElement le = lline.getLineElement();
		if(type.equals("inflow"))
		{
			le.setForegroundColor(inflowColor);
		}
		else if(type.equals("flow"))
		{
			le.setForegroundColor(flowColor);
		}
		else if(type.equals("return"))
		{
			le.setForegroundColor(returnColor);
		}
		else if(type.equals("demand"))
		{
			le.setForegroundColor(demandColor);
		}
		else
		{
			le.setForegroundColor(Color.black);
		}
		return lline;
	}

	/**
	 *
	 */
	public GraphicElement createNode(String label, String type)
	{
		LabeledSymbol labelSym = null;
		if(type.indexOf("storage") >= 0)
		{
			labelSym =
					new LabeledSymbol(label,
							SymbolFactory.createUprightTriangle(true, storageColor, 20));
			labelSym.getTextLine().setFont(symFont);
			labelSym.getTextLine().setForegroundColor(nodeLabelColor);
		}
		else if(type.indexOf("hidden") >= 0)
		{
			labelSym =
					new LabeledSymbol(label,
							SymbolFactory.createSquare(false, Color.black, 5));
			labelSym.getTextLine().setFont(symFont);
			labelSym.getTextLine().setForegroundColor(Color.black);
			labelSym.setVisible(false);
		}
		else
		{
			labelSym =
					new LabeledSymbol(label,
							SymbolFactory.createCircle(true, nodeColor, 1));
			labelSym.getTextLine().setFont(symFont);
			labelSym.getTextLine().setForegroundColor(nodeLabelColor);
		}
		return labelSym;
	}

	/**
	 * creates symbol data for the arcs and nodes in the network.
	 */
	public void createSymbolData()
	{
		Arc[] _arcs = _net.getAllArcs();
		for(int i = 0; i < _arcs.length; i++)
		{
			DefaultSymbolData data = new DefaultSymbolData();
			Arc arc = _arcs[i];
			String label = arc.getName();
			Node n1 = arc.getUpstreamNode();
			Node n2 = arc.getDownstreamNode();
			String symType = "flow";
			if(arc instanceof InputArc)
			{
				symType = "inflow";
			}
			else if(arc instanceof ChannelArc)
			{
				symType = "flow";
			}
			else if(arc instanceof ReturnArc)
			{
				symType = "return";
			}
			else if(arc instanceof DemandArc)
			{
				symType = "demand";
			}
			else
			{
				symType = "flow";
			}
			data.ge = createArc(label, symType);
			double x1 = n1.x;
			double y1 = n1.y;
			double x2 = n2.x;
			double y2 = n2.y;
			if(x1 == 0.0 && y1 == 0.0)
			{
				x1 = x2 - 1.0;
				y1 = y2 - 1.0;
			}
			else if(x2 == 0.0 && y2 == 0.0)
			{
				x2 = x1 + 1.0;
				y2 = y1 + 1.0;
			}
			if(DEBUG)
			{
				System.out.println("Arc " + arc.getName() + " from " + x1 + ", " + y1 +
						" to " + x2 + ", " + y2);
			}
			data.anchorPoint = new DoublePoint(x1, y1);
			data.otherPoint = new DoublePoint(x2, y2);
			data.ref = symType + " " + label;
			//
			_arcSymbolMap.put(arc, data);
			_arcSymbolArray.addElement(data);
		}
		Node[] _nodes = _net.getAllNodes();
		for(int i = 0; i < _nodes.length; i++)
		{
			DefaultSymbolData data = new DefaultSymbolData();
			Node n1 = _nodes[i];
			String label = n1.getName();
			if(n1.hasStorage())
			{
				data.ge = createNode(label, "storage");
				double x1 = n1.x;
				double y1 = n1.y;
				double x2 = 10.0, y2 = 10.0;
				data.anchorPoint = new DoublePoint(x1, y1);
				data.otherPoint = new DoublePoint(x1 + x2, y1 - y2);
				data.ref = "storage" + " " + label;
			}
			else if(n1.getName().indexOf("X") >= 0)
			{
				data.ge = createNode(label, "hidden");
				double x1 = n1.x;
				double y1 = n1.y;
				double x2 = 10.0, y2 = 10.0;
				data.anchorPoint = new DoublePoint(x1, y1);
				data.otherPoint = new DoublePoint(x1 + x2, y1 - y2);
				data.ref = "hidden" + " " + label;
			}
			else
			{
				data.ge = createNode(label, "junction");
				double x1 = n1.x;
				double y1 = n1.y;
				double x2 = 10.0, y2 = 10.0;
				data.anchorPoint = new DoublePoint(x1, y1);
				data.otherPoint = new DoublePoint(x2 + x1, y1 - y2);
				data.ref = "junction" + " " + label;
			}
			_nodeSymbolArray.addElement(data);
			_nodeSymbolMap.put(n1, data);
		}
	}

	/**
	 * resets the symbol iterator to the beginning
	 */
	public void reset()
	{
		_enumArc = _arcSymbolArray.elements();
		_enumNode = _nodeSymbolArray.elements();
	}

	/**
	 * returns the next symbol data for this schematic
	 */
	public CalsimSymbolData nextSymbolData()
	{
		if(_enumArc.hasMoreElements())
		{
			return (CalsimSymbolData) _enumArc.nextElement();
		}
		else
		{
			return (CalsimSymbolData) _enumNode.nextElement();
		}
	}

	/**
	 * @return true while has more  points on curve
	 */
	public boolean hasMoreSymbols()
	{
		return _enumArc.hasMoreElements() || _enumNode.hasMoreElements();
	}

	/**
	 * gets the title text for this schematic
	 */
	public String getTitleText()
	{
		return _titleText;
	}

	/**
	 * sets the title text for this schematic
	 */
	public void setTitleText(String str)
	{
		_titleText = str;
	}

	/**
	 * returns the network for this schematic data
	 */
	public Network getNetwork()
	{
		return _net;
	}

	/**
	 *
	 */
	public Dimension getScreenSize()
	{
		return new Dimension((int) width, (int) height);
	}

	/**
	 *
	 */
	private void initializeMinMax(Network net)
	{
		if(_minExplicitlySet && _maxExplicitlySet)
		{
			return;
		}
		if(net == null)
		{
			return;
		}
		//
		if(DEBUG)
		{
			if(_minExplicitlySet)
			{
				System.out.println("Min Explicitly Set");
			}
			if(_maxExplicitlySet)
			{
				System.out.println("Max Explicitly Set");
			}
		}
		//
		if(!_minExplicitlySet)
		{
			_xmin = Float.MAX_VALUE;
			_ymin = Float.MAX_VALUE;
		}
		//
		if(!_maxExplicitlySet)
		{
			_xmax = Float.MIN_VALUE;
			_ymax = Float.MIN_VALUE;
		}
		Node[] nodes = net.getAllNodes();
		for(int i = 0; i < nodes.length; i++)
		{
			Node n1 = nodes[i];
			if(!_maxExplicitlySet)
			{
				_xmax = Math.max(_xmax, n1.x);
				_ymax = Math.max(_ymax, n1.y);
			}
			if(!_minExplicitlySet)
			{
				_xmin = Math.min(_xmin, n1.x);
				_ymin = Math.min(_ymin, n1.y);
			}
		}
		if(!_minExplicitlySet)
		{
			_xmin = 0.85 * _xmin;
			_ymin = 0.85 * _ymin;
		}
		if(!_maxExplicitlySet)
		{
			_xmax = 1.15 * _xmax;
			_ymax = 1.15 * _ymax;
		}
	}

	/**
	 *
	 */
	public static class DefaultSymbolData implements CalsimSymbolData
	{
		GraphicElement ge;
		DoublePoint anchorPoint, otherPoint;
		Object ref;

		/**
		 * the symbol representing the data
		 */
		public GraphicElement getGraphicElement()
		{
			return ge;
		}

		/**
		 * the anchor point or upper left corner of bounds
		 */
		public DoublePoint getAnchorPoint()
		{
			return anchorPoint;
		}

		/**
		 * the other point or lower left corner of bounds
		 */
		public DoublePoint getOtherPoint()
		{
			return otherPoint;
		}

		/**
		 * the reference object to be associated with this element.
		 * This is the data or identifier object.
		 */
		public Object getReferenceObject()
		{
			return ref;
		}

		/**
		 * set the reference object for this element.
		 */
		public void setReferenceObject(Object obj)
		{
			ref = obj;
		}
	}
}
