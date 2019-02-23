/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package vista.app.schematic;

import java.awt.Color;
import java.awt.Dimension;
import java.io.InputStream;
import java.text.NumberFormat;

import vista.graph.DoublePoint;
import vista.graph.SchematicDataModel;
import vista.graph.SchematicSymbolData;
import vista.graph.SymbolData;
import vista.set.Group;
import vista.set.PathPartPredicate;
import vista.set.Pathname;

/**
 * 
 * 
 * @author Nicky Sandhu
 * @version $Id: DSM2SchematicData.java,v 1.2 1998/10/08 00:03:05 nsandhu Exp $
 */
public class DSM2SchematicData implements SchematicDataModel {
	public Network network;
	public double _xmax, _xmin, _ymax, _ymin;
	public int _index;
	private Object _obj;
	public Group _group;
	private NumberFormat nf;

	/**
   *
   */
	public DSM2SchematicData(InputStream is, Group g) {
		network = Network.createNetwork(is);
		_group = g;
		nf = NumberFormat.getInstance();
		nf.setGroupingUsed(false);
		nf.setMinimumIntegerDigits(3);
		int count = network.getNumberOfNodes();
		_xmax = Float.MIN_VALUE;
		_ymax = Float.MIN_VALUE;
		_xmin = Float.MAX_VALUE;
		_ymin = Float.MAX_VALUE;
		for (int i = 1; i < count; i++) {
			Node node = network.getNode(i);
			if (node == null)
				continue;
			double x = node.getX();
			double y = node.getY();
			_xmax = Math.max(_xmax, x);
			_xmin = Math.min(_xmin, x);
			_ymax = Math.max(_ymax, y);
			_ymin = Math.min(_ymin, y);
		}
		_xmax = 1.05 * _xmax;
		_xmin = 1.05 * _xmin;
		_ymax = 1.05 * _ymax;
		_ymin = 1.05 * _ymin;
		setReferenceObject(_group);
	}

	/**
	 * an object associated with this model.
	 */
	public Object getReferenceObject() {
		return _obj;
	}

	/**
	 * an object associated with this model.
	 */
	public void setReferenceObject(Object obj) {
		_obj = obj;
	}

	/**
	 * gets the maximum value for the x axis
	 */
	public double getXMax() {
		return _xmax;
	}

	/**
	 * gets the maximum value for the x axis
	 */
	public double getXMin() {
		return _xmin;
	}

	/**
	 * gets the maximum value for the x axis
	 */
	public double getYMax() {
		return _ymax;
	}

	/**
	 * gets the maximum value for the x axis
	 */
	public double getYMin() {
		return _ymin;
	}

	/**
	 * resets the data iterator to beginning of curve
	 */
	public void reset() {
		_index = 1;
	}

	/**
	 * gets the next point
	 * 
	 * @param points
	 *            is an array wher points[0] contains the next x value and
	 *            points[1] contains the next y value
	 * @return an integer specifing movevment only or line drawing motion
	 */
	public SymbolData nextSymbol() {
		DefaultSymbolData sd = new DefaultSymbolData();
		Link l = network.getLink(_index);
		if (l instanceof Channel) {
			Channel ch = (Channel) l;
			sd.type = SymbolData.LINE_SYMBOL;
			sd.size = 4; // channel size
			sd.shape = 0;
			Group g = getGroupObjectFor(l);
			sd.obj = g;
			sd.c = Color.blue;
			if (g.getNumberOfDataReferences() > 0)
				sd.c = Color.yellow;
			sd.p.x = l.getNode(0).getX();
			sd.p.y = l.getNode(0).getY();
			sd.op.x = l.getNode(1).getX();
			sd.op.y = l.getNode(1).getY();
			_index++;
		} else {
			sd.type = SymbolData.SYMBOL;
			sd.shape = SymbolData.CIRCLE;
			sd.obj = l;
			sd.c = Color.green;
			sd.size = 4;
			double xval = 0;
			double yval = 0;
			for (int i = 0; i < l.getNumberOfNodes(); i++) {
				xval += l.getNode(i).getX();
				yval += l.getNode(i).getY();
			}
			sd.p.x = xval;
			sd.p.y = yval;
			_index++;
		}
		return sd;
	}

	/**
	 * @return true while has more points on curve
	 */
	public boolean hasMoreSymbols() {
		while (_index < network.getNumberOfLinks()
				&& network.getLink(_index) == null)
			_index++;
		return _index < network.getNumberOfLinks();
	}

	/**
	 * gets the title text for this schematic
	 */
	public String getTitleText() {
		return "DSM2 Schematic";
	}

	/**
   *
   */
	public Group getGroupObjectFor(Link l) {
		Group g = Group.createGroup(_group);
		if (l instanceof Channel) {
			Channel c = (Channel) l;
			// g.filterBy("HIST+CHAN");
			// filter to get channelId_xxxx, e.g. 321_xxx or 005_xxx
			g.filterBy(new PathPartPredicate(nf.format(c.getId()) + "_",
					Pathname.B_PART), true);
		} else {
			// filter to get reservoirId_xxxx, e.g. 321_10000
			g.filterBy(new PathPartPredicate(nf.format(l.getId()) + "_",
					Pathname.B_PART), true);
		}
		return g;
	}

	/**
	 * 
	 * 
	 * @author Nicky Sandhu
	 * @version $Id: DSM2SchematicData.java,v 1.2 1998/10/08 00:03:05 nsandhu
	 *          Exp $
	 */
	public class DefaultSymbolData implements SymbolData {
		int size, type, shape;
		Object obj;
		Color c;
		DoublePoint p = new DoublePoint(0, 0), op = new DoublePoint(0, 0);

		public DefaultSymbolData() {
		}

		/**
   *
   */
		public int getSize() {
			return size;
		}

		/**
   *
   */
		public Color getColor() {
			return c;
		}

		/**
   *
   */
		public DoublePoint getAnchorPoint() {
			return p;
		}

		/**
 *
 */
		public DoublePoint getOtherPoint() {
			return op;
		}

		/**
 *
 */
		public int getType() {
			return type;
		} // LINE_SYMBOL or SYMBOL

		/**
 *
 */
		public int getShape() {
			return shape;
		} // for symbol it's CIRCLE, TRIANGLE, SQUARE

		/**
 *
 */
		public Object getReferenceObject() {
			return obj;
		}

		/**
 *
 */
		public void setReferenceObject(Object obj) {
			this.obj = obj;
		}
	}

	@Override
	public Dimension getScreenSize() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public SchematicSymbolData nextSymbolData() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public void setScreenSize(int width, int height) {
		// TODO Auto-generated method stub

	}

	@Override
	public void setTitleText(String str) {
		// TODO Auto-generated method stub

	}
}
