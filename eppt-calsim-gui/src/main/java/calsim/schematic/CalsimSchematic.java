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
import java.awt.Rectangle;
import java.util.Enumeration;
import java.util.Hashtable;

import vista.graph.DoublePoint;
import vista.graph.DoubleRect;
import vista.graph.GEAttr;
import vista.graph.GEBorderLayout;
import vista.graph.GEContainer;
import vista.graph.GEScaledLayout;
import vista.graph.GraphicElement;
import vista.graph.Scale;
import vista.graph.TextLine;
import vista.graph.TextLineAttr;

/**
 * A schematic of symbols. This is the view part of the schematic and is
 * implemented using GraphicElement's from vista.graph
 *
 * @author Nicky Sandhu
 * @version $Id: CalsimSchematic.java,v 1.1.2.5 1999/07/13 16:14:30 nsandhu Exp $
 * @see vista.graph.GraphicElement
 */
public class CalsimSchematic extends GEContainer
{
	// The data model for the schematic
	public CalsimSchematicDataModel _sdm;
	// an hash table to map a symbol to a reference object
	public Hashtable _somap;
	// the main container for all the graphic elements
	public GEContainer _sc;
	// the scaled layout used with the main container to layout the graphic elements
	public GEScaledLayout _layout;
	// a default size for this schematic
	private Dimension _size = new Dimension(100, 100);

	/**
	 * creates a schematic from a given schematic data model
	 */
	public CalsimSchematic(CalsimSchematicDataModel sdm)
	{
		this(new GEAttr(), sdm);
	}

	/**
	 * creates a schematic from the given attributes and model
	 */
	public CalsimSchematic(GEAttr attr, CalsimSchematicDataModel sdm)
	{
		super(attr);
		setModel(sdm);
	}

	/**
	 * get the data model underlying this schematic
	 */
	public CalsimSchematicDataModel getModel()
	{
		return _sdm;
	}

	/**
	 * sets the model
	 */
	public void setModel(CalsimSchematicDataModel sdm)
	{
		_sc = new GEContainer(new GEAttr());
		_somap = new Hashtable();
		_sdm = sdm;
		// set the layout to scaled layout
		Scale xScale = new Scale(_sdm.getXMin(), _sdm.getXMax(), 0, 10);
		Scale yScale = new Scale(_sdm.getYMin(), _sdm.getYMax(), 0, 10);
		_layout = new GEScaledLayout(xScale, yScale);
		_sc.setLayout(_layout);
		// add the symbols to this container and add their reference objects
		// to a dictionary to keep track of them.
		_sdm.reset();
		while(_sdm.hasMoreSymbols())
		{
			CalsimSymbolData sd = _sdm.nextSymbolData();
			DoublePoint dp1 = sd.getAnchorPoint();
			DoublePoint dp2 = sd.getOtherPoint();
			//_sc.add(sd.getGraphicElement());
			_sc.add(new DoubleRect(dp1.x, dp1.y, dp2.x - dp1.x, dp2.y - dp1.y),
					sd.getGraphicElement());
			_somap.put(sd.getGraphicElement(), sd.getReferenceObject());
		}
		// add title
		new TextLine(new TextLineAttr(), _sdm.getTitleText());
		// remove if added previous elements
		removeAll();
		//
		_sc.setBackgroundColor(Color.white);
		//
		setLayout(new GEBorderLayout());
		add(GEBorderLayout.CENTER, _sc);
		//    add( GEBorderLayout.NORTH, tl);
		//
		if(sdm instanceof NetworkSchematicData)
		{
			NetworkSchematicData nsd = (NetworkSchematicData) sdm;
			setPreferredSize((int) nsd.width, (int) nsd.height);
			setSize(new Dimension((int) nsd.width, (int) nsd.height));
			doLayout();
		}
	}

	/**
	 * the model has changed for only the following symbol data
	 */
	public void modelChanged(CalsimSymbolData[] csdArray)
	{
		removedFromModel(csdArray);
		addedToModel(csdArray);
	}

	/**
	 * Symbols have been added to the model.
	 */
	public void addedToModel(CalsimSymbolData[] csdArray)
	{
		// add the symbols to this container and add their reference objects
		// to a dictionary to keep track of them.
		if(csdArray == null)
		{
			return;
		}
		for(int i = 0; i < csdArray.length; i++)
		{
			CalsimSymbolData sd = csdArray[i];
			if(sd == null)
			{
				continue;
			}
			DoublePoint dp1 = sd.getAnchorPoint();
			DoublePoint dp2 = sd.getOtherPoint();
			GraphicElement ge = sd.getGraphicElement();
			_sc.add(new DoubleRect(dp1.x, dp1.y, dp2.x - dp1.x, dp2.y - dp1.y),
					ge);
			if(ge instanceof LabeledSymbol)
			{
				_sc.drawLast(ge);
			}
			else
			{
				_sc.drawFirst(ge);
			}
			_somap.put(ge, sd.getReferenceObject());
		}
	}

	/**
	 * symbols have been removed, the schematic needs to update itself.
	 */
	public void removedFromModel(CalsimSymbolData[] csdArray)
	{
		// add the symbols to this container and add their reference objects
		// to a dictionary to keep track of them.
		if(csdArray == null)
		{
			return;
		}
		for(int i = 0; i < csdArray.length; i++)
		{
			CalsimSymbolData sd = csdArray[i];
			if(sd == null)
			{
				continue;
			}
			GraphicElement ge = sd.getGraphicElement();
			_sc.remove(ge);
			_sc.getLayout().removeLayoutElement(ge);
			_somap.remove(ge);
		}
	}

	/**
	 *
	 */
	public void setHiddenVisible(boolean b)
	{
		//
		int count = _sc.getElementCount();
		for(int i = 0; i < count; i++)
		{
			GraphicElement ge = _sc.getElement(i);
			if(((String) getReferenceObject(ge)).indexOf("hidden") >= 0)
			{
				ge.setVisible(b);
			}
		}
	}

	/**
	 * gets the reference object associated with this graphic element
	 */
	public Object getReferenceObject(GraphicElement ge)
	{
		return _somap.get(ge);
	}

	/**
	 * gets the symbol related to this reference object
	 */
	public GraphicElement getSymbol(Object ref)
	{
		Enumeration e = _somap.keys();
		while(e.hasMoreElements())
		{
			Object key = e.nextElement();
			if(_somap.get(key).equals(ref))
			{
				return (GraphicElement) key;
			}
		}
		return null;
	}

	/**
	 * gets the scale in the X direction
	 */
	public Scale getXScale()
	{
		return _layout.getXScale();
	}

	/**
	 * gets the scale in the Y direction
	 */
	public Scale getYScale()
	{
		return _layout.getYScale();
	}

	/**
	 * add a graphic element symbol along with its reference object
	 */
	public void addSymbol(GraphicElement ge, Object obj,
						  DoublePoint dp1, DoublePoint dp2)
	{
		_sc.add(ge);
		_sc.add(new DoubleRect(dp1.x, dp1.y, dp2.x - dp1.x, dp2.y - dp1.y),
				ge);
		_somap.put(ge, obj);
	}

	/**
	 * removes symbol from this schematic
	 */
	public void removeSymbol(GraphicElement ge)
	{
		_sc.remove(ge);
		Object obj = _somap.get(ge);
		_somap.remove(obj);
	}

	/**
	 * notifies the layout of a change in the position of a symbol
	 */
	public void changeSymbol(GraphicElement ge, DoublePoint dp1, DoublePoint dp2)
	{
		_layout.removeLayoutElement(ge);
		_layout.addLayoutElement
				(
						new DoubleRect(dp1.x, dp1.y, dp2.x - dp1.x, dp2.y - dp1.y),
						ge);
	}

	/**
	 * add a graphic element symbol along with its reference object
	 */
	public void addSymbol(GraphicElement ge, Object obj)
	{
		_sc.add(ge);
		Rectangle r = ge.getBounds();
		Scale xs = _layout.getXScale();
		Scale ys = _layout.getYScale();
		_sc.add(new DoubleRect(xs.scaleToDC(r.x),
						ys.scaleToDC(r.y),
						xs.scaleToDC(r.x + r.width) - xs.scaleToDC(r.x),
						ys.scaleToDC(r.y + r.height) - ys.scaleToDC(r.y)),
				ge);
		_somap.put(ge, obj);
	}

	/**
	 * returns the symbol at the given x, y location
	 */
	public GraphicElement getHitSymbol(int x, int y)
	{
		// get the element hit
		return _sc.getHitElement(x, y);
	}

	/**
	 * first finds out what element got hit and then uses a mapping to
	 * determine what object is associated with that element.
	 */
	public Object getHitElementObject(int x, int y)
	{
		// get the element hit
		GraphicElement ge = _sc.getHitElement(x, y);
		// get the corresponding reference object for that element
		if(ge != null)
		{
			Object obj = _somap.get(ge);
			return obj;
		}
		else
		{
			return null;
		}
	}

	/**
	 * sets the preferred size for this schematic
	 */
	public void setPreferredSize(int width, int height)
	{
		_size = new Dimension(width, height);
		CalsimSchematicDataModel csm = getModel();
		if(csm instanceof NetworkSchematicData)
		{
			NetworkSchematicData nsd = (NetworkSchematicData) csm;
			nsd.width = width;
			nsd.height = height;
		}
	}

	/**
	 * returns the preferred size for this schematic
	 */
	public Dimension getPreferredSize()
	{
		return _size;
	}

	/**
	 * returns the minimum size for this schematic
	 */
	public Dimension getMinimumSize()
	{
		return _size;
	}
}
