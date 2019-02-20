/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package vista.graph;

import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Rectangle;
import java.util.Enumeration;
import java.util.Properties;
import java.util.Vector;

/**
 * This the base class for all graphic element container classes. This class
 * acts as containers for other graphic elements. It defines the composite in
 * the composite - leaf pattern. This is based on the Composite Pattern from
 * Design Patterns.
 * <p>
 * <p>
 * The design strategy is based on polymorphising or treating as equivalent all
 * drawable elements. This allows uniform treatment of all elements that are
 * visual. In addition the space occupied by each element is allocated through
 * the use of Layout managers which query each element for their preferred
 * sizes.
 * <p>
 * <p>
 * Element management is done via java.util.Vector. Addition and removal of
 * elements can be done quite flexibily.
 * <p>
 * <p>
 * Layout of elements within this container are done by delegating to a class
 * that implements GELayoutManager. This class then arranges the elements
 * according to their preferred sizes with the bounds of this element. In
 * addition the preferred size calculations are done by the GELayoutManager
 * instance.
 * <p>
 * <p>
 * This class can add GraphicElement(s) to itself. It then becomes a parent of
 * that class and shares its graphic context with it. Drawing is done by calling
 * the draw function of each element. The drawing order for now is set by the
 * order in which they are added, however a feature may be added soon which
 * allows the drawing order to be set. This would be useful in the case where
 * setting the background of one element is actually implemented as a
 * java.awt.Graphics.fillRect() function
 * <p>
 * <p>
 * This container can be traversed using an iterator. The iterator can then
 * traverse the composite in the desired order. This is useful in defining the
 * drawing order or other operations on this composite.
 * <p>
 *
 * @author Nicky Sandhu
 * @version $Id: GEContainer.java,v 1.1 2003/10/02 20:48:57 redwood Exp $
 * @see GraphicElement
 * @see BoundedComposite
 * @see Layoutable
 * @see DrawableComposite
 * @see GELayoutManager
 */
public class GEContainer extends GraphicElement implements BoundedComposite,
														   Layoutable, DrawableComposite, FontResizable
{
	/**
	 * for debugging
	 */
	public static final boolean DEBUG = false, DEBUG2 = false;
	/**
	 * the array containing the child elements
	 */
	private Vector _elements = new Vector();
	/**
	 * the layout manager
	 */
	private GELayoutManager _layoutManager;
	/**
	 * an iterator for traversing the composite
	 */
	private CompositeIterator _iterator;
	/**
	 * an iterator for traversing the composite
	 */
	private DrawIterator _drawIterator;

	/**
	 * constructor
	 */
	public GEContainer()
	{
		super(new GEAttr());
	}

	/**
	 * constructor
	 */
	public GEContainer(GEAttr attributes)
	{
		super(attributes);
	}

	/**
	 * creates a deep copy of the current element and its contained elements If
	 * the element refuses to clone it adds the element to its container and
	 * continues
	 */
	public Object clone()
	{
		Object clone = super.clone();
		GEContainer gec = (GEContainer) clone;
		gec._elements = new Vector();
		for(Enumeration iterator = _elements.elements(); iterator
				.hasMoreElements(); )
		{
			GraphicElement ge = (GraphicElement) iterator.nextElement();
			try
			{
				gec._elements.addElement(ge.clone());
			}
			catch(Throwable t)
			{
				gec._elements.addElement(ge);
			}
		}
		return clone;
	}

	/**
	 * gets current layout manager
	 *
	 * @returns GELayoutManager object
	 */
	public final GELayoutManager getLayout()
	{
		return _layoutManager;
	}

	/**
	 * sets layout manager
	 */
	public void setLayout(GELayoutManager lm)
	{
		_layoutManager = lm;
	}

	/**
	 * get the last hit element. This returns the last element in the
	 * containment strategy in this container. Usually the containment strategy
	 * is to keep the last drawn element to tbe what gets returned by this
	 * method.
	 *
	 * @return the last graphic element hit or null if none hit
	 */
	public GraphicElement getHitElement(int x, int y)
	{
		GraphicElement hitElement = null;
		int count = getElementCount();
		for(int i = 0; i < count; i++)
		{
			GraphicElement ge = getElement(i);
			if(ge.hitsDrawing(x, y))
			{
				hitElement = ge;
			}
		}
		return hitElement;
	}

	/**
	 * get the array of all elements which contain the given x and y This method
	 * does not recurse into GEContainers other than itself A recursive method
	 * should really be defined in GraphUtils
	 *
	 * @return an array of hit elements or null if none hit.
	 */
	public GraphicElement[] getHitElements(int x, int y)
	{
		Vector hitElements = new Vector();
		int count = getElementCount();
		for(int i = 0; i < count; i++)
		{
			GraphicElement ge = getElement(i);
			if(ge.hitsDrawing(x, y))
			{
				hitElements.addElement(ge);
			}
		}
		if(hitElements.size() == 0)
		{
			return null;
		}
		GraphicElement[] hits = new GraphicElement[hitElements.size()];
		hitElements.copyInto(hits);
		return hits;
	}

	/**
	 * things to take care of before actual drawing. This includes actually
	 * drawing all its children before calling its own Draw method.
	 */
	public void preDraw()
	{
		super.preDraw();
		if(DEBUG)
		{
			System.out.println("Drawing container: " + this);
		}
		//
		for(DrawIterator iterator = getDrawIterator(); iterator
				.hasMoreElements(); )
		{
			Drawable ge = (Drawable) iterator.nextElement();
			ge.draw();
			if(DEBUG2)
			{ // a prompt based method that shows all the drawn
				// elements
				byte[] b = new byte[2];
				System.out.println("Drawing " + ge);
				Rectangle r = ((Bounded) ge).getBounds();
				System.out.println(r);
				getGraphics().drawRect(r.x, r.y, r.width - 1, r.height - 1);
				try
				{
					System.in.read(b, 0, 2);
				}
				catch(java.io.IOException ioe)
				{
					ioe.printStackTrace();
				}
			}
		}
	}

	/**
	 * draws its child elements and itself. The child elements are drawn in the
	 * order in which they are added (for now).
	 */
	public void Draw()
	{
		if(DEBUG)
		{
			System.out.println("Clip Bounds: " + this + " -> " + getGraphics().getClipBounds());
		}
		/*
		Rectangle bounds = getBounds();
		getGraphics().setColor(Color.BLUE);
		getGraphics().drawRect(bounds.x, bounds.y, bounds.width, bounds.height);
		*/
	}

	/**
	 * The update method is called when the animation frame needs to be updated.
	 * Each element is responsible for drawing itself within the bounds given.
	 */
	public void update(AnimationObservable o, Object arg)
	{
		for(Enumeration e = getIterator(); e.hasMoreElements(); )
		{
			GraphicElement ge = (GraphicElement) e.nextElement();
			if(ge instanceof AnimationObserver)
			{
				((AnimationObserver) ge).update(o, arg);
			}
		}
	}

	/**
	 * animates over all its animatio objects
	 */
	public void animateNext()
	{
		for(Enumeration e = getIterator(); e.hasMoreElements(); )
		{
			GraphicElement ge = (GraphicElement) e.nextElement();
			ge.animateNext();
		}
	}

	/**
	 * sets the graphics of itself and children to the same graphics context.
	 */
	public final void setGraphics(Graphics gc)
	{
		if(getAttributes().getCreateGraphicsCopy())
		{
			super.setGraphics(gc.create());
		}
		else
		{
			super.setGraphics(gc);
		}
		Enumeration iterator = _elements.elements();
		while(iterator.hasMoreElements())
		{
			GraphicElement ge = (GraphicElement) iterator.nextElement();
			ge.setGraphics(gc);
		}
	}

	/**
	 * set this elements bounds and delegates layout of its children to a layout
	 * manager.
	 */
	public void setBounds(Rectangle r)
	{
		super.setBounds(r);
		doLayout();
	}

	/**
	 * gets the child element count
	 *
	 * @return the number of child graphic elements
	 */
	public int getElementCount()
	{
		return _elements.size();
	}

	/**
	 * gets the element at the specified index. This index corresponds to the
	 * order in which the elements were added to this composite.
	 *
	 * @returns the element.
	 */
	public GraphicElement getElement(int n)
	{
		return (GraphicElement) (_elements.elementAt(n));
	}

	/**
	 * gets an array of graphic elements of the given element class type.
	 */
	public GraphicElement[] getElements(Class elementClass)
	{
		int count = getElementCount();
		int classCount = 0;
		GraphicElement[] geArray = new GraphicElement[count];
		for(int i = 0; i < count; i++)
		{
			GraphicElement ge = getElement(i);
			if(elementClass.isInstance(ge))
			{
				geArray[classCount] = ge;
				classCount++;
			}
		}
		if(classCount > 0)
		{
			GraphicElement[] newArray = new GraphicElement[classCount];
			System.arraycopy(geArray, 0, newArray, 0, classCount);
			geArray = newArray;
		}
		else
		{
			geArray = null;
		}
		return geArray;
	}

	/**
	 * returns a copy of the array of graphic elements contained in this
	 * composite.
	 */
	public GraphicElement[] getElements()
	{
		synchronized(this)
		{
			GraphicElement[] list = new GraphicElement[_elements.size()];
			_elements.copyInto(list);
			return list;
		}
	}

	/**
	 * instructs the layout manager to layout the child elements in accordance
	 * with their preferred sizes.
	 */
	public void doLayout()
	{
		if(_layoutManager != null)
		{
			_layoutManager.layoutContainer(this);
		}
		for(int i = 0; i < getElementCount(); i++)
		{
			Object gec = _elements.elementAt(i);
			if(gec instanceof Layoutable)
			{
				((Layoutable) gec).doLayout();
			}
		}
	}

	/**
	 * delegates to the layout manager to determine the preferred size of the
	 * composite based on the preferred sizes of its children and the layout.
	 */
	public Dimension getPreferredSize()
	{
		if(DEBUG)
		{
			System.out.println("Preferred Size: for component " + this + " is "
					+ _layoutManager.preferredLayoutSize(this));
		}
		if(_layoutManager == null)
		{
			return new Dimension(0, 0);
		}
		return _layoutManager.preferredLayoutSize(this);
	}

	/**
	 * delegates to the layout manager to determine the minimum size of the
	 * composite based on the minimum sizes of its children and the layout.
	 */
	public Dimension getMinimumSize()
	{
		return _layoutManager.minimumLayoutSize(this);
	}

	/**
	 * adds a graphic element to the composite
	 */
	public void add(GraphicElement ge)
	{
		if(DEBUG)
		{
			System.out.println("number of elements is " + getElementCount());
			System.out.println("The element is " + ge);
		}
		ge.setParent(this);
		_elements.addElement(ge);
	}

	/**
	 * adds a graphic element to the composite with information No longer needs
	 * to be public as there is an add method that calls this incase of String
	 * object. ?? A left over relic
	 */
	public void add(String name, GraphicElement ge)
	{
		if(name == null)
		{
			add(ge);
		}
		else if(_layoutManager instanceof GELineLayout)
		{
			if(name.equals("Before"))
			{
				ge.setParent(this);
				_elements.insertElementAt(ge, 0);
			}
			else if(name.equals("After"))
			{
				add(ge);
			}
		}
		else if(_layoutManager instanceof GEBorderLayout)
		{
			add(ge);
			_layoutManager.addLayoutElement(name, ge);
		}
	}

	/**
	 * adds a graphic element to the composite with information
	 */
	public void add(Object obj, GraphicElement ge)
	{
		if(obj == null)
		{
			add(ge);
		}
		if(obj instanceof String)
		{
			String name = (String) obj;
			if(_layoutManager instanceof GELineLayout)
			{
				if(name.equals("Before"))
				{
					ge.setParent(this);
					_elements.insertElementAt(ge, 0);
				}
				else if(name.equals("After"))
				{
					add(ge);
				}
			}
			else if(_layoutManager instanceof GEBorderLayout)
			{
				add(ge);
				_layoutManager.addLayoutElement(name, ge);
			}
		}
		else
		{
			add(ge);
			_layoutManager.addLayoutElement(obj, ge);
		}
	}

	/**
	 * inserts an element at the desired index
	 */
	public void insertAt(int index, GraphicElement ge)
	{
		ge.setParent(this);
		_elements.insertElementAt(ge, index);
	}

	/**
	 * removes the particular object from the container and returns the number
	 * of matches removed
	 */
	public int remove(GraphicElement ge)
	{
		_elements.removeElement(ge);
		return 1;
	}

	/**
	 * removes all elements from the composite.
	 */
	public void removeAll()
	{
		_elements.removeAllElements();
	}

	/**
	 * searches for the first element that matches and returns its index
	 */
	public int indexOf(GraphicElement ge)
	{
		return _elements.indexOf(ge);
	}

	/**
	 * reorders element container array to place the given element at the first
	 * index. This has the effect of drawing this element before others.
	 *
	 * @param ge The graphic element to place at the beginning.
	 */
	public void drawFirst(Drawable ge)
	{
		_elements.removeElement(ge);
		_elements.insertElementAt(ge, 0);
	}

	/**
	 * reorders element container array to place the given element at the last
	 * index. This has the effect of drawing this element after others.
	 *
	 * @param ge The graphic element to place at the beginning.
	 */
	public void drawLast(Drawable ge)
	{
		_elements.removeElement(ge);
		_elements.insertElementAt(ge, _elements.size());
	}

	/**
	 * gets a iterator object to iterate over the leaves or composites contained
	 * in this composite
	 */
	public DrawIterator getDrawIterator()
	{
		if(_drawIterator == null)
		{
			_drawIterator = new DrawIterator(_elements);
		}
		_drawIterator.resetIterator();
		return _drawIterator;
	}

	/**
	 * sets the iterator for iterating over the leaves of this composite.
	 */
	public void setDrawIterator(DrawIterator iterator)
	{
		_drawIterator = iterator;
	}

	/**
	 * gets a iterator object to iterate over the leaves or composites contained
	 * in this composite
	 */
	public CompositeIterator getIterator()
	{
		_iterator = new DrawIterator(_elements);
		return _iterator;
	}

	/**
	 * sets the iterator for iterating over the leaves of this composite.
	 */
	public void setIterator(CompositeIterator iterator)
	{
		_iterator = iterator;
	}

	/**
	 * Returns its properties in a Properties object. These are saved on disk.
	 *
	 * @param prefixTag A tag to assign the context for these properties e.g. if these
	 *                  properties belong to Axis class then prefixTag will be "Axis."
	 */
	public void toProperties(Properties p, String prefixTag)
	{
		super.toProperties(p, prefixTag);
		if(p == null)
		{
			return;
		}
		CompositeIterator iterator = getIterator();
		while(iterator.hasMoreElements())
		{
			GraphicElement ge = (GraphicElement) iterator.nextElement();
			ge.toProperties(p, prefixTag);
		}
	}

	/**
	 * Sets the font size using the original font size and resize ratio
	 * calculated by the FontResizeInteractor
	 *
	 * @see FontResizeInteractor
	 */
	public void setFontByRatio(double fontResizeRatio)
	{
		for(Enumeration iterator = getIterator(); iterator.hasMoreElements(); )
		{
			GraphicElement ge = (GraphicElement) iterator.nextElement();
			if(ge instanceof FontResizable)
			{
				((FontResizable) ge).setFontByRatio(fontResizeRatio);
			}
		}
	}

	/**
	 * initializes attributes and state from Properties object.
	 */
	public void fromProperties(Properties p, String prefixTag)
	{
		super.fromProperties(p, prefixTag);
		CompositeIterator iterator = getIterator();
		while(iterator.hasMoreElements())
		{
			GraphicElement ge = (GraphicElement) iterator.nextElement();
			ge.fromProperties(p, prefixTag);
		}
	}
}
