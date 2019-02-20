/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package vista.graph;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.Insets;
import java.awt.Point;
import java.awt.Rectangle;
import java.lang.reflect.Method;
import java.util.Properties;
import java.util.StringTokenizer;

/**
 * This the base class for all graphic components. The design pattern followed
 * here is the composite pattern. The GraphicElement is the Leaf of the
 * heirarchy. All Drawable and Bounded elements are subclasses of this class.
 * GEContainer is a subclass of this class which acts as a container for other
 * GraphicElements.
 * <p>
 * <p>
 * This element has the responsibility of drawing itself in relation to a region
 * and a graphics context. The graphics context used is java.awt.Graphics. This
 * implements only the primitive functions required for drawing. A further
 * update of this class may be needed when java.2d api is available.
 * <p>
 * <p>
 * Each such element and its subclasses have various settable attributes such as
 * color, font, line thicknesses etcetra. These are all encapsulated in GEAttr
 * objects and their subclasses. Access to these objects is provided via
 * getAttributes and setAttributes functions
 * <p>
 * <p>
 * In addition to drawing itself this element contains its drawing size and
 * parent reference. These are initialized and set by its parent classes or by
 * the GELayoutManager classes. The elements preferred size is used in used in
 * GELayoutManger to layout and size the elements.
 * <p>
 * <p>
 * For actual use of these elements they are encapsulated into a java.awt.Canvas
 * object. This object then uses delegation for painting itself.
 *
 * @author Nicky Sandhu
 * @version $Id: GraphicElement.java,v 1.1 2003/10/02 20:49:01 redwood Exp $
 * @see Drawable
 * @see Bounded
 * @see GEContainer
 * @see GELayoutManager
 * @see java.awt.Graphics
 */
public abstract class GraphicElement implements Drawable, Bounded, Cloneable,
												Animate
{
	/**
	 *
	 */
	public static final boolean DEBUG = false;
	/**
	 * true if graphics are to be drawn slowly
	 */
	public static final boolean DEBUG_GRAPHICS = true;
	/**
	 *
	 */
	public static long DRAW_DELAY = 25;
	/**
	 * Load color definitions
	 */
	private static ColorRGB _colorDefinitions;

	static
	{
		try
		{
			_colorDefinitions = new ColorRGB();
		}
		catch(Exception e)
		{
			e.printStackTrace();
		}
	}

	/**
	 * to indicate if orientation should be done by jdk2 capabilities
	 */
	protected boolean _doRotate = false;
	/**
	 * drawing area of component
	 */
	private Rectangle _area = new Rectangle(0, 0, 1, 1);
	/**
	 * the graphics context
	 */
	private Graphics _gc = null;
	/**
	 * the parent of this element
	 */
	private GEContainer _parent = null;
	/**
	 * insets defining borders. The actual drawing area is defined by the bounds
	 * returned by getInsetedBounds()
	 *
	 * @see GraphicElement#getInsetedBounds()
	 */
	private Insets _insets = new Insets(0, 0, 0, 0);
	/**
	 * The attributes of this element. All attributes that could be changed
	 * dynamically are contained in it.
	 */
	private GEAttr _attributes = null;
	/**
	 * name of this element
	 */
	private String _name = "";
	/**
	 *
	 */
	private GraphicsState _gcState = new GraphicsState();

	/**
	 * constructor
	 */
	public GraphicElement()
	{
		this(new GEAttr());
	}

	/**
	 * constructor
	 */
	public GraphicElement(GEAttr attributes)
	{
		_attributes = attributes;
	}

	/**
	 * parses string to get orientation
	 *
	 * @return the orientation as GEAttr.VERTICAL | .HORIZONTAL
	 */
	public static int parseOrientationProperty(String property)
	{
		int o = 0;
		if(property.equals("GEAttr.VERTICAL"))
		{
			o = GEAttr.VERTICAL;
		}
		else if(property.equals("GEAttr.HORIZONTAL"))
		{
			o = GEAttr.HORIZONTAL;
		}
		return o;
	}

	/**
	 * parses string to get color. The string either contains the Color default
	 * toString representation or the name of the color as defined in the
	 * rgb.properties file.
	 */
	public static Color parseColorProperty(String colorProperty)
	{
		StringTokenizer chopper;
		String rgbString = "";
		if(colorProperty.indexOf("[") == -1)
		{
			rgbString = _colorDefinitions.getRGBString(colorProperty);
			// if (DEBUG) _colorDefinitions.list(System.out);
		}
		else
		{
			chopper = new StringTokenizer(colorProperty, "[");
			chopper.nextToken();
			rgbString = chopper.nextToken();
			rgbString = rgbString.substring(0, rgbString.indexOf("]"));
		}
		if(rgbString == null)
		{
			return new Color(0, 0, 0);
		}
		if(DEBUG)
		{
			System.out.println("RGB : " + rgbString);
		}

		chopper = new StringTokenizer(rgbString, ",");
		String colorComponent = chopper.nextToken().trim();
		if(DEBUG)
		{
			System.out.println(colorComponent);
		}
		int red = new Integer(colorComponent.trim().substring(
				colorComponent.indexOf("=") + 1)).intValue();
		colorComponent = chopper.nextToken().trim();
		if(DEBUG)
		{
			System.out.println(colorComponent);
		}
		int green = new Integer(colorComponent.substring(colorComponent
				.indexOf("=") + 1)).intValue();
		colorComponent = chopper.nextToken().trim();
		if(DEBUG)
		{
			System.out.println(colorComponent);
		}
		int blue = new Integer(colorComponent.substring(colorComponent
				.indexOf("=") + 1)).intValue();
		if(DEBUG)
		{
			System.out.println("r = " + red + " g = " + green + " b = " + blue);
		}

		return new Color(red, green, blue);
	}

	/**
	 * draws with the same graphics context and rectangle size.
	 */
	protected abstract void Draw();

	/**
	 * The update method is called when the animation frame needs to be updated.
	 * Each element is responsible for drawing itself within the bounds given.
	 */
	public void update(AnimationObservable o, Object arg)
	{
	}

	/**
	 * Animates and displays next frame.
	 */
	public void animateNext()
	{
	}

	/**
	 * calculates the preferred size of this element
	 *
	 * @return the preferred size
	 */
	public abstract Dimension getPreferredSize();

	/**
	 * calculates the minimum size of this element
	 *
	 * @return the minimum size
	 */
	public abstract Dimension getMinimumSize();

	/**
	 * a clone method that throws an error instead of CloneNotSupportedException
	 */
	public Object clone()
	{
		try
		{
			return super.clone();
		}
		catch(CloneNotSupportedException cnse)
		{
			throw new Error("Clone not supported on " + this);
		}
	}

	/**
	 * draws with the same graphics context and rectangle size
	 */
	public void draw()
	{
		if(isVisible())
		{
			preDraw();
			Draw();
			postDraw();
		}
	}

	/**
	 * set up things to do before drawing operations. Store state if state is to
	 * restored to previous state in postDraw.
	 */
	public void preDraw()
	{
		// cache original state
		if(_gc == null)
		{
			return;
		}
		cacheGraphicsState(_gc);
		// set background color or not if color == null
		if(_attributes._backgroundColor != null)
		{
			// draw background
			_gc.setColor(_attributes._backgroundColor);
			Rectangle r = getBounds();
			_gc.fillRect(r.x, r.y, r.width, r.height);
		}
		// set foreground color
		if(_attributes._foregroundColor != null)
		{
			_gc.setColor(_attributes._foregroundColor);
		}
		// set clip
		if(_attributes._clipWithinBounds)
		{
			Rectangle r = getDrawBounds();
			if(DEBUG)
			{
				System.out.println("Setting clip for :" + this + " -> " + r);
			}
			_gc.setClip(r.x, r.y, r.width, r.height);
		}
		// set orientation ( future graphics )
		if(GraphUtils.isJDK2() && _attributes._orientation == GEAttr.VERTICAL
				&& _doRotate)
		{
			try
			{
				Rectangle r = getBounds();
				String methodName = "rotateVertical";
				Class[] params = {Class.forName("java.awt.Graphics"),
						Integer.TYPE, Integer.TYPE};
				Class r2dcl = Class.forName("vista.graph.Rotator2D");
				Method m = r2dcl.getDeclaredMethod(methodName, params);
				m.invoke(null, _gc,
						new Integer(r.x + r.width / 2),
						new Integer(r.y + r.height / 2));
			}
			catch(Exception exc)
			{
				throw new RuntimeException("Nested Exception: " + exc);
			}
			// use the affine transform to rotate all graphics by
			// Math.toRadians(90)
			// Class cl = Class.forName("java.awt.Graphics2D");
			// make new instance of graphics2d
		}
		// set transperencey/ opaquity (future graphics)
	}

	/**
	 * restore state to previous state
	 */
	public void postDraw()
	{
		// unset orientation ( future graphics )
		if(GraphUtils.isJDK2() && _attributes._orientation == GEAttr.VERTICAL
				&& _doRotate)
		{
			try
			{
				Rectangle r = getBounds();
				String methodName = "rotateHorizontal";
				Class[] params = {Class.forName("java.awt.Graphics"),
						Integer.TYPE, Integer.TYPE};
				Class r2dcl = Class.forName("vista.graph.Rotator2D");
				Method m = r2dcl.getDeclaredMethod(methodName, params);
				m.invoke(null, _gc,
						new Integer(r.x + r.width / 2),
						new Integer(r.y + r.height / 2));
			}
			catch(Exception exc)
			{
				throw new RuntimeException("Nested Exception: " + exc);
			}
		}
		// restore color, font and clip
		uncacheGraphicsState(_gc);
	}

	/**
	 * draws the component onto the graphics context
	 */
	public void draw(Graphics gc)
	{
		setGraphics(gc);

		draw();
	}

	/**
	 * draws the component onto the graphics context within the given Rectangle
	 */
	public void draw(Graphics gc, Rectangle r)
	{
		setGraphics(gc);

		draw(r);
	}

	/**
	 * sets the bound to this Rectangle and calls the draw method also sets the
	 * clipping to bounds if required
	 *
	 * @param r The region within which to draw
	 */
	public void draw(Rectangle r)
	{
		setBounds(r);

		draw();
	}

	/**
	 * gets the graphics context
	 */
	public Graphics getGraphics()
	{
		return _gc;
	}

	/**
	 * sets the graphics context
	 */
	public void setGraphics(Graphics gc)
	{
		if(_attributes.getCreateGraphicsCopy())
		{
			_gc = gc.create();
		}
		else
		{
			_gc = gc;
		}
	}

	/**
	 * gets this elements bounds
	 *
	 * @return A rectangle containing the element.
	 */
	public Rectangle getBounds()
	{
		return _area;
	}

	/**
	 * set this elements bounds
	 */
	public void setBounds(Rectangle r)
	{
		_area.setBounds(r);
	}

	/**
	 * gets this elements parent
	 *
	 * @return p the parent of this element
	 */
	public GEContainer getParent()
	{
		return _parent;
	}

	/**
	 * sets this elements parent
	 *
	 * @param p the parent of this element
	 */
	public void setParent(GEContainer p)
	{
		_parent = p;
	}

	/**
	 * gets the elements size, i.e. the width and height
	 *
	 * @return a new Dimension object with the element size
	 */
	public Dimension getSize()
	{
		return new Dimension(_area.width, _area.height);
	}

	/**
	 * sets size of this element
	 */
	public void setSize(Dimension d)
	{
		_area.width = d.width;
		_area.height = d.height;
	}

	/**
	 * get drawing bounds of rectangle
	 */
	public Rectangle getDrawBounds()
	{
		return getInsetedBounds();
	}

	/**
	 * get bounds of rectangle after allowing for insets
	 */
	public Rectangle getInsetedBounds()
	{
		return new Rectangle(_area.x + _insets.left, _area.y + _insets.top,
				_area.width - (_insets.left + _insets.right), _area.height
				- (_insets.top + _insets.bottom));
	}

	/**
	 * The insets for this element
	 *
	 * @return The Insets object.
	 */
	public Insets getInsets()
	{
		return _insets;
	}

	/**
	 * Sets the insets for this element
	 *
	 * @param i The Inset object to which the reference is stored.
	 */
	public void setInsets(Insets i)
	{
		_insets = i;
	}

	/**
	 * gets the attributes
	 *
	 * @return The GEAttr object
	 */
	public GEAttr getAttributes()
	{
		return _attributes;
	}

	/**
	 * Sets the attributes
	 */
	public void setAttributes(GEAttr gea)
	{
		_attributes = gea;
	}

	/**
	 * set this elements location
	 */
	public void setLocation(Point p)
	{
		_area.setLocation(p);
	}

	/**
	 * checks to see if point is contained with element dimensions
	 */
	public boolean contains(Point p)
	{
		if(_area != null)
		{
			return _area.contains(p);
		}
		else
		{
			return false;
		}
	}

	/**
	 * checks to see if point is contained with element dimensions
	 */
	public boolean contains(int x, int y)
	{
		if(_area != null)
		{
			return _area.contains(x, y);
		}
		else
		{
			return false;
		}
	}

	/**
	 * checks to see if point is on the drawing.
	 */
	public boolean hitsDrawing(int x, int y)
	{
		return contains(x, y);
	}

	/**
	 * Checks if two rectangles intersect.
	 */
	public boolean intersects(Rectangle r)
	{
		if(_area != null)
		{
			return _area.intersects(r);
		}
		else
		{
			return false;
		}
	}

	/**
	 * is this component visible?
	 */
	public boolean isVisible()
	{
		return _attributes._isVisible;
	}

	/**
	 * sets the visibility flag for this object.
	 */
	public void setVisible(boolean visible)
	{
		_attributes._isVisible = visible;
	}

	/**
	 * returns the prefix tag with which the properties of this object are
	 * attached.
	 */
	protected String getPrefixTag(String prefixTag)
	{
		String name = getName();
		if(name != null)
		{
			return prefixTag + name + ".";
		}
		else
		{
			return prefixTag;
		}
	}

	/**
	 * Returns its properties in a Properties object. These are saved on disk.
	 *
	 * @param prefixTag A tag to assign the context for these properties e.g. if these
	 *                  properties belong to Axis class then prefixTag will be "Axis."
	 */
	public void toProperties(Properties p, String prefixTag)
	{

		if(p == null)
		{
			return;
		}
		GEAttr attr = getAttributes();
		if(attr._backgroundColor != null)
		{
			p.put(prefixTag + "backgroundColor", attr._backgroundColor
					.toString());
		}
		if(attr._foregroundColor != null)
		{
			p.put(prefixTag + "foregroundColor", attr._foregroundColor
					.toString());
		}
		if(attr._orientation == GEAttr.HORIZONTAL)
		{
			p.put(prefixTag + "orientation", "GEAttr.HORIZONTAL");
		}
		else if(attr._orientation == GEAttr.VERTICAL)
		{
			p.put(prefixTag + "orientation", "GEAttr.VERTICAL");
		}
		p.put(prefixTag + "createGraphicsCopy", new Boolean(
				attr._createGraphicsCopy).toString());
		p.put(prefixTag + "isVisible", new Boolean(attr._isVisible).toString());
		p.put(prefixTag + "clipWithinBounds", new Boolean(
				attr._clipWithinBounds).toString());

	}

	/**
	 * initializes attributes and state from Properties object.
	 */
	public void fromProperties(Properties p, String prefixTag)
	{

		String property = p.getProperty(prefixTag + "backgroundColor");
		GEAttr attr = getAttributes();
		if(property != null)
		{
			attr._backgroundColor = parseColorProperty(property);
		}

		property = p.getProperty(prefixTag + "foregroundColor");
		if(property != null)
		{
			attr._foregroundColor = parseColorProperty(property);
			if(DEBUG)
			{
				System.out.println("Color of " + prefixTag + " is " + property);
			}
		}

		property = p.getProperty(prefixTag + "orientation");
		if(property != null)
		{
			attr._orientation = parseOrientationProperty(property);
		}

		property = p.getProperty(prefixTag + "createGraphicsCopy");
		if(property != null)
		{
			attr._createGraphicsCopy = new Boolean(property).booleanValue();
		}

		property = p.getProperty(prefixTag + "isVisible");
		if(property != null)
		{
			attr._isVisible = new Boolean(property).booleanValue();
		}

		property = p.getProperty(prefixTag + "clipWithinBounds");
		if(property != null)
		{
			attr._clipWithinBounds = new Boolean(property).booleanValue();
		}

	}

	/**
	 * returns the background color or null if no background. no background is
	 * the same as a transperent background.
	 */
	public Color getBackgroundColor()
	{
		return _attributes._backgroundColor;
	}

	/**
	 * sets the background color
	 */
	public void setBackgroundColor(Color c)
	{
		getAttributes()._backgroundColor = c;
	}

	/**
	 * returns the foreground color if any.
	 */
	public Color getForegroundColor()
	{
		return _attributes._foregroundColor;
	}

	/**
	 * sets the foreground color
	 */
	public void setForegroundColor(Color c)
	{
		getAttributes()._foregroundColor = c;
	}

	/**
	 * gets ClipWithinBounds
	 */
	public boolean getClipWithinBounds()
	{
		return getAttributes()._clipWithinBounds;
	}

	/**
	 * sets ClipWithinBounds
	 */
	public void setClipWithinBounds(boolean clipWithinBounds)
	{
		getAttributes()._clipWithinBounds = clipWithinBounds;
	}

	/**
	 * gets Orientation
	 */
	public int getOrientation()
	{
		return getAttributes()._orientation;
	}

	/**
	 * sets Orientation
	 */
	public void setOrientation(int orientation)
	{
		getAttributes()._orientation = orientation;
	}

	/**
	 * gets the name of this component
	 */
	public String getName()
	{
		return _name;
	}

	/**
	 * sets the name of this component
	 */
	public void setName(String name)
	{
		_name = name;
	}

	/**
	 * get short class name
	 */
	private String getShortClassName(Object obj)
	{
		String lname = obj.getClass().getName();
		int dotIndex = lname.lastIndexOf('.');
		return lname.substring(dotIndex + 1);
	}

	/**
	 * returns the name of this graphic element object
	 */
	public String toString()
	{
		if(!_name.equals(""))
		{
			return getShortClassName(this) + " (" + getName() + ")";
		}
		else
		{
			return getShortClassName(this);
		}
	}

	/**
	 * create dialog panel for this element.
	 */
	public GEDialogPanel createDialogPanel()
	{
		return new GEDialogPanel(this);
	}

	/**
	 * loads color names to RGB mapping from file The format is
	 * color_name=r=xxx,g=xxx,b=xxx where 0 <= xxx <= 255 If the properties
	 * could not be loaded default color names and mappings are created.
	 */
	public void loadColorProperties(String filename)
	{
		_colorDefinitions = new ColorRGB(filename);
	}

	/**
	 * caches the state of graphics
	 */
	protected final void cacheGraphicsState(Graphics g)
	{
		_gcState._color = g.getColor();
		_gcState._font = g.getFont();
		if(DEBUG)
		{
			System.err.println(this + " Graphics: " + System.identityHashCode(g));
			System.out.println("Clip bounds in cacheGraphicsState " + this + " -> " + g.getClipBounds());
		}
		_gcState._clip = g.getClipBounds();
	}

	/**
	 * uncache the saved graphics state.
	 */
	protected final void uncacheGraphicsState(Graphics g)
	{
		g.setColor(_gcState._color);
		g.setFont(_gcState._font);
		if(DEBUG)
		{
			System.err.println(this + " Graphics: " + System.identityHashCode(g));
		}
		if(_gcState._clip != null)
		{
			g.setClip(_gcState._clip.x, _gcState._clip.y, _gcState._clip.width,
					_gcState._clip.height);
			if(DEBUG)
			{
				System.out.println("Setting clip in uncacheGraphicsState: " + this + " -> " + _gcState._clip);
			}
		}
	}

	/**
	 *
	 */
	protected class GraphicsState
	{
		public Color _color;
		public Font _font;
		public Rectangle _clip;
	}
}
