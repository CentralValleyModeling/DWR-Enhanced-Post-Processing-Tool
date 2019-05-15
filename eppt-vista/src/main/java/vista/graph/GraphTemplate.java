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
package vista.graph;

import java.awt.Color;
import java.awt.Font;
import java.util.StringTokenizer;

/**
 *
 */
public class GraphTemplate
{
	/**
	 *
	 */
	public final static String Graph_Insets = "Graph.Insets";
	/**
	 *
	 */
	public final static String Graph_ForegroundColor = "Graph.ForegroundColor";
	/**
	 *
	 */
	public final static String Graph_BackgroundColor = "Graph.BackgroundColor";
	/**
	 *
	 */
	public final static String Graph_TitleText = "Graph.TitleText";
	/**
	 *
	 */
	public final static String Graph_TitleFont = "Graph.TitleFont";
	/**
	 *
	 */
	public final static String Graph_TitleColor = "Graph.TitleColor";
	/**
	 *
	 */
	public final static String Graph_TitleBackgroundColor = "Graph.TitleBackgroundColor";
	/**
	 *
	 */
	public final static String Plot_Insets = "Plot.Insets";
	/**
	 *
	 */
	public final static String Plot_ForegroundColor = "Plot.ForegroundColor";
	/**
	 *
	 */
	public final static String Plot_BackgroundColor = "Plot.BackgroundColor";
	/**
	 *
	 */
	public final static String Plot_LeftAxisTitle = "Plot.LeftAxisTitle";
	/**
	 *
	 */
	public final static String Plot_RightAxisTitle = "Plot.RightAxisTitle";
	/**
	 *
	 */
	public final static String Plot_TopAxisTitle = "Plot.TopAxisTitle";
	/**
	 *
	 */
	public final static String Plot_BottomAxisTitle = "Plot.BottomAxisTitle";
	/**
	 *
	 */
	public final static String Plot_LeftAxisTickLocation = "Plot.LeftAxisTickLocation";
	/**
	 *
	 */
	public final static String Plot_RightAxisTickLocation = "Plot.RightAxisTickLocation";
	/**
	 *
	 */
	public final static String Plot_TopAxisTickLocation = "Plot.TopAxisTickLocation";
	/**
	 *
	 */
	public final static String Plot_BottomAxisTickLocation = "Plot.BottomAxisTickLocation";
	/**
	 *
	 */
	public final static String Plot_LeftAxisTickGenerator = "Plot.LeftAxisTickGenerator";
	/**
	 *
	 */
	public final static String Plot_RightAxisTickGenerator = "Plot.RightAxisTickGenerator";
	/**
	 *
	 */
	public final static String Plot_TopAxisTickGenerator = "Plot.TopAxisTickGenerator";
	/**
	 *
	 */
	public final static String Plot_BottomAxisTickGenerator = "Plot.BottomAxisTickGenerator";
	/**
	 *
	 */
	public final static String Axis_Color = "Axis.Color";
	/**
	 *
	 */
	public final static String Axis_TitleColor = "Axis.TitleColor";
	/**
	 *
	 */
	public final static String Axis_MajorTickSize = "Axis.MajorTickSize";
	/**
	 *
	 */
	public final static String Axis_MinorTickSize = "Axis.MinorTickSize";
	/**
	 *
	 */
	public final static String Plot_GridVisible = "Plot.GridVisible";
	/**
	 *
	 */
	public final static String SEPERATOR = ":";
	/**
	 *
	 */
	public final static String COMMENT_START = "#";

	/**
	 *
	 */
	public GraphTemplate(String templateFile, Graph graph)
	{
		try
		{
			java.io.LineNumberReader reader = new java.io.LineNumberReader(
					new java.io.FileReader(templateFile));
			String line = null;
			while((line = reader.readLine()) != null)
			{
				initializeGraphFromTokens(line, graph);
			}
		}
		catch(java.io.IOException ioe)
		{
			System.out.println(ioe);
			System.out.println("Error reading template file " + templateFile);
		}
	}

	/**
	 * returns the specified color described by string
	 *
	 * @str A string description of the color such as yellow, red, etc.
	 * @returns The object associated with the color described by the string
	 */
	public static Color getSpecifiedColor(String str)
	{
		if(str == null)
		{
			return Color.yellow;
		}
		else if(str != null)
		{
			if(str.compareTo("Color.yellow") == 0)
			{
				return Color.yellow;
			}
			else if(str.compareTo("Color.red") == 0)
			{
				return Color.red;
			}
			else if(str.compareTo("Color.white") == 0)
			{
				return Color.white;
			}
			else if(str.compareTo("Color.gray") == 0)
			{
				return Color.gray;
			}
			else if(str.compareTo("Color.darkGray") == 0)
			{
				return Color.darkGray;
			}
			else if(str.compareTo("Color.black") == 0)
			{
				return Color.black;
			}
			else if(str.compareTo("Color.pink") == 0)
			{
				return Color.pink;
			}
			else if(str.compareTo("Color.orange") == 0)
			{
				return Color.orange;
			}
			else if(str.compareTo("Color.green") == 0)
			{
				return Color.green;
			}
			else if(str.compareTo("Color.magenta") == 0)
			{
				return Color.magenta;
			}
			else if(str.compareTo("Color.cyan") == 0)
			{
				return Color.cyan;
			}
			else
			{
				return Color.yellow;
			}
		}
		else
		{
			return Color.yellow;
		}
	}

	/**
	 * Initializes the graph from the token. Each line of the template file is
	 * read. Each line is seperated into the descriptor string and the value
	 * string by the ":" seperator. Then the value string is used in the context
	 * provided by the descriptor string.
	 *
	 * @param Graph
	 */
	private void initializeGraphFromTokens(String line, Graph graph)
	{
		if(line.trim().startsWith(COMMENT_START))
		{
			return;
		}
		StringTokenizer tokenizer = new StringTokenizer(line, SEPERATOR);
		String token = null;
		String value = null;
		try
		{
			token = tokenizer.nextToken();
			value = tokenizer.nextToken();
		}
		catch(java.util.NoSuchElementException e)
		{
			System.out.println("Incorrect format: " + line);
			return;
		}

		if(line.trim().startsWith(COMMENT_START))
		{
			System.out.println("Comment");
		}
		else if(token.equals(Graph_Insets))
		{
			try
			{
				StringTokenizer tt = new StringTokenizer(value, ",");
				int top = new Integer(tt.nextToken().trim()).intValue();
				int left = new Integer(tt.nextToken().trim()).intValue();
				int bottom = new Integer(tt.nextToken().trim()).intValue();
				int right = new Integer(tt.nextToken().trim()).intValue();
				graph.setInsets(new java.awt.Insets(top, left, bottom, right));
				System.out.println("Graph Insets " + graph.getInsets());
			}
			catch(java.util.NoSuchElementException e)
			{
				System.out.println("Incorrect format: " + line);
			}
		}
		else if(token.equals(Graph_ForegroundColor))
		{
			Color c = getSpecifiedColor(value.trim());
			setForegroundColor(graph, c);
			System.out.println("Graph_ForegroundColor " + value);
		}
		else if(token.equals(Graph_BackgroundColor))
		{
			Color c = getSpecifiedColor(value.trim());
			setBackgroundColor(graph, c);
			System.out.println("Graph_BackgroundColor " + value);
		}
		else if(token.equals(Graph_TitleText))
		{
			graph.setTitle(value.trim());
		}
		else if(token.equals(Graph_TitleFont))
		{
			TextLine title = graph.getTitle();
			TextLineAttr attr = (TextLineAttr) title.getAttributes();
			attr._font = new Font("Times Roman", Font.PLAIN, 30);
		}
		else if(token.equals(Graph_TitleColor))
		{
			Color c = getSpecifiedColor(value.trim());
			TextLine title = graph.getTitle();
			TextLineAttr attr = (TextLineAttr) title.getAttributes();
			attr._foregroundColor = c;
		}
		else if(token.equals(Graph_TitleBackgroundColor))
		{
			TextLine title = graph.getTitle();
			TextLineAttr attr = (TextLineAttr) title.getAttributes();
			attr._backgroundColor = getSpecifiedColor(value.trim());
		}
		else if(token.equals(Plot_Insets))
		{
		}
		else if(token.equals(Plot_ForegroundColor))
		{
		}
		else if(token.equals(Plot_BackgroundColor))
		{
		}
		else if(token.equals(Plot_LeftAxisTitle))
		{
		}
		else if(token.equals(Plot_RightAxisTitle))
		{
		}
		else if(token.equals(Plot_TopAxisTitle))
		{
		}
		else if(token.equals(Plot_BottomAxisTitle))
		{
		}
		else if(token.equals(Plot_LeftAxisTickLocation))
		{
		}
		else if(token.equals(Plot_RightAxisTickLocation))
		{
		}
		else if(token.equals(Plot_TopAxisTickLocation))
		{
		}
		else if(token.equals(Plot_BottomAxisTickLocation))
		{
		}
		else if(token.equals(Plot_LeftAxisTickGenerator))
		{
		}
		else if(token.equals(Plot_RightAxisTickGenerator))
		{
		}
		else if(token.equals(Plot_TopAxisTickGenerator))
		{
		}
		else if(token.equals(Plot_BottomAxisTickGenerator))
		{
		}
		else if(token.equals(Axis_Color))
		{
		}
		else if(token.equals(Axis_TitleColor))
		{
		}
		else if(token.equals(Axis_MajorTickSize))
		{
		}
		else if(token.equals(Axis_MinorTickSize))
		{
		}
		else if(token.equals(Plot_GridVisible))
		{
		}
		// } else if (token.equals(X)){
		// }
		else
		{
			System.out.println("Token " + token + " not recognized");
			System.out.println("Ignoring " + token);
		}
	}

	/**
	 * sets the background color for all the components of the graph
	 */
	private void setBackgroundColor(GEContainer gec, java.awt.Color c)
	{
		gec.getAttributes()._backgroundColor = c;
		int count = gec.getElementCount();
		for(int i = 0; i < count; i++)
		{
			GraphicElement ge = gec.getElement(i);
			GEAttr attr = ge.getAttributes();
			attr._backgroundColor = c;
			if(ge instanceof GEContainer)
			{
				setBackgroundColor((GEContainer) ge, c);
			}
		}
	}

	/**
	 * sets the foreground color for all components of graph
	 */
	private void setForegroundColor(GEContainer gec, java.awt.Color c)
	{
		gec.getAttributes()._foregroundColor = c;
		int count = gec.getElementCount();
		for(int i = 0; i < count; i++)
		{
			GraphicElement ge = gec.getElement(i);
			GEAttr attr = ge.getAttributes();
			attr._foregroundColor = c;
			if(attr instanceof TickLineAttr)
			{
				((TickLineAttr) attr)._color = c;
			}
			if(ge instanceof GEContainer)
			{
				setForegroundColor((GEContainer) ge, c);
			}
		}
	}
}
