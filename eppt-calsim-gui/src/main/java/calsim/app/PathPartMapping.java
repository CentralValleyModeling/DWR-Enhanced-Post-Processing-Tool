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

package calsim.app;

import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.LineNumberReader;
import java.io.PrintWriter;
import java.util.HashMap;
import java.util.Map;
import java.util.StringTokenizer;

/**
 * A mapping from path parts to another path parts.
 *
 * @author Nicky Sandhu
 * @version $Id: PathPartMapping.java,v 1.1.2.2 2000/12/20 20:02:18 amunevar Exp $
 */
public class PathPartMapping
{
	private Map<String, String> _map;

	/**
	 *
	 */
	public PathPartMapping()
	{
		_map = new HashMap<>();
	}

	/**
	 *
	 */
	public PathPartMapping(String file) throws IOException
	{
		this();
		try(LineNumberReader reader = new LineNumberReader(new FileReader(file)))
		{
			String[] keys = new String[2];
			String[] vals = new String[2];
			while(true)
			{
				String line = reader.readLine();
				if(line == null)
				{
					break;
				}
				StringTokenizer st = new StringTokenizer(line, ",");
				if(st.countTokens() != 4)
				{
					continue;
				}
				keys[0] = st.nextToken();
				keys[1] = st.nextToken();
				vals[0] = st.nextToken();
				vals[1] = st.nextToken();
				this.addMap(keys, vals);
			}
		}
	}

	/**
	 *
	 */
	public void saveTo(String file) throws IOException
	{
		PrintWriter writer = new PrintWriter(new FileWriter(file));
		for(Map.Entry<String, String> e : _map.entrySet())
		{
			String key = e.getKey();
			String val = e.getValue();
			String[] keys = createPlural(key);
			String[] vals = createPlural(val);
			for(final String key1 : keys)
			{
				writer.print(key1);
				writer.print(",");
			}
			for(final String val1 : vals)
			{
				writer.print(val1);
				writer.print(",");
			}
			writer.println();
		}
		writer.close();
	}

	/**
	 *
	 */
	public String createSingular(String[] keys)
	{
		if(keys == null)
		{
			return null;
		}
		StringBuilder key = new StringBuilder();
		for(int i = 0; i < keys.length; i++)
		{
			key.append("/").append((keys[i] == null) ? "" : keys[i].toUpperCase());
		}
		return key.toString();
	}

	/**
	 *
	 */
	public String[] createPlural(String key)
	{
		if(key == null)
		{
			return null;
		}
		StringTokenizer st = new StringTokenizer(key, "/");
		String[] vals = new String[st.countTokens()];
		int count = 0;
		while(st.hasMoreTokens())
		{
			vals[count++] = st.nextToken();
		}
		return vals;
	}

	/**
	 *
	 */
	public void addMap(String[] keys, String[] vals)
	{
		if(keys == null || vals == null)
		{
			return;
		}
		String key = createSingular(keys);
		String val = createSingular(vals);
		_map.put(key, val);
	}

	/**
	 *
	 */
	public String[] getMap(String[] keys)
	{
		if(keys == null)
		{
			return null;
		}
		String key = createSingular(keys);
		return createPlural(_map.get(key));
	}
}
