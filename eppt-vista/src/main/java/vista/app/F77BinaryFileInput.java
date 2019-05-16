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
package vista.app;

import java.io.DataInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.lang.reflect.Array;
import java.lang.reflect.Field;

/**
 * Reads from a binary file
 */
public class F77BinaryFileInput extends GenericFileInput
{
	/**
	 *
	 */
	private DataInputStream dis;

	/**
	 * Initialize streams and/or open connection to file input
	 */
	public void initializeInput(String filename) throws IOException
	{
		InputStream is = getInputStream(filename);
		if(is == null)
		{
			throw new IOException("Could not initialize from " + filename);
		}
		dis = new DataInputStream(is);
	}

	/**
	 * reads line and returns as string
	 */
	public String readString() throws IOException
	{
		int len = dis.readInt();

		byte[] data = new byte[len];
		dis.readFully(data, 0, len);
		char[] str = new char[data.length];
		for(int i = 0; i < str.length; i++)
		{
			str[i] = (char) data[i];
		}

		if(len != dis.readInt())
		{
			throw new IOException("Corrupted file");
		}
		return new String(str);
	}

	/**
	 * read a short or interpret next data input as short
	 */
	public short readShort() throws IOException
	{
		if(dis.readInt() != 2)
		{
			throw new IOException(" error reading short");
		}
		short s = dis.readShort();
		if(dis.readInt() != 2)
		{
			throw new IOException(" error reading short");
		}
		return s;
	}

	/**
	 * read an integer
	 */
	public int readInt() throws IOException
	{
		if(dis.readInt() != 4)
		{
			throw new IOException(" error reading integer");
		}
		int i = dis.readInt();
		if(dis.readInt() != 4)
		{
			throw new IOException(" error reading integer");
		}
		return i;
	}

	/**
	 * read a float
	 */
	public float readFloat() throws IOException
	{
		// if (dis.readInt() != 4) throw new IOException("Error reading float");
		dis.readInt();
		return dis.readFloat();
	}

	/**
	 * read a double
	 */
	public double readDouble() throws IOException
	{
		// if (dis.readInt() != 8) throw new
		// IOException("Error reading double");
		dis.readInt();
		return dis.readDouble();
	}

	/**
	 *
	 */
	public Object readObject(Object obj) throws IOException
	{
		dis.readInt();

		Class objectClass = obj.getClass();
		if(objectClass.isArray())
		{
			int len = Array.getLength(obj);
			Field[] fields = objectClass.getComponentType().getFields();
			for(int i = 0; i < len; i++)
			{
				Array.set(obj, i, readThisObject(Array.get(obj, i), fields));
			}
		}
		else
		{
			obj = readThisObject(obj, obj.getClass().getFields());
		}

		dis.readInt();
		return obj;
	}

	/**
	 *
	 */
	private Object readThisObject(Object obj, Field[] fields)
			throws IOException
	{
		try
		{
			for(int i = 0; i < fields.length; i++)
			{
				// System.out.println(fields[i].getType());
				if(fields[i].getType() == Short.TYPE)
				{
					fields[i].setShort(obj, dis.readShort());
				}
				else if(fields[i].getType() == Integer.TYPE)
				{
					fields[i].setInt(obj, dis.readInt());
				}
				else if(fields[i].getType() == Float.TYPE)
				{
					fields[i].setFloat(obj, dis.readFloat());
				}
				else if(fields[i].getType() == Double.TYPE)
				{
					fields[i].setDouble(obj, dis.readDouble());
				}
			}
		}
		catch(IllegalAccessException iae)
		{
			throw new IOException(" Illegal Access Exception " + iae);
		}

		return obj;
	}

	/**
	 * move to next record, in this case it does nothing except throw exceptions
	 */
	public void nextRecord() throws IOException
	{
		// readUTF();
		throw new IOException("No concept of record for binary input");
	}

	/**
	 * close the input stream and release resources
	 */
	public void closeStream() throws IOException
	{
		// is.close();
	}
}
