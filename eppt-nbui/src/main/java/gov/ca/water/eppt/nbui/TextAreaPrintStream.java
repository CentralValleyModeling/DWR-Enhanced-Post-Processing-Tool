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

package gov.ca.water.eppt.nbui;

import java.io.OutputStream;
import java.io.PrintStream;
import javax.swing.*;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 05-10-2019
 */
public class TextAreaPrintStream extends PrintStream
{
	private final JTextArea _textArea;
	private boolean atLineStart = true;
	private String indent = "";

	public TextAreaPrintStream(JTextArea textArea, OutputStream outputStream)
	{
		super(outputStream);
		_textArea = textArea;
	}

	private void writeToTextArea(String str)
	{
		if(_textArea != null)
		{
			synchronized(_textArea)
			{
				_textArea.setCaretPosition(_textArea.getDocument().getLength());
				_textArea.append(str);
			}
		}
	}

	private void write(String str)
	{
		String[] s = str.split("\n", -1);
		if(s.length == 0)
		{
			return;
		}
		for(int i = 0; i < s.length - 1; i++)
		{
			writeWithPotentialIndent(s[i]);
			writeWithPotentialIndent("\n");
			atLineStart = true;
		}
		String last = s[s.length - 1];
		if(!last.equals(""))
		{
			writeWithPotentialIndent(last);
		}
	}

	private void writeWithPotentialIndent(String s)
	{
		if(atLineStart)
		{
			writeToTextArea(indent + s);
			atLineStart = false;
		}
		else
		{
			writeToTextArea(s);
		}
	}

	private void newLine()
	{
		write("\n");
	}

	@Override
	public void print(boolean b)
	{
		synchronized(this)
		{
			super.print(b);
			write(String.valueOf(b));
		}
	}

	@Override
	public void print(char c)
	{
		synchronized(this)
		{
			super.print(c);
			write(String.valueOf(c));
		}
	}

	@Override
	public void print(char[] s)
	{
		synchronized(this)
		{
			super.print(s);
			write(String.valueOf(s));
		}
	}

	@Override
	public void print(double d)
	{
		synchronized(this)
		{
			super.print(d);
			write(String.valueOf(d));
		}
	}

	@Override
	public void print(float f)
	{
		synchronized(this)
		{
			super.print(f);
			write(String.valueOf(f));
		}
	}

	@Override
	public void print(int i)
	{
		synchronized(this)
		{
			super.print(i);
			write(String.valueOf(i));
		}
	}

	@Override
	public void print(long l)
	{
		synchronized(this)
		{
			super.print(l);
			write(String.valueOf(l));
		}
	}

	@Override
	public void print(Object o)
	{
		synchronized(this)
		{
			super.print(o);
			write(String.valueOf(o));
		}
	}

	@Override
	public void print(String s)
	{
		synchronized(this)
		{
			super.print(s);
			if(s == null)
			{
				write("null");
			}
			else
			{
				write(s);
			}
		}
	}

	@Override
	public void println()
	{
		synchronized(this)
		{
			newLine();
			super.println();
		}
	}

	@Override
	public void println(boolean x)
	{
		synchronized(this)
		{
			print(x);
			newLine();
			super.println();
		}
	}

	@Override
	public void println(char x)
	{
		synchronized(this)
		{
			print(x);
			newLine();
			super.println();
		}
	}

	@Override
	public void println(int x)
	{
		synchronized(this)
		{
			print(x);
			newLine();
			super.println();
		}
	}

	@Override
	public void println(long x)
	{
		synchronized(this)
		{
			print(x);
			newLine();
			super.println();
		}
	}

	@Override
	public void println(float x)
	{
		synchronized(this)
		{
			print(x);
			newLine();
			super.println();
		}
	}

	@Override
	public void println(double x)
	{
		synchronized(this)
		{
			print(x);
			newLine();
			super.println();
		}
	}

	@Override
	public void println(char x[])
	{
		synchronized(this)
		{
			print(x);
			newLine();
			super.println();
		}
	}

	@Override
	public void println(String x)
	{
		synchronized(this)
		{
			print(x);
			newLine();
			super.println();
		}
	}

	@Override
	public void println(Object x)
	{
		String s = String.valueOf(x);
		synchronized(this)
		{
			print(s);
			newLine();
			super.println();
		}
	}
}