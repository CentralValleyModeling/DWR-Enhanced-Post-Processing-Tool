package gov.ca.water.eppt.nbui;

import static java.lang.System.lineSeparator;

import java.io.ByteArrayOutputStream;
import java.io.OutputStream;
import java.io.PrintStream;
import java.nio.charset.StandardCharsets;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * NetBeans only redirects System.err into the log; this does the same for System.out so its text reaches
 * messages.log and any other root logger handler.
 */
final class StdoutLogRedirect extends OutputStream
{
	//NbFormatter prints records from the "stderr" logger as bare lines, matching how NetBeans shows System.err
	private static final Logger STDOUT_LOGGER = Logger.getLogger("stderr");
	public static final String SEP = lineSeparator();

	private final ByteArrayOutputStream buffer = new ByteArrayOutputStream();

	private StdoutLogRedirect()
	{
	}

	static void install()
	{
		System.setOut(new PrintStream(new StdoutLogRedirect(), true, StandardCharsets.UTF_8));
	}

	@Override
	public synchronized void write(int b)
	{
		if(b == '\n')
		{
			flushLine();
		}
		else
		{
			buffer.write(b);
		}
	}

	@Override
	public synchronized void write(byte[] bytes, int off, int len)
	{
		for(int i = off; i < off + len; i++)
		{
			write(bytes[i]);
		}
	}

	private void flushLine()
	{
		String line = buffer.toString(StandardCharsets.UTF_8);
		buffer.reset();
		if(line.endsWith("\r"))
		{
			line = line.substring(0, line.length() - 1);
		}
		STDOUT_LOGGER.log(Level.INFO, line + SEP);
	}
}
