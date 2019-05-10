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

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.io.OutputStream;
import java.io.PrintStream;
import java.util.ArrayList;
import java.util.List;
import javax.swing.*;

import gov.ca.water.calgui.project.EpptScenarioRun;
import gov.ca.water.calgui.wresl.WreslOutputConsumer;
import org.openide.windows.WindowManager;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 05-10-2019
 */
public class WreslRunDialog extends JDialog implements WreslOutputConsumer
{
	private final JTabbedPane _tabbedPane;
	private final List<TextAreaPrintStream> _textAreaPrintStreams = new ArrayList<>();

	public WreslRunDialog()
	{
		super(WindowManager.getDefault().getMainWindow(), "Run WRESL Script", false);
		setSize(new Dimension(600, 500));
		setLocationRelativeTo(WindowManager.getDefault().getMainWindow());
		setLayout(new BorderLayout());
		_tabbedPane = new JTabbedPane();
		add(_tabbedPane, BorderLayout.CENTER);
	}


	@Override
	public void consume(EpptScenarioRun scenarioRun, OutputStream outputStream)
	{
		SwingUtilities.invokeLater(() ->
		{

			JTextArea textArea = new JTextArea();
			TextAreaPrintStream textAreaPrintStream = new TextAreaPrintStream(textArea, outputStream);
			_tabbedPane.addTab(scenarioRun.getName(), textArea);
			_textAreaPrintStreams.add(textAreaPrintStream);
		});
	}

	@Override
	public void dispose()
	{
		super.dispose();
		_textAreaPrintStreams.forEach(PrintStream::close);
	}
}
