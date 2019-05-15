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
package vista.app.commands;

import java.awt.Graphics;
import java.awt.Image;
import java.awt.PrintJob;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.util.Properties;
import javax.swing.*;
import javax.swing.text.StyledDocument;

import vista.gui.Command;
import vista.gui.ExecutionException;
import vista.report.MonthlyReport;
import vista.report.RAppUtils;
import vista.report.TextDisplay;
import vista.set.DataReference;
import vista.set.Group;
import vista.set.RegularTimeSeries;

/**
 * Encapsulates commands implementing group related commands
 *
 * @author Tawnly Pranger
 */
public class MonthlyAverageCommand implements Command
{
	private Group _group;
	private int[] _rNumbers;
	private String _filename;
	private JFrame parent = null;
	private Image image = null;
	private JTextPane tp = null;

	/**
	 * opens group and sets current group to
	 */
	public MonthlyAverageCommand(Group g, int[] referenceNumbers)
	{
		_group = g;
		_rNumbers = referenceNumbers;
	}

	/**
	 * executes command
	 */
	public void execute() throws ExecutionException
	{
		if(_rNumbers == null || _rNumbers.length == 0)
		{
			throw new ExecutionException("No data selected");
		}
		for(int i = 0; i < _rNumbers.length; i++)
		{
			DataReference ref = _group.getDataReference(_rNumbers[i]);
			RAppUtils.useCFS = !ref.getData().getAttributes().getYUnits().equalsIgnoreCase(
					"TAF");
			MonthlyReport mr = new MonthlyReport((RegularTimeSeries) ref
					.getData(), ref.getPathname(), ref.getFilename());
			StyledDocument sd = mr.getStyledDocument();
			TextDisplay td = new TextDisplay(sd);

			// /*
			tp = new JTextPane();
			tp.setStyledDocument(sd);
			// */
			JFrame frame = new JFrame();
			parent = frame;
			frame.addWindowListener(new WindowAdapter()
			{
				public void windowClosing(WindowEvent evt)
				{
					((JFrame) evt.getSource()).dispose();
				}
			});
			// /*
			JMenuBar bar = new JMenuBar();
			JMenu printMenu = new JMenu("Print");
			JMenuItem printItem = new JMenuItem("Print");
			printItem.addActionListener(new ActionListener()
			{
				public void actionPerformed(ActionEvent evt)
				{
					/*
					 * // printer option for 1.2 and greater PrinterJob ptjb =
					 * PrinterJob.getPrinterJob(); PageFormat pf =
					 * ptjb.defaultPage(); pf = ptjb.pageDialog(pf);
					 */
					PrintJob pjob;
					Properties p = new Properties();
					Graphics pgraphics;
					pjob = Toolkit.getDefaultToolkit().getPrintJob(parent,
							"Print Monthly Average", p);
					JFrame f2 = new JFrame();
					if(pjob != null)
					{
						pgraphics = pjob.getGraphics();
						if(pgraphics != null)
						{
							/*
							 * image = parent.createImage(0,0);
							 * pgraphics.drawImage(image,0,0,parent); JFrame f =
							 * new JFrame(){ public void paint(Graphics gphs){
							 * gphs.drawImage(image,0,0,new Frame()); } };
							 * f.setSize(700,700); f.show();
							 */
							parent.getComponent(0).paint(pgraphics);
							// parent.paint(pgraphics);
						}
						pgraphics.dispose();
					}
					pjob.end();
				}
				/*
				 *
				 * Style s = null; StyledDocument _doc =
				 * (StyledDocument)tp.getDocument(); try { s =
				 * _doc.getStyle("main"); StyleConstants.setFontSize(s,7); Style
				 * dateStyle = _doc.getStyle("date style"); if ( dateStyle !=
				 * null ) StyleConstants.setFontSize(dateStyle,5);
				 *
				 * Frame fr = JOptionPane.getFrameForComponent(tp); Toolkit dtk
				 * = Toolkit.getDefaultToolkit(); PrintJob pjob =
				 * dtk.getPrintJob(fr,"Print Dialog",null); if ( pjob != null ){
				 * Graphics pg = pjob.getGraphics(); if (pg!=null) {
				 * tp.paint(pg); pg.dispose(); // flush page } pjob.end(); }
				 * else { try { throw new
				 * RuntimeException("No print job available!!");
				 * }catch(Exception e){ VistaUtils.displayException(null,e); } }
				 * }catch(Exception e){ VistaUtils.displayException(tp,e); }
				 * finally{ if ( s != null){ s = _doc.getStyle("main");
				 * StyleConstants.setFontSize(s,12); Style dateStyle =
				 * _doc.getStyle("date style"); if ( dateStyle != null )
				 * StyleConstants.setFontSize(dateStyle,5); } } }
				 */
			});
			printMenu.add(printItem);
			bar.add(printMenu);
			frame.setJMenuBar(bar);
			JScrollPane jsp = new JScrollPane(tp);
			frame.getContentPane().add(jsp);
			// */
			// frame.setJMenuBar(td.getJMenuBar());
			// frame.getContentPane().add(td);
			frame.setSize(840, 840);
			frame.show();
		}
	}

	/**
	 * unexecutes command or throws exception if not unexecutable
	 */
	public void unexecute() throws ExecutionException
	{
		throw new ExecutionException("Cannot undo tabulation of data");
	}

	/**
	 * checks if command is executable.
	 */
	public boolean isUnexecutable()
	{
		return false;
	}

	/**
	 * writes to script
	 */
	public void toScript(StringBuffer buf)
	{
	}
} // end of TabulateDataCommand
