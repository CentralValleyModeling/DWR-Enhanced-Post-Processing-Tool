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

package calsim.schematic.input;

import java.awt.Container;
import java.awt.GridLayout;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import javax.swing.*;

//import vista.gui.*;

/* Author: Yan-Ping Zuo  Date 12/99 */
public class TestDataSourceUI extends JFrame
{
	public TestDataSourceUI()
	{
		super("Data Source UI");
		DataSource ds1 = new DataSource();
		DataSource ds2 = new DataSource("LOOKUP");
		//System.out.println("ds1=" + ds1.toString());
		//System.out.println("ds2=" + ds2.toString());
		JPanel ui1 = new DataSourceUI(ds1);
		JPanel ui2 = new DataSourceUI(ds2);
		//System.out.println("ui1=" + ui1.toString());
		//System.out.println("ui2=" + ui2.toString());
		//    double num1 = ds1.getNumber();
		Container pane = getContentPane();
		pane.setLayout(new GridLayout(2, 1));
		pane.add(ui1);
		pane.add(ui2);
    /*    try {
      ds2.getNumber();
    }catch(Exception e){
      VistaUtils.displayException(this,e);
      } */
		addWindowListener(new WindowAdapter()
		{
			public void windowClosing(WindowEvent evt)
			{
				System.exit(0);
			}
		});
		setSize(600, 400);
		setVisible(true);
		setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE);
	}

	/**
	 * the main method
	 */
	public static void main(String[] args)
	{
		new TestDataSourceUI();
	}
}

