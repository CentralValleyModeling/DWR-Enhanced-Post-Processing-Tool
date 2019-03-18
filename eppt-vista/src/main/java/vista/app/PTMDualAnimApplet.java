/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package vista.app;

import java.awt.BorderLayout;
import java.awt.Button;
import java.awt.Container;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.*;

/**
 * @author Nicky Sandhu
 * @version $Id: PTMDualAnimApplet.java,v 1.2 2000/08/03 17:07:30 miller Exp $
 */
public class PTMDualAnimApplet extends JApplet
{
	/**
	 * The study name to be displayed is in the _studies array with
	 * the corresponding animation file in the _animFiles array.
	 */
	String[] _animFiles;
	String[] _studies;
	JComboBox _cbox;
	String strDelim, strPath;

	/**
	 *
	 */
	public void init()
	{
		//  public PTMDualAnimApplet(){
		String str;
		str = getParameter("delimitate");
		strDelim = (str != null) ? str : ";";

		str = getParameter("path");
		String strPath = str;

		str = getParameter("studies");
		String strStudies = str;
		_studies = extractArray(strStudies);

		str = getParameter("propsfiles");
		String strProps = str;
		_animFiles = extractArray(strPath, strProps);

		_cbox = new JComboBox(_studies);
		Button mainButton = new Button("Press here to start");
		mainButton.addActionListener(new DisplayGUI());
		Container mainPane = getContentPane();
		mainPane.setLayout(new BorderLayout());
		mainPane.add(_cbox, BorderLayout.NORTH);
		mainPane.add(mainButton, BorderLayout.SOUTH);
		setVisible(true);
	}

	private String[] extractArray(String str)
	{
		return extractArray("", str);
	}

	private String[] extractArray(String preStr, String str)
	{
		int cnt = 0;
		int indx1 = 0, indx2 = 0;
		//count number of sections
		do
		{
			indx2 = str.indexOf(strDelim, indx1);
			indx1 = indx2 + 1;
			cnt = cnt + 1;
		} while(indx2 != -1);

		indx1 = 0;
		indx2 = 0;
		//size array
		//    System.out.println("cnt "+cnt);
		String[] strArray = new String[cnt];
		for(int i = 0; i < strArray.length; i++)
		{
			indx2 = str.indexOf(strDelim, indx1);
			if(indx2 > 0)
			{
				strArray[i] = preStr + str.substring(indx1, indx2);
			}
			else
			{
				strArray[i] = preStr + str.substring(indx1);
			}
			//      System.out.println(strArray[i]);
			indx1 = indx2 + 1;
		}

		return strArray;
	}

	/**
	 * @author Nicky Sandhu
	 * @version $Id: PTMDualAnimApplet.java,v 1.2 2000/08/03 17:07:30 miller Exp $
	 */
	class DisplayGUI implements ActionListener
	{
		/**
		 *
		 */
		public void actionPerformed(ActionEvent evt)
		{
			new PTMDualAnimator(_animFiles[_cbox.getSelectedIndex()]);
		}
	}// end of DisplayGUI class
}
