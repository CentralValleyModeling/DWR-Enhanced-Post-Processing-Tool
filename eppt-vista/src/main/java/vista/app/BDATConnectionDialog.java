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

import java.awt.GridLayout;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.Date;
import java.util.Properties;
import javax.swing.*;

import vista.db.jdbc.bdat.BDATConnection;

@SuppressWarnings("serial")
public class BDATConnectionDialog
{
	private JTextField userField;
	private JPasswordField passwordField;
	private JTextField serverField;
	private JTextField portField;
	private JTextField databaseField;
	private int confirmDialogReturnValue;

	public BDATConnectionDialog()
	{
		JPanel mainPanel = new JPanel();
		mainPanel.setLayout(new GridLayout(5, 2, 1, 3));
		mainPanel.add(new JTextField("username:"));
		mainPanel.add(userField = new JTextField(10));
		mainPanel.add(new JTextField("password:"));
		mainPanel.add(passwordField = new JPasswordField(10));
		mainPanel.add(new JTextField("server:"));
		mainPanel.add(serverField = new JTextField(10));
		mainPanel.add(new JTextField("port:"));
		mainPanel.add(portField = new JTextField(10));
		mainPanel.add(new JTextField("database:"));
		mainPanel.add(databaseField = new JTextField(10));
		//
		initializeFields();
		confirmDialogReturnValue = JOptionPane.showConfirmDialog(null,
				mainPanel, "Configure connection: ",
				JOptionPane.OK_CANCEL_OPTION);
	}

	private void initializeFields()
	{
		// jdbc:oracle:thin:@grsbldbe00308.np.water.ca.gov:1522:orcl
		Properties props = new Properties();
		String propsFile = getPropsFilename();
		try
		{
			props.load(new FileInputStream(propsFile));
		}
		catch(Exception ex)
		{
			System.err.println("Could not load connection info from "
					+ propsFile + " ; using defaults");
		}
		userField.setText(props.getProperty("db.user"));
		serverField.setText(props.getProperty("db.servername"));
		portField.setText(props.getProperty("db.portnumber"));
		databaseField.setText(props.getProperty("db.databaseName"));
	}

	public BDATConnection getConnection()
	{
		if(confirmDialogReturnValue == 0)
		{
			Properties props = new Properties();
			props.setProperty("db.user", userField.getText());
			props.setProperty("db.servername", serverField.getText());
			props.setProperty("db.portnumber", portField.getText());
			props.setProperty("db.databaseName", databaseField.getText());
			try
			{
				FileOutputStream fos = new FileOutputStream(getPropsFilename());
				props.store(fos, "" + new Date());
				fos.close();
			}
			catch(FileNotFoundException e)
			{
			}
			catch(IOException e)
			{
			}
			// don't store password so added after save
			props.setProperty("db.password", new String(passwordField
					.getPassword()));
			return new BDATConnection(props);
		}
		else
		{
			return null;
		}
	}

	private String getPropsFilename()
	{
		return System.getProperty("user.home")
				+ System.getProperty("file.separator")
				+ "vista.jdbc.properties";
	}
}
