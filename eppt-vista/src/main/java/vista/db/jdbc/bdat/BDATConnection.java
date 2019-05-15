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

package vista.db.jdbc.bdat;

import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;
import java.sql.Connection;
import java.sql.SQLException;
import java.util.Properties;
import javax.swing.*;

import oracle.jdbc.pool.OracleDataSource;

public class BDATConnection{
	
	private String user;
	private String serverName;
	private int portNumber;
	private String password;
	private String databaseName;

	public BDATConnection(Properties props){
		user = props.getProperty("db.user","user");
		password = props.getProperty("db.password",null);
		serverName = props.getProperty("db.servername","server");
		portNumber = Integer.parseInt(props.getProperty("db.portnumber","1000"));
		databaseName = props.getProperty("db.databaseName","orcl");
	}
	
	public Connection getConnection() throws SQLException{
		OracleDataSource ds = new OracleDataSource();
		ds.setUser(user);
		if (password==null){
			password=showPasswordDialog();
			if (password==null){
				return null;
			}
		}
		ds.setPassword(password);
		ds.setServerName(serverName);
		ds.setPortNumber(portNumber);
		ds.setDatabaseName(databaseName);
		ds.setURL("jdbc:oracle:thin:@"+serverName+":"+portNumber+":"+databaseName);
		return ds.getConnection();
	}

	private String showPasswordDialog() {
		final JPasswordField jpf = new JPasswordField();
		JOptionPane jop = new JOptionPane(jpf,
		  JOptionPane.QUESTION_MESSAGE,
		  JOptionPane.OK_CANCEL_OPTION);
		JDialog dialog = jop.createDialog("Password:");
		dialog.addComponentListener(new ComponentAdapter(){
		  @Override
		  public void componentShown(ComponentEvent e){
		    SwingUtilities.invokeLater(new Runnable(){
		      @Override
		      public void run(){
		        jpf.requestFocusInWindow();
		      }
		    });
		  }
		});
		dialog.setVisible(true);
		int result = (Integer)jop.getValue();
		dialog.dispose();
		char[] password = null;
		if(result == JOptionPane.OK_OPTION){
		  password = jpf.getPassword();
		}
		return new String(password);
	}

	public void setPassword(String password) {
		this.password = password;
	}

	public String getPassword() {
		return password;
	}

	public String getUser() {
		return user;
	}

	public void setUser(String user) {
		this.user = user;
	}

	public String getServerName() {
		return serverName;
	}

	public void setServerName(String serverName) {
		this.serverName = serverName;
	}

	public int getPortNumber() {
		return portNumber;
	}

	public void setPortNumber(int portNumber) {
		this.portNumber = portNumber;
	}

	public String getDatabaseName() {
		return databaseName;
	}

	public void setDatabaseName(String databaseName) {
		this.databaseName = databaseName;
	}
}
