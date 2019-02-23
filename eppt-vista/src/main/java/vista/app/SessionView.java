/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package vista.app;

import javax.swing.*;

/**
 * Displays session as a list of groups. An .dss files are defaulted to group
 * representations with name<.dss> modifier attached to the end of name. Thus a
 * xyz.dss file would default to group xyz.
 * 
 * @author Nicky Sandhu
 * @version $Id: SessionView.java,v 1.1 2003/10/02 20:48:41 redwood Exp $
 */
public abstract class SessionView extends JPanel implements View {
	/**
	 * views onto the session in the context
	 */
	protected SessionView() {
	}

	/**
	 * gest the context of the data
	 */
	public SessionContext getContext() {
		return MainGUI.getContext();
	}

	/**
	 * get menu associated with this session view
	 */
	protected abstract JMenu getMenu();

	/**
	 * update information panel...
	 */
	protected abstract void updateInfoPanel();

	/**
	 * create the view...
	 */
	protected abstract void createView();
}
