/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package vista.gui;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.lang.reflect.Method;

/**
 * A action listener which uses reflection to invoke a callback on the given
 * method in given object's class.
 * 
 * @author Nicky Sandhu
 * @version $Id: GenericActionAdapter.java,v 1.1 2003/10/02 20:49:15 redwood Exp
 *          $
 */
public class GenericActionAdapter implements ActionListener {
	/**
   *
   */
	public GenericActionAdapter(Object obj, String methodName)
			throws InstantiationException {
		try {
			_callbackObject = obj;
			Class callbackClass = obj.getClass();
			Class[] parameters = { ActionEvent.class };
			_callbackMethod = callbackClass.getMethod(methodName, parameters);
		} catch (Exception e) {
			e.printStackTrace();
			throw new InstantiationException(
					"Could not generate action adapter for "
							+ obj.getClass().getName() + "." + methodName);
		}
	}

	/**
	 * Invoked when an action occurs.
	 */
	public void actionPerformed(ActionEvent evt) {
		try {
			Object[] args = { evt };
			_callbackMethod.invoke(_callbackObject, args);
		} catch (Exception ex) {
			ex.printStackTrace();
			throw new RuntimeException("Exception while invoking call back "
					+ _callbackMethod.getName());
		}
	}

	/**
	 * The method to call when event is activated
	 */
	private Method _callbackMethod;
	/**
	 * The object on which the above method is invoked
	 */
	private Object _callbackObject;
}
