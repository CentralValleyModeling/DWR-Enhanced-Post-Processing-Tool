/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */

package gov.ca.water.calgui.bo;
//! Root exception

/**
 * I am the root exception for CalLiteGUI.
 *
 * @author Mohan
 */
public class CalLiteGUIException extends Exception
{

    private static final long serialVersionUID = 1L;

    /**
     * Instantiates a new CalLiteGUI exception.
     */
    public CalLiteGUIException()
    {
        super();
    }

    /**
     * The Constructor.
     *
     * @param message The message to store in the exception.
     */
    public CalLiteGUIException(String message)
    {
        super(message);
    }

    /**
     * The Constructor.
     *
     * @param message The message
     * @param cause   The cause
     */
    public CalLiteGUIException(String message, Throwable cause)
    {
        super(message, cause);
    }


    /**
     * The Constructor.
     *
     * @param cause the cause
     */
    public CalLiteGUIException(Throwable cause)
    {
        super(cause);
    }


}
