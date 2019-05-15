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
