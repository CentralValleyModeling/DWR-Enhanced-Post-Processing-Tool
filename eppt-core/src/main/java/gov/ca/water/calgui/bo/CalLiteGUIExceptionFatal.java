/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */

package gov.ca.water.calgui.bo;

public class CalLiteGUIExceptionFatal extends CalLiteGUIException
{
    public CalLiteGUIExceptionFatal(String message)
    {
        super(message);
    }

    public CalLiteGUIExceptionFatal(String message, Throwable cause)
    {
        super(message, cause);
    }
}
