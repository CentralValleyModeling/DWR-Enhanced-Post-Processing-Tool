/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */

package gov.ca.water.calgui;

public class EpptInitializationException extends Exception
{


    public EpptInitializationException(String message )
    {
        super(message);
    }

    public EpptInitializationException(String message, Throwable cause)
    {
        super(message, cause);
    }

}
