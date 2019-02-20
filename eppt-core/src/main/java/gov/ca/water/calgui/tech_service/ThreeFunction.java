/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */

package gov.ca.water.calgui.tech_service;

import gov.ca.water.calgui.bo.CalLiteGUIException;

/**
 * This is a functional Interface which takes three parameters and one return
 * type. This interface is used for passing the lambda function
 *
 * @param <A> 1st parameter
 * @param <B> 2nd parameter
 * @param <C> 3ed parameter
 * @param <R> return type
 * @author Mohan
 */
@FunctionalInterface
public interface ThreeFunction<A, B, C, R>
{
	R apply(A a, B b, C c) throws CalLiteGUIException;
}
