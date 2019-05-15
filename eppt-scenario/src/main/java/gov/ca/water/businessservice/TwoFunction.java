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

package gov.ca.water.businessservice;

import gov.ca.water.calgui.bo.CalLiteGUIException;

/**
 * This is a functional Interface which takes three parameters and one return
 * type. This interface is used for passing the lambda function
 *
 * @param <A> 1st parameter
 * @param <B> 2nd parameter
 * @param <R> return type
 * @author Mohan
 */
@FunctionalInterface
public interface TwoFunction<A, B, R>
{
	R apply(A a, B b) throws CalLiteGUIException;
}
