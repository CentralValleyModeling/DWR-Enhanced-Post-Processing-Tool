/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package vista.graph;

import java.util.ArrayList;

/**
 * A non-thread based class for efficiency. This is not thread safe but it is
 * fairly small and fast.
 * 
 * @author Nicky Sandhu
 * @version $Id: AnimationObservable.java,v 1.1 2003/10/02 20:48:47 redwood Exp
 *          $
 */
public class AnimationObservable {
	/**
	 * initializes an observable with no observers
	 */
	public AnimationObservable() {
		observers = new ArrayList<AnimationObserver>();
	}

	/**
	 * adds an observer to be notified
	 */
	public void addObserver(AnimationObserver observer) {
		observers.add(observer);
	}

	/**
	 * removes an observer
	 */
	public void removeObserver(AnimationObserver observer) {
		observers.remove(observer);
	}

	/**
	 * notifies the observers and sends messages in the Object args
	 */
	public void notifyAll(Object args) {
		for(AnimationObserver observer: observers){
			observer.update(this, args);
		}
	}

	private ArrayList<AnimationObserver> observers;
}
