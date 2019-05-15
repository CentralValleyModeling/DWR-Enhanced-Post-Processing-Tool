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

package vista.set;

import java.util.ArrayList;
import java.util.List;

public class CollectionUtils {
	/**
	 * Filters a list, returning a copy of the elements for which the predicate apply
	 * returns true. If selecting then only matches are kept, else only matches are removed and
	 * the remaining elements returned.
	 * @param <T>
	 * @param list
	 * @param predicate
	 * @param selecting
	 * @return
	 */
	public static <T> List<T> filter(List<T> list, Predicate<T> predicate, boolean selecting){
		List<T> filtered = new ArrayList<T>();
		if (!selecting){
			filtered.addAll(list);
		}
		for(T t: list){
			if (predicate.apply(t)){
				if (selecting){
					filtered.add(t);
				} else {
					filtered.remove(t);
				}
			}
		}
		return filtered;
	}
}
