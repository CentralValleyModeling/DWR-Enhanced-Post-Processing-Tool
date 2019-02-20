package vista.set;
/**
 * Primarily for defining a filtering criteria for a collection
 * @author psandhu
 * @param <T>
 */
public interface Predicate<T> {
	/**
	 *
	 * @param type
	 * @return true or false if the type is to be accepted or rejected in a collections.
	 */
	boolean apply(T type);
}
