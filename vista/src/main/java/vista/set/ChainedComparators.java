package vista.set;

import java.util.Comparator;

public class ChainedComparators implements Comparator<DataReference>{

	private Comparator<DataReference> primaryComparator;
	private Comparator<DataReference> secondaryComparator;
	
	public ChainedComparators(Comparator<DataReference> primaryComparator, Comparator<DataReference> secondaryComparator){
		this.primaryComparator = primaryComparator;		
		this.secondaryComparator = secondaryComparator;
	}
	@Override
	public int compare(DataReference o1, DataReference o2) {
		int value = primaryComparator.compare(o1, o2);
		if (value == 0){
			return secondaryComparator.compare(o1, o2);
		} else {
			return value;
		}
	}

}
