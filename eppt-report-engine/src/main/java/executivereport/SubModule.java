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

package executivereport;import java.util.ArrayList;
import java.util.List;

public class SubModule
{
    private final List<FlagViolation> _violations = new ArrayList<>();

    private final FlagType _flagValue;
    private final List<String> _linkedRecords;
    private final String _name;

    public enum FlagType
    {
        ZERO,
        ONE,
        TWO,
        NEGATIVE_INFINITY,
        OTHER
    }

    public SubModule(String name, FlagType flagValue)
    {
        _name = name;
        _flagValue = flagValue;
        _linkedRecords = new ArrayList<>();
    }

    public void addLinkedRecord(String recordName)
    {
        _linkedRecords.add(recordName);
    }
    public String getName()
    {
        return _name;
    }

    public FlagType getFlagValue()
    {
        return _flagValue;
    }

    public List<String> getLinkedRecords()
    {
        return _linkedRecords;
    }

    public void addAllViolations(List<FlagViolation> violations)
    {
        _violations.addAll(violations);
    }
    public List<FlagViolation> getViolations()
    {
        return _violations;
    }

}
