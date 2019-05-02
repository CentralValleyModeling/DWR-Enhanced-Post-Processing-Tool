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

package gov.ca.water.reportengine.filechanges;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;

public class CodeChangesType
{

    private final String _name;
    private final List<CodeChangesSubType> _subTypes = new ArrayList<>();

    public CodeChangesType(String name)
    {
        _name = name;
    }

    public void addSubtype(CodeChangesSubType subType)
    {
        _subTypes.add(subType);
    }

    public List<CodeChangesSubType> getSubTypes()
    {
        return _subTypes;
    }

    public String getName()
    {
        return _name;
    }
}
