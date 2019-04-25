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

package gov.ca.water.reportengine.executivereport;
import java.util.ArrayList;
import java.util.List;

public class Module
{

    private final String _modName;

    private List<SubModule> _subModules = new ArrayList<>();

    public Module(String modName)
    {
        _modName = modName;
    }

    public String getName()
    {
        return _modName;
    }

    public void addSubModule(SubModule subMod)
    {
        _subModules.add(subMod);
    }

    public List<SubModule> getSubModules()
    {
        return _subModules;
    }




}
