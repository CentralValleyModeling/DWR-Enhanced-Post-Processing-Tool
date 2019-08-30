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

package gov.ca.water.calgui.wresl;

import java.awt.Color;
import java.io.BufferedReader;
import java.io.File;
import java.io.InputStreamReader;
import java.util.List;
import java.util.stream.DoubleStream;
import javax.script.ScriptEngine;
import javax.script.ScriptEngineManager;
import javax.xml.bind.JAXBContext;
import javax.xml.bind.Marshaller;

import org.junit.jupiter.api.Test;

import rma.util.RGBColorContour;
import rma.util.TwoColorColorContour;

import static java.util.stream.Collectors.toList;
import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 08-12-2019
 */
public class TestJythonLambda
{
	private static final String JYTHON_TEST_BASE = "dssReader.getBaseDts(dtsLinkId).filter(jpInt(lambda x: x == 100)).count()";
	private static final String JYTHON_TEST_ALT = "dssReader.getAltDts(dtsLinkId).filter(jpInt(lambda x: x == 100)).count()";

	@Test
	public void testJythonLambda() throws Exception
	{
		System.setProperty("python.import.site", "false");
		ScriptEngine engine = new ScriptEngineManager().getEngineByName("python");

		DssReader dssReader = new DssReader();
		engine.put("dssReader", dssReader);
		engine.put("dtsLinkId", 102);
		try(InputStreamReader inputReader = new InputStreamReader(TestJythonLambda.class.getResourceAsStream("StreamAPI.py"));
			BufferedReader reader = new BufferedReader(inputReader))
		{
			engine.eval(reader);
		}
		engine.eval("import sys");
		engine.eval("from gov.ca.water.calgui.wresl import TestJythonLambda, DssReader");
		engine.eval("from java.util.Arrays import asList");
		Object baserRetval = engine.eval(JYTHON_TEST_BASE);
		System.out.println("Base retval: " + baserRetval);
		Object altRetval = engine.eval(JYTHON_TEST_ALT);
		System.out.println("Alt retval: " + altRetval);
	}

	@Test
	public void testGradient() throws Exception
	{
		TwoColorColorContour positiveContour = new TwoColorColorContour();
		positiveContour.setMinColor(new Color(253, 239, 160));
		positiveContour.setMaxColor(new Color(109, 192, 127));
		positiveContour.setMinValue(0.0);
		positiveContour.setMaxValue(37.0);
		TwoColorColorContour negativeContour = new TwoColorColorContour();
		negativeContour.setMaxColor(new Color(253, 239, 160));
		negativeContour.setMinColor(new Color(240, 96, 99));
		negativeContour.setMinValue(-37.0);
		negativeContour.setMaxValue(0.0);

		List<Color> collect = DoubleStream.iterate(0, i -> --i).limit(37).mapToObj(negativeContour::getColor).collect(toList());
		List<Color> positive = DoubleStream.iterate(0, i -> ++i).limit(37).mapToObj(positiveContour::getColor).collect(toList());
		JAXBContext jc = JAXBContext.newInstance(TwoColorColorContour.class);

		Marshaller marshaller = jc.createMarshaller();
		marshaller.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, true);
		marshaller.marshal(positiveContour, new File("positiveContour.xml"));
		marshaller.marshal(negativeContour, new File("negativeContour.xml"));
		assertEquals(collect.size(), positive.size());
	}

}
