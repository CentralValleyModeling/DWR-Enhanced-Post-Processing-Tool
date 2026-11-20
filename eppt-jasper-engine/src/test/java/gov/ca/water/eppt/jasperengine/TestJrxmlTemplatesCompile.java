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

package gov.ca.water.eppt.jasperengine;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import net.sf.jasperreports.engine.JasperCompileManager;
import net.sf.jasperreports.engine.design.JasperDesign;
import net.sf.jasperreports.engine.xml.JRXmlLoader;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Loads and compiles every .jrxml template that the installer ships (eppt-nbui/src/main/jasper) with the bundled
 * JasperReports library. A JasperReports version that cannot read the templates (see GH issue #267) fails here
 * instead of at runtime in the QA/QC report.
 * <p>
 * Tagged "slow" (about 40s), so it is skipped by default. Run with {@code -DexcludedTestGroups=none}; CI does.
 */
@Tag("slow")
class TestJrxmlTemplatesCompile
{
	private static final Path TEMPLATE_DIR = Paths.get("..", "eppt-nbui", "src", "main", "jasper");

	@Test
	void testAllTemplatesLoadAndCompile() throws IOException
	{
		Path templateDir = TEMPLATE_DIR.toAbsolutePath().normalize();
		assertTrue(Files.isDirectory(templateDir), "Template directory not found: " + templateDir);
		List<Path> templates;
		try(Stream<Path> paths = Files.walk(templateDir))
		{
			templates = paths.filter(Files::isRegularFile)
							 .filter(p -> p.getFileName().toString().endsWith(".jrxml"))
							 .sorted()
							 .collect(Collectors.toList());
		}
		assertFalse(templates.isEmpty(), "No .jrxml templates found under " + templateDir);

		// Templates are independent, so compile them in parallel to keep the test fast
		List<String> failures = Collections.synchronizedList(new ArrayList<>());
		templates.parallelStream().forEach(template ->
		{
			try
			{
				JasperDesign design = JRXmlLoader.load(template.toFile());
				JasperCompileManager.compileReport(design);
			}
			catch(Exception | LinkageError e)
			{
				failures.add(templateDir.relativize(template) + " -> " + rootCause(e));
			}
		});
		Collections.sort(failures);
		assertTrue(failures.isEmpty(), failures.size() + " of " + templates.size() + " templates failed to compile:\n"
				+ String.join("\n", failures));
	}

	private static String rootCause(Throwable t)
	{
		Throwable root = t;
		while(root.getCause() != null && root.getCause() != root)
		{
			root = root.getCause();
		}
		return root.getClass().getSimpleName() + ": " + root.getMessage();
	}
}
