//Title:        Java/ECLiPSe interface
//Version:      $Id: DataExample1.java,v 1.4 2001/08/29 13:50:06 josh Exp $
//Copyright:    Copyright (c) 2001
//Author:       Josh Singer
//Company:      Parc Technologies
//Description:  Java/ECLiPSe Interface example Java program
import com.parctechnologies.eclipse.*;
import java.io.*;

public class DataExample1
{
  public static void main(String[] args) throws Exception
  {
    // Create some default Eclipse options
    EclipseEngineOptions eclipseEngineOptions = new EclipseEngineOptions();

    // Object representing the Eclipse process
    EclipseEngine eclipse;

    // Connect the Eclipse's standard streams to the JVM's
    eclipseEngineOptions.setUseQueues(false);

    // Initialise Eclipse
    eclipse = EmbeddedEclipse.getInstance(eclipseEngineOptions);

    // Construct a term
    CompoundTerm a_term = construct_term();

    // Get Eclipse to write the term to stdout and flush
    eclipse.rpc(
		new CompoundTermImpl(",",
			      new CompoundTermImpl("write",
					    new Atom("output"), a_term),
			      new CompoundTermImpl("flush", new Atom("output"))
			      )
		);

    // Destroy the Eclipse
    ((EmbeddedEclipse) eclipse).destroy();
  }

  // Construct a term in Java to represent the Eclipse term foo(a, b, 3).
  private static CompoundTerm construct_term()
  {
    Atom a = new Atom("a");
    Atom b = new Atom("b");
    Integer numberThree = new Integer(3);
    CompoundTerm theTerm = new CompoundTermImpl("foo", a, b, numberThree);

    return(theTerm);
  }

}

