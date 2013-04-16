//Title:        Java/ECLiPSe interface
//Version:      $Id: DataExample2.java,v 1.4 2001/08/29 13:50:06 josh Exp $
//Copyright:    Copyright (c) 2001
//Author:       Josh Singer
//Company:      Parc Technologies
//Description:  Java/ECLiPSe Interface example Java program
import com.parctechnologies.eclipse.*;
import java.io.*;
import java.util.*;

public class DataExample2
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

    // Construct a collection
    Collection a_collection = construct_collection();

    // Write out the collection
    System.out.println(a_collection);

    // Get Eclipse to write the collection (a list) to stdout and flush
    eclipse.rpc(
		new CompoundTermImpl(",",
			      new CompoundTermImpl("write",
					    new Atom("output"),a_collection),
			      new CompoundTermImpl("flush", new Atom("output"))
			      )
		);

    // Destroy the Eclipse process
    ((EmbeddedEclipse) eclipse).destroy();

  }

  // Construct a collection in Java to represent the Eclipse
  // list [1, foo(3.5), bar].
  private static Collection construct_collection()
  {
      Collection theCollection = new LinkedList();

      theCollection.add(new Integer(1));
      theCollection.add(new CompoundTermImpl("foo", new Double(3.5)));
      theCollection.add(new Atom("bar"));

      return(theCollection);
  }

}

