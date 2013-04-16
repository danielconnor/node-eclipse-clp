//Title:        Java/ECLiPSe interface
//Version:      $Id: QuickTest.java,v 1.5 2001/08/29 13:50:07 josh Exp $
//Copyright:    Copyright (c) 2001
//Author:       Josh Singer
//Company:      Parc Technologies
//Description:  Java/ECLiPSe Interface example Java program
import com.parctechnologies.eclipse.*;
import java.io.*;

public class QuickTest
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

    // Write a message
    eclipse.rpc("write(output, 'hello world'), flush(output)");

    // Destroy the Eclipse process
    ((EmbeddedEclipse) eclipse).destroy();
  }
}
