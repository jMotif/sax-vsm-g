package edu.hawaii.jmotif.cbf;

import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

/**
 * This code I used to asses generator correctness through R plotting.
 * 
 * @author psenin
 * 
 */
public class TestCBFGenerator {

  /** The timeseries length. */
  private static final int SERIES_LENGTH = 128;

  /**
   * @param args
   * @throws IOException
   */
  public static void main(String[] args) throws IOException {

    // ticks
    int[] t = new int[SERIES_LENGTH];
    for (int i = 0; i < SERIES_LENGTH; i++) {
      t[i] = i;
    }

    // cylinder sample
    List<double[]> cylinders = new ArrayList<double[]>();
    for (int i = 0; i < 3; i++) {
      cylinders.add(CBFGenerator.cylinder(t));
    }
    save("RCode/test/cylinder.csv", "cylinder", cylinders);

    // bell sample
    List<double[]> bells = new ArrayList<double[]>();
    for (int i = 0; i < 3; i++) {
      bells.add(CBFGenerator.bell(t));
    }
    save("RCode/test/bell.csv", "bell", bells);

    // funnel sample
    List<double[]> funnels = new ArrayList<double[]>();
    for (int i = 0; i < 3; i++) {
      funnels.add(CBFGenerator.funnel(t));
    }
    save("RCode/test/funnel.csv", "funnel", funnels);

  }

  private static void save(String fname, String prefix, List<double[]> data) throws IOException {
    BufferedWriter bw = new BufferedWriter(new FileWriter(fname));
    bw.write("x," + prefix + String.valueOf(0) + "," + prefix + String.valueOf(1) + "," + prefix
        + String.valueOf(2) + "\n");
    for (int i = 0; i < SERIES_LENGTH; i++) {
      bw.write(String.valueOf(i) + "," + data.get(0)[i] + "," + data.get(1)[i] + ","
          + data.get(2)[i] + "\n");
    }
    bw.close();
  }
}
