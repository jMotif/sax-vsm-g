package net.seninp.saxvsm.util;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

/**
 * Converts the UCR format to Josif's data set format.
 * 
 * @author psenin
 * 
 */
public class Converter {

  private static final String SPACE = " ";

  private static final String IN_FILENAME = "/home/psenin/workspace-seninp/JMLToolkit/data/class_2.txt";
  private static final String OUT_FILENAME = "/home/psenin/workspace-seninp/JMLToolkit/data/GP2_TRAIN";

  public static void main(String[] args) throws IOException {
    StringBuffer sb = new StringBuffer(1024 * 512);
    Map<String, List<double[]>> trainData = readUCRData(IN_FILENAME);
    int globalIncrement = 0;
    for (Entry<String, List<double[]>> e : trainData.entrySet()) {
      String className = e.getKey();
      for (int i = 0; i < e.getValue().size(); i++) {
        double[] series = e.getValue().get(i);
        sb.append(String.valueOf(globalIncrement + i)).append(SPACE);
        sb.append(className).append(SPACE);
        sb.append("0").append(SPACE);
        sb.append(series.length).append(SPACE);
        sb.append(Arrays.toString(series).replaceAll("[\\,\\[\\]]", " ").replaceAll("\\s+ ", " "))
            .append("\n");
      }
      globalIncrement = e.getValue().size();
    }
    BufferedWriter bw = new BufferedWriter(new FileWriter(new File(OUT_FILENAME)));
    bw.write(sb.toString());
    bw.close();

  }

  /**
   * Reads bunch of series from file. First column treats as a class label. Rest as a real-valued
   * series.
   * 
   * @param fileName
   * @return
   * @throws IOException
   */
  public static Map<String, List<double[]>> readUCRData(String fileName) throws IOException {

    Map<String, List<double[]>> res = new HashMap<String, List<double[]>>();

    BufferedReader br = new BufferedReader(new FileReader(new File(fileName)));
    String line = "";
    while ((line = br.readLine()) != null) {
      if (line.trim().length() == 0) {
        continue;
      }
      String[] split = line.trim().split("[\\,\\s]+");

      String label = split[0];
      Double num = parseValue(label);
      String seriesLabel = label;
      if (!(Double.isNaN(num))) {
        seriesLabel = String.valueOf(num.intValue());
      }
      double[] series = new double[split.length - 1];
      for (int i = 1; i < split.length; i++) {
        series[i - 1] = Double.valueOf(split[i].trim()).doubleValue();
      }

      if (!res.containsKey(seriesLabel)) {
        res.put(seriesLabel, new ArrayList<double[]>());
      }

      res.get(seriesLabel).add(series);
    }

    br.close();
    return res;

  }

  private static Double parseValue(String string) {
    Double res = Double.NaN;
    try {
      Double r = Double.valueOf(string);
      res = r;
    }
    catch (NumberFormatException e) {
      assert true;
    }
    return res;
  }

}
