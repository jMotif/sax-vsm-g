package net.seninp.saxvsm.direct;

import java.util.List;
import java.util.Map;
import net.seninp.saxvsm.util.UCRUtils;

/**
 * Prints dataset statistics.
 * 
 * @author psenin
 * 
 */
public class SAXVSMDatasetStatsPrinter {

  /**
   * Main runnable.
   * 
   * @param args
   * @throws Exception
   */
  public static void main(String[] args) throws Exception {

    // odd a bit, but whatever, it works
    if (2 == args.length) {

      // working on train data
      String TRAINING_DATA = args[0];
      Map<String, List<double[]>> trainData = UCRUtils.readUCRData(TRAINING_DATA);
      System.out.println(UCRUtils.datasetStats(trainData, "trainData").replace(";", "\n   "));

      String TEST_DATA = args[1];
      Map<String, List<double[]>> testData = UCRUtils.readUCRData(TEST_DATA);
      System.out.println(UCRUtils.datasetStats(testData, "testData").replace(";", "\n   "));

    }
    else {
      System.exit(-10);
    }
  }

}
