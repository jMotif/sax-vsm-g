package edu.hawaii.jmotif.direct;

import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Map.Entry;
import net.seninp.jmotif.sax.NumerosityReductionStrategy;
import net.seninp.jmotif.sax.alphabet.NormalAlphabet;
import org.slf4j.LoggerFactory;
import ch.qos.logback.classic.Level;
import ch.qos.logback.classic.Logger;
import edu.hawaii.jmotif.text.TextUtils;
import edu.hawaii.jmotif.text.WordBag;
import edu.hawaii.jmotif.util.UCRUtils;

/**
 * This implements a classifier.
 * 
 * @author psenin
 * 
 */
public class SAXVSMClassifier {

  private static final DecimalFormatSymbols otherSymbols = new DecimalFormatSymbols(Locale.US);
  private static DecimalFormat fmt = new DecimalFormat("0.00###", otherSymbols);

  private static String TRAINING_DATA;
  private static String TEST_DATA;
  private static Map<String, List<double[]>> trainData;
  private static Map<String, List<double[]>> testData;

  private static Integer WINDOW_SIZE;
  private static Integer PAA_SIZE;
  private static Integer ALPHABET_SIZE;
  private static NumerosityReductionStrategy STRATEGY;

  private static final double DEFAULT_NORMALIZATION_THRESHOLD = 0.05;
  private static double NORMALIZATION_THRESHOLD = DEFAULT_NORMALIZATION_THRESHOLD;

  // static block - we instantiate the logger
  //
  private static final Logger consoleLogger;
  private static final Level LOGGING_LEVEL = Level.INFO;
  static {
    consoleLogger = (Logger) LoggerFactory.getLogger(SAXVSMClassifier.class);
    consoleLogger.setLevel(LOGGING_LEVEL);
  }

  /**
   * Main CLI runnable.
   * 
   * @param args the command line args.
   * @throws Exception if error occurs.
   */
  public static void main(String[] args) throws Exception {

    try {
      // args: <train dataset>, <test dataset>, Wsize , Psize, Asize, Startegy
      consoleLogger.info("processing parameters: " + Arrays.toString(args));

      WINDOW_SIZE = Integer.valueOf(args[2]);
      PAA_SIZE = Integer.valueOf(args[3]);
      ALPHABET_SIZE = Integer.valueOf(args[4]);

      STRATEGY = NumerosityReductionStrategy.valueOf(args[5].toUpperCase());

      if (args.length > 6) {
        NORMALIZATION_THRESHOLD = Double.valueOf(args[6]);
      }

      TRAINING_DATA = args[0];
      TEST_DATA = args[1];
      trainData = UCRUtils.readUCRData(TRAINING_DATA);
      consoleLogger.info("trainData classes: " + trainData.size() + ", series length: "
          + trainData.entrySet().iterator().next().getValue().get(0).length);
      for (Entry<String, List<double[]>> e : trainData.entrySet()) {
        consoleLogger.info(" training class: " + e.getKey() + " series: " + e.getValue().size());
      }

      testData = UCRUtils.readUCRData(TEST_DATA);
      consoleLogger.info("testData classes: " + testData.size() + ", series length: "
          + testData.entrySet().iterator().next().getValue().get(0).length);
      for (Entry<String, List<double[]>> e : testData.entrySet()) {
        consoleLogger.info(" test class: " + e.getKey() + " series: " + e.getValue().size());
      }

    }
    catch (Exception e) {
      System.err.println("There was parameters error....");
      System.exit(-10);
    }

    NormalAlphabet na = new NormalAlphabet();
    TextUtils tu = new TextUtils();

    List<WordBag> bags = tu.labeledSeries2WordBags(trainData, WINDOW_SIZE, PAA_SIZE,
        na.getCuts(ALPHABET_SIZE), STRATEGY, NORMALIZATION_THRESHOLD);

    HashMap<String, HashMap<String, Double>> tfidf = tu.computeTFIDF(bags);

    // classifying
    int testSampleSize = 0;
    int positiveTestCounter = 0;
    for (String label : tfidf.keySet()) {
      List<double[]> testD = testData.get(label);
      for (double[] series : testD) {
        positiveTestCounter = positiveTestCounter
            + tu.classify(label, series, tfidf, WINDOW_SIZE, PAA_SIZE, na.getCuts(ALPHABET_SIZE),
                STRATEGY, NORMALIZATION_THRESHOLD);
        testSampleSize++;
      }
    }

    // accuracy and error
    double accuracy = (double) positiveTestCounter / (double) testSampleSize;
    double error = 1.0d - accuracy;

    // report results
    consoleLogger.info("classification results: accuracy " + fmt.format(accuracy) + ", error "
        + fmt.format(error));

  }

}
