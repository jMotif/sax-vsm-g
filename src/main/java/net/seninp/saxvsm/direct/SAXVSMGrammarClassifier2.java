package net.seninp.saxvsm.direct;

import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Map.Entry;
import org.slf4j.LoggerFactory;
import ch.qos.logback.classic.Level;
import ch.qos.logback.classic.Logger;
import net.seninp.jmotif.sax.NumerosityReductionStrategy;
import net.seninp.jmotif.sax.alphabet.NormalAlphabet;
import net.seninp.saxvsm.gi.repair.BagConstructionStrategy;
import net.seninp.saxvsm.gi.repair.RePairFactory;
import net.seninp.saxvsm.text.TextUtils;
import net.seninp.saxvsm.text.WordBag;
import net.seninp.saxvsm.util.UCRUtils;

/**
 * This implements a classifier.
 * 
 * @author psenin
 * 
 */
public class SAXVSMGrammarClassifier2 {

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
  private static NormalAlphabet na;
  private static TextUtils tu;
  private static BagConstructionStrategy BAG_STRATEGY;

  // static block - we instantiate the logger
  //
  private static final Logger consoleLogger;
  private static final Level LOGGING_LEVEL = Level.INFO;
  static {
    consoleLogger = (Logger) LoggerFactory.getLogger(SAXVSMGrammarClassifier2.class);
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
      // args: <train dataset>, <test dataset>, Wsize , Psize, Asize, Strategy
      consoleLogger.info("processing parameters: " + Arrays.toString(args));

      WINDOW_SIZE = Integer.valueOf(args[2]);
      PAA_SIZE = Integer.valueOf(args[3]);
      ALPHABET_SIZE = Integer.valueOf(args[4]);

      STRATEGY = NumerosityReductionStrategy.valueOf(args[5].toUpperCase());

      if (args.length > 6) {
        NORMALIZATION_THRESHOLD = Double.valueOf(args[6]);
      }

      BAG_STRATEGY = BagConstructionStrategy.valueOf(args[7].toUpperCase());

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

    na = new NormalAlphabet();
    tu = new TextUtils();

    // making training bags collection
    List<WordBag> bags = RePairFactory.labeledSeries2ConcatenatedGrammarWordBags(trainData,
        WINDOW_SIZE, PAA_SIZE, na.getCuts(ALPHABET_SIZE), STRATEGY, NORMALIZATION_THRESHOLD,
        BAG_STRATEGY);
    // getting TFIDF done
    HashMap<String, HashMap<String, Double>> tfidf = tu.computeTFIDFInstrumented(bags);
    System.out.println(tu.tfidfToTable(tfidf));
    // classifying
    int testSampleSize = 0;
    int positiveTestCounter = 0;
    for (String label : tfidf.keySet()) {
      List<double[]> testD = testData.get(label);
      for (double[] series : testD) {
        WordBag test = RePairFactory.seriesToGrammarWordBag("tmp", series, WINDOW_SIZE, PAA_SIZE,
            na.getCuts(ALPHABET_SIZE), STRATEGY, NORMALIZATION_THRESHOLD, BAG_STRATEGY);
        String testLabel = tu.classify(test, tfidf);
        if (label.equalsIgnoreCase(testLabel)) {
          positiveTestCounter++;
        }
        System.out.println("label " + label + ", series " + testSampleSize + ", positives "
            + positiveTestCounter);
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
