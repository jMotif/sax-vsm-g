package edu.hawaii.jmotif.gi.repair;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Hashtable;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.StringTokenizer;
import java.util.concurrent.atomic.AtomicInteger;
import net.seninp.jmotif.sax.NumerosityReductionStrategy;
import net.seninp.jmotif.sax.SAXProcessor;
import net.seninp.jmotif.sax.datastructures.SAXRecords;
import net.seninp.jmotif.sax.datastructures.SaxRecord;
import org.slf4j.LoggerFactory;
import ch.qos.logback.classic.Level;
import ch.qos.logback.classic.Logger;
import edu.hawaii.jmotif.gi.GrammarRuleRecord;
import edu.hawaii.jmotif.gi.GrammarRules;
import edu.hawaii.jmotif.text.WordBag;

public final class RePairFactory {

  private static final char SPACE = ' ';

  // logging stuff
  //
  private static Logger consoleLogger;
  private static Level LOGGING_LEVEL = Level.WARN;
  static {
    consoleLogger = (Logger) LoggerFactory.getLogger(RePairFactory.class);
    consoleLogger.setLevel(LOGGING_LEVEL);
  }

  //
  private static final SAXProcessor sp = new SAXProcessor();

  /**
   * Disable constructor.
   */
  private RePairFactory() {
    assert true;
  }

  /**
   * Converts a time series sample to word bags.
   * 
   * @param data
   * @param windowSize
   * @param paaSize
   * @param cuts
   * @param strategy
   * @param nThreshold
   * @return
   * @throws Exception
   */
  public static List<WordBag> labeledSeries2GrammarWordBags(Map<String, List<double[]>> data,
      int windowSize, int paaSize, double[] cuts, NumerosityReductionStrategy strategy,
      double nThreshold, BagConstructionStrategy bagStrategy) throws Exception {

    // make a map of resulting bags
    Map<String, WordBag> preRes = new HashMap<String, WordBag>();

    // process series one by one building word bags
    for (Entry<String, List<double[]>> e : data.entrySet()) {

      String classLabel = e.getKey();
      WordBag bag = new WordBag(classLabel);

      for (double[] series : e.getValue()) {
        WordBag cb = seriesToGrammarWordBag("tmp", series, windowSize, paaSize, cuts, strategy,
            nThreshold, bagStrategy);
        bag.mergeWith(cb);
      }

      preRes.put(classLabel, bag);
    }

    List<WordBag> res = new ArrayList<WordBag>();
    res.addAll(preRes.values());
    return res;
  }

  /**
   * Converts a single time series into a words bag.
   * 
   * @param label
   * @param series
   * @param windowSize
   * @param paaSize
   * @param cuts
   * @param strategy
   * @param nThreshold
   * @param bagStrategy
   * @return
   * @throws Exception
   */
  public static WordBag seriesToGrammarWordBag(String label, double[] series, int windowSize,
      int paaSize, double[] cuts, NumerosityReductionStrategy strategy, double nThreshold,
      BagConstructionStrategy bagStrategy) throws Exception {

    WordBag resultBag = new WordBag(label);

    SAXRecords saxData = sp
        .ts2saxViaWindow(series, windowSize, paaSize, cuts, strategy, nThreshold);
    saxData.buildIndex();

    // add all SAX words if ALL strategy THIS WILL NOT WORK FOR REDUCED and COMPRESSED strategies
    //
    if (BagConstructionStrategy.ALL == bagStrategy) {
      for (SaxRecord sr : saxData) {
        String word = String.valueOf(sr.getPayload());
        int frequency = sr.getIndexes().size();
        resultBag.addWord(word, frequency);
      }
    }

    @SuppressWarnings("unused")
    RePairRule rePairGrammar = RePairFactory.buildGrammar(saxData);
    RePairRule.expandRules();
    GrammarRules rules = RePairRule.toGrammarRulesData();

    for (GrammarRuleRecord r : rules) {

      // if the strategy is REDUCED we shall be adding basic tokens here
      //
      if (0 == r.getRuleNumber()) {

        if (BagConstructionStrategy.REDUCED == bagStrategy) {
          // extracting all basic tokens
          //
          GrammarRuleRecord r0 = rules.get(0);
          String[] split = r0.getRuleString().trim().split("\\s");
          for (String s : split) {
            if (s.startsWith("R")) {
              continue;
            }
            resultBag.addWord(s);
          }
        }

      }
      else {
        // adding the expanded rule
        //
        String str = r.getExpandedRuleString();
        resultBag.addWord(str, r.getOccurrences().size());
      }
    }

    return resultBag;
  }

  public static RePairRule buildGrammar(SAXRecords saxRecords) {

    consoleLogger.debug("Starting RePair with an input string of " + saxRecords.getIndexes().size()
        + " words.");

    RePairRule.numRules = new AtomicInteger(0);
    RePairRule.theRules = new Hashtable<Integer, RePairRule>();

    RePairRule theRule = new RePairRule();

    // get all indexes and sort them
    Set<Integer> index = saxRecords.getIndexes();
    Integer[] sortedSAXWords = index.toArray(new Integer[index.size()]);

    // two data structures
    //
    // 1.0. - the string
    ArrayList<Symbol> string = new ArrayList<Symbol>();
    // LinkedList<Symbol> string = new LinkedList<Symbol>();

    //
    // 2.0. - the digram frequency table, digram, frequency, and the first occurrence index
    DigramFrequencies digramFrequencies = new DigramFrequencies();

    // build data structures
    int stringPositionCounter = 0;
    for (Integer saxWordPosition : sortedSAXWords) {
      // i is the index of a symbol in the input discretized string
      // counter is the index in the grammar rule R0 string
      SaxRecord r = saxRecords.getByIndex(saxWordPosition);
      Symbol symbol = new Symbol(r, stringPositionCounter);
      // put it into the string
      string.add(symbol);
      // and into the index
      // take care about digram frequencies
      if (stringPositionCounter > 0) {

        StringBuffer digramStr = new StringBuffer();
        digramStr.append(string.get(stringPositionCounter - 1).toString()).append(SPACE)
            .append(string.get(stringPositionCounter).toString());

        DigramFrequencyEntry entry = digramFrequencies.get(digramStr.toString());
        if (null == entry) {
          digramFrequencies.put(new DigramFrequencyEntry(digramStr.toString(), 1,
              stringPositionCounter - 1));
        }
        else {
          digramFrequencies.incrementFrequency(entry, 1);
        }
      }
      // go on
      stringPositionCounter++;
    }

    consoleLogger.debug("String length " + string.size() + " unique digrams "
        + digramFrequencies.size());

    DigramFrequencyEntry entry;
    while ((entry = digramFrequencies.getTop()) != null && entry.getFrequency() >= 2) {

      // take the most frequent rule
      //
      // Entry<String, int[]> entry = entries.get(0);
      // DigramFrequencyEntry entry = digramFrequencies.getTop();

      consoleLogger.info("re-pair iteration, digram \"" + entry.getDigram() + "\", frequency: "
          + entry.getFrequency());

      consoleLogger.debug("Going to substitute the digram " + entry.getDigram()
          + " first occurring at position " + entry.getFirstOccurrence() + " with frequency "
          + entry.getFrequency() + ", '" + string.get(entry.getFirstOccurrence()) + SPACE
          + string.get(entry.getFirstOccurrence() + 1) + "'");

      // create new rule
      //
      RePairRule r = new RePairRule();
      r.setFirst(string.get(entry.getFirstOccurrence()));
      r.setSecond(string.get(entry.getFirstOccurrence() + 1));
      r.assignLevel();

      // substitute each digram entry with a rule
      //
      String digramToSubstitute = entry.getDigram();
      int currentIndex = entry.getFirstOccurrence();
      while (currentIndex < string.size() - 1) {

        StringBuffer currentDigram = new StringBuffer();
        currentDigram.append(string.get(currentIndex).toString()).append(SPACE)
            .append(string.get(currentIndex + 1).toString());

        if (digramToSubstitute.equalsIgnoreCase(currentDigram.toString())) {
          consoleLogger.debug(" next digram occurrence is at  " + currentIndex + ", '"
              + string.get(currentIndex) + SPACE + string.get(currentIndex + 1) + "'");

          // correct entries at left and right
          if (currentIndex > 0) {
            // taking care about immediate neighbor
            removeDigramFrequencyEntry(currentIndex - 1, string, digramFrequencies);
          }
          if (currentIndex < string.size() - 2) {
            removeDigramFrequencyEntry(currentIndex + 1, string, digramFrequencies);
          }

          // create the new guard to insert
          RePairGuard g = new RePairGuard(r);
          g.setStringPosition(string.get(currentIndex).getStringPosition());
          r.addPosition(string.get(currentIndex).getStringPosition());
          substituteDigramAt(currentIndex, g, string, digramFrequencies);

        }
        currentIndex++;
      }

      // // sort the entries of digram table by the size of indexes
      // entries = new ArrayList<Entry<String, int[]>>();
      // entries.addAll(digramFrequencies.entrySet());
      // Collections.sort(entries, new Comparator<Entry<String, int[]>>() {
      // @Override
      // public int compare(Entry<String, int[]> o1, Entry<String, int[]> o2) {
      // return -Integer.valueOf(o1.getValue()[0]).compareTo(Integer.valueOf(o2.getValue()[0]));
      // }
      // });

      consoleLogger.debug("*** iteration finished, top count "
          + digramFrequencies.getTop().getFrequency());
    }
    RePairRule.setRuleString(stringToDisplay(string));
    return theRule;
  }

  public static RePairRule buildGrammar(String inputString) {

    // consoleLogger.debug("Starting RePair with an input string of " +
    // saxRecords.getIndexes().size()
    // + " words.");

    RePairRule.numRules = new AtomicInteger(0);
    RePairRule.theRules = new Hashtable<Integer, RePairRule>();

    RePairRule theRule = new RePairRule();

    // two data structures
    //
    // 1.0. - the string
    ArrayList<Symbol> string = new ArrayList<Symbol>();
    // LinkedList<Symbol> string = new LinkedList<Symbol>();

    //
    // 2.0. - the digram frequency table, digram, frequency, and the first occurrence index
    DigramFrequencies digramFrequencies = new DigramFrequencies();

    // build data structures
    // tokenize the input string
    //
    StringTokenizer st = new StringTokenizer(inputString, " ");

    int stringPositionCounter = 0;

    // while there are tokens
    while (st.hasMoreTokens()) {

      String token = st.nextToken();

      Symbol symbol = new Symbol(token, stringPositionCounter);
      // put it into the string
      string.add(symbol);
      // and into the index
      // take care about digram frequencies
      if (stringPositionCounter > 0) {

        StringBuffer digramStr = new StringBuffer();
        digramStr.append(string.get(stringPositionCounter - 1).toString()).append(SPACE)
            .append(string.get(stringPositionCounter).toString());

        DigramFrequencyEntry entry = digramFrequencies.get(digramStr.toString());
        if (null == entry) {
          digramFrequencies.put(new DigramFrequencyEntry(digramStr.toString(), 1,
              stringPositionCounter - 1));
        }
        else {
          digramFrequencies.incrementFrequency(entry, 1);
        }
      }
      // go on
      stringPositionCounter++;
    }

    consoleLogger.debug("String length " + string.size() + " unique digrams "
        + digramFrequencies.size());

    DigramFrequencyEntry entry;
    while ((entry = digramFrequencies.getTop()) != null && entry.getFrequency() > 1) {

      // take the most frequent rule
      //
      // Entry<String, int[]> entry = entries.get(0);
      // DigramFrequencyEntry entry = digramFrequencies.getTop();

      consoleLogger.info("re-pair iteration, digram \"" + entry.getDigram() + "\", frequency: "
          + entry.getFrequency());

      consoleLogger.debug("Going to substitute the digram " + entry.getDigram()
          + " first occurring at position " + entry.getFirstOccurrence() + " with frequency "
          + entry.getFrequency() + ", '" + string.get(entry.getFirstOccurrence()) + SPACE
          + string.get(entry.getFirstOccurrence() + 1) + "'");

      // create new rule
      //
      RePairRule r = new RePairRule();
      r.setFirst(string.get(entry.getFirstOccurrence()));
      r.setSecond(string.get(entry.getFirstOccurrence() + 1));
      r.assignLevel();

      // substitute each digram entry with a rule
      //
      String digramToSubstitute = entry.getDigram();
      int currentIndex = entry.getFirstOccurrence();
      while (currentIndex < string.size() - 1) {

        StringBuffer currentDigram = new StringBuffer();
        currentDigram.append(string.get(currentIndex).toString()).append(SPACE)
            .append(string.get(currentIndex + 1).toString());

        if (digramToSubstitute.equalsIgnoreCase(currentDigram.toString())) {
          consoleLogger.debug(" next digram occurrence is at  " + currentIndex + ", '"
              + string.get(currentIndex) + SPACE + string.get(currentIndex + 1) + "'");

          // correct entries at left and right
          if (currentIndex > 0) {
            // taking care about immediate neighbor
            removeDigramFrequencyEntry(currentIndex - 1, string, digramFrequencies);
          }
          if (currentIndex < string.size() - 2) {
            removeDigramFrequencyEntry(currentIndex + 1, string, digramFrequencies);
          }

          // create the new guard to insert
          RePairGuard g = new RePairGuard(r);
          g.setStringPosition(string.get(currentIndex).getStringPosition());
          r.addPosition(string.get(currentIndex).getStringPosition());
          substituteDigramAt(currentIndex, g, string, digramFrequencies);

        }
        currentIndex++;
      }

      // // sort the entries of digram table by the size of indexes
      // entries = new ArrayList<Entry<String, int[]>>();
      // entries.addAll(digramFrequencies.entrySet());
      // Collections.sort(entries, new Comparator<Entry<String, int[]>>() {
      // @Override
      // public int compare(Entry<String, int[]> o1, Entry<String, int[]> o2) {
      // return -Integer.valueOf(o1.getValue()[0]).compareTo(Integer.valueOf(o2.getValue()[0]));
      // }
      // });

      consoleLogger.debug("*** iteration finished, top count "
          + digramFrequencies.getTop().getFrequency());
    }

    RePairRule.setRuleString(stringToDisplay(string));

    RePairRule.expandRules();

    return theRule;

  }

  private static void substituteDigramAt(Integer currentIndex, RePairGuard g,
      ArrayList<Symbol> string, DigramFrequencies digramFrequencies) {

    // create entry for two new digram
    //
    StringBuffer digram = new StringBuffer();
    digram.append(string.get(currentIndex).toString()).append(SPACE)
        .append(string.get(currentIndex + 1));

    consoleLogger.debug("  substituting the digram " + digram + " at " + currentIndex + " with "
        + g.toString());

    if (currentIndex > 0) {
      consoleLogger.debug("   previous " + string.get(currentIndex - 1).toString());
    }
    if (currentIndex < string.size() - 2) {
      consoleLogger.debug("   next " + string.get(currentIndex + 2).toString());
    }

    // update the new left digram frequency
    //
    if (currentIndex > 0) {
      StringBuffer newDigram = new StringBuffer();
      newDigram.append(string.get(currentIndex - 1).toString()).append(SPACE).append(g.toString());
      consoleLogger.debug("   updating the frequency entry for digram " + newDigram.toString());
      DigramFrequencyEntry entry = digramFrequencies.get(newDigram.toString());
      if (null == entry) {
        digramFrequencies.put(new DigramFrequencyEntry(newDigram.toString(), 1, currentIndex - 1));
      }
      else {
        digramFrequencies.incrementFrequency(entry, 1);
        if (currentIndex - 1 < entry.getFirstOccurrence()) {
          entry.setFirstOccurrence(currentIndex - 1);
        }
      }
    }

    // update the new right digram frequency
    //
    if (currentIndex < string.size() - 2) {
      StringBuffer newDigram = new StringBuffer();
      newDigram.append(g.toString()).append(SPACE).append(string.get(currentIndex + 2));
      consoleLogger.debug("   updating the frequency entry for digram " + newDigram.toString());
      DigramFrequencyEntry entry = digramFrequencies.get(newDigram.toString());
      if (null == entry) {
        digramFrequencies.put(new DigramFrequencyEntry(newDigram.toString(), 1, currentIndex));
      }
      else {
        digramFrequencies.incrementFrequency(entry, 1);
        if (currentIndex + 1 < entry.getFirstOccurrence()) {
          entry.setFirstOccurrence(currentIndex);
        }
      }
    }

    // remove and substitute
    //
    // 1. decrease to be substituted digram frequency
    //
    consoleLogger.debug("   updating the frequency entry for digram " + digram.toString());
    DigramFrequencyEntry entry = digramFrequencies.get(digram.toString());
    if (1 == entry.getFrequency()) {
      consoleLogger.debug("    removing the frequency entry");
      digramFrequencies.remove(digram.toString());
    }
    else {
      consoleLogger.debug("    setting the frequency entry to "
          + Integer.valueOf(entry.getFrequency() - 1));
      digramFrequencies.incrementFrequency(entry, -1);
      if (currentIndex == entry.getFirstOccurrence()) {
        consoleLogger.debug("    this was an index entry, finding another digram index...");
        for (int i = currentIndex + 1; i < string.size() - 1; i++) {
          StringBuffer cDigram = new StringBuffer();
          cDigram.append(string.get(i).toString()).append(SPACE)
              .append(string.get(i + 1).toString());
          if (digram.toString().equals(cDigram.toString())) {
            consoleLogger.debug("   for digram " + cDigram.toString() + " new index " + i);
            entry.setFirstOccurrence(i);
            break;
          }
        }
      }
    }
    // 2. substitute
    string.set(currentIndex, g);
    consoleLogger.debug("   deleting symbol " + string.get(currentIndex + 1).toString() + " at "
        + Integer.valueOf(currentIndex + 1));
    // 3. delete
    string.remove(Integer.valueOf(currentIndex + 1).intValue());

    // need to take care about all the indexes
    // as all the indexes above _currentIndex_ shall be shifted by -1
    // NO NEED for TLinkedList<Symbol> string = new TLinkedList<Symbol>();
    // HashMap<String, int[]> digramFrequencies = new HashMap<String, int[]>();
    //
    // traverse the string to the right decreasing indexes
    for (Entry<String, DigramFrequencyEntry> e : digramFrequencies.getEntries().entrySet()) {
      int idx = e.getValue().getFirstOccurrence();
      if (idx >= currentIndex + 2) {
        // consoleLogger.debug("   shifting entry for  " + e.getValue().getDigram() + " from "
        // + e.getValue().getFirstOccurrence() + " to " + Integer.valueOf(idx - 1));
        e.getValue().setFirstOccurrence(idx - 1);
      }
    }

  }

  private static void removeDigramFrequencyEntry(int index, ArrayList<Symbol> string,
      DigramFrequencies digramFrequencies) {

    StringBuffer digramToRemove = new StringBuffer();
    digramToRemove.append(string.get(index).toString()).append(SPACE)
        .append(string.get(index + 1).toString());

    DigramFrequencyEntry digramEntry = digramFrequencies.get(digramToRemove.toString());

    if (digramEntry.getFrequency() == 1) {
      digramFrequencies.remove(digramToRemove.toString());
      consoleLogger.debug("  completely removing the frequency entry for digram "
          + digramToRemove.toString() + " at position " + index);
    }
    else {
      consoleLogger.debug("  decreasing the frequency entry for digram "
          + digramToRemove.toString() + " at position " + index + " from "
          + digramEntry.getFrequency() + " to " + Integer.valueOf(digramEntry.getFrequency() - 1));
      digramFrequencies.incrementFrequency(digramEntry, -1);
      if (index == digramEntry.getFirstOccurrence()) {
        consoleLogger.debug("  this was an index entry, finding another digram index...");
        for (int i = index + 1; i < string.size() - 1; i++) {
          StringBuffer cDigram = new StringBuffer();
          cDigram.append(string.get(i).toString()).append(SPACE)
              .append(string.get(i + 1).toString());
          if (digramToRemove.toString().equals(cDigram.toString())) {
            consoleLogger.debug("   for digram " + cDigram.toString() + " new index " + i);
            digramEntry.setFirstOccurrence(i);
            break;
          }
        }
      }
    }

  }

  private static String stringToDisplay(ArrayList<Symbol> string) {
    StringBuffer sb = new StringBuffer();
    for (int i = 0; i < string.size(); i++) {
      sb.append(string.get(i).toString()).append(SPACE);
    }
    return sb.toString();
  }

  public static List<WordBag> labeledSeries2ConcatenatedGrammarWordBags(
      Map<String, List<double[]>> data, int windowSize, int paaSize, double[] cuts,
      NumerosityReductionStrategy strategy, double nThreshold, BagConstructionStrategy bagStrategy)
      throws Exception {

    // make a map of resulting bags
    Map<String, WordBag> preRes = new HashMap<String, WordBag>();

    // process series one by one building word bags
    for (Entry<String, List<double[]>> e : data.entrySet()) {

      String classLabel = e.getKey();
      WordBag bag = new WordBag(classLabel);

      // compute the length of the new time-series and identify the break points
      ArrayList<Integer> skips = new ArrayList<Integer>();
      int tl = 0;
      for (double[] arr : e.getValue()) {
        tl = tl + arr.length;
        for (int i = tl - windowSize; i < tl; i++) {
          skips.add(i - 1);
        }
      }

      // compose the time-series
      double[] ts = new double[tl];
      int globalI = 0;
      for (double[] arr : e.getValue()) {
        for (int i = 0; i < arr.length; i++) {
          ts[globalI] = arr[i];
          globalI++;
        }
      }

      // convert the ts to list of SAX words
      SAXRecords saxData = sp.ts2saxViaWindowSkipping(ts, windowSize, paaSize, cuts, strategy,
          nThreshold, skips);
      // no clean-up those that overlap
      // saxData.excludePositions(skips);
      saxData.buildIndex();

      // build the grammar
      @SuppressWarnings("unused")
      RePairRule rePairGrammar = RePairFactory.buildGrammarWithSkips(saxData, skips);
      RePairRule.expandRules();
      GrammarRules rules = RePairRule.toGrammarRulesData();

      for (GrammarRuleRecord r : rules) {

        // if the strategy is REDUCED we shall be adding basic tokens here
        //
        if (0 == r.getRuleNumber()) {

          if (BagConstructionStrategy.REDUCED == bagStrategy) {
            // extracting all basic tokens
            //
            GrammarRuleRecord r0 = rules.get(0);
            String[] split = r0.getRuleString().trim().split("\\s");
            for (String s : split) {
              if (s.startsWith("R")) {
                continue;
              }
              bag.addWord(s);
            }
          }

        }
        else {

          // adding the expanded rule
          //
          String str = r.getExpandedRuleString();
          bag.addWord(str, r.getOccurrences().size());
        }
      }
      preRes.put(classLabel, bag);
    }

    List<WordBag> res = new ArrayList<WordBag>();
    res.addAll(preRes.values());
    return res;

  }

  public static RePairRule buildGrammarWithSkips(SAXRecords saxRecords, ArrayList<Integer> skips) {

    consoleLogger.debug("Starting RePair with an input string of " + saxRecords.getIndexes().size()
        + " words.");

    HashSet<Integer> skipSet = new HashSet<Integer>();
    skipSet.addAll(skips);

    RePairRule.numRules = new AtomicInteger(0);
    RePairRule.theRules = new Hashtable<Integer, RePairRule>();

    RePairRule theRule = new RePairRule();

    // get all indexes and sort them
    Set<Integer> index = saxRecords.getIndexes();
    Integer[] sortedSAXWords = index.toArray(new Integer[index.size()]);

    // two data structures
    //
    // 1.0. - the string
    ArrayList<Symbol> string = new ArrayList<Symbol>();
    // LinkedList<Symbol> string = new LinkedList<Symbol>();

    //
    // 2.0. - the digram frequency table, digram, frequency, and the first occurrence index
    DigramFrequencies digramFrequencies = new DigramFrequencies();

    // build data structures
    int stringPositionCounter = 0;
    for (Integer saxWordPosition : sortedSAXWords) {
      // if (stringPositionCounter == 260) {
      // System.out.println("gotcha!");
      // }
      // i is the index of a symbol in the input discretized string
      // counter is the index in the grammar rule R0 string
      SaxRecord r = saxRecords.getByIndex(saxWordPosition);
      Symbol symbol = new Symbol(r, stringPositionCounter);
      // / MOD *****
      if (skipSet.contains(saxWordPosition + 1)) {
        symbol.connectRight = false;
      }
      else if (skipSet.contains(saxWordPosition - 1)) {
        symbol.connectLeft = false;
        string.get(stringPositionCounter - 1).connectRight = false;
      }
      // put it into the string
      string.add(symbol);
      // and into the index
      // take care about digram frequencies
      // / MOD *****
      if (stringPositionCounter > 0 && symbol.connectLeft) {

        // create new digram
        StringBuffer digramStr = new StringBuffer();
        digramStr.append(string.get(stringPositionCounter - 1).toString()).append(SPACE)
            .append(string.get(stringPositionCounter).toString());

        // add digram to the frequency table
        DigramFrequencyEntry entry = digramFrequencies.get(digramStr.toString());
        if (null == entry) {
          digramFrequencies.put(new DigramFrequencyEntry(digramStr.toString(), 1,
              stringPositionCounter - 1));
        }
        else {
          digramFrequencies.incrementFrequency(entry, 1);
        }

      }
      // go on
      stringPositionCounter++;
    }

    consoleLogger.debug("String length " + string.size() + " unique digrams "
        + digramFrequencies.size());

    DigramFrequencyEntry entry;
    while ((entry = digramFrequencies.getTop()) != null && entry.getFrequency() >= 2) {

      // take the most frequent rule
      //
      // Entry<String, int[]> entry = entries.get(0);
      // DigramFrequencyEntry entry = digramFrequencies.getTop();

      consoleLogger.info("re-pair iteration, digram \"" + entry.getDigram() + "\", frequency: "
          + entry.getFrequency());
      if (entry.getDigram().equalsIgnoreCase("bbg aff") && entry.getFrequency() == 5) {
        System.out.println("gotcha!");
      }

      consoleLogger.debug("Going to substitute the digram " + entry.getDigram()
          + " first occurring at position " + entry.getFirstOccurrence() + " with frequency "
          + entry.getFrequency() + ", '" + string.get(entry.getFirstOccurrence()) + SPACE
          + string.get(entry.getFirstOccurrence() + 1) + "'");

      // create new rule
      //
      RePairRule r = new RePairRule();
      r.setFirst(string.get(entry.getFirstOccurrence()));
      r.setSecond(string.get(entry.getFirstOccurrence() + 1));
      r.assignLevel();

      // substitute each digram entry with a rule
      //
      String digramToSubstitute = entry.getDigram();
      int currentIndex = entry.getFirstOccurrence();
      while (currentIndex < string.size() - 1) {

        // searching for the digram
        //
        if (!(string.get(currentIndex).connectRight) && !(string.get(currentIndex + 1).connectLeft)) {
          currentIndex++;
          continue;
        }

        StringBuffer currentDigram = new StringBuffer();
        currentDigram.append(string.get(currentIndex).toString()).append(SPACE)
            .append(string.get(currentIndex + 1).toString());

        if (digramToSubstitute.equalsIgnoreCase(currentDigram.toString())) {
          consoleLogger.debug(" next digram occurrence is at  " + currentIndex + ", '"
              + string.get(currentIndex) + SPACE + string.get(currentIndex + 1) + "'");

          // correct entries at left and right
          if (currentIndex > 0 && string.get(currentIndex - 1).connectRight) {
            // taking care about immediate neighbor
            removeDigramFrequencyEntry(currentIndex - 1, string, digramFrequencies);
          }
          if (currentIndex < string.size() - 2 && string.get(currentIndex + 2).connectLeft) {
            removeDigramFrequencyEntry(currentIndex + 1, string, digramFrequencies);
          }

          // create the new guard to insert
          RePairGuard g = new RePairGuard(r);
          g.setStringPosition(string.get(currentIndex).getStringPosition());
          r.addPosition(string.get(currentIndex).getStringPosition());
          if (!(string.get(currentIndex).connectLeft)) {
            g.connectLeft = false;
          }
          if (!(string.get(currentIndex + 1).connectRight)) {
            g.connectRight = false;
          }

          // place it into string
          substituteDigramAtWithSkips(currentIndex, g, string, digramFrequencies, skipSet);

        }
        currentIndex++;
      }

      consoleLogger.debug("*** iteration finished, top count "
          + digramFrequencies.getTop().getFrequency());
    }
    RePairRule.setRuleString(stringToDisplay(string));
    return theRule;
  }

  private static void substituteDigramAtWithSkips(Integer currentIndex, RePairGuard g,
      ArrayList<Symbol> string, DigramFrequencies digramFrequencies, HashSet<Integer> skipSet) {

    // create entry for two new digram
    //
    StringBuffer digram = new StringBuffer();
    digram.append(string.get(currentIndex).toString()).append(SPACE)
        .append(string.get(currentIndex + 1));

    consoleLogger.debug("  substituting the digram " + digram + " at " + currentIndex + " with "
        + g.toString());

    if (currentIndex > 0) {
      consoleLogger.debug("   previous " + string.get(currentIndex - 1).toString());
    }
    if (currentIndex < string.size() - 2) {
      consoleLogger.debug("   next " + string.get(currentIndex + 2).toString());
    }

    // update the new left digram frequency
    //
    if (currentIndex > 0 && string.get(currentIndex - 1).connectRight) {
      StringBuffer newDigram = new StringBuffer();
      newDigram.append(string.get(currentIndex - 1).toString()).append(SPACE).append(g.toString());
      consoleLogger.debug("   updating the frequency entry for digram " + newDigram.toString());
      DigramFrequencyEntry entry = digramFrequencies.get(newDigram.toString());
      if (null == entry) {
        digramFrequencies.put(new DigramFrequencyEntry(newDigram.toString(), 1, currentIndex - 1));
      }
      else {
        digramFrequencies.incrementFrequency(entry, 1);
        if (currentIndex - 1 < entry.getFirstOccurrence()) {
          entry.setFirstOccurrence(currentIndex - 1);
        }
      }
    }

    // update the new right digram frequency
    //
    if (currentIndex < string.size() - 2 && string.get(currentIndex + 2).connectLeft) {
      StringBuffer newDigram = new StringBuffer();
      newDigram.append(g.toString()).append(SPACE).append(string.get(currentIndex + 2));
      consoleLogger.debug("   updating the frequency entry for digram " + newDigram.toString());
      DigramFrequencyEntry entry = digramFrequencies.get(newDigram.toString());
      if (null == entry) {
        digramFrequencies.put(new DigramFrequencyEntry(newDigram.toString(), 1, currentIndex));
      }
      else {
        digramFrequencies.incrementFrequency(entry, 1);
        if (currentIndex + 1 < entry.getFirstOccurrence()) {
          entry.setFirstOccurrence(currentIndex);
        }
      }
    }

    // remove and substitute
    //
    // 1. decrease to be substituted digram frequency
    //
    consoleLogger.debug("   updating the frequency entry for digram " + digram.toString());
    DigramFrequencyEntry entry = digramFrequencies.get(digram.toString());
    if (null == entry) {
      System.out.println("gotcha!");
    }
    if (1 == entry.getFrequency()) {
      consoleLogger.debug("    removing the frequency entry");
      digramFrequencies.remove(digram.toString());
    }
    else {
      consoleLogger.debug("    setting the frequency entry to "
          + Integer.valueOf(entry.getFrequency() - 1));
      digramFrequencies.incrementFrequency(entry, -1);
      if (currentIndex == entry.getFirstOccurrence()) {
        consoleLogger.debug("    this was an index entry, finding another digram index...");
        for (int i = currentIndex + 1; i < string.size() - 1; i++) {
          StringBuffer cDigram = new StringBuffer();
          cDigram.append(string.get(i).toString()).append(SPACE)
              .append(string.get(i + 1).toString());
          if (digram.toString().equals(cDigram.toString())) {
            consoleLogger.debug("   for digram " + cDigram.toString() + " new index " + i);
            entry.setFirstOccurrence(i);
            break;
          }
        }
      }
    }
    // 2. substitute
    string.set(currentIndex, g);
    consoleLogger.debug("   deleting symbol " + string.get(currentIndex + 1).toString() + " at "
        + Integer.valueOf(currentIndex + 1));
    // 3. delete
    string.remove(Integer.valueOf(currentIndex + 1).intValue());

    // need to take care about all the indexes
    // as all the indexes above _currentIndex_ shall be shifted by -1
    // NO NEED for TLinkedList<Symbol> string = new TLinkedList<Symbol>();
    // HashMap<String, int[]> digramFrequencies = new HashMap<String, int[]>();
    //
    // traverse the string to the right decreasing indexes
    for (Entry<String, DigramFrequencyEntry> e : digramFrequencies.getEntries().entrySet()) {
      int idx = e.getValue().getFirstOccurrence();
      if (idx >= currentIndex + 2) {
        // consoleLogger.debug("   shifting entry for  " + e.getValue().getDigram() + " from "
        // + e.getValue().getFirstOccurrence() + " to " + Integer.valueOf(idx - 1));
        e.getValue().setFirstOccurrence(idx - 1);
      }
    }

  }

}
