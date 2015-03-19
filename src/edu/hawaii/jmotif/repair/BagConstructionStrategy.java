package edu.hawaii.jmotif.repair;

/**
 * The SAX Collection srategy.
 * 
 * @author Pavel Senin
 * 
 */
public enum BagConstructionStrategy {

  /** All words simply dumped into the bag along with expanded rules. */
  ALL(0),

  /** Only stand-alone words and expanded rules. */
  REDUCED(1),

  /** Only rule sequences. */
  COMPRESSED(2);

  private final int index;

  /**
   * Constructor.
   * 
   * @param index The strategy index.
   */
  BagConstructionStrategy(int index) {
    this.index = index;
  }

  /**
   * Gets the integer index of the instance.
   * 
   * @return integer key of the instance.
   */
  public int index() {
    return index;
  }

  /**
   * Makes a strategy out of integer. 0 stands for NONE, 1 for EXACT, and 3 for MINDIST.
   * 
   * @param value the key value.
   * @return the new Strategy instance.
   */
  public static BagConstructionStrategy fromValue(int value) {
    switch (value) {
    case 0:
      return BagConstructionStrategy.ALL;
    case 1:
      return BagConstructionStrategy.REDUCED;
    case 2:
      return BagConstructionStrategy.COMPRESSED;
    default:
      throw new RuntimeException("Unknown index:" + value);
    }
  }

  public String toString() {
    switch (this.index) {
    case 0:
      return "ALL";
    case 1:
      return "REDUCED";
    case 2:
      return "COMPRESSED";
    default:
      throw new RuntimeException("Unknown index:" + this.index);
    }
  }
}
