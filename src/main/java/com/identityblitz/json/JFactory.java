package com.identityblitz.json;

import scala.collection.convert.WrapAsScala$;
import scala.math.BigDecimal$;

import java.util.Map;

/**
  */
public class JFactory {

    private JFactory() {
    }

    public static JNum jNum(final long val) {
        return new JNum(BigDecimal$.MODULE$.apply(val));
    }

    public static JBool jBool(final boolean val) {
        return new JBool(val);
    }

    public static JStr jStr(final String val) {
        return new JStr(val);
    }

    public static JArr jArr(final JVal[] jVals) {
        return JArr$.MODULE$.apply(jVals);
    }

    public static JObj jObj(final String name, final JVal jVal) {
        return JObj$.MODULE$.apply(name, jVal);
    }

    public static JObj jObj(final Map<String, JVal> vals) {
        return JObj$.MODULE$.apply(WrapAsScala$.MODULE$.mapAsScalaMap(vals).toSeq());
    }
}
