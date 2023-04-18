package com.phasmidsoftware.gryphon.util

/**
 * Utilities: especially functional utilities.
 */
object Util {

    /**
     * Method to get the value of an Option[X] but throwing a given exception rather than the usual NoSuchElement.
     *
     * @param xo an optional value of X.
     * @param t  a throwable.
     * @tparam X the underlying type of xo and the type of the result.
     * @return the value of xo or throws t.
     * @throws Throwable t
     */
    def getOrThrow[X](xo: Option[X], t: => Throwable): X = xo.getOrElse(throw t)
}
