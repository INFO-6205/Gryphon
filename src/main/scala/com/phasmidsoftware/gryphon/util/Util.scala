package com.phasmidsoftware.gryphon.util

import scala.util.Try

/**
 * Utilities: especially functional utilities.
 */
object Util {

    /**
     * Method to get the value of an Option[X] but throwing a given exception rather than the usual NoSuchElement.
     *
     * @param xo an optional value of X (called by name).
     * @param t  a throwable.
     * @tparam X the underlying type of xo and the type of the result.
     * @return the value of xo or throws t.
     * @throws Throwable t
     */
    def getOrThrow[X](xo: => Option[X], t: => Throwable): X = xo.getOrElse(throw t)

    /**
     * Method to get the value of an Option[X] but returning a Try with the Failure case having a specific Throwable.
     *
     * @param xo an optional value of X (called by name).
     * @param t  a throwable.
     * @tparam X the underlying type of xo and the underlying type of the result.
     * @return the value of <code>Try(getOrThrow(xo, t))</code>.
     */
    def optionToTry[X](xo: => Option[X], t: => Throwable): Try[X] = Try(getOrThrow(xo, t))

    /**
     * Method to get the value of a X, ensuring that its value is not null. but returning a Try with the Failure case having a specific Throwable.
     *
     * @param x an optional value of X (called by name).
     * @param t a throwable.
     * @tparam X the type of x and the underlying type of the result.
     * @return the value of <code>optionToTry(Option(x), t)</code>.
     */
    def tryNonNull[X](x: => X, t: => Throwable): Try[X] = optionToTry(Option(x), t)


//    def resourceForClass(resourceName: String, clazz: Class[_] = getClass): Try[URL] = Option(clazz.getResource(resourceName)) match {
//        case Some(u) => Success(u)
//        case None => Failure(GraphException(s"$resourceName is not a valid resource for $clazz"))
//    }

//    def resource[C: ClassTag](resourceName: String): Try[URL] = resourceForClass(resourceName, implicitly[ClassTag[C]].runtimeClass)

}
