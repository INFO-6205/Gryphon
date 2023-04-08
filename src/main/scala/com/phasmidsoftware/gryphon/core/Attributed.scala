package com.phasmidsoftware.gryphon.core

trait Attributed[+A] {

    /**
     * An attribute.
     *
     * @return the value of the attribute, for example, a weight.
     */
    val attribute: A
}
