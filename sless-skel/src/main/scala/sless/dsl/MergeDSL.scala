package sless.dsl

trait MergeDSL extends BaseDSL{

  def mergeSheets(cssSheets : Css*) : Css

}
